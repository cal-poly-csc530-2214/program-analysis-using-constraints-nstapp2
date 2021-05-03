#lang typed/racket

(define-type num (U Real Symbol))
(define-type stmt (U seteq bool-expr add assertion))
(define-type bool-expr (U less-than less-than-or-equal greater-than greater-than-or-equal))
(define-type Index Integer)

(struct add ([target : Symbol] [left : num] [right : num])#:transparent)
(struct seteq ([target : Symbol] [val : num])#:transparent)
(struct less-than ([target : num] [compare-to : num])#:transparent)
(struct less-than-or-equal ([target : num] [compare-to : num])#:transparent)
(struct greater-than ([target : num] [compare-to : num])#:transparent)
(struct greater-than-or-equal ([target : num] [compare-to : num])#:transparent)
(struct node ([index : Index] [stmts : (Listof stmt)] [children : (Listof Integer)])#:transparent)
(struct assertion ([st : (U greater-than greater-than-or-equal less-than)])#:transparent)
(struct binop ([op : Symbol] [left : num] [right : num])#:transparent)
(struct sub ([val : (U binop num)] [var : Symbol])#:transparent)
;; a -> b
(struct impl ([a : (Listof (U 'true sub bool-expr))]
              [b : (Listof (U sub bool-expr))])#:transparent)
(struct edge ([parent : Index] [child : Index] [cond : (U Boolean 'N-A)])#:transparent)

;(define-type value (U Boolean num Void))

;(define-type bindings (Listof (Pairof Symbol Real)))

(define (get-node-by-index [i : Index] [cfg : (Listof node)]) : node
  (match cfg
    [(cons first rest) (if (equal? i (node-index first)) first (get-node-by-index i rest))]
    ['() (error 'get-node-by-index "index ~a not found" i)]))

(define (convert-stmt [st : stmt]) : (U sub bool-expr)
  (match st
    [(assertion as) as]
    [(seteq targ val) (sub val targ)]
    [(add targ left right) (sub (binop '+ left right) targ)]
    [_ (cast st (U bool-expr))]))

(define (make-first-impl [first-node : node]) : impl
  (impl (list 'true)
        (map convert-stmt (node-stmts first-node))))

(define (opposite-day
         [things : (Listof (U 'true sub bool-expr))]) : (Listof (U 'true sub bool-expr))
  (match things
    [(cons first rest)
     (match first
       [(less-than t c) (cons (greater-than-or-equal t c) (opposite-day rest))]
       [(less-than-or-equal t c) (cons (greater-than t c) (opposite-day rest))]
       [(greater-than-or-equal t c) (cons (less-than t c) (opposite-day rest))]
       [(greater-than t c) (cons (less-than-or-equal t c) (opposite-day rest))])]
    ['() '()]))

(define (make-impl [node1 : node] [node2 : node] [tf : (U 'N-A Boolean)]) : impl
  (impl (if (equal? tf #f)
            (opposite-day (map convert-stmt (node-stmts node1)))
            (map convert-stmt (node-stmts node1)))
        (map convert-stmt (node-stmts node2))))

(define (convert [e : edge] [cfg : (Listof node)]) : impl
  (if (equal? e (edge -1 0 'N-A))
      (make-first-impl (get-node-by-index 0 cfg))
      (make-impl (get-node-by-index (edge-parent e) cfg)
                 (get-node-by-index (edge-child e) cfg)
                 (edge-cond e))))

(define (get-edges [cfg : (Listof node)]) : (Listof edge)
  (match cfg
    [(cons (node i _ children) rest)
     (define num-children (length children))
     (if (equal? i 0)
         (append (list (edge -1 0 'N-A))
                 (map
                  (lambda ([ci : Index]) : edge
                    (edge i ci
                          (if (equal? num-children 1)
                              'N-A
                              (next-cond))))
                  children)
                 (get-edges rest))
         (append
          (map
           (lambda ([ci : Index]) : edge
             (edge i ci
                   (if (equal? num-children 1)
                       'N-A
                       (next-cond))))
           children)
          (get-edges rest)))]
    ['() '()]))

(define (sub-within?
         [stuff : (Listof (U 'true sub bool-expr))]) : Boolean
  (match stuff
    [(cons (sub _ _) rest) #t]
    [(cons first rest) (sub-within? rest)]
    ['() #f]))

;; throw out the ones that have substitutions on the LHS
(define (cull-impls [impls : (Listof impl)]) : (Listof impl)
  (match impls
    [(cons first rest)
     (if (sub-within? (impl-a first))
         (cull-impls rest)
         (cons first (cull-impls rest)))]
    ['() '()]))

(define (convert-to-2nd-order-constraints [cfg : (Listof node)]) : (Listof impl)
  (cull-impls
   (map
    (lambda ([e : edge]) : impl
      (convert e cfg))
    (get-edges cfg))))

(define (to-string-2nd-order-constraints [impls : (Listof impl)]) : String
  (define (lst-to-str [lst : (Listof String)]) : String
    (match lst
      ['() ""]
      [(cons first rest) (string-append first ", " (lst-to-str rest))]))
  (define (to-string-impl-list-item [a : (U 'true sub bool-expr)]) : String
    (define (to-string-val [val : (U binop num)]) : String
      (match val
        [(binop sym left right) (string-append
                                 (to-string-val left)
                                 (~a sym)
                                 (to-string-val right))]
        [other (~a other)]))
    (match a
      ['true "true"]
      [(sub val var) (string-append
                      (to-string-val val)
                      "/"
                      (~a var))]
      [(less-than t c) (string-append (~a t) " < " (~a c))]
      [(less-than-or-equal t c) (string-append (~a t) " <= " (~a c))]
      [(greater-than t c) (string-append (~a t) " > " (~a c))]
      [(greater-than-or-equal t c) (string-append (~a t) " >= " (~a c))]))
  (match impls
    [(cons (impl left right) rest)
     (define leftstr (lst-to-str
                      (map
                       to-string-impl-list-item
                       left)))
     (define rightstr (lst-to-str
                      (map
                       to-string-impl-list-item
                       right)))
     (string-append
      (substring leftstr 0 (- (string-length leftstr) 2))
      " => "
      (substring rightstr 0 (- (string-length rightstr) 2))
      "\n"
      (to-string-2nd-order-constraints rest))]
    ['() ""]))

#;(define curr-index (box 0))
#;(define (fresh-index) : Integer
  (define res (unbox curr-index))
  (set-box! curr-index (+ res 1))
  res)

(define curr-cond (box #t))
(define (next-cond) : Boolean
  (define res (unbox curr-cond))
  (set-box! curr-cond (not res))
  res)

(define CFG
  (list
   (node 0 (list (seteq 'x -50)) '(1))
   (node 1 (list (less-than 'x 0)) '(2 3))
   (node 2 (list (add 'x 'y 'x)
                 (add 'y 'y 1)) '(1))
   (node 3 (list (assertion (greater-than 'y 0))) '())))

(convert-to-2nd-order-constraints CFG)
(display (to-string-2nd-order-constraints (convert-to-2nd-order-constraints CFG)))

;; print 2nd order constraints nicely
;; convert to 1st order and call it a day




