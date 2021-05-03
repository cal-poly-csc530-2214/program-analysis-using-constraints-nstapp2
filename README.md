#Program Analysis as Constraint Solving?

Oh gosh, it's monday already. What did I do? I made a program that will take a CFG and convert to 2nd order constraints. I was hoping to go further but it seems time has escaped me.

### A rackety CFG: (taken from the first example in the paper)
	(list
	 (node
	  0
	  (list (seteq 'x -50))
	  '(1))
	 (node
	  1
	  (list (less-than 'x 0))
	  '(2 3))
	 (node
	  2
	  (list
	   (add 'x 'y 'x)
	   (add 'y 'y 1))
	  '(1))
	 (node
	  3
	  (list
	   (assertion (greater-than 'y 0)))
	  '()))
### Generated Constraints
	true => -50/x
	x < 0 => (y + x)/x, (y + 1)/y
	x >= 0 => y > 0

### Another rackety CFG:
	(list
	 (node
	  0
	  (list (seteq 'x 0) (seteq 'y 5))
	  '(1))
	 (node
	  1
	  (list (greater-than 'y 0))
	  '(2 3))
	 (node
	  2
	  (list
	   (add 'x 'x 'y)
	   (add 'y 'y -1))
	  '(1))
	 (node
	  3
	  (list
	   (assertion
	    (less-than-or-equal 'x 15)))
	  '()))
### Generated Constraints
	true => 0/x, 5/y
	y > 0 => (x + y)/x, (y + -1)/y
	y <= 0 => x <= 15


