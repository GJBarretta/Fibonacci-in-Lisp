;;;CSC 345 Homework 2
;;;Giuseppe Barretta

;;function 1: sum the arguments recursively
(defun sum (n m)
  
        ;;perform a type-check to make sure the arguments are integers
  (cond ((or (not (integerp n)) (not (integerp m))) nil)
	
	;;this function converges both arguments towards zero, adding or subtracting 1 at a time 
	;;based on the sign of the integer. once both arguments are 0 the function winds back
	((and (= 0 n) (= 0 m)) 0)
	((> m 0) (1+ (sum n (1- m))))
	((> n 0) (1+ (sum (1- n) m)))
	((< m 0) (1- (sum n (1+ m))))
	((< n 0) (1- (sum (1+ n) m)))))

;;function 2: replace argument 1 with argument 2 in the list with deep recursion
(defun my-replace (e1 e2 L)
  (cond ((endp L) nil)

	;;check to see if the first element has the same structure as argument e1
	((equal e1 (first L)) (cons e2 (my-replace e1 e2 (rest L))))

	;;check to see if the first element of the list is itself a list -- if it is then
	;;recurse on that list in order to replace all e1 elements with e2 within the list
	((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))

	;;else just recurse on the rest of the list changing nothing
        (t (cons (first L) (my-replace e1 e2 (rest L))))))

;;function 3: naive implementation of fibonnaci 
(defun fibonacci (m)

        ;;base case: if m is 0 or 1, return m
  (cond ((or (= 0 m) (= 1 m)) m)
	;;recursive case: recursively return the sum of the previous 2 terms 
	(t (+ (fibonacci (1- m)) (fibonacci (- m 2))))))

;;function 4: tail-recursive implementation of fibonnaci. "private" function
  (defun fib-tr (m f l)

          ;;base case
    (cond ((= m 0) f)

	  ;;recursive case: f becomes l, l becomes the sum of f and l
          (t (fib-tr (1- m) l (+ f l)))))

;;function 4: tail-recursive implementation of fibonnaci. "public" function
(defun fibonacci-TR (m)
  (fib-tr m 0 1))
