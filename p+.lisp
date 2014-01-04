(defun p+(poly1 poly2)
	(format t "~%poly1 ~A" poly1)
	(format t "~%poly2 ~A" poly2)
;;(-2x^3 + 3y +z + 5 ) ’((-2 3 x) (3 1 y) (1 1 z) (5 0 0)）
	(let ((result (cons (+ (car poly1) (car poly2))
				(cons (+ (cadr poly1) (cadr poly2))
				(cons (caddr poly1) ())))))
		(print result)
	)
)




(defun comp(poly1 poly2)
	(let ((p1 (caddr poly1)) 
			(p2 (caddr poly2)))
		(if(eq p1 p2)
			(print t)
			(print nil)
			)
		)

	)

(defun try()
	(let* ((result '(2)))		
		(cons 1 result)
		(print result)
		)
	)