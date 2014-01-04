;;new try
(defun poly+ (poly1 poly2)

	(format t "~%poly1 ~A" poly1)
	(format t "~%poly2 ~A" poly2)

	(let ((l1 poly1) (l2 poly2))
		(labels (
			;'((1 2 x)) '((1 2 y))
			 	(main (p1 p2) 
			 		(if (null p1)
			 			()
			 			(let ((pass (test (car p1) p2))) 
			 				 (if (listp pass) 
			 				 	(cons pass (main (cdr p1) p2))
			 				 	(cons (car p1) (main (cdr p1) p2)))
			 				)))
			 	;'(1 2 x) '((1 2 y))
				(test (l1 l2) (if (null l2)
	 					t
	 					(if (test2 (cdr l1) (cdar l2))	;size!!!一样测，不一样不测
	 						(cons (+ (car l1) (caar l2)) (cdr l1))
	 						(test l1 (cdr l2)))
	 						)

	 					)
				;'(2 x 2 y) '(2 y 2 x)
				(test2 (l1 l2) (if (null l1)
								t				
								(if (test3 (cons (car l1) (list (cadr l1))) l2)	
									(if (= 2 (list-length l1))
										(test2 () l2)
										(test2 (cddr l1) l2))
									nil	
									)									
								
					 )
				)
				;'(2 x) '(2 y 2 x)
				(test3 (l1 l2) (if (null l2)
								nil
								(if (equal l1 (cons (car l2) (list (cadr l2))))
									t
									(if (= 2 (list-length l2))
										(test3 l1 ())
										(test3 l1 (cddr l2))
										
									
									)
								)
					))
				(result (l1 l2) (if (null l1)
									l2
									(if(listp (test (car l1) l2))
										(result (cdr l1) l2)
										(cons (car l1) (result (cdr l1) l2))
										
										)
									)									
					)	
		)
		(result l2 (main l1 l2))

		)
		)
)

(defun test()
	(poly+ '((1 2 x 2 y)) '((1 2 y 2 x)))
	)





