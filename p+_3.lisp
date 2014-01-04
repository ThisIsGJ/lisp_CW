;;new try
(defun poly+ (poly1 poly2)

	(format t "~%poly1 ~A" poly1)
	(format t "~%poly2 ~A" poly2)

	(let ((l1 poly1) (l2 poly2))
		(labels (
			 	(main (p1 p2) 
			 		(if (null p1)
			 			()
			 			(let ((pass (test (car p1) p2))) 
			 				 (if (listp pass) 
			 				 	(cons pass (main (cdr p1) p2))
			 				 	(cons (car p1) (main (cdr p1) p2)))
			 				)))

				(test (l1 l2) (if (null l2)
	 					t
	 					(if (equal (cdr l1) (cdar l2)) 
	 						(cons (+ (car l1) (caar l2)) (cdr l1))
	 						(test l1 (cdr l2)))
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
	(poly+ '((-1 1 x)) '((2 1 z)))
	)





