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



(defun poly*(poly1 poly2)

	(format t "~%poly1 ~A" poly1)
	(format t "~%poly2 ~A" poly2)

	(let ((l1 poly1) (l2 poly2))
		(labels ((addf (l1 l2) (if (null l2)
									()
									(cons (cons (* (car l1) (caar l2)) (cdar l2)) (addf l1 (cdr l2)))))

				(addr (l1 l2) (if (null l1)
									l2
									(if (numberp (car l1))
										(addr (cdr l1) l2)
										(let ((newl2 (test (car l1) (cadr l1) l2)))
											(addr (cdr l1) newl2)))))
				(rev (l1) (if (null l1)
							()
							(if (numberp (car l1))
								(reverse l1)
								(cons (reverse (car l1)) (rev (cdr l1))))))

				(test (e1 v1 l2) (if (null l2)
								()
								(cons (test2 e1 v1 (car l2)) (test e1 v1 (cdr l2)))))

				(test2 (e1 v1 l2) (if (null l2)
								()
								(if (equal e1 (car l2))
											(cons e1 (test2 e1 v1 (cons (+ v1 (cadr l2)) (cddr l2))))
											;(cons (cons e1 (+ v1 (cadr l2))) (test2 e1 v1 (cddr l2)))
											;(cons e1 (cons v1 (test2 e1 v1 (cdr l2))))
											(if (numberp (car l2))
												(cons (car l2) (test2 e1 v1 (cdr l2)))
												(cons e1 (cons v1 (cons (car l2) (test2 e1 v1 (cdr l2)))))
												)
											)))
				(addp (l1 l2) (if (null l1)
								()
								(poly+ (rev(addr (reverse (car l1)) (rev (addf (car l1) l2)))) (addp (cdr l1) l2))


								)
				) ) (addp l1 l2)
					) ))


(defun test()
	(poly* '((1 1 y) (1 1 x)) '((1 1 x) (1 1 y)))
	)


