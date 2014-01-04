(defun poly*(poly1 poly2)

	(format t "~%poly1 ~A" poly1)
	(format t "~%poly2 ~A" poly2)
	
	(let ((l1 poly1) (l2 poly2))
		(labels ((addf (l1 l2) (if (null l2)
									()
									(cons (cons (+ (car l1) (caar l2)) (cdar l2)) (addf l1 (cdr l2)))))

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
											(cons (car l2) (test2 e1 v1 (cdr l2)))
											))))
			(rev(addr (reverse l1) (rev (addf l1 l2)))))))

(defun test()
	(poly* '(2 2 x 2 y) '((2 3 x 1 y) (3 3 y)))
	)


