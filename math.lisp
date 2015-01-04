;lisp
;common lisp compiler
;editor GuanJun

; I have given a example to show how the poly+ function work
;example: poly1= '((1 1 x)(1 1 y)) poly2='((1 1 y)(1 1 z)) 
(defun poly+ (poly1 poly2)	
	(labels (
		;p1='((1 1 x)(1 1 y)) p2='((1 1 y)(1 1 z)) which can be expressed as (x+y)+(y+z)
		;firstly I will add the p1 and p2,if they have same element, add then.
		;if the element in p1 doesn't included in p2, just put that element into p2.
		;if p2 have the element which p1 doesn't have, just ingore it 
		;after (main (p1 p2) will go out -> ((1 1 X) (2 1 Y))
	 	(main (p1 p2) 
	 		(if (null p1)
	 			()
	 			(let ((pass (test (car p1) p2))) 
	 				 (if (listp pass) 
	 				 	(cons pass (main (cdr p1) p2))
	 				 	(cons (car p1) (main (cdr p1) p2))))))

		(test (l1 l2) (if (null l2)
	 					t
	 					(if (= (list-length l1) (list-length (car l2)))
	 						;test whether element in l1 could add to l2
							;if element in l1 also can be found in l2,return t and add the coefficient together
							;if not, nil, and continue to do next test
	 						(if (test2 (cdr l1) (cdar l2))	
	 							(cons (+ (car l1) (caar l2)) (cdr l1))
	 							(test l1 (cdr l2)))
	 						(test l1 (cdr l2)))))	
		;test each element in l1 with l2 
		(test2 (l1 l2) (if (null l1)
						t		
						;compile the variable and its power together eg:(1 x) ->x^1
						;then go to test3 to test whether l2 have the same value
						(if (test3 (cons (car l1) (list (cadr l1))) l2)	
							(if (= 2 (list-length l1))
								(test2 () l2)
								(test2 (cddr l1) l2))
							nil)))
		;test each element in l1 with each element in l2
		(test3 (l1 l2) (if (null l2)
						nil
						(if (equal l1 (cons (car l2) (list (cadr l2))))
							t
							(if (= 2 (list-length l2))
								(test3 l1 ())
								(test3 l1 (cddr l2))))))
		;;l1 = '((1 1 y)(1 1 z)) l2 = ((1 1 X) (2 1 Y))
		;;combine the list which doesn't include in l2 but in l1 with l2 together
		;;the result will come out : ((1 1 Z) (1 1 X) (2 1 Y))
		(result (l1 l2) (if (null l1)
							l2
							(if(listp (test (car l1) l2))
								(result (cdr l1) l2)
								(cons (car l1) (result (cdr l1) l2))))))
	
		(result poly2 (main poly1 poly2))))

;poly- have the same ideal with poly+, there just have a little differents which I have indicate out
(defun poly- (poly1 poly2)
	(labels (
	 	(main (p1 p2) 
	 		(if (null p1)
	 			()
	 			(let ((pass (test (car p1) p2))) 
	 				 (if (listp pass) 
	 				 	(cons pass (main (cdr p1) p2))
	 				 	(cons (car p1) (main (cdr p1) p2))))))
	
		(test (l1 l2) (if (null l2)
	 					t
	 					(if (= (list-length l1) (list-length (car l2)))
	 						(if (test2 (cdr l1) (cdar l2))	
	 							;here is "-" which is different from poly+
	 							(cons (- (car l1) (caar l2)) (cdr l1))
	 							(test l1 (cdr l2)))
	 						(test l1 (cdr l2)))))
		
		(test2 (l1 l2) (if (null l1)
						t		
						(if (test3 (cons (car l1) (list (cadr l1))) l2)	
							(if (= 2 (list-length l1))
								(test2 () l2)
								(test2 (cddr l1) l2))
							nil	
							)))
		
		(test3 (l1 l2) (if (null l2)
						nil
						(if (equal l1 (cons (car l2) (list (cadr l2))))
							t
							(if (= 2 (list-length l2))
								(test3 l1 ())
								(test3 l1 (cddr l2))))))
		;make the p2 be negative
		 (reduce (l2) (if (null l2)
		 				()
		 				(cons (cons (- 0 (caar l2)) (cdar l2)) (reduce (cdr l2)))))

		(result (l1 l2) (if (null l1)
							(reduce l2)
							(if(listp (test (car l1) l2))
								(result (cdr l1) l2)
								(cons (car l1) (result (cdr l1) l2))))))

		(result poly1 (main poly2 poly1))))

;my basic ideal is using poly* and poly+ together 
;for example: p1 = x+y p2= y+z
;poly* will do x*(y+z) and y*(y+z)
;after that poly* will use poly+ function to add x*(y+z) and y*(y+z) together
(defun poly*(poly1 poly2)
	
	(labels (
		(addf (l1 l2) (if (null l2) ;;multipy the first facetor together to l2
								()
								(cons (cons (* (car l1) (caar l2)) (cdar l2)) (addf l1 (cdr l2)))))
			
		(rev (l1) (if (null l1)		;;reverse all the number in the sublist
					()
					(if (numberp (car l1))
						(reverse l1)
						(cons (reverse (car l1)) (rev (cdr l1))))))
		
		(addr (l1 l2) (if (null l1);;multipy the reset factor together to l2
							l2
							(if (numberp (car l1))
								(addr (cdr l1) l2)
								(let ((newl2 (test (car l1) (cadr l1) l2)))
									(addr (cdr l1) newl2)))))

		;test and test2 are using for adding power of the variable of 
		;p1 and p2 if they have same value
		(test (e1 v1 l2) (if (null l2)
						()
						(cons (test2 e1 v1 (car l2) (car l2)) (test e1 v1 (cdr l2)))))

		(test2 (e1 v1 l2 l3) (if (null l2)
				(cons e1 (cons v1 l3)) 
				(if (equal e1 (car l2))
						(combine (cons e1 (cons (+ v1 (cadr l2)) (cddr l2))) l3)								
						(test2 e1 v1 (cdr l2) l3))))

		(combine (n l) (if (equal (car n) (car l))
							n
							(cons (car l) (combine n (cdr l)))))

		(addp (l1 l2) (if (null l1)	;; add polynormal together 
						()
						(poly+ (rev(addr (reverse (car l1)) (rev (addf (car l1) l2)))) (addp (cdr l1) l2))))) 
		
		(addp poly1 poly2)))

