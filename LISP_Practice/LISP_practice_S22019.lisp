;; Part 1: Short questions

;; 1. (caar '(a b c))
;; ERROR

;; 2. (caar (cddr '(a b (c d))))
;; C

;; 3. (cons (append '(b c) '(d e)) (list '(b c) '(d e)))
((b c d e) (b c) (d e))

;; 4. (list '(car (a b c)) '(cdr (d ef)))
((car (a b c)) (cdr (d ef)))

;; 5. (list (1- 1) (1+ 1) '(2 * 3))
(0 2 (2 * 3))

;; 6. Given (defun test (&optional a b) (append a b)), what is returned with a call, (test '(a b c d e))?
(a b c d e)

;; 7. Given (defun test1 (&key a (b 5) c) (+ a b c)), how do you call test1 with a assigned a value of 10 and c a value of 12?
(test1 :a 10 :c 12)

;; 8. (cadr (last (last (last '(a b c)))))
NIL

;; 9. Given (defun test2 (&rest L) (car L)), what is assigned to L with a call (test2 a b c d)?
'(a b c d)

;; 10. (list '(a b) '(c d))
((a b) (c d))

;; Part 2: 

;; 1. Write a LISP function, sum-first-three, which given a list will return the sum of the first three numbers in it.
(defun sum-first-three (L &optional (cnt 3) (ans 0))
	(cond ((or (null L) (zerop cnt)) ans)
		((numberp (car L)) (sum-first-three (cdr L) (1- cnt) (+ ans (car L))))
		(t (sum-first-three (cdr L) cnt ans))))

;; 2. Write a LISP function, remove-lists which given a list will return a list without any lists in it.
(defun remove-lists (L)
	(remove-if #'(lambda (x) (not (numberp x))) L))

;; 3. Write a LISP function, combine, which given a list of lists will return a single list of all the elements in it.
(defun combine (L &optional (ans nil))
	(cond ((null L) ans)
		((numberp (car L)) (combine (cdr L) (append ans (list (car L)))))
	(t (combine (append (car L) (cdr L)) ans))))

;; 4. Write a LISP function, remove-evenL, which given a list of lists will return a list of lists that are even in length.
(defun remove-evenL(L)
	(remove-if #'(lambda (x) (evenp (length x))) L))

;; 5. Write a LISP function, duplicate-second, which given a list will return the same list with its second element duplicated.
(defun duplicate-second (L)
	(cond ((null L) nil)
		(t (append (list (car L) (cadr L) (cadr L)) (cdr L)))))
