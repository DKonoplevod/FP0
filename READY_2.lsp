;Description
;#2
;Определите функционал (maplist fn список) для одного списочного аргумента

;Code
(defun map-list (fn lst)
    (cond
        ((null lst)
            nil
        )
        (t
            (cons (funcall fn lst) (map-list fn (cdr lst)))
        )
    )
    
)

;Test cases
(print (map-list (lambda (x) (list 'start x 'end)) '(1 2 3 4)))
(print (map-list (lambda (x) (cons 'foo x)) '(a b c d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#4
;Определите функциональный предикат (КАЖДЫй пред список),
;который истинен в том и только в том случае,
;когда, являющейся функциональным аргументом предикат пред
;истинен для всех элементов списка список.

;Code
(defun each (pred lst)
    (null (member nil (mapcar pred lst)))
)

;Test cases
(print (each (lambda (x) (> x 0)) '(1 2 3 4)))
(print (each (lambda (x) (> x 2)) '(1 2 3 4)))
(print (each (lambda (x) (> x 5)) '(1 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#6
;Определите фильтр (УДАЛИТЬ-ЕСЛИ пред список),
;удаляющий из списка все элементы, которые обладают свойством,
;наличие которого проверяет предикат пред. 

;Code
(defun del-if (pred lst)
     (mapcan 
        (lambda (x)
            (cond
                ((funcall pred x)
                    nil
                )
                (t 
                    (list x)
                )
            )
        )
        lst
    )
)

;Test cases
(print (del-if (lambda (x) (> x 0)) '(1 2 3 4)))
(print (del-if (lambda (x) (> x 2)) '(1 2 3 4)))
(print (del-if (lambda (x) (> x 5)) '(1 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#8
;Напишите генератор натуральных чисел: 
;0, 1, 2, 3, 4, 5, ...

;Code
(defun gen-nat ()
	(let ((c -1))
		(lambda ()
			(setq
				c (+ c 1)
            )
        )
	)
)

;Test cases
(setq gen1 (gen-nat))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))

(write-line "")

(setq gen1 (gen-nat))
(setq gen2 (gen-nat))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#10
;Напишите генератор, порождающий последовательность
;(A), (B A), (A B A), (B A B A), ...  

;Code
(defun gen-ab-str ()
	(let ((s1 'A) (s2 'B) (s nil))
		(lambda ()
			(setq
				s 
                (cond 
                    ((eq s1 (car s)) (cons s2 s))
                    (t (cons s1 s))
                )
            )
        )
	)
)

;Test cases
(setq gen1 (gen-ab-str))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))

(write-line "")

(setq gen1 (gen-ab-str))
(setq gen2 (gen-ab-str))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen2))
(print (funcall gen1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#12
;Определите функцию, которая возвращает в качестве значения свой вызов. 

;Code
(defun return-call ()
   'return-call
)

;Test cases
(print (return-call))
(print (funcall (funcall (return-call))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;