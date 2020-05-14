;Description
;#4
;Определите функциональный предикат (КАЖДЫй пред список),
;который истинен в том и только в том случае,
;когда, являющейся функциональным аргументом предикат пред
;истинен для всех элементов списка список.

;Code
(defun each (pred lst)
    (eval (cons 'and (mapcar pred lst)))
)

;Test cases
(print (each (lambda (x) (> x 0)) '(1 2 3 4)))
(print (each (lambda (x) (> x 2)) '(1 2 3 4)))
(print (each (lambda (x) (> x 5)) '(1 2 3 4)))


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
   '(return-call)
)

;Test cases
(print (return-call))
(print (eval (eval (return-call))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;