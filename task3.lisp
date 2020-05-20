;Description
;#1
;Определите макрос, который возвращает свой вызов.

;Code
(defmacro return-macro-call ()
    `'(return-macro-call)
)

;Test cases
(print (return-macro-call))
(print (eval (eval (return-macro-call))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#2
;Определите макрос (POP стек), который читает из стека верхний элемент и меняет значение переменной стека.

;Code
(defmacro popa (stack)
    `(prog1
        (car ,stack)
        (setq ,stack (cdr ,stack))
    )
)

;Test cases
(setq stuck '(a b c))
(print (popa stuck))
(print stuck)
(print (popa stuck))
(print stuck)
(print (popa stuck))
(print stuck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Description
;#3
;Определите лисповскую форму (IF условие p q) в виде макроса.

;Code
(defmacro esli (con q p)
    `(cond
        (,con ,q)
        (,t ,p)
    )
)

;Test cases
(setq a 1)
(setq b 1)
(setq c 2)

(print (esli (= a b) 'equal 'not-equal))
(print (esli (= a c) 'equal 'not-equal))

(print (esli (= a b) (car '(a b c)) (cdr '(a b c))))
(print (esli (= b c) (car '(a b c)) (cdr '(a b c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#4
;Определите в виде макроса форму (FIF тест отр нуль полож).

;Code
(defmacro fif (test n z p)
    `(cond
        ((< ,test 0)
            ,n
        )
        ((= ,test 0)
            ,z
        )
        (t
            ,p
        )
    )
)

;Test cases
(print (fif (* -1 -1) 'отрицательное 'нуль 'положительное))
(print (fif (* -1 0) 'отрицательное 'нуль 'положительное))
(print (fif (* -1 1) 'отрицательное 'нуль 'положительное))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;