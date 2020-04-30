;Description
;#13
;Удалить из исходного списка все повторные вхождения элементов

;Code
(defun delete-all (lst element)
	((lambda (head tail)
		(cond
            ((null lst)
                nil
            )
            ((= head element)
                (delete-all tail element)
            )
            (t
                (cons head (delete-all tail element))
            )
        )
	) (car lst) (cdr lst))
    
)

(defun remove-all-duplicates (lst)
    (cond
        ((null lst)
             nil
        )
        (t
            (cons 
                 (car lst)
                 (remove-all-duplicates
                      (delete-all (cdr lst) (car lst))
                 )
             )
        )
    )
)

;Test cases
(print (remove-all-duplicates '(1 2 3 3 2)))
(print (remove-all-duplicates '(1 2 4 4 3 3 4 4 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;