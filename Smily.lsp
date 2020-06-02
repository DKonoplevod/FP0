(defmacro parse-smile (smile)
    `(cond
        ((string-equal ,smile ":)")
            2
        )
        ((string-equal ,smile "=)")
            2
        )

        ((string-equal ,smile ":]")
            1
        )
        ((string-equal ,smile "=]")
            1
        )

        ((string-equal ,smile ":|")
            0
        )
        ((string-equal ,smile "=|")
            0
        )

        ((string-equal ,smile ":C")
            -1
        )
        ((string-equal ,smile "=C")
            -1
        )

        ((string-equal ,smile ":(")
            -2
        )
        ((string-equal ,smile "=(")
            -2
        )

        (,t 0)
    )
)

(defmacro make-smile (res)
    `(cond
        ((eq ,res t)
            t
        )
        ((eq ,res nil)
            nil
        )
        ((>= ,res 2)
            ":)"
        )
        ((= ,res 1)
            ":]"
        )
        ((= ,res 0)
            ":|"
        )
        ((= ,res -1)
            ":C"
        )
        ((<= ,res -2)
            ":("
        )
        (,t 0)
    )
)

(defmacro make-result (res)
    `(cond
        ((atom ,res)
            (make-smile ,res)
        )
        (,t
            (cons (make-result (car ,res)) (make-result (cdr ,res)))
        )
    )
)

(defmacro get-smile (smile)
    `(parse-smile
        ,smile
    )
)

(defmacro get-operation (operator)
    `(read-from-string
        ,operator
    )
)

(defmacro print-smile (smile)
    `(prog1 
        (princ ,smile)
        (write-line "")
    )
)

(defun split-1 (string &optional (separator " ") (r nil))
    (let 
        ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
        (if n
	        (split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
            (cons string r)
        )
    )
)

(defun split-str (string &optional (separator " "))
    (split-1 string separator)
)

(defmacro parse-string (line)
    `(eval
        ((lambda (list)
            (list
                (get-operation (cadr list))
                (get-smile (car list))
                (get-smile (caddr list))
            ) 
        ) (split-str ,line))
    )
)

(with-open-file (file #P"test_smile.sml")
    (loop for i from 0
        for line = (read-line file nil nil)
        while line
        do 
            (print-smile (make-result (parse-string line)))
    )
)