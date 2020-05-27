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

(defmacro make-smile (smile)
    `(cond
        ((>= ,smile 2)
            ":)"
        )
        ((= ,smile 1)
            ":]"
        )
        ((= ,smile 0)
            ":|"
        )
        ((= ,smile -1)
            ":C"
        )
        ((<= ,smile -2)
            ":("
        )
        (,t 0)
    )
)

(defmacro get-first (line)
    `(parse-smile
        (subseq ,line 0 2)
    )
)

(defmacro get-second (line)
    `(parse-smile
        (subseq ,line 5 7)
    )
)

(defmacro get-operation (line)
    `(read-from-string
        (subseq ,line 3 4)
    )
)

(defmacro make-operation (line)
    `(eval 
        (list 
            (get-operation ,line)
            (get-first ,line)
            (get-second ,line)
        )
    )
)

(defmacro print-smile (smile)
    `(prog1 
        (princ ,smile)
        (write-line "")
    )
)

(with-open-file (file #P"test_smile.5sm")
    (loop for i from 0
        for line = (read-line file nil nil)
        while line
        do 
            (print-smile (make-smile (make-operation line)))
    )
)