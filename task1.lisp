;Description
;#8
;Разделить исходный список из целых чисел на два списка:
;список положительных чисел и список отрицательных чисел.

;Code
(defun split (lst)
    (cond                   
        ((null lst)        
            '(nil nil)     
        )
        (t                  
            ((lambda (l n)                            
                    (cond 
                        ((>= n 0)
                            (cons (cons n (car l)) (cdr l))
                        )
                        (t
                            (cons (car l) (cons (cons n (cadr l)) nil ))
                        )
                    )
            ) (split (cdr lst)) (car lst))
        )
    )
)

;Test cases
(print (split '(-1 2 -3 4)))
(print (split '(0 0 -1 5)))
(print (split '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#13
;Удалить из исходного списка все повторные вхождения элементов

;Code


;Test cases

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#15
;Вычислить скалярное произведение векторов, заданных списками целых чисел

;Code
(defun scalar (lst1 lst2)
    (cond
		((null lst1)
			0
		)
		(t
			(+ (* (car lst1) (car lst2)) (scalar (cdr lst1) (cdr lst2)))
		)
	)
)

;Test cases;
(print (scalar '(1 3) '(2 2)))
(print (scalar '(-1 -3) '(0 2)))
(print (scalar NIL NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#21
;Удалить из списка первое вхождение данного элемента на верхнем уровне.

;Code
(defun delete-element (lst element)
	(cond
		((null lst)
			nil
		)
		((= (car lst) element)
			(cdr lst)
		)
		(t
			(cons (car lst) (delete-element (cdr lst) element))
		)
	)
)

;Test cases
(print (delete-element (1 2 3) 3))
(print (delete-element '(1 2 3 5 3 4) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#25
;Удалить из списка каждый четный элемент

;Code
(defun delete-even (lst)
    (cond                   
        ((null lst)        
            nil   
        )
        (t
			(cons (car lst) (delete-even (cddr lst)))            
        )
    )
)

;Test cases
(print (delete-even '(1 2 3 4 5))) 
(print (delete-even '(-1 0 -2 5 7 9 11 0 -3 3454 -4646)))
(print (delete-even NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#28
;Вычислить, сколько всего атомов в списке (списочной структуре)

;Code
(defun count-atoms (lst)
	(cond
		((null lst)
			0
		)
		((atom (car lst))
			(+ 1 (count-atoms (cdr lst)))
		)
		(t
			(+ (count-atoms (car lst)) (count-atoms (cdr lst)))
		)		
	)
)

;Test cases
(print (count-atoms '(1 2 3))
(print (count-atoms '(1 (2 3 5) ((4 3) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#32
;Определить предикат МНОЖЕСТВО-Р, который проверяет, является ли список
;множеством, т.е. входит ли каждый элемент в список лишь один раз.

;Code
(defun find-in-list (lst element)
	(cond
		((null lst)
			nil
		)
		((= (car lst) element)
			t
		)
		(t
			(find-in-list (cdr lst) element)
		)
	)
)

(defun is-set (lst)
	(cond
		((null lst)
			t
		)
		((find-in-list (cdr lst) (car lst))
			nil
		)
		(t
			(is-set (cdr lst))
		)
	)
)
;Test cases
(print (is-set '(1 2 2 3 4)))
(print (is-set '(1 2 3 4)))
(print (is-set '(1 2 2 0 3 4 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#42
;Найти максимальное из значений, находящихся в вершинах дерева.

;Code
(defun tree-max (tree)
	(cond
		((null (cdr tree))
			(car tree)
		)
		(t
			(max (car tree) (tree-max (cadr tree)) (tree-max(caddr tree)))
		)		
	)
)

;Test cases
(print (tree-max '(1 (4) (5))))
(print (tree-max '(1 (4 (1) (7)) (5))))
(print (tree-max '(1 (4 (1) (7)) (5 (14) (1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#45
;Предположим, что у имени города есть свойства х и у, которые содержат координаты места нахождения города
;относительно некоторого начала координат.
;Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.

;Code
(defun dist (city1 city2)
	(sqrt
		(+
			(* 
				(- (get city1 'x) (get city2 'x))
				(- (get city1 'x) (get city2 'x))
			)
			(*
				(- (get city1 'y) (get city2 'y))
				(- (get city1 'y) (get city2 'y))
			)
		)
	)
)

(defun make-city (name x y)
    (setf (get name 'x) x)
    (setf (get name 'y) y)
)

;Test cases
(make-city 'city1 0 3)
(make-city 'city2 4 0)
(print (dist 'city1 'city2))

(make-city 'city1 1 3)
(make-city 'city2 1 9)
(print (dist 'city1 'city2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Description
;#46
;Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо.
;Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей,
;и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае,
;если x1 и x2 — сестры или братья, родные или с одним общим родителем.

;Code
;NOT READY
(defun make-child (name mother father)
    (setf (get name 'mother) mother)
    (setf (get name 'father) father)
)

(defun parents (child)
	(cons (get child 'father) (get child 'mother))
)

(defun brother-or-sister (x y)
	((lambda (child1 child2)
        (or
            (string-equal (car child1) (car child2))
            (string-equal (cadr child1) (cadr child2))
        )
    ) (parents x) (parents y))	
)

;Test cases