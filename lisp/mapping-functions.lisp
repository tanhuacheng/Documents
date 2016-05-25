(mapcar #'(lambda (x y) (+ x y))
        '( 1 2 3)
        '( 4 5 6))

(mapcar #'list
        '(a b c)
        '(1 2 3 4))

(maplist #'(lambda (x) x)
         '(a b c))

