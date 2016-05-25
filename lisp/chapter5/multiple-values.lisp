(get-decoded-time)
(values 'a nil (+ 2 4))
((lambda () ((lambda () (values 1 2)))))
((lambda () (values 1 2)))
(let ((x (values 1 2)))
  x)
(values)
(let ((x (values)))
  x)

(multiple-value-bind (x y z a) (values 1 2 3)
  (list x y z a))

(multiple-value-bind (s m h) (get-decoded-time)
  (list h m s))

(multiple-value-call #'+ (values 1 2 3))
(multiple-value-list (values 1 2 3))
(multiple-value-setq (values 1 2 3))



