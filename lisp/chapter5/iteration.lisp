(defun show-squares (start end)
  (do ((i start (+ i 1)))
    ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(show-squares 1 9)

(let ((x 'a))
  (do ((x 1 (+ x 1))
       (y x x))
    ((> x 5))
    (format t "(~A ~A) " x y)))

(setf y nil)
(dolist (x '(a b c d) 'done)
  (setf y (push x y))
  (format t "~A " x))
(eval `(reverse ',y))
`(reverse y)

(dotimes (x 5 (+ 1 x))
  (format t "~A " x))

(mapc #'(lambda (x y)
          (format t "~A ~A " x y))
      '(hip flip slip done)
      '(hop flop slop))

