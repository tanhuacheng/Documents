(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
         args))

(setf fn (let ((i 3))
           #'(lambda (x) (+ x i))))

(defun add-to-list (num lst)
  (mapcar #'(lambda (x) (+ num x))
          lst))

(defun make-adder (n)
  #'(lambda (x) (+ n x)))

(let ((counter 0))
  (defun reset ()
    (setf counter 0))
  (defun stamp ()
    (setf counter (+ counter 1))))
