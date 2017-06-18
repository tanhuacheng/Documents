(fboundp '+)
(symbol-function '+)
(setf (symbol-function 'add2) #'(lambda (x) (+ 2 x)))
(add2 2)


(defun foo (x)
  "Implements an enhanced paradigm of diversity"
  x)
(documentation 'foo 'function)


(labels ((add10 (x) (+ x 10))
         (consa (x) (cons 'a x)))
  (consa (add10 3)))

(labels ((len (lst)
           (if (null lst)
               0
               (+ (len (cdr lst)) 1))))
  (len '(a b c)))


(defun our-funcall (fn &rest args)
  (apply fn args))
(our-funcall #'+ 1 2 3 4)

(defun philosoph (thing &optional property)
  (list thing 'is property))
(philosoph 'death)

(defun philosoph (thing &optional (property 'fun))
  (list thing 'is property))
(philosoph 'death)
(philosoph 'death 'bad)


(defun keylist (a &key (x 1) (y 1) (z 1))
  (list a x y z))
(keylist 1 :y 2)
(keylist 1 :y 3 :x 2)


; Utilities

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun appendl (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
