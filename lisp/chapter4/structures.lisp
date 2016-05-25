(defstruct point
  x
  y)

(setf p (make-point :x 0 :y 0))
(setf (point-x p) 2)
(point-p p)
(typep p 'point)

(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it?~%")
          (read)))
  (effect nil))

(make-polemic :type 'int :effect 1)

(defstruct (point (:conc-name p)
                  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "#<~A, ~A>" (px p) (py p)))

(setf p (make-point))
(setf (px p) 1)
p


