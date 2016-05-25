(defstruct rectangle
  height width)

(defstruct circle
  radius)

(defun area (x)
  (cond ((rectangle-p x)
         (* (rectangle-height x) (rectangle-width x)))
        ((circle-p x)
         (* pi (expt (circle-radius x) 2)))))

(let ((r (make-rectangle)))
  (setf (rectangle-height r) 2)
  (setf (rectangle-width r) 3)
  (area r))

(let ((c (make-circle)))
  (setf (circle-radius c) 2)
  (area c))

(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defgeneric area x)

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(let ((r (make-instance 'rectangle)))
  (setf (slot-value r 'height) 2
        (slot-value r 'width) 3)
  (area r))

(defclass circle1 ()
  ((radius :accessor circle1-radius)
   (center :accessor circle1-center)))

(let ((c (make-instance 'circle1)))
  (setf (circle1-radius c) 1)
  (circle1-radius c))

(defclass point ()
  ((pos
     :accessor point-pos
     :initarg :pos
     :initform (cons 0 0))))

(let ((p (make-instance 'point :pos (cons 1 1))))
  (point-pos p)
  (setf (point-pos p) (cons 1 1))
  (cons
    (point-pos p)
    (slot-value p 'pos)))






























