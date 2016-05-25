(defclass circle ()
  ((radius
     :accessor circle-radius
     :initarg :radius
     :initform 1)
   (center
     :accessor circle-center
     :initarg :center
     :initform (cons 0 0))))

(defclass graphic ()
  ((color
     :accessor graphic-color
     :initarg :color)
   (visible
     :accessor graphic-visible
     :initarg :visible
     :initform t)))

(defclass screen-circle (circle graphic)
  ())

(let ((sc (make-instance 'screen-circle)))
  (setf (slot-value sc 'color) 1
        (slot-value sc 'visible) nil)
  (setf (circle-radius sc) 2
        (circle-center sc) (cons 1 1))
  (list
    (circle-radius sc)
    (circle-center sc)
    (graphic-color sc)
    (graphic-visible sc)))

(let ((sc1 (make-instance 'screen-circle
                          :radius 3
                          :center (cons 2 2)
                          :color 'red
                          :visible nil)))
    (list
        (circle-radius sc1)
    (circle-center sc1)
    (graphic-color sc1)
    (graphic-visible sc1)))

(defmethod combine (x y)
  (list x y))

(combine 1 2)

;通用函数

(defclass stuff ()
  ((name :accessor name
         :initarg :name)))

(defclass ice-cream (stuff)
  ())

(defclass topping (stuff)
  ())

(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          (name top)))

(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A."
          (name ic)
          x))

(defmethod combine ((x number) (y number))
  (+ x y))

(combine (make-instance 'ice-cream :name 'fig)
         (make-instance 'topping :name 'treacle))
(combine 1 (make-instance 'ice-cream :name 'fig))
(combine (make-instance 'ice-cream :name 'fig) 1)
(combine (make-instance 'topping :name 'top) 1)
(combine 1 2)
(combine 1.1 2.2)
(combine 'a 'b)

;辅助方法

(defclass speaker () ())
(defclass intellectual (speaker) ())

(defmethod speak :before ((s speaker) string)
  (princ "i think "))
(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))
(defmethod speak :after ((i intellectual) string)
  (princ " in some sense"))
(defmethod speak ((s speaker) string)
  (format t "~A" string))
(defmethod speak :around ((i intellectual) string)
  (format t "shut up.~%")
  (if (next-method-p)
    (call-next-method))
  (format t "~%Indeed, it is a preposterous idea.~%")
  'bow)

(speak (make-instance 'speaker) "I'm hungry")
(speak (make-instance 'intellectual) "I'm hungry")























