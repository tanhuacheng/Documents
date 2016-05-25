(setf arr (make-array '(2 3) :initial-element nil))
(setf (aref arr 0 0) "hi")
(setf (aref arr 0 1) 2)
(setf (aref arr 1 0) 1)
(aref arr 0 0)
(aref arr 0 1)
(aref arr 1 0)
(setf x #2a((b nil nil) (nil nil nil)))
(setf (aref x 0 0) 1)
*print-array*

(setf x (vector "a" 'b 3))
(svref x 0)

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))


(defun finder (obj vec start end)
  (format t "~a~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
      (if (eql obj (aref vec start))
        obj
        nil)
      (let ((mid (+ start (round (/ range 2)))))
        (let ((obj2 (aref vec mid)))
          (if (< obj obj2)
            (finder obj vec start (- mid 1))
            (if (> obj obj2)
              (finder obj vec (+ mid 1) end)
              obj)))))))

(setf x (vector 1 3 4 5 6 7 8 9))
(bin-search 2 x)

(sort "elbow" #'char<)
(aref "abc" 0)
(char "abc" 0)
(> 3 2)

