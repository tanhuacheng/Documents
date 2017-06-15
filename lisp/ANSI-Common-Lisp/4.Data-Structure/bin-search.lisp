#!/usr/bin/clisp

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
      (if (eql obj (aref vec start))
        obj
        nil)
      (let ((mid (+ start (round (/ range 2)))))
        (let ((obj2 (aref vec mid)))
          (if (< obj obj2)
            (and (/= start mid)
                 (finder obj vec start (- mid 1)))
            (if (> obj obj2)
              (and (/= end mid)
                   (finder obj vec (+ mid 1) end))
              obj)))))))

(format t "~A~%" (bin-search 3 #(1 2 3 4 5 6)))
