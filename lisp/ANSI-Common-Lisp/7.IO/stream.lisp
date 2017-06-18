(setf str (open (make-pathname :name "myfile")
                :direction :output
                :if-exists :supersede))
(format str "Somethine~%")
(close str)

(setf str (open (make-pathname :name "myfile")
                :direction :input))
(read-line str)
(close str)

(with-open-file (str
                 (make-pathname :name "myfile")
                 :direction :output
                 :if-exists :supersede)
  (format str "Somethine else~%"))


(progn
  (format t "Please enter your name: ")
  (read-line))

(defun wc-l (file)
  (with-open-file (str file :direction :input)
    (do ((counter 0 (+ counter 1))
         (line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql 'eof line) counter)
        (format t "~A~%" line))))
(wc-l (make-pathname :name "myfile"))
