#!/usr/bin/clisp

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (1+ forward))
              (back (- len 1) (1- back)))
             ((or (> forward back)
                  (not (eql (elt s forward)
                            (elt s back))))
              (> forward back))))))

(format t "~A~%" (mirror? "123321"))    ; T
(format t "~A~%" (mirror? "121"))       ; NIL
