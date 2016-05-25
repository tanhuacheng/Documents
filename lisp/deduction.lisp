(defun match (x y &optional binds)
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    (t
     (when (and (consp x) (consp y))
       (multiple-value-bind (b2 yes)
         (match (car x) (car y) binds)
         (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b
      (or (binding (cdr b) binds)
          (cdr b)))))



(defun prove (expr &optional binds)
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t (prove-simple (car expr) (cdr expr) binds))))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes)
                (match args (car r)
                       binds)
                (when yes
                  (if (cdr r)
                    (prove (cdr r) b2)
                    (list b2)))))
          (mapcar #'change-vars
                  (gethash pred *rules*))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
                      (cons v (gensym "?")))
                  (vars-in r))
          r))

(defun vars-in (expr)
  (if (atom expr)
    (if (var? expr) (list expr))
    (union (vars-in (car expr))
           (vars-in (cdr expr)))))


(defun prove-and (clauses binds)
  (if (null clauses)
    (list binds)
    (mapcan #'(lambda (b)
                (prove (car clauses) b))
            (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

(defun prove-not (clauses binds)
  (unless (prove clauses binds)
    (list binds)))

(defun prove-eql (clauses binds)
  (dolist (var clauses)
    (


(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query))
         ,@body))))


;(defvar *rules* (make-hash-table))
(defmacro <- (con &optional ant)
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))
(setf *rules* nil)
(setf *rules* (make-hash-table))
(<- (parent donald nancy))
(<- (parent donald debbie))
(<- (male donald))
(<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))
(<- (= ?x ?y) (eql ?y ?x))
;(<- (sibling ?x ?y) (and
;                      (parent ?z ?x)
;                      (parent ?z ?y)
;                      (not (= ?x ?y))))
(<- (sibling ?x ?y) (and
                      (parent ?z ?x)
                      (parent ?z ?y)))

(with-answer (father ?x ?y)
  (format t "~A is the father of ~A.~%" ?x ?y))

(with-answer (sibling ?x ?y)
  (format t "~A is the sibling of ~A.~%" ?x ?y))


