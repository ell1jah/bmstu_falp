(defun get-el (matr i j)
  (nth j (nth i matr)))

(defun create-list (n &optional (acc '()) (i 1))
  (if (> i n)
      (reverse acc)
      (create-list n (cons i acc) (1+ i))))

(defun u-elem (a i j)
    (cond ((> i j) 0)
            ((<= i j) (- (get-el a i j) (reduce #'+ (mapcar #'* (mapcar #'(lambda (x) (l-elem a i (- x 1))) (create-list i)) (mapcar #'(lambda (x) (u-elem a (- x 1) j)) (create-list i))))))))

(defun l-elem (a i j)
    (cond ((< i j) 0)
            ((= i j) 1)
            ((> i j) (/ (- (get-el a i j) (reduce #'+ (mapcar #'* (mapcar #'(lambda (x) (l-elem a i (- x 1))) (create-list j)) (mapcar #'(lambda (x) (u-elem a (- x 1) j)) (create-list j))))) (u-elem a j j)))))

(defun lu_j_rec (a elem i j len)
    (cond ((< j 0) nil)
            ((>= j 0) (append (lu_j_rec a elem i (- j 1) len) (list(funcall elem a i j))))
    ))

(defun lu_i_rec (a elem i len)
    (cond
            ((= i 0) (list (lu_j_rec a elem i (- (length (car a)) 1) (- (length (car a)) 1))))
            ((> i 0) (append (lu_i_rec a elem (- i 1) len) (list (lu_j_rec a elem i (- (length (car a)) 1) (- (length (car a)) 1)))))
    ))

(defun lu (a)
    (list (lu_i_rec a 'l-elem (- (length a) 1) (- (length a) 1)) (lu_i_rec a 'u-elem (- (length a) 1) (- (length a) 1))))

(print (lu '((10 -3 1) (4 5 0) (1 2 7))))
