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
            ((>= j 0) (append (lu_j_rec a elem i (- j 1) len) (elem a i j)))
    ))

;; (defun lu_j_rec (a l u i j len)
;;     (cond ((>= j 0) (lu_j_rec a l u i (- j 1) len)
;;         (cond ((<= i j)
;;         (setf (nth j (nth i u)) (u-elem a i j)))
;;                ((> i j)
;;         (setf (nth j (nth i l)) (l-elem a i j)))))))

(defun lu_i_rec (a elem i len)
    (cond ((< i 0) nil)
            ((>= i 0) (append (lu_i_rec a elem (- i 1) len) (lu_j_rec a elem i (- (length (car a)) 1) (- (length (car a)) 1))))
    ))

;; (defun lu_i_rec (a l u i len)
;;     (cond ((>= i 0)
;;         (lu_i_rec a l u (- i 1) len)
;;         (lu_j_rec a l u i (- (length (car a)) 1) (- (length (car a)) 1)))))

(defun lu (a)
    (list (lu_i_rec a #'l-elem (- (length a) 1) (- (length a) 1)) (lu_i_rec a #'u-elem (- (length a) 1) (- (length a) 1))))

;; (defun lu (a l u)
;;     (lu_i_rec a l u (- (length a) 1) (- (length a) 1)))

;; (setf arrl '((1 0 0) (0 1 0) (0 0 1)))
;; (setf arru '((0 0 0) (0 0 0) (0 0 0)))
;; (print (lu '((10 -3 1) (4 5 0) (1 2 7)) arrl arru))
;; (print arrl)
;; (print arru)
;; (print ())

(print (lu '((10 -3 1) (4 5 0) (1 2 7))))
