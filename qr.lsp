;gnuclisp2.49.60

(defun cars (matrix)
  "Return a list with all the cars of the lists in matrix"
  (if (null matrix)
      nil
      (cons (car (car matrix)) (cars (cdr matrix)))))

(defun cdrs (matrix)
  "Return a list with all the cdrs of the lists in matrix"
  (if (null matrix)
      nil
      (cons (cdr (car matrix)) (cdrs (cdr matrix)))))

(defun transpose (matrix)
  "Transpose matrix"
  (cond ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (cars matrix) (transpose (cdrs matrix))))))

(defun create-list (n &optional (acc '()) (i 1))
  (if (> i n)
      (reverse acc)
      (create-list n (cons i acc) (1+ i))))

(defun get-row 
    (matr i)
    (nth i matr))

(defun get-column-rec
    (matr j cnt)
    (if (= cnt j) (cars matr) (get-column-rec (cdrs matr) j (+ cnt 1))))

(defun get-column
    (matr j)
    (get-column-rec matr j 0))


(defun get-norm (arr)
    (
        sqrt (reduce #'+ (mapcar #'* arr arr))
    ))

(defun get-dot-product (arr1 arr2)
    (
        reduce #'+ (mapcar #'* arr1 arr2)
    ))

(defun get-projection (arr1 arr2)
    (
        let ((dot (get-dot-product arr1 arr2)) (norm (get-norm arr2))) (mapcar (lambda (x) (* x (/ dot (* norm norm)))) arr2)
    ))

;; (defun r-elem (a i j)
;;     (
;;         cond ((= i j) (get-norm (reduce (lambda (x y) (mapcar #'- x y)) (append (list (get-column a j)) (mapcar (lambda (x) (get-projection (mapcar (lambda (x) (get-column a (- x 1))))) (create-list j)) x) )))))
;;             ((or (< i j) (> i j)) ())

;;     ))

;; (get-projection (get-column a j) (q-elem i))

(defun q-elem (a i j)
    (
        / (nth i (reduce (lambda (x y) (mapcar #'- x y)) (append (list (get-column a j)) (mapcar (lambda (x) (get-projection (get-column a j) x)) (q-cols a j))))) (r-elem a j j)
    ))

(defun q-cols (a j)
    (
        mapcar (lambda (x) (mapcar (lambda (y) (q-elem a (- y 1) (- x 1))) (create-list (length a)))) (create-list j)
    ))

(defun r-elem (a i j)
    (
        cond ((= i j) (get-norm (reduce (lambda (x y) (mapcar #'- x y)) (append (list (get-column a j)) (mapcar (lambda (x) (get-projection (get-column a j) x)) (q-cols a j))))))
            ((< i j) (get-dot-product (get-column a j) (mapcar (lambda (y) (q-elem a (- y 1) i)) (create-list (length a)))))
            ((> i j) 0)

    ))

(defun qr_j_rec (a elem i j len)
    (cond ((< j 0) nil)
            ((>= j 0) (append (qr_j_rec a elem i (- j 1) len) (list(funcall elem a i j))))
    ))

(defun qr_i_rec (a elem i len)
    (cond
            ((= i 0) (list (qr_j_rec a elem i (- (length (car a)) 1) (- (length (car a)) 1))))
            ((> i 0) (append (qr_i_rec a elem (- i 1) len) (list (qr_j_rec a elem i (- (length (car a)) 1) (- (length (car a)) 1)))))
    ))

(defun qr (a)
    (list (qr_i_rec a 'q-elem (- (length a) 1) (- (length a) 1)) (qr_i_rec a 'r-elem (- (length a) 1) (- (length a) 1))))

(print (qr '((10 -3 1) (4 5 2) (1 2 7))))
