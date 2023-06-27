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
        
    ))

(defun q-cols (a j)
    (
        mapcar (lambda (x) ((mapcar (lambda (y) (q-elem a (- y 1) (- x 1))) (create-list (length a))))) (create-list j)
    ))

(defun r-elem (a i j)
    (
        cond ((= i j) (get-norm (reduce (lambda (x y) (mapcar #'- x y)) (append (list (get-column a j)) (mapcar (lambda (x) (get-projection (get-column a j) x)) (q-cols a j))))))
            ((or (< i j) (> i j)) (get-dot-product (get-column a j) (mapcar (lambda (y) (q-elem a (- y 1) i)) (create-list (length a)))))

    ))

(defun qr_i_rec (a i j)
    (
        cond ((>= i 0) (qr_i_rec a (- i 1) j) (setf (nth j (nth i arrr)) (get-dot-product (get-column arrq j) (get-column arrq i))) (setf (nth j (setf arrq (transpose arrq))) (mapcar #'- (nth j arrq) (get-projection (nth j arrq) (nth i arrq)))) (setf arrq (transpose arrq)))
    ))

(defun qr_j_rec (a j)
    (
        cond ((>= j 0) (qr_j_rec a (- j 1)) (progn (setf (nth j (setf arrq (transpose arrq))) (get-column a j)) (setf arrq (transpose arrq))) (qr_i_rec a (- j 1) j) (let ((norm (get-norm (get-column arrq j)))) (setf (nth j (setf arrq (transpose arrq))) (mapcar (lambda (x) (/ x norm)) (nth j arrq))) (setf arrq (transpose arrq)) (setf (nth j (nth j arrr)) norm)))
    ))

(defun qr (a)
    (
        qr_j_rec a (- (length (nth 1 a)) 1)
    ))

(setf arrq '
    (
        (0 0 0) 
        (0 0 0) 
        (0 0 0)))
(setf arrr '
    (
        (0 0 0) 
        (0 0 0) 
        (0 0 0)))
(qr '
        (
            (10 -3 1) 
            (4 5 2) 
            (1 2 7)))
(print arrq)
(print arrr)
