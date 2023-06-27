(defn q-cols [a i j] ())
(defn r-elem [a i j] ())

(defn cars [matrix]
  (if (empty? matrix)
    nil
    (cons (first (first matrix)) (cars (rest matrix)))))

(defn cdrs [matrix]
  (if (empty? matrix)
    nil
    (cons (rest (first matrix)) (cdrs (rest matrix)))))

(defn transpose [matrix]
  (cond
    (empty? matrix) nil
    (empty? (first matrix)) nil
    :else (cons (cars matrix) (transpose (cdrs matrix)))))

(defn create-list
  ([n] (create-list n '() 1))
  ([n acc i]
   (if (> i n)
     (reverse acc)
     (create-list n (cons i acc) (inc i)))))

(defn get-row [matr i]
  (nth matr i))

(defn get-column-rec [matr j cnt]
  (if (= cnt j)
    (cars matr)
    (get-column-rec (cdrs matr) j (inc cnt))))

(defn get-column [matr j]
  (get-column-rec matr j 0))

(defn get-norm [arr]
  (Math/sqrt (reduce + (map #(* % %) arr))))

(defn get-dot-product [arr1 arr2]
  (reduce + (map * arr1 arr2)))

(defn get-projection [arr1 arr2]
  (map #(* % (/ (get-dot-product arr1 arr2) (* (get-norm arr2) (get-norm arr2)))) arr2))

(defn q-elem [a i j]
  (/ (nth (reduce (fn [x y] (map - x y)) (concat [(get-column a j)] (map (fn [x] (get-projection (get-column a j) x)) (q-cols a j)))) i) (r-elem a j j)))

(defn r-elem [a i j]
  (cond
    (= i j) (get-norm (reduce (fn [x y] (map - x y)) (concat [(get-column a j)] (map (fn [x] (get-projection (get-column a j) x)) (q-cols a j)))))
    (< i j) (get-dot-product (get-column a j) (map (fn [y] (q-elem a (dec y) i)) (create-list (count a))))
    (> i j) 0))

(defn q-cols [a j]
  (map (fn [x] (map (fn [y] (q-elem a (dec y) (dec x))) (create-list (count a)))) (create-list j)))

(defn qr-j-rec [a elem i j]
  (cond
    (< j 0) nil
    (>= j 0) (concat (qr-j-rec a elem i (dec j)) (list (elem a i j)))))

(defn qr-i-rec [a elem i]
  (cond
    (= i 0) (list (qr-j-rec a elem i (dec (count (first a)))))
    (> i 0) (concat (qr-i-rec a elem (dec i)) (list (qr-j-rec a elem i (dec (count (first a))))))))

(defn qr [a]
  (list (qr-i-rec a q-elem (dec (count a))) (qr-i-rec a r-elem (dec (count (first a))))))

(println (qr [[10 -3 1] [4 5 2] [1 2 7] [4 2 75]]))

