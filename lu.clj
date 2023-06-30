(defn l-elem [a i j] ())

(defn get-el [matr i j]
  (nth (nth matr i) j))

(defn create-list
  ([n] (create-list n '() 1))
  ([n acc i]
   (if (> i n)
     (reverse acc)
     (create-list n (cons i acc) (inc i)))))

(defn u-elem [a i j]
  (cond
    (> i j) 0
    (<= i j) (-
              (get-el a i j)
              (reduce
               + (map
                  * 
                  (map
                     #(l-elem a i (dec %))
                     (create-list i))
                  (map 
                   #(u-elem a (dec %) j) 
                   (create-list i)))))))

(defn l-elem [a i j]
  (cond
    (< i j) 0
    (= i j) 1
    (> i j) (/
             (-
              (get-el a i j)
              (reduce 
               + 
               (map 
                * 
                (map 
                 #(l-elem a i (dec %))
                 (create-list j)) 
                (map 
                 #(u-elem a (dec %) j)
                 (create-list j)))))
             (u-elem a j j))))

(defn lu-j-rec [a elem i j len]
  (cond
    (< j 0) nil
    (>= j 0) (concat
              (lu-j-rec a elem i (dec j) len)
              (list 
               (elem a i j)))))

(defn lu-i-rec [a elem i len]
  (cond
    (= i 0) (list 
             (lu-j-rec a elem i (dec
                                 (count
                                  (first a)))
                       len))
    (> i 0) (concat 
             (lu-i-rec a elem(dec
                               i)
                       len) 
             (list
              (lu-j-rec a elem i (dec
                                  (count
                                   (first a)))
                        len)))))

(defn lu [a]
  (list 
   (lu-i-rec a l-elem (dec (count a)) (dec (count a)))
   (lu-i-rec a u-elem (dec (count a)) (dec (count a)))))

(println (lu [[10 -3 1] [4 5 0] [1 2 7]]))
