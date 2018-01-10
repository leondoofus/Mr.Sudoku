(ns mrsudoku.engine
  (:use midje.sweet)
  (:require [mrsudoku.grid :as g]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))

(defn values
  "Return the set of values of a vector or grid `cells`."
  [cells]
  (loop [s cells, res #{}]
    (if (seq s)
      (if-let [premier (first s)]
        (if (= (premier :status) :init)
          (recur (rest s) (conj res (premier :value)))
          (recur (rest s) res))
        (recur (rest s) res))
      res)))

(fact
 (values (g/block sudoku-grid 1)) => #{5 3 6 9 8})

(fact
 (values (g/row sudoku-grid 1)) => #{5 3 7})

(fact
 (values (g/col sudoku-grid 1)) => #{5 6 8 4 7})

(fact
 (values (g/block sudoku-grid 8)) => #{4 1 9 8})

(fact
 (values (g/row sudoku-grid 8)) => #{4 1 9 5})

(fact
 (values (g/col sudoku-grid 8)) => #{6 8 7})

(defn values-except
  "Return the set of values of a vector of cells, except the `except`-th."
  [cells except]
  (if (first ({:pre [(<= 1 except (count cells))]} :pre))
    (loop [s cells, acc 1, res #{}]
      (if (seq s)
        (if-let [premier (first s)]
          (if (= acc except)
            (recur (rest s) (inc acc) res)
            (if (= (premier :status) :init)
              (recur (rest s) (inc acc) (conj res (premier :value)))
              (recur (rest s) (inc acc) res)))
          (recur (rest s) (inc acc) res))
         res))
    #{}))

(fact
 (values-except (g/block sudoku-grid 1) 1) => #{3 9 6 8})

(fact
 (values-except (g/block sudoku-grid 1) 4) => #{3 9 5 8})

(defn mk-conflict [kind cx cy value]
  {:status :conflict
   :kind kind
   :value value})

(defn merge-conflict-kind
  [kind1 kind2]
  (cond
    (and (set? kind1) (set? kind2)) (clojure.set/union kind1 kind2)
    (set? kind1) (conj kind1 kind2)
    (set? kind2) (conj kind2 kind1)
    (= kind1 kind2) kind1
    :else (hash-set kind1 kind2)))

(fact
 (merge-conflict-kind :row :row) => :row)

(fact
 (merge-conflict-kind :row :block) => #{:row :block})

(fact
 (merge-conflict-kind :row #{:row :block}) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} :block) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} #{:block :col}) => #{:row :block :col})


(defn merge-conflict [conflict1 conflict2]
  (assoc conflict1 :kind (merge-conflict-kind (:kind conflict1) (:kind conflict2))))

(defn merge-conflicts [& conflicts]
  (apply (partial merge-with merge-conflict) conflicts))


(defn update-conflicts
  [conflict-kind cx cy value conflicts]
  (if-let [conflict (get conflicts [cx, cy])]
    (assoc conflicts [cx, cy] (mk-conflict (merge-conflict-kind conflict-kind (:kind conflict))
                                           cx cy value))
    (assoc conflicts [cx, cy] (mk-conflict conflict-kind cx cy value))))

(defn conflict-value [values except cell]
  (when-let [value (g/cell-value cell)]
    (when (and (not= (:status cell) :init)
               (contains? (values-except values except) value))
      value)))

(defn vals-init
  "Returns a vector containing all the elements initials"
  [row]
  (loop [row row, res #{}]
    (if (seq row)
      (let [cell (first row)]
        (recur (rest row) (if (= :init (cell :status))
                            (conj res (cell :value))
                            res)))
      res)))

(defn vals-freq-not-init
  "Returns a map with keys val and value frequency of apparition"
  [row]
  (loop [row row, res {}]
    (if (seq row)
      (let [cell (first row)]
        (recur (rest row) (if (or (= :set (cell :status)) (= :conflict (cell :status)))
                            (if (res (cell :value))
                              (assoc res (cell :value) (inc (res (cell :value))))
                              (assoc res (cell :value) 1))
                            res)))
      res)))

(defn row-conflicts
  "Returns a map of conflicts in a `row`."
  [row cy]
  (let [init-vect (vals-init row)
        freq-vect (vals-freq-not-init row)]
    (loop [row row, res {}, cpt 1]
      (if (seq row)
        (let [cell (first row)]
          (if (= :set (cell :status))
            (if (contains? init-vect (cell :value))
              (recur (rest row) (assoc res [cpt cy] (mk-conflict :row nil nil (cell :value))) (inc cpt))
              (if (> (freq-vect (cell :value)) 1)
                (recur (rest row) (assoc res [cpt cy] (mk-conflict :row nil nil (cell :value))) (inc cpt))
                (recur (rest row) res (inc cpt))))
            (recur (rest row) res (inc cpt))))
        res))))


(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :row, :value 1},
     [4 1] {:status :conflict, :kind :row, :value 1}})

(fact
 (row-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[6 4] {:status :conflict, :kind :row, :value 6}})

(defn rows-conflicts [grid]
  (reduce merge-conflicts {}
          (map (fn [r] (row-conflicts (g/row grid r) r)) (range 1 10))))

(defn col-conflicts
  "Returns a map of conflicts in a `col`."
  [col cy]
   (let [init-vect (vals-init col)
        freq-vect (vals-freq-not-init col)]
    (loop [col col, res {}, cpt 1]
      (if (seq col)
        (let [cell (first col)]
          (if (= :set (cell :status))
            (if (contains? init-vect (cell :value))
              (recur (rest col) (assoc res [cy cpt] (mk-conflict :col nil nil (cell :value))) (inc cpt))
              (if (> (freq-vect (cell :value)) 1)
                (recur (rest col) (assoc res [cy cpt] (mk-conflict :col nil nil (cell :value))) (inc cpt))
                (recur (rest col) res (inc cpt))))
            (recur (rest col) res (inc cpt))))
        res))))


(fact
 (col-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :col, :value 1},
     [1 4] {:status :conflict, :kind :col, :value 1}})

(fact
 (col-conflicts (map #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (col-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[4 6] {:status :conflict, :kind :col, :value 6}})

(defn cols-conflicts
  [grid] (reduce merge-conflicts {}
                 (map (fn [c] (col-conflicts (g/col grid c) c)) (range 1 10))))

(def sudoku-grid1
  [[[{:status :init, :value 5} {:status :init, :value 3} {:status :empty}
     {:status :init, :value 6} {:status :empty} {:status :empty}
     {:status :empty} {:status :init, :value 9} {:status :init, :value 8}]
    [{:status :empty} {:status :init, :value 7} {:status :empty}
     {:status :init, :value 1} {:status :init, :value 9} {:status :init, :value 5}
     {:status :empty} {:status :empty} {:status :empty}]
    [{:status :empty} {:status :empty} {:status :empty}
     {:status :empty} {:status :empty} {:status :empty}
     {:status :empty} {:status :init, :value 6} {:status :empty}]]
   [[{:status :init, :value 8} {:status :empty} {:status :empty}
     {:status :init, :value 4} {:status :empty} {:status :empty}
     {:status :init, :value 7} {:status :empty} {:status :empty}]
    [{:status :empty} {:status :init, :value 6} {:status :empty}
     {:status :init, :value 8} {:status :empty} {:status :init, :value 3}
     {:status :empty} {:status :init, :value 2} {:status :empty}]
    [{:status :empty} {:status :empty} {:status :init, :value 3}
     {:status :empty} {:status :empty} {:status :init, :value 1}
     {:status :empty} {:status :empty} {:status :init, :value 6}]]
   [[{:status :empty} {:status :init, :value 6} {:status :empty}
     {:status :empty} {:status :empty} {:status :empty}
     {:status :empty}{:status :empty} {:status :empty}]
    [{:status :empty} {:status :empty} {:status :empty}
     {:status :init, :value 4} {:status :init, :value 1} {:status :init, :value 9}
     {:status :empty} {:status :init, :value 8} {:status :empty}]
    [{:status :init, :value 2} {:status :init, :value 8} {:status :empty}
     {:status :empty} {:status :empty} {:status :init, :value 5}
     {:status :empty} {:status :init, :value 7} {:status :init, :value 9}]]])


(defn block-to-col-row
  "Return la position de la case sur la grille en fonction du bloc et son indice dans le bloc"
  [blocknum indice]
  (let [col (mod (- blocknum 1) 3)
        lig (int (/ (- blocknum 1) 3))
        icol (+ (mod (- indice 1) 3) 1)
        ilig (+ (int (/ (- indice 1) 3)) 1)]
    [(+ (* col 3) icol) (+ (* lig 3) ilig)]))

(defn block-conflicts
  "Returns a map of conflicts in a `col`."
  [block b]
   (let [init-vect (vals-init block)
        freq-vect (vals-freq-not-init block)]
    (loop [block block, res {}, cpt 1]
      (if (seq block)
        (let [cell (first block)]
          (if (= :set (cell :status))
            (if (contains? init-vect (cell :value))
              (recur (rest block) (assoc res (block-to-col-row b cpt) (mk-conflict :block nil nil (cell :value))) (inc cpt))
              (if (> (freq-vect (cell :value)) 1)
                (recur (rest block) (assoc res (block-to-col-row b cpt) (mk-conflict :block nil nil (cell :value))) (inc cpt))
                (recur (rest block) res (inc cpt))))
            (recur (rest block) res (inc cpt))))
        res))))

(fact
 (block-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :block, :value 1},
     [1 2] {:status :conflict, :kind :block, :value 1}})

(fact
 (block-conflicts (map #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (block-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[3 5] {:status :conflict, :kind :block, :value 6}})

;;; Ecrire les 'fact' nécessaires...

(defn blocks-conflicts
  [grid]
  (reduce merge-conflicts {}
          (map (fn [b] (block-conflicts (g/block grid b) b)) (range 1 10))))

(defn grid-conflicts
  "Compute all conflicts in the Sudoku grid."
  [grid]
  (merge-conflicts (rows-conflicts grid)
                   (cols-conflicts grid)
                   (blocks-conflicts grid)))

