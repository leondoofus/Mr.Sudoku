(ns mrsudoku.solve
  (:use midje.sweet)
  (:require
    [mrsudoku.grid :as g]
    [mrsudoku.engine :as e]
    [mrsudoku.solver :as s]
    [mrsudoku.utils :refer [concatv]]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]))

(def grid-empty
  [[;; row 1
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]],
   [;; row 2
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]],
   [;; row 3
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]
    [(g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)
     (g/mk-cell) (g/mk-cell) (g/mk-cell)]]])

;;----------TRAITEMENT DE FICHIER----------
(defn csv-seq
  "Retourne une séquence à partir d'un fichier SUDO."
  [filename]
  (with-open [in-file (io/reader filename)]
    (doall
     (into [] (csv/read-csv in-file)))))



(defn load-grid [sudo]
  (loop [lignes sudo, nligne 1, res grid-empty]
    (if (seq lignes)
      (recur (rest lignes) (inc nligne) (loop [ligne (first lignes), ncol 1, res' res]
                                         (if (seq ligne)
                                           (if (= (first ligne) ".")
                                             (recur (rest ligne) (inc ncol) res')
                                             (recur (rest ligne) (inc ncol) (g/change-cell res' ncol nligne (g/mk-cell (read-string (first ligne))))))
                                           res')))
      res)))

(defn reset-grid [grid]
  (loop [nligne 1, res grid-empty]
    (if (< nligne 10)
      (recur (inc nligne) (loop [ncol 1, res' res]
                            (if (< ncol 10)
                              (if (or (= :init ((g/cell grid ncol nligne) :status)) (= :solved ((g/cell grid ncol nligne) :status)))
                                (recur (inc ncol) (g/change-cell res' ncol nligne (g/cell grid ncol nligne)))
                                (recur (inc ncol) res'))
                              res')))
      res)))

;;----------FONCTIONS AUXILIAIRES----------
(def empty-sudoku #{1 2 3 4 5 6 7 8 9})

(defn create-key
  [n]
  (let [i1 (str (first n))
        i2 (str (second n))]
    (keyword (clojure.string/join ["C" i1 "L" i2]))))

(defn from-key
  [key]
  [(read-string (subs (name key) 1 2)) (read-string (subs (name key) 3))])

(defn create-dom-block
  [block iblock]
  (loop [block block, res {}, cpt 1]
    (if (seq block)
      (let [e (first block)]
        (if (or (= (e :status) :init) (= (e :status) :solved))
          (recur (rest block) (assoc res (create-key (e/block-to-col-row iblock cpt)) #{(e :value)}) (inc cpt))
          (recur (rest block) (assoc res (create-key (e/block-to-col-row iblock cpt)) empty-sudoku) (inc cpt))))
      (s/alldiff res))))

(defn create-dom-row
  [row irow]
  (loop [row row, res {}, cpt 1]
    (if (seq row)
      (let [e (first row)]
        (if (or (= (e :status) :init) (= (e :status) :solved))
          (recur (rest row) (assoc res (create-key [cpt irow]) #{(e :value)}) (inc cpt))
          (recur (rest row) (assoc res (create-key [cpt irow]) empty-sudoku) (inc cpt))))
      (s/alldiff res))))

(defn create-dom-col
  [col icol]
  (loop [col col, res {}, cpt 1]
    (if (seq col)
      (let [e (first col)]
        (if (or (= (e :status) :init) (= (e :status) :solved))
          (recur (rest col) (assoc res (create-key [icol cpt]) #{(e :value)}) (inc cpt))
          (recur (rest col) (assoc res (create-key [icol cpt]) empty-sudoku) (inc cpt))))
      (s/alldiff res))))

;;----------DOM PAR BLOCK----------
(defn create-all-block-dom
  [grid]
  (loop [cpt 1, res []]
    (if (<= cpt 9)
      (recur (inc cpt) (conj res (create-dom-block (g/block grid cpt) cpt)))
      (reduce (fn [m e] (merge m e)) {} res))))

;;----------DOM PAR ROW----------
(defn create-all-row-dom
  [grid]
  (loop [cpt 1, res []]
    (if (<= cpt 9)
      (recur (inc cpt) (conj res (create-dom-row (g/row grid cpt) cpt)))
      (reduce (fn [m e] (merge m e)) {} res))))

;;----------DOM PAR COL----------
(defn create-all-col-dom
  [grid]
  (loop [cpt 1, res []]
    (if (<= cpt 9)
      (recur (inc cpt) (conj res (create-dom-col (g/col grid cpt) cpt)))
      (reduce (fn [m e] (merge m e)) {} res))))

;;----------INITIATION----------
(defn init-sudoku
  []
  (let [i (into [] (range 1 10))
        keys (reduce (fn [v e] (concatv v(reduce (fn [v' e'] (conj v' (create-key [e e']))) [] i))) [] i)]
    (reduce (fn [s e] (assoc s e empty-sudoku)) {} keys)))

(defn win?
  "Test si chaque clé associe à exactement une valeur"
  [map]
  (loop [m map]
    (if (seq m)
      (if (> (count (second (first m))) 1)
        false
        (recur (rest m)))
      true)))

;;----------INTERSECTION DOMS----------
(defn merge-doms
  [dom0 dom1 dom2 dom3]
  (loop [dom1 dom1, dom2 dom2, dom3 dom3, res dom0]
    (if (seq dom1)
      (let [e1 (first dom1)
            e2 (first dom2)
            e3 (first dom3)]
        (recur (rest dom1) (rest dom2) (rest dom3) (as-> res v
                                                       (assoc v (first e1) (clojure.set/intersection (v (first e1)) (second e1)))
                                                       (assoc v (first e2) (clojure.set/intersection (v (first e2)) (second e2)))
                                                       (assoc v (first e3) (clojure.set/intersection (v (first e3)) (second e3))))))
      res)))

(defn fill-grid
  [grid dom]
  (loop [grid grid, dom dom]
    (if (seq dom)
      (let [e (first dom)
            [c l] (from-key (first e))
            d (second e)]
        (if (> (count d) 1)
          (recur grid (rest dom))
          (if (or (= ((g/cell grid c l) :status) :init) (= ((g/cell grid c l) :status) :solved))
            (recur grid (rest dom))
            (recur (g/change-cell grid c l (g/mk-cell :solved (first d))) (rest dom)))))
      grid)))

;;----------REFAIRE ALLDIFF APRES INTERSECTION----------

(defn take-ligne [dom ligne]
  (let [keys-ligne (reduce (fn [s e] (conj s (create-key [e ligne]))) [] (range 1 10))
        dom-ligne (reduce (fn [s e] (assoc s e (e dom))) {} keys-ligne)]
    dom-ligne))

(defn take-col [dom col]
  (let [keys-col (reduce (fn [s e] (conj s (create-key [col e]))) [] (range 1 10))
        dom-col (reduce (fn [s e] (assoc s e (e dom))) {} keys-col)]
    dom-col))

(defn take-block [dom block]
  (let [keys-block (reduce (fn [s e] (conj s (create-key (e/block-to-col-row block e)))) [] (range 1 10))
        dom-block (reduce (fn [s e] (assoc s e (e dom))) {} keys-block)]
    dom-block))

(defn diff-ligne [dom ligne]
  (let [keys-ligne (reduce (fn [s e] (conj s (create-key [e ligne]))) [] (range 1 10))
        dom-ligne (s/alldiff (reduce (fn [s e] (assoc s e (e dom))) {} keys-ligne))]
    (reduce (fn [s e] (assoc s (first e) (clojure.set/intersection (second e) ((first e) dom)))) dom dom-ligne)))

(defn diff-col [dom col]
  (let [keys-col (reduce (fn [s e] (conj s (create-key [col e]))) [] (range 1 10))
        dom-col (s/alldiff (reduce (fn [s e] (assoc s e (e dom))) {} keys-col))]
    (reduce (fn [s e] (assoc s (first e) (clojure.set/intersection (second e) ((first e) dom)))) dom dom-col)))

(defn diff-block [dom block]
  (let [keys-block (reduce (fn [s e] (conj s (create-key (e/block-to-col-row block e)))) [] (range 1 10))
        dom-block (s/alldiff (reduce (fn [s e] (assoc s e (e dom))) {} keys-block))]
    (reduce (fn [s e] (assoc s (first e) (clojure.set/intersection (second e) ((first e) dom)))) dom dom-block)))

(defn red [dom]
(-> dom
    (diff-ligne 1)
    (diff-ligne 2)
    (diff-ligne 3)
    (diff-ligne 4)
    (diff-ligne 5)
    (diff-ligne 6)
    (diff-ligne 7)
    (diff-ligne 8)
    (diff-ligne 9)
    (diff-col 1)
    (diff-col 2)
    (diff-col 3)
    (diff-col 4)
    (diff-col 5)
    (diff-col 6)
    (diff-col 7)
    (diff-col 8)
    (diff-col 9)
    (diff-block 1)
    (diff-block 2)
    (diff-block 3)
    (diff-block 4)
    (diff-block 5)
    (diff-block 6)
    (diff-block 7)
    (diff-block 8)
    (diff-block 9)))

(defn solve [grid]
  "Solve en utilisant alldiff"
  (loop [grid (reset-grid grid), dom (init-sudoku)]
    (if (win? dom)
      [grid dom]
      (let [domblock (create-all-block-dom grid)
            domrow (create-all-row-dom grid)
            domcol (create-all-col-dom grid)
            inter (merge-doms dom domblock domcol domrow)
            inter (red inter)]
        (if (= inter dom)
          [grid dom]
          (recur (fill-grid grid inter) inter))))))

(defn take-first-free-val [dom]
  (loop [d dom]
    (if (seq d)
      (let [e (first d)]
        (if (> (count (second e)) 1)
          [(first e) (first (second e))]
          (recur (rest d))))
      [nil nil])))

(defn new-solve [grid]
  "Cette fonction n'est pas complete, on pourrait l'utiliser à résoudre le sudoku mais il y a un petit soucis en terme de mémoire"
  (loop [[g,d] (solve grid), cpt 0]
    (if (or (win? d) (= cpt 2))
      [g,d]
      (let [[k v] (take-first-free-val d)
            [c l] (from-key k)]
        (println k v c l cpt)
        (recur (solve (g/change-cell g c l (g/mk-cell :solved v))) (inc cpt))))))

