(ns mrsudoku.view
  (:require
   [mrsudoku.grid :as g]
   [mrsudoku.solve :as solve]
   ;;[mrsudoku.control :as control] ;;test
   [seesaw.core :refer [frame label text config! grid-panel
                        horizontal-panel vertical-panel button separator
                        alert invoke-later text! pack! show!]]
   [seesaw.border :refer [line-border]]
   [seesaw.chooser :refer [choose-file]]))

(def default-color "white")
(def conflict-color "red")
(def set-color "blue")
(def solved-color "gray")

(defn mk-cell-view
  [cell cx cy ctrl]
  (case (:status cell)
    :init (label :text (str (:value cell))
                 :h-text-position :center
                 :v-text-position :center
                 :halign :center
                 :valign :center
                 :background default-color)
    :solved (label :text (str (:value cell))
                 :h-text-position :center
                 :v-text-position :center
                 :halign :center
                 :valign :center
                 :background solved-color)
    :empty (let [cell-widget (text :columns 1
                                   :halign :center
                                   :id (keyword (str "cell-" cx "-" cy))
                                   :foreground set-color
                                   :background default-color)]
             (config! cell-widget
                      :listen [:document
                               ;; XXX: normally, we should not depend from the controller
                               ;;      but it's an emblamatic counter-example
                               ((resolve 'mrsudoku.control/cell-input-handler) ctrl cell-widget cx cy)])
             cell-widget)
    (throw (ex-info "Can only build widget for :init or :empty cells." {:cell cell,
                                                                        :cx cx,
                                                                        :cy cy}))))

(defn mk-block-view
  [block bref ctrl]
  (let [cell-widgets (g/reduce-block
                      (fn [widgets _ cx cy cell]
                        (conj widgets (mk-cell-view cell cx cy ctrl))) [] block bref)]
    (grid-panel :rows 3
                :columns 3
                :hgap 3
                :vgap 3
                :border (line-border :thickness 2 :color "black")
                :items cell-widgets
                :id (keyword (str "block-" bref)))))

(defn mk-grid-view [grid ctrl]
  (let [block-widgets (for [i (range 1 10)]
                        (mk-block-view (g/block grid i) i ctrl))]
    (grid-panel :rows 3
                :columns 3
                :border 6
                :hgap 6
                :vgap 6
                :items (into [] block-widgets))))

(defn update-cell-view!
  [cell cell-widget]
  (case (:status cell)
    :conflict (config! cell-widget :background conflict-color)
    (:set :init :empty) (config! cell-widget :background default-color)
    :solved (config! cell-widget :backround solved-color :editable? false)
    (throw (ex-info "Cannot update cell widget." {:cell cell :cell-widget cell-widget}))))


(defn mk-main-frame [grid ctrl]
  (let [grid-widget (mk-grid-view grid ctrl)
        main-frame (frame :title "MrSudoku"
                          :content (horizontal-panel
                                    :items [grid-widget
                                            [:fill-h 32]
                                            (vertical-panel
                                             :items [:fill-v
                                                     (grid-panel
                                                      :columns 1
                                                      :vgap 20
                                                      :items [(button :text "Load"
                                                                      :listen [:mouse-clicked (fn [event]
                                                                                                (swap! ctrl (fn[m] (assoc m :grid (solve/load-grid (solve/csv-seq (choose-file :filters [["Sudoku (.sudo)" ["sudo"]]] :remember-directory? true))))))
                                                                                                (config! grid-widget :items (into [] (for [i (range 1 10)] (mk-block-view (g/block ((deref ctrl) :grid) i) i ctrl)))))])
                                                              (button :text "Solve"
                                                                      :listen [:mouse-clicked (fn [event]
;;                                                                                                 (swap! ctrl (fn[m] (assoc m :grid (first (solve/solve (m :grid))))))
                                                                                                (swap! ctrl (fn[m] (assoc m :grid (first (solve/new-solve (m :grid))))))
                                                                                                (config! grid-widget :items (into [] (for [i (range 1 10)] (mk-block-view (g/block ((deref ctrl) :grid) i) i ctrl))))
                                                                                                ((resolve 'mrsudoku.control/update-conflicts!) ctrl)
                                                                                                (alert "Solved !"))])
                                                              (button :text "Quit"
                                                                      :listen [:action (fn [event] (System/exit 0))])])
                                                     :fill-v])
                                            [:fill-h 32]])
                          :minimum-size [540 :by 380]
                          :on-close :exit)]
    (swap! ctrl #(assoc % :grid-widget grid-widget :main-frame main-frame))
    main-frame))
