(ns mrsudoku.solver
  (:gen-class))

;;Structure de graph :- { cle_noeud1 #{successeur1 succeseur2 ..}, cle_noeud2 #{successeur1 succeseur2} ... }
(def graph1 {:A #{:B :D}
             :B #{:C}
             :C #{:F}
             :D #{:E}
             :E #{:F :B}
             :F #{} } )

(defn add-vertex [graph id]
  (if (get graph id)  ;; si id deja dans graph
    graph       ;;rien faire
    (assoc graph id #{} ))) ;;ajouter vertex avec successeurs vide


(defn add-edge [graph src dest]
  (update graph src
          (fn [dests] (conj dests dest))))

(defn remove-edge [graph src dest]
  (update graph src (fn [dests] (disj dests dest))))

(defn all-edges [graph]
  "Retourne la sequence paresseuse des arretes (couples [src dst]) du graph"
  (if (seq graph)
    (let  [ [src dsts] (first graph)]
      (lazy-cat (map (fn [dst] [src,dst]) dsts)   ;; fn rend le couple [src,dst] d'un dst; en appliquant le map on le fait sur tous les dsts
                (all-edges (rest graph))))
    ()))

(defn transpose[graph]
  "Construit la transpose du graph ( 'change le sens des fleches dans le graph')"
  (loop [edges (all-edges graph), res {}]
    (if (seq edges)
      (let [ [src, dst] (first edges)]
             (recur (rest edges) (-> res
                                    (add-vertex src)
                                    (add-vertex dst)
                                    (add-edge dst src))))
             res)))

(defn dfs
  ([graph vert fvert facc init]
   (first (dfs graph vert fvert facc init #{})))   ;;premiere arite -> retourner dfs avec visited = #{}
  ([graph vert fvert facc acc visited]   ;; 2e arrite
    (if (visited vert)
      [acc visited]
      ;;pas encore visitée
      (loop [todo (list vert), visited visited, acc acc]
        (if (seq todo)
          (let [vert (first todo)
                todo' (rest todo)
                valvert (fvert vert)
                acc' (facc acc valvert)
                visited' (conj visited vert)
                succs (filter #(not (contains? visited' %)) (get graph vert))]
            (recur (distinct (concat succs todo')) visited' acc'))
          ;;plus de sommets
          [acc visited])))))

(defn bfs [graph vert fvert facc init]
  (loop [todo (list vert), visited #{}, acc init]
    (if (seq todo)
      (let [vert (first todo)
            todo' (rest todo)
            valvert (fvert vert)
            acc' (facc acc valvert)
            visited' (conj visited vert)
            succs (filter #(not (contains? visited' %)) (get graph vert))]
        (recur (distinct (concat todo' succs)) visited' acc'))
      ;;plus de sommets
      [acc visited])))

(defn dfs-post
  ([graph vert fvert facc init]
   (first (dfs-post graph vert fvert facc init #{})))
  ([graph vert fvert facc acc visited]
   (if (visited vert)
     [acc visited]
     ;;pas encore visité
     (let [visited (conj visited vert)
           succs (filter #(not (contains? visited %)) (get graph vert))]
       (loop [succs succs, visited visited, acc acc]
         (if (seq succs)
           (let [[acc',visited'] (dfs-post graph (first succs) fvert facc acc visited)]
             (recur (rest succs) visited' acc'))
             ;;plus de successeur
             [(facc acc (fvert vert)) visited]))))))

(dfs-post graph1 :A (fn [_] 1) + 0)

(defn augment [bigraph src visited match]
  (loop [dests (get bigraph src), visited visited, match match]
    (if (seq dests)
      (if (visited (first dests))
        (recur (rest dests) visited match)
        ;;pas encore visité
        (if-let [msrc (get match (first dests))]
          (let [[found,visited',match'] (augment bigraph msrc (conj visited (first dests)) match)]
            (if found
              [true, visited', (assoc match' (first dests) src)]
              (recur (rest dests) visited' match'))) ;;(first dests) n'est pas dispo
          ;;pas encore de match pour (first dests)
          [true, (conj visited (first dests)), (assoc match (first dests) src)]))
      ;;plus de destination possible
      [false,visited, match])))

(defn max-matching [bigraph]
  (loop [verts (keys bigraph), match {}]
    (if (seq verts)
      (let [[_,_,match'] (augment bigraph (first verts) #{} match)]
        (recur (rest verts) match'))
      ;;plus de sommets
      (if (= (count match) (count bigraph))
        match
        nil))))

(def alldiff-doms {:v1 #{1 2 3}
            :v2 #{1 2 4 5}
            :v3 #{4 5 6}
            :v4 #{4 5 6}
            :v5 #{4 5 6}})

(defn cmgraph [doms]
  (if-let [matching (max-matching doms)]
    (reduce (fn [graph [src,dest]]
              (-> graph
                  (add-vertex src)
                  (remove-edge dest src)
                  (add-edge src dest))) doms matching)
    nil))

(def cmgraph1 {:v1 #{1 2} :v2 #{2 4 5} :v3 #{4 6} :v4 #{4 5} :v5 #{5 6}
               1 #{:v2} 3 #{:v1} 4 #{:v5} 6 #{:v4} 5 #{:v2}})

(defn dfs-stack-from [graph vert visited stk]
  (let [visited (conj visited vert)
        dests (clojure.set/difference (get graph vert) visited)]
    (loop [dests dests, visited visited, stk stk]
      (if (seq dests)
        (if (visited (first dests))
          (recur (rest dests) visited stk)
          (let [[stk',visited'] (dfs-stack-from graph (first dests) visited stk)]
            (recur (rest dests) visited' stk')))
        ;;plus de successeur
        [(cons vert stk) visited]))))

;;Phase 1 : DFS avec construction d'une pile
(defn dfs-stack [graph]
  (loop [verts (keys graph), visited #{}, stk ()]
    (if (seq verts)
      (if (visited (first verts))
        (recur (rest verts) visited stk)
        (let [[stk',visited'] (dfs-stack-from graph (first verts) visited stk)]
          (recur (rest verts) visited' stk')))
      stk)))

;;Phase 2: DFS sur le graphe transposé pour produire les SCC
(def scc-init [#{} #{}])

(defn scc-add [doms scc vert]
  (let [[vars,vals] scc]
    (if (contains? doms vert)
      [(conj vars vert), vals]
      [vars, (conj vals vert)])))

(defn compute-scc [doms graph stack]
  (let [tgraph (transpose graph)]
    (loop [stack stack, visited #{}, sccs []]
      (if (seq stack)
        (if (visited (first stack))
          (recur (rest stack) visited sccs)
          (let [[scc, visited'] (dfs tgraph (first stack) identity
                                      (fn [scc vert]
                                        (scc-add doms scc vert))
                                     scc-init
                                     visited)]
            (recur (rest stack) visited' (conj sccs scc))))
        ;; plus de sommets à traiter
        sccs))))

;;Algorithme principal
(defn scc [doms]
  (if-let [cgraph (cmgraph doms)]
    (compute-scc doms cgraph (dfs-stack cgraph))
    nil))

;;Phase 3: Solution modulo permutations
(defn doms-from-component [doms component]
  (let [[cvars,cvals] component]
    (reduce (fn [res var] (assoc res var cvals)) doms cvars)))

(defn doms-from-scc [scc]
  (reduce doms-from-component {} scc))

;;Phase 4: Accessibilité des valeurs isolées
(defn isolated-values [scc]
  (reduce (fn [res component]
            (let [[cvars cvals] component]
              (if (empty? cvars)
                (clojure.set/union res cvals)
                res))) #{} scc))

(defn isolated-vars [scc]
  (reduce (fn [res component]
            (let [[cvars cvals] component]
              (if (empty? cvals)
                (clojure.set/union res cvars)
                res))) #{} scc))

(defn has-value [doms isolated-var value]
  (reduce (fn [vars [var vals]]
            (if (and (isolated-var var)
                     (contains? vals value))
              (conj vars var)
              vars)) #{} doms))

(defn add-value [doms vars value]
  (reduce (fn [doms var]
            (update doms var (fn [values]
                               (if (seq values)
                                 (conj values value)
                                 #{value})))) doms vars))

(defn isolated-vars-doms
  [doms new-vars]
  (loop [vars new-vars]
    (if (seq vars)
      (if (= 1 (count (get doms (first vars))))
        #{(first vars)}
        (recur (rest vars)))
      new-vars)))

(defn access [doms scc]
  (let [doms' (doms-from-scc scc)
        isolated-vals (isolated-values scc)
        isolated-vars (isolated-vars scc)]
    (reduce (fn [ndoms value]
              (add-value ndoms (isolated-vars-doms doms (has-value doms isolated-vars value)) value))
            doms' isolated-vals)))

;;Algorithme alldiff
(defn alldiff [doms]
  (if-let [doms' (scc doms)]
    (access doms doms')
    ;;pas de solution
    nil))

(alldiff alldiff-doms)
(alldiff {:v1 #{1} :v2 #{1}})

(defn choix-variable [doms] (first (first doms)))

(defn test-solution
  [constraints sol]
  (every? (fn [constraint] ((:check constraint) (sol (:var1 constraint)) (sol (:var2 constraint)))) constraints))

(defn generate-and-test
  [constraints doms sol]
  (if (empty? doms)
    (if (test-solution constraints sol)
      sol
      nil)
    ;;solution incomplet
    (let [x (choix-variable doms)]
      (loop [xdom (doms x)]
        (if (seq xdom)
          (let [xval (first xdom)]
            (if-let [sol' (generate-and-test constraints (dissoc doms x) (assoc sol x xval))]
              sol'
              ;;pas de solution avec x <- xval
              (recur (rest xdom))))
          ;;plus de valeur possible
          nil)))))
