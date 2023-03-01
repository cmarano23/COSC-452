;; Clojure code for an evolutionary algorithm for traveling salesman problems
;; Lee Spector, 20200221

(ns dist.findDist)

(def all-cities '(amherst northampton greenfield north-adams springfield holyoke ashfield belchertown))

(def distances {#{'amherst 'northampton}     7.8
                #{'amherst 'greenfield}      20.5
                #{'amherst 'north-adams}     55.2
                #{'amherst 'springfield}     25.6
                #{'amherst 'holyoke}         14.8
                #{'amherst 'ashfield}        24.8
                #{'amherst 'belchertown}     9.9
                #{'northampton 'greenfield}  25.0
                #{'northampton 'north-adams} 41.5
                #{'northampton 'springfield} 19.6
                #{'northampton 'holyoke}     11.3
                #{'northampton 'ashfield}    20.2
                #{'northampton 'belchertown} 15.2
                #{'greenfield 'north-adams}  41.5
                #{'greenfield 'springfield}  43.8
                #{'greenfield 'holyoke}      35.5
                #{'greenfield 'ashfield}     24.9
                #{'greenfield 'belchertown}  37.8
                #{'north-adams 'springfield} 73.4
                #{'north-adams 'holyoke}     65.2
                #{'north-adams 'ashfield}    28.6
                #{'north-adams 'belchertown} 55.3
                #{'springfield 'holyoke}     8.2
                #{'springfield 'ashfield}    43.0
                #{'springfield 'belchertown} 19.3
                #{'holyoke 'ashfield}        34.7
                #{'holyoke 'belchertown}     13.3
                #{'ashfield 'belchertown}    34.0})

(defn total-distance [cities]
  (reduce + (map #(get distances (set %))
                 (partition 2 1 cities))))

;; the distance of the path specified by the order in all-cities
#_(total-distance all-cities)

;; a random other oder
#_(total-distance (shuffle all-cities))

(defn new-individual
  "Returns a new, random individual."
  []
  (let [genome (shuffle all-cities)]
    {:genome genome
     :error  (total-distance genome)}))

(defn best
  "Returns the best of the given individuals."
  [individuals]
  (reduce (fn [i1 i2]
            (if (< (:error i1) (:error i2))
              i1
              i2))
          individuals))

(defn select
  "Returns an individual selected from population using a tournament."
  [population]
  (best (repeatedly 2 #(rand-nth population))))

(defn mutate
  "Returns a mutated copy of genome."
  [cities]
  (let [[c1 c2] (take 2 (shuffle cities))]
    (replace {c1 c2, c2 c1} cities)))

(defn make-child
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  [population]
  (let [new-genome (mutate (:genome (select population)))]
    {:genome new-genome
     :error  (total-distance new-genome)}))

(defn report
  "Prints a report on the status of the population at the given generation."
  [generation population]
  (let [current-best (best population)]
    (println {:generation   generation
              :best-error   (:error current-best)
              :diversity    (float (/ (count (distinct population))
                                      (count population)))
              :best-genome  (:genome current-best)})))

(defn evolve-tsp
  "Runs an evolutionary algorithm to solve, or approximately solve, a traveling
  salesman problem in the context of globally specified cities (in all-cities)
  and distances (in distances). Will always run for the specified number of
  generations"
  [population-size generations]
  (loop [population (repeatedly population-size new-individual)
         generation 0]
    (report generation population)
    (if (>= generation generations)
      (best population)
      (recur (conj (repeatedly (dec population-size) #(make-child population))
                   (best population))
             (inc generation)))))

#_(evolve-tsp 100 50)