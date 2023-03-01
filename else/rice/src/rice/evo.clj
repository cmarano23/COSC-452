(ns rice.evo)

;; Clojure code for producing student groups for RICE assignments
;; Lee Spector (lspector@amherst.edu) 20230213

(def students
  ["Alison Weiss <asweiss23@amherst.edu>"
   "Andrew Ni <ani24@amherst.edu>"
   "Arjun Kejriwal <akejriwal24@amherst.edu>"
   "Bayard DeMallie <bdemallie23@amherst.edu>"
   "Carson Marano <cmarano23@amherst.edu>"
   "Charles Clary <cclary24@amherst.edu>"
   "Esteban Sanchez <easanchez23@amherst.edu>"
   "Ian Husler Matute <ihuslermatute23@amherst.edu>"
   "Jason DeGraff <jdegraaff25@amherst.edu>"
   "John Cox <jcox25@amherst.edu>"
   "Megan Huang <mhuang25@amherst.edu>"
   "Mikhail Verozub <mverozub23@amherst.edu>"
   "Owen Cannon <ocannon24@amherst.edu>"
   "Regina Deri <rderi25@amherst.edu>"
   "Reihaneh Iranmanesh <riranmanesh25@amherst.edu>"
   "Sarah Park <shpark23@amherst.edu>"
   "Stefan Walzer-Goldfeld <swalzergoldfeld23@amherst.edu>"
   "Tina Zhang <szhang24@amherst.edu>"
   "William DeGroot <wdegroot23@amherst.edu>"
   "Michael Xu <mjxu25@amherst.edu>"])

(def newStudents [{:name "Carson Marano"
                :email "cmarano23@amherst.edu"
                :college "Amherst College"},
                {:name "Bayard DeMallie"
                 :email "bdemallie23@amherst.edu"
                 :college "Amherst College"}])

(:name (last newStudents))

(def prev-groups [[#{"Esteban Sanchez <easanchez23@amherst.edu>"
                     "Alison Weiss <asweiss23@amherst.edu>"
                     "Stefan Walzer-Goldfeld <swalzergoldfeld23@amherst.edu>"}
                   #{"Andrew Ni <ani24@amherst.edu>"
                     "Tina Zhang <szhang24@amherst.edu>"
                     "Arjun Kejriwal <akejriwal24@amherst.edu>"}
                   #{"Bayard DeMallie <bdemallie23@amherst.edu>"
                     "William DeGroot <wdegroot23@amherst.edu>"
                     "Jason DeGraff <jdegraaff25@amherst.edu>"}
                   #{"Carson Marano <cmarano23@amherst.edu>"
                     "Mikhail Verozub <mverozub23@amherst.edu>"
                     "Owen Cannon <ocannon24@amherst.edu>"}
                   #{"Sarah Park <shpark23@amherst.edu>"
                     "Charles Clary <cclary24@amherst.edu>"
                     "Reihaneh Iranmanesh <riranmanesh25@amherst.edu>"
                     "Michael Xu <mjxu25@amherst.edu>"}
                   #{"Regina Deri <rderi25@amherst.edu>"
                     "Ian Husler Matute <ihuslermatute23@amherst.edu>"
                     "Megan Huang <mhuang25@amherst.edu>"
                     "John Cox <jcox25@amherst.edu>"}]
                  [#{"Tina Zhang <szhang24@amherst.edu>"
                     "Bayard DeMallie <bdemallie23@amherst.edu>"
                     "John Cox <jcox25@amherst.edu>"}
                   #{"Andrew Ni <ani24@amherst.edu>"
                     "Mikhail Verozub <mverozub23@amherst.edu>"
                     "Reihaneh Iranmanesh <riranmanesh25@amherst.edu>"}
                   #{"Megan Huang <mhuang25@amherst.edu>"
                     "Esteban Sanchez <easanchez23@amherst.edu>"
                     "Sarah Park <shpark23@amherst.edu>"}
                   #{"William DeGroot <wdegroot23@amherst.edu>"
                     "Ian Husler Matute <ihuslermatute23@amherst.edu>"
                     "Charles Clary <cclary24@amherst.edu>"}
                   #{"Owen Cannon <ocannon24@amherst.edu>"
                     "Regina Deri <rderi25@amherst.edu>"
                     "Arjun Kejriwal <akejriwal24@amherst.edu>"}
                   #{"Carson Marano <cmarano23@amherst.edu>"
                     "Michael Xu <mjxu25@amherst.edu>"
                     "Alison Weiss <asweiss23@amherst.edu>"}
                   #{"Jason DeGraff <jdegraaff25@amherst.edu>"
                     "Stefan Walzer-Goldfeld <swalzergoldfeld23@amherst.edu>"}]])

(defn new-groups []
  "Returns a vector random groups, each of which is set of students.
   For now, the maximum set size is hardcoded here."
  (mapv set (partition-all 3 (shuffle students))))

(defn conflicts [groups]
  "Returns the number of groups overlapping with prev-groups by at 
   least 2 members."
  (count (filter (fn [g]
                   (some (fn [prev]
                           (>= (count (clojure.set/intersection g prev)) 
                               2))
                         (apply concat prev-groups)))
                 groups)))

(defn better [groups1 groups2]
  "Returns true if groups1 is better than groups2."
  (< (conflicts groups1) 
     (conflicts groups2)))

(defn mutate [groups]
  "Returns groups after a single random swap between two groups."
  (let [shuffled (shuffle groups)
        g1 (shuffle (vec (first shuffled)))
        g2 (shuffle (vec (second shuffled)))]
    (concat (drop 2 shuffled)
            [(set (conj (rest g1) (first g2)))
             (set (conj (rest g2) (first g1)))])))

(defn evolve
  "Runs a genetic algorithm, using the given population size and 
   maximum number of generations, to find a good next set of RICE
   groups. The population size should be an even number that is at
   least 2, and it will be coerced to such a value if it isn't."
  [popsize maxgens]
  (let [ps (max 2 (if (odd? popsize)
                    (inc popsize)
                    popsize))]
    (println "Population size:" ps)
    (loop [generation 0
           population (sort better (repeatedly ps new-groups))]
      (let [best (first population)]
        (println "Generation:" generation ", least conflicts:" (conflicts best))
        (if (or (zero? (conflicts best)) ;; found groups with no conflicts
                (>= generation maxgens)) ;; or tried for maxgens generations
          (println "Success:" best)
          (let [better-half (take (int (/ ps 2)) population)]
            (recur
             (inc generation)
             (sort better (map mutate
                               (concat better-half better-half))))))))))

(evolve 4 100) 