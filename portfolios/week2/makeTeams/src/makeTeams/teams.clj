(ns makeTeams.teams)

;; Clojure code for producing even teams of players
;; Inspired by Prof. Lee Spector's Rice Group Code

;; Define a list of players
;; (def players
;;   [{:name "Carson"
;;     :position "G"},
;;    {:name "Mitch"
;;     :position "G"},
;;    {:name "Rob"
;;     :position "G"},
;;    {:name "Spencer"
;;     :position "G"},
;;    {:name "Ayden",
;;     :position "G"},
;;    {:name "Montana"
;;     :position "A"},
;;    {:name "Paul"
;;     :position "M"},
;;    {:name "Ethan"
;;     :position "D"},
;;    {:name "Matt"
;;     :position "A"},
;;    {:name "Tanyr",
;;     :position "A"},
;;    {:name "Bayard"
;;     :position "M"},
;;    {:name "Jake"
;;     :position "A"},
;;    {:name "Will M"
;;     :position "M"},
;;    {:name "Brodie"
;;     :position "M"},
;;    {:name "Brock",
;;     :position "A"},
;;    {:name "Will F"
;;     :position "M"},
;;    {:name "Mason"
;;     :position "M"},
;;    {:name "Connor"
;;     :position "M"},
;;    {:name "Thomas"
;;     :position "M"},
;;    {:name "Ryan",
;;     :position "D"},
;;    {:name "Nick"
;;     :position "M"},
;;    {:name "Dylan"
;;     :position "A"},
;;    {:name "Matt S"
;;     :position "M"},
;;    {:name "Robinson"
;;     :position "D"},
;;    {:name "John",
;;     :position "D"},
;;    {:name "Lawson"
;;     :position "D"},
;;    {:name "Steve"
;;     :position "M"},
;;    {:name "Jacob"
;;     :position "D"},
;;    {:name "Alex"
;;     :position "A"},
;;    {:name "Andrew",
;;     :position "D"},
;;    {:name "Jack"
;;     :position "D"},
;;    {:name "Louie"
;;     :position "D"},
;;    {:name "Tim"
;;     :position "D"},
;;    {:name "Jack M"
;;     :position "M"},
;;    {:name "Bennet",
;;     :position "D"},
;;    {:name "Myles"
;;     :position "D"},
;;    {:name "Alex V"
;;     :position "M"},
;;    {:name "Jordan"
;;     :position "D"},
;;    {:name "Thompson"
;;     :position "D"},
;;    {:name "Patrick",
;;     :position "D"},
;;    {:name "Zion"
;;     :position "M"},
;;    {:name "Bob"
;;     :position "A"},
;;    {:name "Ben"
;;     :position "M"},
;;    {:name "Sam",
;;     :position "M"},
;;    {:name "Carter"
;;     :position "D"}])

;; Define a map of players
(def players
  {"Carson" "G",
    "Mitch" "G",
    "Rob" "G",
    "Spencer" "G",
    "Ayden" "G",
    "Montana" "A",
    "Paul" "M",
    "Ethan" "D",
    "Matt" "A",
    "Tanyr" "A",
    "Bayard" "M",
    "Jake" "A",
    "Will M" "M",
    "Brodie" "M",
    "Brock" "A",
    "Will F" "M",
    "Mason" "M",
    "Connor" "M",
    "Thomas" "M",
    "Ryan" "D",
    "Nick" "M",
    "Dylan" "A",
    "Matt S" "M",
    "Robinson" "D",
    "John" "D",
    "Lawson" "D",
    "Steve" "M",
    "Jacob" "D",
    "Alex" "A",
    "Andrew" "D",
    "Jack" "D",
    "Louie" "D",
    "Tim" "D",
    "Jack M" "M",
    "Bennet" "D",
    "Myles" "D",
    "Alex V" "M",
    "Jordan" "D",
    "Thompson" "D",
    "Patrick" "D",
    "Zion" "M",
    "Bob" "A",
    "Ben" "M",
    "Sam" "M",
    "Carter" "D"})

;; Test a player name
(get players "Carson")

;; Return all the players with a given position
(defn players-with-position [map-of-players position]
  (vec (for [[name value] map-of-players :when (= value position)] name)))

;; Test the goalies
(players-with-position players "G")

;; Test the defenders
(players-with-position players "D")

;; Test the midfielders
(players-with-position players "M")

;; Test the attackers
(players-with-position players "A")

;; Yesterday's teams
(def prev-team [#{"Esteban Sanchez <easanchez23@amherst.edu>"
                     "Alison Weiss <asweiss23@amherst.edu>"
                     "Stefan Walzer-Goldfeld <swalzergoldfeld23@amherst.edu>"}
                   #{"Andrew Ni <ani24@amherst.edu>"
                     "Tina Zhang <szhang24@amherst.edu>"
                     "Arjun Kejriwal <akejriwal24@amherst.edu>"}])

(defn new-teams []
  "Returns a vector random groups, each of which is set of students.
   For now, the maximum set size is hardcoded here."
  (mapv set (partition-all 22 (shuffle players))))

(defn conflicts [groups]
  "Returns the number of groups overlapping with prev-groups by at 
   least 10 members."
  (count (filter (fn [g]
                   (some (fn [prev]
                           (>= (count (clojure.set/intersection g prev)) 
                               10))
                         (apply concat prev-team)))
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
           population (sort better (repeatedly ps new-teams))]
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