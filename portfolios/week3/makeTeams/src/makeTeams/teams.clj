(ns makeTeams.teams)

;; Clojure code for producing random, even teams of players
;; Some code taken / adapted from Prof. Spector's RICE Group Generation Code

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

;; Count the number of goalies
(def num-G (count (players-with-position players "G")))

;; Count the number of defenders
(def num-D (count (players-with-position players "D")))

;; Count the number of midfielders
(def num-M (count (players-with-position players "M")))

;; Count the number of attackers
(def num-A (count (players-with-position players "A")))

(quot num-G 2)

;; new teams function from last week
;; (defn new-teams-old
;;   "Returns a vector of random teams, each of which is even numbers."
;;   []
;;   ;; Shuffle the players and split them into two teams
;;   (let [goalies (shuffle (players-with-position players "G"))]
;;     (def team-1-G (subvec goalies 0 (quot num-G 2)))
;;     (def team-2-G (subvec goalies (quot num-G 2) num-G)))
;;   (let [defenders (shuffle (players-with-position players "D"))]
;;     (def team-1-D (subvec defenders 0 (quot num-D 2)))
;;     (def team-2-D (subvec defenders (quot num-D 2) num-D)))
;;   (let [midfielders (shuffle (players-with-position players "M"))]
;;     (def team-1-M (subvec midfielders 0 (quot num-M 2)))
;;     (def team-2-M (subvec midfielders (quot num-M 2) num-M)))
;;   (let [attackers (shuffle (players-with-position players "A"))]
;;     (def team-1-A (subvec attackers 0 (quot num-A 2)))
;;     (def team-2-A (subvec attackers (quot num-A 2) num-A)))
;;   ;; Concatenate team 1 and team 2
;;   (def team-1 (concat team-1-G team-1-D team-1-M team-1-A))
;;   (def team-2 (concat team-2-G team-2-D team-2-M team-2-A)))

;; Updated new teams function to remove inline def statements
;; and make code to make the teams much cleaner
(defn new-teams
  "Returns a vector of 2 random teams, each of which is even numbers."
  []
  ;; Shuffle the players and split them into two teams
  (let [goalies (shuffle (players-with-position players "G"))
        defenders (shuffle (players-with-position players "D"))
        midfielders (shuffle (players-with-position players "M"))
        attackers (shuffle (players-with-position players "A"))
        ;; Make splits for team 1
        team-1-G (subvec goalies 0 (quot num-G 2))
        team-1-D (subvec defenders 0 (quot num-D 2))
        team-1-M (subvec midfielders 0 (quot num-M 2))
        team-1-A (subvec attackers 0 (quot num-A 2))
        ;; Concatenate and make team 1
        team-1 (concat team-1-G team-1-D team-1-M team-1-A)
        ;; Make splits for team 2
        team-2-G (subvec goalies (quot num-G 2) num-G)
        team-2-D (subvec defenders (quot num-D 2) num-D)
        team-2-M (subvec midfielders (quot num-M 2) num-M)
        team-2-A (subvec attackers (quot num-A 2) num-A)
        ;; Concatenate and make team 2
        team-2 (concat team-2-G team-2-D team-2-M team-2-A)]
      ;; Return the vector of teams
      (vec [team-1 team-2])))

;; Call new teams
(new-teams)

;; Previous Teams
(def prev-teams [[#{"Mitch"
                  "Ayden"
                  "Robinson"
                  "Ryan"
                  "Ethan"
                  "Carter"
                  "Jordan"
                  "Jacob"
                  "John"
                  "Louie"
                  "Thomas"
                  "Steve"
                  "Brodie"
                  "Will F"
                  "Will M"
                  "Alex V"
                  "Sam"
                  "Mason"
                  "Montana"
                  "Alex"
                  "Tanyr"
                  "Bob"}
                 #{"Spencer"
                  "Rob"
                  "Carson"
                  "Myles"
                  "Lawson"
                  "Thompson"
                  "Patrick"
                  "Andrew"
                  "Jack"
                  "Tim"
                  "Bennet"
                  "Ben"
                  "Matt S"
                  "Jack M"
                  "Nick"
                  "Paul"
                  "Connor"
                  "Zion"
                  "Bayard"
                  "Dylan"
                  "Brock"
                  "Jake"
                  "Matt"}]
                ;;  [#{"Ayden"
                ;;   "Spencer"
                ;;   "Bennet"
                ;;   "Tim"
                ;;   "Patrick"
                ;;   "Robinson"
                ;;   "Jordan"
                ;;   "Jacob"
                ;;   "Lawson"
                ;;   "Thompson"
                ;;   "Paul"
                ;;   "Brodie"
                ;;   "Ben"
                ;;   "Will M"
                ;;   "Matt S"
                ;;   "Steve"
                ;;   "Mason"
                ;;   "Connor"
                ;;   "Brock"
                ;;   "Bob"
                ;;   "Alex"
                ;;   "Montana"}
                ;;  #{"Mitch"
                ;;   "Rob"
                ;;   "Carson"
                ;;   "Myles"
                ;;   "Carter"
                ;;   "Jack"
                ;;   "Ethan"
                ;;   "Andrew"
                ;;   "Louie"
                ;;   "John"
                ;;   "Ryan"
                ;;   "Bayard"
                ;;   "Will F"
                ;;   "Nick"
                ;;   "Zion"
                ;;   "Thomas"
                ;;   "Alex V"
                ;;   "Jack M"
                ;;   "Sam"
                ;;   "Tanyr"
                ;;   "Matt"
                ;;   "Jake"
                ;;   "Dylan"}]
                  ])



;; Find and ID Conflicts
(defn conflicts
  "Returns the number of teams overlapping with prev-teams by at 
   least 12 members."
  [teams]
  (count (filter (fn [g]
                   (some (fn [prev]
                           (>= (count (clojure.set/intersection (set g) prev))
                               12))
                         (apply concat prev-teams)))
                 teams)))

;; Test conflicts
(conflicts (new-teams))

;; Function to check if one set of teams is better than the other
(defn better
  "Returns true if teams-1 is better than teams-2."
  [teams-1 teams-2]
  (< (conflicts teams-1) 
     (conflicts teams-2)))

;; A function to mutate a set of teams
(defn mutate
  "Returns teams after a single random swap between two teams."
  [teams]
  (let [shuffled (shuffle teams)
        t1 (shuffle (vec (first shuffled)))
        t2 (shuffle (vec (second shuffled)))]
    (concat (drop 2 shuffled)
            [(set (conj (rest t1) (first t2)))
             (set (conj (rest t2) (first t1)))])))

;; Function to evolve the teams
(defn evolve
  "Runs a genetic algorithm, using the given population size and 
   maximum number of generations, to find a good next set of RICE
   groups. The population size should be an even number that is at
   least 20, and it will be coerced to such a value if it isn't."
  [popsize maxgens]
  (let [ps (max 22 (if (odd? popsize)
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

(evolve 22 1000)