(ns makeTeams.teams)

;; Clojure code for producing random, even teams of players

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
(defn names-with-value [map-of-players position]
  (vec (for [[name value] map-of-players :when (= value position)] name)))

;; Test the goalies
(names-with-value players "G")

;; Test the defenders
(names-with-value players "D")

;; Test the midfielders
(names-with-value players "M")

;; Test the attackers
(names-with-value players "A")

;; Count the number of goalies
(def num-G (count (names-with-value players "G")))

;; Count the number of defenders
(def num-D (count (names-with-value players "D")))

;; Count the number of midfielders
(def num-M (count (names-with-value players "M")))

;; Count the number of attackers
(def num-A (count (names-with-value players "A")))

(quot num-G 2)

(defn new-teams []
  "Returns a vector random groups, each of which is set of students.
   For now, the maximum set size is hardcoded here."
  ;; Shuffle the players and split them into two teams
  (let [goalies (shuffle (names-with-value players "G"))]
    (def team-1-G (subvec goalies 0 (quot num-G 2)))
    (def team-2-G (subvec goalies (quot num-G 2) num-G)))
  (let [defenders (shuffle (names-with-value players "D"))]
    (def team-1-D (subvec defenders 0 (quot num-D 2)))
    (def team-2-D (subvec defenders (quot num-D 2) num-D)))
  (let [midfielders (shuffle (names-with-value players "M"))]
    (def team-1-M (subvec midfielders 0 (quot num-M 2)))
    (def team-2-M (subvec midfielders (quot num-M 2) num-M)))
  (let [attackers (shuffle (names-with-value players "A"))]
    (def team-1-A (subvec attackers 0 (quot num-A 2)))
    (def team-2-A (subvec attackers (quot num-A 2) num-A)))
  ;; Concatenate team 1 and team 2
  (def team-1 (concat team-1-G team-1-D team-1-M team-1-A))
  (def team-2 (concat team-2-G team-2-D team-2-M team-2-A)))

;; Call new teams
(new-teams)

;; Print positions of teams
(println "Team 1 Goalies: " team-1-G)
(println "Team 2 Goalies: " team-2-G)
(println "Team 1 Defenders: " team-1-D)
(println "Team 2 Defenders: " team-2-D)
(println "Team 1 Midfielders: " team-1-M)
(println "Team 2 Midfielders: " team-2-M)
(println "Team 1 Attackers: " team-1-A)
(println "Team 2 Attackers: " team-2-A)

;; Print teams
(println "Team 1: " team-1)
(println "Team 2: " team-2)