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

(quot num-G 2)

(defn new-teams []
  "Returns a vector random groups, each of which is set of students.
   For now, the maximum set size is hardcoded here."
  (mapv set (partition-all 22 (shuffle players))))

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