(ns shiftScheduler.shifts)

;; Code for producing a random schedule of shifts for a group of people using a Genetic Algorithm

;; Define a map of people and their available times
(def availability
  {"Carson" #{8, 12, 16, 20},
   "Ryan" #{10, 14, 18},
   "Bob" #{8, 16, 18},
   "Sam" #{12, 14, 20},
   "George" #{8, 10, 12, 14, 16, 18, 20},
   "Jack" #{10, 16, 20}
   "Dylan" #{12, 16, 18, 22},
   "Carter" #{6, 14, 20},
   "Smith" #{8, 10, 12, 22},
   "John" #{12, 16, 20},
   "Paul" #{6, 8, 10, 12, 14, 16, 18, 20},
   "Lori" #{10, 16, 20, 22}})

;; Test a person's availability
(get availability "Carson")
(get availability "George")

;; Get a name from the map of availability
(rand-nth (keys availability))

;; Create a random schedule of shifts for a group of people
(defn new-schedule 
  "Creates a new random schedule of shifts for a group of people"
  [available-times]
  (let [schedule (for [person (keys available-times)]
                   (rand-nth (vec (get available-times person))))]
    (zipmap (keys available-times) schedule)))

;; Test the new-schedule function
(new-schedule availability)

;; Function to find the number of overlap between people in a schedule
(defn numOverlaps
  "Finds the overlap between people in a schedule"
  [schedule]
  (println "numOverlaps schedule: " schedule)
  (let [s (vals schedule)
        multiples (frequencies s)
        overmap (filter #(> % 1) (vals multiples))]
    (count overmap)))

;; Test the numOverlaps function
(numOverlaps (new-schedule availability))

;; Function to test my numOverlaps function
(defn test-numOverlaps
  "Tests the numOverlaps function"
  []
  (let [s (new-schedule availability)]
    (println "Schedule: " s)
    (println "numOverlaps: " (numOverlaps s))))

(test-numOverlaps)
;; Not working?

(defn findOverlapTimes
  "Finds the time of an overlap between people in a schedule"
  [schedule]
  (let [s (vals schedule)
        multiples (frequencies s)
        keys (keep #(if (> (val %) 1) (key %)) multiples)]
    ;; (println "s: " s)
    ;; (println "multiples: " multiples)
    ;; (println "keys: " keys)
    keys))

(findOverlapTimes (new-schedule availability))

;; Function to find overlap between two schedules
(defn conflicts
  "Finds the conflicts between two schedules"
  [schedule1 schedule2]
  (let [s1 (set (vals schedule1))
        s2 (set (vals schedule2))]
    (count (clojure.set/intersection s1 s2))))

;; Test the conflicts function
(conflicts (new-schedule availability) (new-schedule availability))

;; Function to test the conflicts function
(defn test-conflicts
  "Tests the conflicts function"
  []
  (let [s1 (new-schedule availability)
        s2 (new-schedule availability)]
    (println "Schedule 1: " s1)
    (println "Schedule 2: " s2)
    (println "conflicts: " (conflicts s1 s2))))

(test-conflicts)

(defn better
  "Returns true if the first schedule is better than second schedule, and false otherwise."
  [s1 s2]
  (< (numOverlaps s1)
     (numOverlaps s2)))

;; Test the better function
(better (new-schedule availability) (new-schedule availability))

;; Function to get a key given a value (adapted from last week's teams code)
(defn person-with-time [schedule time]
  (vec (for [[name value] schedule :when (= value time)] name)))

(person-with-time (new-schedule availability) 8)

;; Function to mutate
(defn mutate
  "Returns schedules after a swap of times."
  [schedules]
  (let [s1overlap (findOverlapTimes (first schedules))
        s1people (for [time s1overlap] (person-with-time (first schedules) time))
        s1peopleconcat (apply concat s1people)
        s2times (for [person s1peopleconcat] (get (second schedules) person))
        newMap (zipmap s1peopleconcat s2times)
        swapped (merge (first schedules) newMap)]
    ;; (println "s2times:" s2times)
    ;; (println "s1overlap:" s1overlap)
    ;; (println "s1peopleconcat:" s1peopleconcat)
    ;; (println "newMap:" newMap)
    (println "swapped: " swapped)
    swapped))

;; Test the mutate function
(let [s1 (new-schedule availability)
      s2 (new-schedule availability)
      s3 (new-schedule availability)
      mu1 (mutate [s1 s2])
      mu2 (mutate [mu1 s3])]
  (println "Schedule 1: " s1)
  (println "Schedule 2: " s2)
  (println "Mutated: " mu2))

(mutate [(new-schedule availability) (new-schedule availability)])

;; evolution function from scratch
(defn evolve
  "Runs an evolutionary algorithm"
  [maxgens]
  (loop [generation 0
         population (vec (sort better (repeatedly 12 #(new-schedule availability))))]
    (let [best (first population)]
      (println "Generation:" generation ", least overlaps:" (numOverlaps best))
      (if (or (zero? (numOverlaps best)) ;; found groups with no conflicts
              (>= generation maxgens)) ;; or tried for maxgens generations
        (println "Success:" best)
        (let [better-half (take (int (/ 12 2)) population)
              best-two (take 2 population)]
          (println "best-two: " best-two)
          (println "here")
          ;; (println "sort thing: " (sort better (mutate best-two)))
          (recur
           (inc generation)
           (sort better (mutate better-half))))))))

;; Test the evolve function
(evolve 100)

;; Error is coming from the mutate function only performing on first two schedules
;; Thus, the numOverlaps function is receiving only a vector of a key and value instead of the entire map
;; I need to figure out how to get the mutate function to perform on all schedules in the better-half vector