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
   "John" #{12, 15, 16, 20},
   "Paul" #{6, 8, 10, 12, 13, 14, 16, 18, 20},
   "Lori" #{10, 16, 17, 20, 22}})

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
  ;; (println "numOverlaps schedule: " schedule)
  (let [s (vals schedule)
        multiples (frequencies s)
        overmap (filter #(> % 1) (vals multiples))]
    ;; (println "s: " s)
    ;; (println "multiples: " multiples)
    ;; (println "overmap: " overmap)
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
        s2overlap (findOverlapTimes (second schedules))
        s1people (for [time s1overlap] (person-with-time (first schedules) time))
        s2people (for [time s2overlap] (person-with-time (second schedules) time))
        s1peopleconcat (apply concat s1people)
        s2peopleconcat (apply concat s2people)
        s1times (for [person s2peopleconcat] (get (first schedules) person))
        s2times (for [person s1peopleconcat] (get (second schedules) person))
        newMap1 (zipmap s1peopleconcat s2times)
        newMap2 (zipmap s2peopleconcat s1times)
        swapped1 (merge (first schedules) newMap1)
        swapped2 (merge (second schedules) newMap2)]
    (concat (drop 2 schedules)
            [swapped1 swapped2])))

;; Test the mutate function
;; (let [s1 (new-schedule availability)
;;       s2 (new-schedule availability)
;;       s3 (new-schedule availability)
;;       mu1 (mutate [s1 s2])
;;       mu2 (mutate [mu1 s3])]
;;   (println "Schedule 1: " s1)
;;   (println "Schedule 2: " s2)
;;   (println "Mutated: " mu2))

(mutate [(new-schedule availability) (new-schedule availability)])

;; The old not as efficient mutate function
(defn old-mutate
  "Returns teams after a single random swap between two schedules."
  [schedules]
  (let [shuffled (shuffle schedules)
        t1 (shuffle (vec (first shuffled)))
        t2 (shuffle (vec (second shuffled)))]
    (concat (drop 2 shuffled)
            [(set (conj (rest t1) (first t2)))
             (set (conj (rest t2) (first t1)))])))

;; evolution function
(defn evolve
  "Runs an evolutionary algorithm"
  [maxgens]
  (loop [generation 0
         population (vec (sort better (repeatedly 100 #(new-schedule availability))))]
    (let [best (first population)]
      (println "Generation:" generation ", least overlaps:" (numOverlaps best))
      (if (or (zero? (numOverlaps best)) ;; found groups with no conflicts
              (>= generation maxgens)) ;; or tried for maxgens generations
        (println "Success:" best)
        (let [better-half (take (int (/ 100 2)) population)]
          ;; (println "best: " best)
          ;; (println "overlap: " (findOverlapTimes best))
          (recur
           (inc generation)
           (sort better (mutate better-half ))))))))

;; Try to implement some sort of tournamnet selection

;; evolution function
(defn random-evolve
  "Runs an evolutionary algorithm"
  [maxgens]
  (loop [generation 0
         population (vec (sort better (repeatedly 100 #(new-schedule availability))))]
    (let [best (first population)]
      (println "Generation:" generation ", least overlaps:" (numOverlaps best))
      (if (or (zero? (numOverlaps best)) ;; found groups with no conflicts
              (>= generation maxgens)) ;; or tried for maxgens generations
        (println "Success:" best)
        (let [random-half (take (int (/ 100 2)) (shuffle population))]
          ;; (println "best: " best)
          ;; (println "overlap: " (findOverlapTimes best))
          (recur
           (inc generation)
           (mutate random-half)))))))

;; evolution function using the old mutate function
(defn old-evolve
  "Runs an evolutionary algorithm"
  [maxgens]
  (loop [generation 0
         population (vec (sort better (repeatedly 100 #(new-schedule availability))))]
    (let [best (first population)]
      (println "Generation:" generation ", least overlaps:" (numOverlaps best))
      (if (or (zero? (numOverlaps best)) ;; found groups with no conflicts
              (>= generation maxgens)) ;; or tried for maxgens generations
        (println "Success:" best)
        (let [better-half (take (int (/ 100 2)) population)]
          ;; (println "best: " best)
          ;; (println "overlap: " (findOverlapTimes best))
          (recur
           (inc generation)
           (sort better (old-mutate (concat better-half better-half)))))))))

;; Test evolve
(evolve 1000)

;; Test the random evolve function
(random-evolve 1000)

;; Test the old evolve function
(old-evolve 1000)

;; Random works best!