(ns knapsack.core)

;; Clojure code for solving 0-1 weight=value knapsack problems with a genetic algorithm
;; Lee Spector (lspector@amherst.edu) 20220219

;; The problems solved by this code are all of this form: Given a collection of
;; objects of specified integer sizes, and a total size limit for a knapsack, determine
;; the subset of the objects that would get the knapsack as full as possible, without
;; over filling. Technically, I think these are 0-1 knapsack problems with values equal 
;; to weights, which are referred to here as sizes, or variants of subset sum
;; problems in which we want the largest subset even if it doesn't sum precisely
;; to the target som.

;; The top-level call here, evolve-knapsack, takes:

;; population-size = the number of individuals in the population
;; generations = the number of generations for which it will run evolution
;; capacity = the size of the knapsack (a positive integer)
;; object-sizes = a collection of positive integer sizes of objects

;; We will represent an individual as a map with these keys:

;; :genome = a vector of 0 and 1 indicating if corresponding object is included
;; :fitness = sum of objects if <= capacity, 0 otherwise

;; All output is generated in clojure-readable form, and code is included
;; in comments showing how to direct output to a file, and how to read it back
;; in from the file for analysis.

(defn fitness [genome capacity object-sizes]
  "Returns the fitness of genome in the context of capacity and object-sizes. "
  (let [total (reduce + (map * genome object-sizes))]
    (if (> total capacity)
      0
      total)))

(defn new-individual [object-sizes capacity]
  "Returns a new, random individual in the context of capacity and object-sizes."
  (let [genome (vec (repeatedly (count object-sizes) #(rand-int 2)))]
    {:genome  genome
     :fitness (fitness genome capacity object-sizes)}))

(defn fittest [individuals]
  "Returns the fittest of the given individuals."
  (reduce (fn [i1 i2]
            (if (> (:fitness i1) (:fitness i2))
              i1
              i2))
          individuals))

(defn select [population]
  "Returns an individual selected from population using a tournament."
  (fittest (repeatedly 2 #(rand-nth population))))

(defn mutate [genome]
  "Returns a possibly-mutated copy of genome."
  (for [gene genome]
    (if (> (rand) 0.1)
      gene
      (- 1 gene))))

(defn crossover [genome1 genome2]
  "Returns a uniform crossover product of genome1 and genome2."
  (map (fn [g1 g2]
         (if (rand-nth [true false]) g1 g2))
       genome1
       genome2))

(defn make-child [population capacity object-sizes]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (vec (mutate (crossover (:genome (select population))
                                           (:genome (select population)))))]
    {:genome  new-genome
     :fitness (fitness new-genome capacity object-sizes)}))

(defn report [generation population]
  "Prints a report on the status of the population at the given generation."
  (println {:generation generation :best (fittest population)}))

(defn evolve-knapsack [population-size generations capacity object-sizes]
  "Runs a genetic algorithm to solve, or approximately solve, the knapsack
  problem in the context of the given population-size, number of generations
  to run, capacity of the knapsack, and collection of object-sizes."
  (loop [population (repeatedly population-size
                                #(new-individual object-sizes capacity))
         generation 0]
    (report generation population)
    (if (>= generation generations)
      (fittest population)
      (recur (conj (repeatedly (dec population-size)
                               #(make-child population capacity object-sizes))
                   (fittest population))
             (inc generation)))))

;; A simple test, for which the best fitness is 18.
;; Note that the population is small enough that sometimes all will have
;; fitness 0, which provides no guidance and search becomes completely random
;; until something with non-zero fitness arises.
(evolve-knapsack 10 10 20 [1 4 4 4 4 5 30 40 50 60])

;; A test on a larger, randomly-generated problem:
(let [object-sizes (repeatedly 20 #(inc (rand-int 100)))
        capacity (+ 500 (rand-int 1000))]
    (println {:object-sizes object-sizes :capacity capacity})
    (evolve-knapsack 5 20 capacity object-sizes))

;; You can redirect the output to a file with spit and with-out-str:
(spit "data.txt"
        (with-out-str (evolve-knapsack 10 10 20 [1 4 4 4 4 5 30 40 50 60])))

(spit "data.txt"
        (with-out-str
          (let [object-sizes (repeatedly 20 #(inc (rand-int 100)))
                capacity (+ 500 (rand-int 1000))]
            (println {:object-sizes object-sizes :capacity capacity})
            (evolve-knapsack 5 20 capacity object-sizes))))

;; You can read in the data file like this:
(read-string (str "[" (slurp "data.txt") "]"))

;; And process it like this
(let [data (read-string (str "[" (slurp "data.txt") "]"))]
    (map :fitness (map :best (filter :generation data))))