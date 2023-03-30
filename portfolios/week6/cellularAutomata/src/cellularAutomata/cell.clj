(ns cellularAutomata.cell 
  (:require [clojure.string :as str]))

;;---------------Read in a rule number from the user-----------------

;; Ask for a rule number
(defn askRule
  "Asks the user for a rule number."
  []
  (println "Please enter a rule number between 0 and 255: ")
  (let [ruleNumStr (read-line)
        ruleNum (Integer/parseInt ruleNumStr)]
    (if (and (>= ruleNum 0) (< ruleNum 255))
      ruleNum
      (do
        (println "Invalid Number: Number must be between 0 and 255!")
        (recur)))))

;; Test the askRule function
(askRule)

;; Function to remove unnecessary parentheses
(defn removeParentheses
  "Removes the parentheses from the string."
  [ruleNumBinary]
    (str/replace ruleNumBinary #"[() ]" ""))

;; Function to convert the rule number to binary
(defn convertRuleToBinary
  "Converts the rule number to binary."
  [ruleNum]
    (let [ruleNumBinary (Integer/toBinaryString ruleNum)]
        (if (< (count ruleNumBinary) 8)
            (removeParentheses (str (repeat (- 8 (count ruleNumBinary)) 0) ruleNumBinary))
            ruleNumBinary)))

;; Test the convertRuleToBinary function
(convertRuleToBinary (askRule))

;;---------------Establish Rules and Initial Config-----------------

(defn getRules
  "Gets the rules from the rule number."
  [ruleNumBinary]
  (let [num7 (Integer/parseInt (subs ruleNumBinary 0 1))
        num6 (Integer/parseInt (subs ruleNumBinary 1 2))
        num5 (Integer/parseInt (subs ruleNumBinary 2 3))
        num4 (Integer/parseInt (subs ruleNumBinary 3 4))
        num3 (Integer/parseInt (subs ruleNumBinary 4 5))
        num2 (Integer/parseInt (subs ruleNumBinary 5 6))
        num1 (Integer/parseInt (subs ruleNumBinary 6 7))
        num0 (Integer/parseInt (subs ruleNumBinary 7 8))]
    {:case7 num7,
      :case6 num6,
      :case5 num5,
      :case4 num4,
      :case3 num3
      :case2 num2
      :case1 num1
      :case0 num0}))

;; Test the getRules function
(:case5 (getRules (convertRuleToBinary (askRule))))

;; Create the first row of the cellular automata
(defn createFirstRow
  "Creates the first row of the cellular automata."
  [numCells]
  (let [row (for [i (range numCells)]
              (if (= i (quot numCells 2))
                1
                0))
        vecRow (vec row)]
    vecRow))

;; Test the createFirstRow function
(createFirstRow 9)

;;---------------Create the Rows-----------------

;; Function to get the next row of the cellular automata
(defn getNextRow
  "Gets the next row of the cellular automata."
  [rules row]
;;   (println "rules: " rules)
;;   (println "row: " row)
  (let [numCells (count row)
        newRow (for [i (range numCells)]
                 (let [left (if (= i 0)
                              (get row (- numCells 1))
                              (get row (- i 1)))
                       right (if (= i (- numCells 1))
                               (get row 0)
                               (get row (+ i 1)))
                       center (get row i)]
                ;;    (println "left: " left)
                ;;    (println "center: " center)
                ;;    (println "right: " right)
                   (if (= left 1)
                     (if (= center 1)
                       (if (= right 1)
                         (:case7 rules) ;; 1 1 1
                         (:case6 rules)) ;; 1 1 0
                       (if (= right 1)
                         (:case5 rules) ;; 1 0 1
                         (:case4 rules))) ;; 1 0 0
                     (if (= center 1)
                       (if (= right 1)
                         (:case3 rules) ;; 0 1 1
                         (:case2 rules)) ;; 0 1 0
                       (if (= right 1)
                         (:case1 rules) ;; 0 0 1
                         (:case0 rules))))))
        vecNewRow (vec newRow)]
    vecNewRow))

;; Test the getNextRow function
(let [rules (getRules (convertRuleToBinary (askRule)))
      row (createFirstRow 9)]
  (getNextRow rules row))

(defn askNumRows
  "Asks the user for a number of rows."
  []
  (println "Please enter a number of rows (above 0): ")
  (let [rowNumStr (read-line)
        rowNum (Integer/parseInt rowNumStr)]
    (if (> rowNum 0)
      rowNum
      (do
        (println "Invalid Number: Number must be above 0!")
        (recur)))))

(defn askLenRows
  "Asks the user for a number of rows."
  []
  (println "Please enter the number of cells per row (above 0): ")
  (let [rowLenStr (read-line)
        rowLen (Integer/parseInt rowLenStr)]
    (if (> rowLen 0)
      rowLen
      (do
        (println "Invalid Number: Number must be above 0!")
        (recur)))))

;;---------------Create the Cellular Automata-----------------

;; Function to print the row with ■ and □
(defn printRow
  "Replaces ones with ■ and zeros with □"
  [row]
  (let [convRow (for [i (range (count row))]
                 (if (= (get row i) 1)
                   "■"
                   "□"))]
    (println (vec convRow))))

;; Test the printRow function
(printRow (createFirstRow 9))

;; Function to create the cellular automata and print it
(defn getCellularAutomata
  "Generates a cellular automata."
  []
  (let [ruleNumBinary (convertRuleToBinary (askRule))
        rules (getRules ruleNumBinary)
        numRows (askNumRows)
        firstRow (createFirstRow (askLenRows))]
    (reduce (fn [rows i]
              (if (empty? rows)
                (do
                  (printRow firstRow)
                  (conj rows firstRow))
                (let [prevRow (last rows)
                      nextRow (getNextRow rules prevRow)]
                  (printRow nextRow)
                  (conj (pop rows) nextRow))))
            []
            (range numRows))))

(defn getCellularAutomataHard
  "Generates a cellular automata."
  [ruleNum numRows lenRow]
  (let [ruleNumBinary (convertRuleToBinary ruleNum)
        rules (getRules ruleNumBinary)
        firstRow (createFirstRow lenRow)]
    (reduce (fn [rows i]
              (if (empty? rows)
                (do
                  ;; (printRow firstRow)
                  (conj rows firstRow))
                (let [prevRow (last rows)
                      nextRow (getNextRow rules prevRow)]
                  ;; (printRow nextRow)
                  (conj (pop rows) nextRow))))
            []
            (range numRows))))

;; Test the getCellularAutomata function
(getCellularAutomata)

;;---------------Create Genetic Algorithm-----------------

;; Function to get the fitness of a rule
;; Get fitness by counting the number of 1s in the last row and comparing it to the test rule
(defn getFitness
  "Gets the fitness of a rule."
  [ruleNum lastRow numRows]
  (let [thisRuleLastRow (getCellularAutomataHard ruleNum numRows (count (first lastRow)))
        thisRuleLastRowOnes (reduce + (first thisRuleLastRow))
        testRuleLastRowOnes (reduce + (first lastRow))
        fitness (if (< thisRuleLastRowOnes testRuleLastRowOnes)
                  (float (/ thisRuleLastRowOnes testRuleLastRowOnes))
                  (float (/ testRuleLastRowOnes thisRuleLastRowOnes)))]
    ;; (println "lastRow: " lastRow)
    ;; (println "thisRuleLastRow: " thisRuleLastRow)
    ;; (println "thisOnes: " thisRuleLastRowOnes)
    ;; (println "testOnes: " testRuleLastRowOnes)
    ;; (println "fitness: " fitness)
    fitness))

;; Test the getFitness function
(getFitness 115 (getCellularAutomataHard 99 100 35) 100)

;; Function to crossover
(defn crossover
  "Crossover two rules."
  [fitnessVecs]
  (let [newFirstRow (second fitnessVecs)
        newSecondRow (first fitnessVecs)]
    (concat (drop 2 fitnessVecs) newFirstRow newSecondRow)))

;; function to figure out the rule using evolution
(defn figureOutRuleWithEvo
  "Figures out the rule."
  [numGen lastRow numRows]
  (let [lenRow (count (first lastRow))
        ruleNums (for [i (range 256)]
                   i)
        fitnesses (for [i (range 256)]
                    (getFitness i lastRow numRows))
        fitnessMap (zipmap ruleNums fitnesses)
        maxFitnessMap (into {} (filter #(= (val %) 1.0) fitnessMap))
        maxFitnessVec (vec maxFitnessMap)]
    ;; (println "maxFitness: " maxFitnessMap)
    (loop [generation 0]
      (let [best (first (crossover (shuffle maxFitnessVec)))]
        (println "Generation: " generation " Leading Candidate: " (first best) " Fitness: " (second best))
        (if (>= generation numGen)
          (println "Reached Max Generation!")
          ;; (do (println "comparing: " (getCellularAutomataHard (first best) numRows lenRow) lastRow)
            (if (= (getCellularAutomataHard (first best) numRows lenRow) lastRow)
            (println "Found the rule! It is: " (first best))
            (recur
             (inc generation))))))))

;; Test the figureOutRule function
(figureOutRuleWithEvo 100 (getCellularAutomataHard 99 100 35) 100)