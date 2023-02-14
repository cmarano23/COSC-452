(ns test.core)

;; Testing a bunch of things
(+ 9 4)

(defn add
  "Adds two numbers together."
  [x y]
  (+ x y))

(add 8 1)

(/ 9.8 2.3)

(* Math/PI 2)

(defn getArea
  "Calculates the area of a circle."
  [radius]
  (* Math/PI (Math/pow radius 2)))

(getArea 4)

(defn getVolume
  "Calculates the volume of a sphere."
  [radius]
  (* 4/3 Math/PI (Math/pow radius 3)))

(getVolume 2.5)

(defn keplersThirdLaw
  "Calculates the period of a planet given semi-major axis."
  [sa-axis]
  (Math/sqrt (* 4 (Math/pow Math/PI 2) (Math/pow sa-axis 3) 6.67E-11 5.97E24)))

(keplersThirdLaw 1.496E11)

;; Trying to make a blackjack game

(defn getSuit
  "Selects a random suit from a deck of cards."
  []
  (case (rand-int 4)
    0 "Spades"
    1 "Clubs"
    2 "Hearts"
    3 "Diamonds"))

(def cardValues {:Ace 11 :1 1 :2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8 :9 9 :10 10 :Jack 10 :Queen 10 :King 10})

;; (defn getCardNum
;;   "Selects a card number from 2 to Ace."
;;   []
;;   (case (rand-int 13)
;;     0 "Ace"
;;     1 "2"
;;     2 "3"
;;     3 "4"
;;     4 "5"
;;     5 "6"
;;     6 "7"
;;     7 "8"
;;     8 "9"
;;     9 "10"
;;     10 "Jack"
;;     11 "Queen"
;;     12 "King"))

(defn getCardNum
  "Selects a card number from 2 to Ace."
  []
  (case (rand-int 13)
    0 :Ace
    1 :2
    2 :3
    3 :4
    4 :5
    5 :6
    6 :7
    7 :8
    8 :9
    9 :10
    10 :Jack
    11 :Queen
    12 :King))

;; (defn getCard
;;   "Selects a random card from a deck of cards."
;;   []
;;   ((getCardNum) " of " (getSuit)))

;; (getCard)

(getCardNum)

(cardValues :Ace)

(:Jack cardValues)

(map cardValues [:Ace :Jack])

(reduce + (map cardValues [:Ace :Jack]))

(defn getScore
  "Gets the score of a hand of cards."
  [cards]
  (reduce + (map cardValues cards))
)

(getScore[:Ace :Jack])

(let [test-cards [:7 :King]]
  (getScore test-cards))

(let [test-cards [(getCardNum) (getCardNum)]]
  (getScore test-cards))

(defn playBlackjack
  "Plays a game of Blackjack."
  []
;;   (def firstPlayerCard (getCardNum))
;;   (def secondPlayerCard (getCardNum))
;;   (def firstDealerCard (getCardNum))
;;   (def secondDealerCard (getCardNum))
;;   (def dealer-cards [(cardValues (keyword (firstDealerCard))) (cardValues (keyword (secondDealerCard)))])
;;   (def player-cards [(:firstPlayerCard cardValues) (:secondPlayerCard cardValues)])
  (def player-cards [(getCardNum) (getCardNum)])
  (def dealer-cards [(getCardNum) (getCardNum)])
  (println "Dealer is showing: " (first dealer-cards))
  (println "Player has: " (first player-cards) " and " (second player-cards) ". This results in a score of " (getScore[player-cards]))
  (println "Would you like to hit? (yes/no)")
  (let [userChoice (read-line)]
    (if (= userChoice "yes") (def player-cards [(conj player-cards (getCardNum))]) "Player has not typed 'hit'" ))
  (println "Player has: " player-cards)
  (println "Dealer has " dealer-cards)
;;   (if (> (getScore[player-cards]) (getScore(dealer-cards))) (println "Player wins!") (println "Dealer wins!"))
)

(getScore[player-cards])

(playBlackjack)

;; I cannot seem to get the getScore function to return anything but nil. 