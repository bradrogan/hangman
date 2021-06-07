(ns hangman.core
  (:require [clojure.repl :refer [source apropos dir pst doc find-doc]]
            [clojure.string :as str]
            [clojure.test :refer [is are]])
  (:gen-class))



(def words ["HERMIONE" "HUFFLEPUFF" "RAVENCLAW" "GRIFFINDOR" "SLYTHERIN" "DUMBLEDORE" "HARRY" "VOLDEMORT" "ARAGOG" "BUCKBEAK"])

(defn get-word
  [words]
  (rand-nth words))

(defn clear-screen
  []
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
  )

(defn get-difficulty
  []
  (clear-screen)
  (println "\nChoose your difficulty. How many wrong guesses before GAME OVER?")
  (flush)
  (let [turns (read-line)
        error-fn (fn []
                   (println "You must enter a positive whole number. Try again.\n")
                   (get-difficulty))]
    (try
      (let [int-turns (Integer/parseInt turns)]
        (if (pos-int? int-turns)
          int-turns
          (error-fn)))
      (catch Exception e
        (error-fn)))))

  (defn get-guess
    [guesses]
    (println "Enter your guess: ")
    (flush)
    (clear-screen)
    (into guesses (str (first (str/upper-case (read-line))))))


(defn check-guess
  [guesses word guesses-left]
  (let [output
        (apply str (map
                    (fn [letter] (if (some #(= % letter) guesses)
                                   (str letter " ")
                                   "_ ")) word))
        win? (not (str/includes? output "_"))
        guesses-left (if (str/includes? output (str (last guesses))) guesses-left (dec guesses-left))]
    {:output output :guesses-left guesses-left :win? win?}))
  
  (defn print-win-message
    [solved-puzzle]
    (println "\n" solved-puzzle)
    (println "\nYOU WIN!"))

(defn print-game-over-message
  [guesses answer]
  (println "Guesses:" (str/join ", " guesses))
  (println "\nThe answer was:" answer "\n")
  (println "\nGAME OVER. Try again."))

  (defn print-turn-message
    [puzzle guesses guesses-left]
    (println "\n" puzzle)
    (println "Guesses:" (str/join ", " guesses))
    (println "Guesses Remaining:" guesses-left "\n"))

(defn take-turn
  [word num-guesses guesses]
  (let [guess (get-guess guesses)
        turn-result (check-guess guess word num-guesses)
        {:keys [win? output guesses-left]} turn-result]
    (cond
      win?
      (print-win-message output)
      (and (not win?) (pos? guesses-left))
      (do
        (print-turn-message output guess guesses-left)
        (take-turn word guesses-left guess))
      (and (not win?) (zero? guesses-left))
      (print-game-over-message guess word))))

(defn -main
  "Hangman!"
  [& args]

  (let [turns (get-difficulty)
        word (get-word words)]
    (println "Here's the Puzzle!" "\n" (:output (check-guess nil word 1)))
    (take-turn  word turns []))

)

