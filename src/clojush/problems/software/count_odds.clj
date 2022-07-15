;; count_odds.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers with length <= 50, with each
;; integer in [-1000,1000], return the number of integers that are odd.
;;
;; input stack has 1 input vector of integers

(ns clojush.problems.software.count-odds
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        )
  (:require [clojush.pushgp.case-auto-generation :as cag]))

; Atom generators
(def count-odds-atom-generators
  (concat (list
            0
            1
            2
            ;;; end constants
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))


;; Define test cases
(defn count-odds-input
  "Makes a Count Odds input vector of length len with probability prob of being odd."
  [len prob]
  (vec (repeatedly len
                   #(if (< (lrand) prob)
                      (inc (* 2 (- (lrand-int 1000) 500)))
                      (* 2 (- (lrand-int 1001) 500))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def count-odds-data-domains
  [[(list []) 1 0] ;; Empty vector
   [(concat (map vector (range -10 11))
            (list [-947] [-450] [303] [886])) 25 0] ;; Length 1 vectors
   [(list [0 0]
          [0 1]
          [7 1]
          [-9 -1]
          [-11 40]
          [944 77]) 6 0] ;; Length 2 vectors
   [(fn [] (count-odds-input (inc (lrand-int 50)) 1.0)) 9 100] ;; Random length, all odd
   [(fn [] (count-odds-input (inc (lrand-int 50)) 0.0)) 9 100] ;; Random length, all even
   [(fn [] (count-odds-input (inc (lrand-int 50)) (lrand))) 150 1800] ;; Random length, random prob of odd
   ])

;;Can make Count Odds test data like this:
;(test-and-train-data-from-domains count-odds-data-domains)

; Helper function for error function
(defn count-odds-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map #(vector (vector %)
                (vector (count (filter odd? %))))
       inputs))

(defn count-odds-solver
  "Given a vetor, it returns the count of 
   odd integers inside that vector."
  [input]
  (count (filter odd? input)))

(defn make-count-odds-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-count-odds-error-function
    ([individual]
      (the-actual-count-odds-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-count-odds-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1] [correct-output]] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     data-cases)]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)))
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %2d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer error
                         (if (number? result)
                           (abs (- result correct-output)) ; distance from correct integer
                           1000) ; penalty for no return value
                         )))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual :behaviors @behavior :errors errors))))))

(defn get-count-odds-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map count-odds-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def count-odds-train-and-test-cases
  (get-count-odds-train-and-test count-odds-data-domains))

(defn count-odds-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first count-odds-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second count-odds-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn count-odds-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Count Odds problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-count-odds-error-function-from-cases (first count-odds-train-and-test-cases)
                                                              (second count-odds-train-and-test-cases))
   :training-cases (first count-odds-train-and-test-cases)

   :atom-generators count-odds-atom-generators

   :oracle-function count-odds-solver
   :input-parameterization [(cag/create-new-parameter :vector_integer 0 50 (cag/create-new-parameter :integer -1000 1000))]
   :output-stacks [:integer]

   :sub-training-cases-selection :intelligent ; :random ; :intelligent
   :num-of-cases-in-sub-training-set 5
   :num-of-edge-cases-in-sub-training-set 2 ; probably not 5 since there's only 1 input
   :sub-training-cases '()

       ;; Human-driven counterexamples
   :counterexample-driven true
   :counterexample-driven-case-checker :simulated-human ; :automatic ; :human ; :simulated-human

   ;; Options, as a list: :hard-coded ; :randomly-generated ; :edge-cases ; :selecting-new-cases-based-on-outputs
   :counterexample-driven-case-generators '(:edge-cases :branch-coverage-test :selecting-new-cases-based-on-outputs :randomly-generated)

   :max-num-of-cases-added-from-edge 2
   :num-of-cases-added-from-random 8
   :num-of-cases-used-for-output-selection 1000
   :num-of-cases-added-from-output-selection 5
   :num-of-cases-used-for-branch-coverage 1000
   :num-of-cases-added-from-branch-coverage 5

   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :problem-specific-report count-odds-report
   :problem-specific-initial-report count-odds-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   :single-vector-input true})
