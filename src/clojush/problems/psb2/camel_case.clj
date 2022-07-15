;; camel_case.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem inspired by: https://www.codewars.com/kata/517abf86da9663f1d2000003

(ns clojush.problems.psb2.camel-case
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
  (:require [clojure.string :as str]
            [clojush.pushgp.case-auto-generation :as cag]))

(defn word-generator
  "Generates words at a nice distribution for Camel Case
   All words will have length [1, 5]."
  []
  (let [chars-between #(map char (range (int %1) (inc (int %2))))
        chars (chars-between \a \z)
        word-len (inc (rand-int 5))]
    (apply str (repeatedly word-len #(rand-nth chars)))))

(defn cleanup-length
  [string len]
  (let [result (take len string)]
    (if (or (= (last result) \space)
            (= (last result) \-))
      (apply str (butlast result))
      (apply str result))))

; Define test cases
(defn camel-case-input
  "Makes a Camel Case input of length len.
   Note that 2/3 of spaces/dashes are dashes."
  [len]
  (loop [result-string (word-generator)]
    (if (>= (count result-string) len)
      (cleanup-length result-string len)
      (recur (str result-string
                  (if (< (lrand) 0.66) \- \space)
                  (word-generator))))))

; Atom generators
(def atom-generators
  (make-proportional-atom-generators
   (concat
    (registered-for-stacks [:integer :boolean :string :char :exec :print]) ; stacks
    (list (tag-instruction-erc [:integer :boolean :string :char :exec] 1000) ; tags
          (tagged-instruction-erc 1000)))
   (list 'in1) ; inputs
   (list \-
         \space ; constants
         (fn [] (lrand-nth (map char (range 97 122)))) ;Visible character ERC
         (fn [] (camel-case-input (lrand-int 21))) ;String ERC
) ; constants
   {:proportion-inputs 0.15
    :proportion-constants 0.05}))

; A list of data domains for the problem. Each domain is a vector containing
; a "set" of inputs and two integers representing how many cases from the set
; should be used as training and testing cases respectively. Each "set" of
; inputs is either a list or a function that, when called, will create a
; random element of the set.
(def data-domains
  [[(list ""
          "nospaceordash"
          "two-words"
          "two words"
          "all separate words"
          "all-one-word-dashed"
          "loooooong-wooooords"
          "loooooong wooooords"
          "a-b-c-d-e-f-g-h-i-j"
          "a b c d e f g h i j"
          "saaaaaaaaaaaaaaaaame") 11 0] ; "Special" inputs covering some base cases
   [(fn [] (camel-case-input (inc (lrand-int 20)))) 189 2000]])

; Helper function for error function
(defn create-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector (vector in)
                 (vector (if (or (= (str in) "") (every? #{\-} (str in))) ""
                             (let [full-string (str/join (map str/capitalize (str/split (str in) #"-")))]
                               (apply str (str/lower-case (first full-string)) (drop 1 full-string)))))))
       inputs))

(defn camel-case-solver
  [inputs]
  (apply (fn [& pairs]
           (for [pair pairs]
             (second pair))) (map (fn [in]
         (vector in
                 (if (or (= (str in) "") (every? #{\-} (str in))) ""
                     (let [full-string (str/join (map str/capitalize (str/split (str in) #"-")))]
                       (apply str (str/lower-case (first full-string)) (drop 1 full-string))))))
       inputs)))

(defn violet's-solver
  [in]
  (if (or (= (str in) "") (every? #{\-} (str in))) ""
      (let [full-string (str/join (map str/capitalize (str/split (str in) #"-")))]
        (apply str (str/lower-case (first full-string)) (drop 1 full-string)))))

(defn make-error-function-from-cases
  "Creates and returns the error function based on the train/test cases."
  [train-cases test-cases]
  (fn the-actual-error-function
    ([individual]
     (the-actual-error-function individual :train))
    ([individual data-cases] ; data-cases should be :train or :test
     (the-actual-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[[input] [correct-output]] (case data-cases
                                                  :train train-cases
                                                  :test test-cases
                                                  data-cases)]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input :input)))
                           result (stack-ref :string 0 final-state)]
                       (when print-outputs
                         (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                       (swap! behavior conj result)
                         ; Error is Levenshtein distance
                       (if (string? result)
                         (levenshtein-distance correct-output (str result))
                         10000) ; penalty for no return value
                       )))]
       (if (= data-cases :test)
         (assoc individual :test-errors errors)
         (assoc individual
                :behaviors (reverse @behavior)
                :errors errors))))))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map create-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def train-and-test-cases
  (get-train-and-test data-domains))

(defn initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn custom-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Camel Case problem report - generation %s\n" generation) (flush)
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
    (println ";;******************************"))) ; To do validation, could have this function return an altered best individual
       ; with total-error > 0 if it had error of zero on train but not on validation
       ; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-error-function-from-cases (first train-and-test-cases)
                                                   (second train-and-test-cases))
   :training-cases (first train-and-test-cases)
   :oracle-function camel-case-solver
   :input-parameterization [(cag/create-new-parameter :string 1 20 [:lower-case] ["-" "_"])]
   :output-stacks [:string]

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

   :atom-generators atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :problem-specific-report custom-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 10000})