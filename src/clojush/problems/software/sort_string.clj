;; sort_string.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Given a vector of integers, return a sorted version of the vector
;;
;;

(ns clojush.problems.software.sort-string
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def sort-string-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec :print] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec :print])))


;; Define test cases
(defn sort-string-input
  "Makes a Sort String input vector of length len."
  [len]
  (apply str (repeatedly len #(rand-nth (map char (flatten (list '(32) (range 65 91) (range 97 123))))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def sort-string-data-domains
  [[(list ""
          " "
          "a"
          "A"
          "sAAAAAAAAAAAAAAAAAAAAAAAAAAAAme"
          "SoYrqavT iQTAmfnZLiYbUOmYRgugWHhPiKpnDyNPjNxtC taFYoJnkDmxptcKVoLGbIZRGlJIJkoBdwtmdlveCZDliirvWkRPeU"  ; max length
          "abcdefghijklmnopqrstuvwxyz "
          "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ") 8 0]
   [(fn [] (sort-string-input (inc (lrand-int 100)))) 92 1000] ;; Random length, random strings
   ])

;;Can make Sort String test data like this:
;(test-and-train-data-from-domains sort-string-data-domains)

; Helper function for error function
(defn sort-string-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
              (apply str (sort-by clojure.string/upper-case (apply str (clojure.string/split in #" "))))))
       inputs))

(defn make-sort-string-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-sort-string-error-function
    ([individual]
     (the-actual-sort-string-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-sort-string-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[input correct-output] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input :input)
                                                      (push-item "" :output)))
                           result (stack-ref :output 0 final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is levenshtein distance
                       (levenshtein-distance correct-output result))))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-sort-string-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort-string-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def sort-string-train-and-test-cases
  (get-sort-string-train-and-test sort-string-data-domains))

(defn sort-string-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first sort-string-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second sort-string-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn sort-string-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Sort String problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-sort-string-error-function-from-cases (first sort-string-train-and-test-cases)
                                                                  (second sort-string-train-and-test-cases))
   :atom-generators sort-string-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 200
   :evalpush-limit 1500
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report sort-string-report
   :problem-specific-initial-report sort-string-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
