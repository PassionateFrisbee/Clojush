;; double_letters.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string, print the string, doubling every letter character, and
;; trippling every exclamation point. All other non-alphabetic and non-exclamation
;; characters should be printed a single time each. The input string will have
;; maximum length of 20 characters.
;;
;; input stack has the input string

(ns clojush.problems.software.double-letters
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def double-letters-atom-generators
  (concat (list
            \!
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))

(defn make-double-letters-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-double-letters-error-function
    ([individual]
      (the-actual-double-letters-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-double-letters-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input correct-output]] (map-indexed vector (case data-cases
                                                                                :train train-cases
                                                                                :test test-cases
                                                                                []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input :input)
                                                      (push-item "" :output)))
                           printed-result (stack-ref :output 0 final-state)]
                       (when print-outputs
                         (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str printed-result))))

                       ; print if wrong answer
                       (when (not= printed-result correct-output)
                         (println "############################################################")
                         (println "Wrong result:" input correct-output printed-result)
                         (println "############################################################"))
                       ; print case numbers sometimes
                       (when (or (= (mod case-num 10000) 9999)
                                 (= (mod case-num 10000) 1))
                         (prn "At case" case-num ", input =", input))  


                       ))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def double-letters-train-and-test-cases
  (map #(sort-by (comp count first) %)
    (train-and-test-cases-from-dataset "double-letters" 0 1000000000)))

(defn double-letters-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first double-letters-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second double-letters-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn double-letters-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Double Letters problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-double-letters-error-function-from-cases (first double-letters-train-and-test-cases)
                                                                  (second double-letters-train-and-test-cases))
   :atom-generators double-letters-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1600
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
   :problem-specific-report double-letters-report
   :problem-specific-initial-report double-letters-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })



;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-evalpush-limit 1600)

(reset! global-max-points 3200)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(in1 \! exec_string_iterate
        (exec_dup char_dup exec_do*while () exec_do*while
                  (exec_do*while
                   (print_char integer_gte boolean_empty char_isletter)
                   integer_stackdepth \! char_eq) integer_pop)))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind  false)

(test-program-on-testing tom-ind false)





