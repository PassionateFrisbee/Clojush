(ns synthesis.qfe
  (:require [clojush]
            [synthesis.core :as synth-core]
            [synthesis.db :as db]
            [synthesis.examples_tables :as et]))

;;;;;;;;;;
;; Some globals for testing
(def QUERY-FITNESSES (atom (hash-map)))

;;;;;;;;;;
;; Helper functions

(defn precision
  [true-positives false-positives]
  (if (zero? true-positives)
    0
    (float (/ true-positives (+ true-positives false-positives)))))

(defn recall
  [true-positives false-negatives]
  (if (zero? true-positives)
    0
    (float (/ true-positives (+ true-positives false-negatives)))))

(defn f1-score
  ([true-positives false-positives false-negatives]
    (f1-score (precision true-positives false-positives)
              (recall true-positives false-negatives)))
  ([prec rec]
    (if (zero? (+ prec rec))
      0
      (/ (* 2 prec rec) (+ prec rec)))))

;;;;;;;;;;
;; Report - Add printing of query

(defn qfe-report
  "Customize this for your own problem. It will be called at the end of the generational report."
  [best population generation error-function report-simplifications]
  (let [best-program (clojush/not-lazy (:program best))
        best-final-state (clojush/run-push
                           best-program
                           (clojush/push-item "adult"
                                              :from
                                              (clojush/push-item "*"
                                                                 :select
                                                                 (clojush/make-push-state))))
        best-result-query-string (synth-core/stacks-to-query-string best-final-state)]
    (printf "\nQuery Generated by Best Program:\n%s" best-result-query-string)(flush)
    (printf "\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")
    (flush)))

;;;;;;;;;;
;; Error function and atom generators

(def qfe-atom-generators
  (concat #_(clojush/registered-for-type :where)
          (list ;'where_dup
                ;'where_swap
                ;'where_rot
                'where_constraint_distinct_from_index
                'where_constraint_from_index
                'where_constraint_from_stack
                'where_constraint_from_pos_ex
                'where_constraint_from_neg_ex
                'where_and
                'where_or
                ;'where_not
                )
          (list 'string_length
                ;'string_take
                ;'string_concat
                'string_stackdepth
                ;'string_dup
                ;'string_swap
                ;'string_rot
                )
          (list 'integer_add
                'integer_sub
                'integer_mult
                'integer_div
                'integer_mod
                'integer_stackdepth
                'integer_dup
                'integer_swap
                'integer_rot)
          (list (fn []
                  (let [choice (clojush/lrand-int 5)]
                    (case choice
                      0 (clojush/lrand-int 10)
                      1 (clojush/lrand-int 100)
                      2 (clojush/lrand-int 1000)
                      3 (clojush/lrand-int 10000)
                      4 (clojush/lrand-int 100000))))
                (fn [] (clojush/lrand-int 100000))
                (fn [] (let [chars (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "abcdefghijklmnopqrstuvwxyz"
                                        "0123456789")
                             chars-count (count chars)]
                         (apply str (repeatedly (+ 1 (clojush/lrand-int 9))
                                                #(nth chars (clojush/lrand-int chars-count)))))))))

(defn qfe-error-function-creator
  "Creates an error function based on vectors positive-examples and negative-examples."
  [positive-examples negative-examples]
  (fn [program]
    (list
      (let [final-state (clojush/run-push
                          program
                          (clojush/push-item "adult_examples"
                                             :from
                                             (clojush/push-item "*"
                                                                :select
                                                                (clojush/make-push-state))))
            result-query-string (synth-core/stacks-to-query-string final-state)]
        ;(println "\nEvaluating query:")
        ;(println result-query-string)
        (if (= (clojush/top-item :where final-state) :no-stack-item)
          (do 3) ; Penalty of 3.0 for having an empty :where stack
          (if-let [fitness (get @QUERY-FITNESSES result-query-string)] ; See if we remember the fitness
            fitness ; If we remember the fitness, just return it; else, calculate and store it
            (let [query-future (future
                                 (db/run-db-function db/synthesis-db
                                                     db/db-query
                                                     result-query-string))]
              (try
                (let [result-rows (.get query-future 500
                                        (java.util.concurrent.TimeUnit/MILLISECONDS))
                      true-positives (count (clojure.set/intersection (set positive-examples)
                                                                      (set result-rows)))
                      false-positives (count (clojure.set/intersection (set negative-examples)
                                                                       (set result-rows)))
                      error (- 1.0 (f1-score true-positives
                                             false-positives
                                             (- (count positive-examples) true-positives)))]
                  ;(println "True positives: " true-positives)
                  ;(println "False positives: " false-positives)
                  ;(println "True negatives: " (- (count negative-examples) false-positives))
                  ;(println "False negatives: " (- (count positive-examples) true-positives))
                  ;(println "Total Positives: " (count positive-examples))
                  ;(println "Total Negatives: " (count negative-examples))
                  ;(println "Total: " (+ (count negative-examples) (count positive-examples)))
                  (swap! QUERY-FITNESSES assoc result-query-string error)
                  error)
                (catch java.util.concurrent.TimeoutException e
                       (when (not (future-cancel query-future))
                         (println "future could not be cancelled"))
                       2))))))))) ; Penalty of 2.0 for not returning


;;;;;;;;;;
;; Main pushgp calling function

(defn query-from-examples
  "Takes vectors of positive and negative row examples and starts a pushgp run to find a query that
   matches those examples."
  [positive-examples negative-examples]
  (et/drop-examples-table)
  (et/create-and-populate-examples-table positive-examples negative-examples)
  (try
    (clojush/pushgp
      :error-function (qfe-error-function-creator positive-examples negative-examples)
      :atom-generators qfe-atom-generators
      :max-points 300
      :evalpush-limit 300
      :population-size 400
      :max-generations 100
      :mutation-probability 0.12
      :crossover-probability 0.8
      :simplification-probability 0.05
      :tournament-size 6
      :trivial-geography-radius 10
      :report-simplifications 0
      :final-report-simplifications 10
      :reproduction-simplifications 1
      :use-single-thread true
      :problem-specific-report qfe-report
      :use-historically-assessed-hardness false
      :use-historically-assessed-similarity false
      )
    (finally
      (et/drop-examples-table))))
    

;;;;;;;;;;
;; Example usage

(query-from-examples et/pos-ex et/neg-ex)

; Reset things
#_(do
  (et/drop-examples-table)
  (reset! QUERY-FITNESSES {}))
