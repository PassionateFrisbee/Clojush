;; simple_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010
;; Stolen by Tom Helmuth to test using Clojush outside of the project.

(ns synthesis.test_push
  (:require [clojush] [clojure.contrib.math])
  (:use [clojush] [clojure.contrib.math]))

; Test string-rand
;(println (run-push '(string-rand string-rand string-rand) (make-push-state)  false false))


;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions and an 
;; input instruction that uses the auxiliary stack.

#_(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

#_(pushgp 
  :error-function (fn [program]
                    (doall
                      (for [input (range 10)]
                        (let [state (run-push program 
                                      (push-item input :auxiliary 
                                        (push-item input :integer 
                                          (make-push-state))))
                              top-int (top-item :integer state)]
                          (if (number? top-int)
                            (abs (- top-int 
                                   (- (* input input input) 
                                     (* 2 input input) input)))
                            1000)))))
	 :atom-generators (list (fn [] (rand-int 10))
                     'in
                     'integer_div
                     'integer_mult
                     'integer_add
                     'integer_sub)
	 :tournament-size 3)
