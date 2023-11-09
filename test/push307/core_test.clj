(ns push307.core-test
  (:require [clojure.test :refer [deftest is]]
             [push411.core :as core]))

(comment
  "Kate Bondarenko.
   
  The issues with writing tests was that a lot of results from
   core functions are at random, so there is no way to properly test them 
   unless test them on size, and return type. 
   
  Another issue is that a lot of functions run on initialized population,
   individuals and their property, which is difficult to write out to a 
   separate file with consistency.
   
  I hope this is somewhat acceptable.."
  )

(def example-push-state
  {:exec '(integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-with42
  {:exec '(integer_+ integer_-)
   :integer '(42 1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-with+
  {:exec '(integer_+ integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :input {}})

(def example-push-state-within1
  {:exec '(integer_+ integer_-)
   :integer '(4 1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-after+
  {:exec '(integer_+ integer_-)
   :integer '(3 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-after-
  {:exec '(integer_+ integer_-)
   :integer '(1 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-after*
  {:exec '(integer_+ integer_-)
   :integer '(2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-after%
  {:exec '(integer_+ integer_-)
   :integer '(2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-afterexec_dup
  {:exec '(integer_+ integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-interpret-one
  {:exec '(integer_-)
   :integer '(3 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-interpret-all
  {:exec '()
   :integer '(0 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-population 
  '({:genome (integer_+ exec_dup integer_- exec_dup)} 
    {:genome (integer_- in1 integer_* integer_* in1 0 in1 integer_-)} 
    {:genome (in1)} 
    {:genome (1 integer_- integer_* integer_* 0 exec_dup close close integer_+ in1)} 
    {:genome (in1 integer_% 1 integer_% close in1 integer_% integer_+ in1 1)} 
    {:genome (integer_+ integer_* integer_+)} 
    {:genome (integer_* 1 integer_% integer_% 1)} 
    {:genome (0 1 integer_% integer_* 1 integer_*)} 
    {:genome (integer_* close 0 in1 0 close in1 integer_% in1 1)} 
    {:genome (integer_* integer_* exec_dup close)})
)

(def example-genome1 '(integer_+ exec_dup integer_- exec_dup))
(def example-genome2 '(integer_- in1 integer_* integer_* in1 0 in1 integer_-))

(def correct-individual
  {:genome '(in1 in1 in1 integer_* integer_* in1 integer_+ 3 integer_+)})

(def instructions 
  (list
   push411.core/in1
   push411.core/integer_+ 
   push411.core/integer_-
   push411.core/integer_*
   push411.core/integer_%
   push411.core/exec_dup
   'close
   0
   1))

;; TESTS

(deftest stack
  (is (core/push-to-stack example-push-state :integer 42) example-push-state-with42)
  (is (core/push-to-stack example-push-state :exec 'integer_+) example-push-state-with+)
  (is (core/pop-stack example-push-state :exec) 'integer_+)
  (is (core/pop-stack example-push-state :integer) 1)
  (is (core/pop-stack example-push-state :string) "abc")
  (is (core/pop-stack example-push-state-with42 :integer) '42)
  (is (core/peek-stack example-push-state :integer) 1)
  (is (core/peek-stack example-push-state :exec) 'integer_+)
  (is (core/peek-stack example-push-state :string) "abc")
  (is (core/empty-stack? empty-push-state :integer) true))

(deftest instructions-test
  (is (core/in1 example-push-state) example-push-state-within1)
  (is (core/integer_+ example-push-state) example-push-state-after+)
  (is (core/integer_- example-push-state) example-push-state-after-)
  (is (core/integer_* example-push-state) example-push-state-after*)
  (is (core/integer_% example-push-state) example-push-state-after%)
  (is (core/exec_dup example-push-state) example-push-state-afterexec_dup))

(comment
  "I would continue one with writing the tests for interpreter but!
   I think someone else had the issue that while testing interpreter,
   symbols from default instructions are not being recognized."
  
  (deftest interpreter
    (is (core/interpret-one-step example-push-state) example-push-state-interpret-one)
    (is (core/interpret-push-program example-push-state) example-push-state-interpret-all))
)

(deftest GP 
  (is (type (core/make-random-plushy-genome instructions 10)) list)
  (is (<= (count (core/make-random-plushy-genome instructions 10)) 10))
  (is (<= (count (core/make-random-plushy-genome instructions 50)) 50))
  (is (type (core/tournament-selection example-population)) hash-map)
  (is (type (core/crossover example-genome1 example-genome2)) list)
  (is (<= (count (core/crossover example-genome1 example-genome2)) 8))
  (is (>= (count (core/uniform-addition example-genome2 instructions)) 8))
  (is (<= (count (core/uniform-deletion example-genome2)) 8))
  )
