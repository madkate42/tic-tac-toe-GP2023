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
   separate file with consistency."
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

(deftest integer-plus-test
  (is (= (core/integer_+ {:integer '(2 3)}) {:integer '(5)}))  ; Normal operation
  (is (= (core/integer_+ {:integer '(-5 10)}) {:integer '(5)}))  ; Negative numbers
  (is (= (core/integer_+ {:integer '()}) {:integer '()}))  ; Empty stack
  (is (= (core/integer_+ {:integer '(1000000 2000000)}) {:integer '(3000000)})))  ; Large numbers

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

(deftest integer-equals-test
  (is (= (core/integer_= {:integer '(3 3)}) {:integer '() :boolean '(true)}))  ; Equal numbers
  (is (= (core/integer_= {:integer '(2 3)}) {:integer '() :boolean '(false)}))  ; Not equal
  (is (= (core/integer_= {:integer '(1)}) {:integer '(1)}))  ; One item in stack
  (is (= (core/integer_= {:integer '()}) {:integer '()}))  ; Empty stack
  )
(deftest integer-<
  (is (= (core/integer_< {:integer '(4 3)}) {:integer '() :boolean '(true)}))  ; 4 > 3
  (is (= (core/integer_< {:integer '(2 3)}) {:integer '() :boolean '(false)}))  ; 2 < 3
  (is (= (core/integer_< {:integer '(3 3)}) {:integer '() :boolean '(false)}))  ; 3 = 3
  (is (= (core/integer_< {:integer '()}) {:integer '()}))  ; Empty stack
  )

(deftest integer->
  (is (= (core/integer_> {:integer '(2 3)}) {:integer '() :boolean '(true)}))  ; 2 < 3
  (is (= (core/integer_> {:integer '(4 3)}) {:integer '() :boolean '(false)}))  ; 4 > 3
  (is (= (core/integer_> {:integer '(3 3)}) {:integer '() :boolean '(false)}))  ; 3 = 3
  (is (= (core/integer_> {:integer '()}) {:integer '()}))  ; Empty stack
  )

(deftest boolean-equals-test
  (is (= (core/boolean_= {:boolean '(true true)}) {:boolean '(true)}))  ; Both true
  (is (= (core/boolean_= {:boolean '(false true)}) {:boolean '(false)}))  ; One false, one true
  (is (= (core/boolean_= {:boolean '(false false)}) {:boolean '(true)}))  ; Both false
  (is (= (core/boolean_= {:boolean '(true)}) {:boolean '(true)}))  ; One item in stack
  (is (= (core/boolean_= {:boolean '()}) {:boolean '()}))  ; Empty stack
  )

(deftest boolean-and-test
  (is (= (core/boolean_and {:boolean '(true true)}) {:boolean '(true)}))  ; Both true
  (is (= (core/boolean_and {:boolean '(false true)}) {:boolean '(false)}))  ; One false
  (is (= (core/boolean_and {:boolean '(false false)}) {:boolean '(false)}))  ; Both false
  (is (= (core/boolean_and {:boolean '(true)}) {:boolean '(true)}))  ; One item in stack
  (is (= (core/boolean_and {:boolean '()}) {:boolean '()}))  ; Empty stack
  )

(deftest boolean-or-test
  (is (= (core/boolean_or {:boolean '(true false)}) {:boolean '(true)}))  ; One true
  (is (= (core/boolean_or {:boolean '(false false)}) {:boolean '(false)}))  ; Both false
  (is (= (core/boolean_or {:boolean '(true true)}) {:boolean '(true)}))  ; Both true
  (is (= (core/boolean_or {:boolean '(false)}) {:boolean '(false)}))  ; One item in stack
  (is (= (core/boolean_or {:boolean '()}) {:boolean '()}))  ; Empty stack
  )

(deftest boolean-not-test
  (is (= (core/boolean_not {:boolean '(true)}) {:boolean '(false)}))  ; True becomes false
  (is (= (core/boolean_not {:boolean '(false)}) {:boolean '(true)}))  ; False becomes true
  (is (= (core/boolean_not {:boolean '()}) {:boolean '()}))  ; Empty stack
  )

(deftest exec-if-test
  (is (= (core/exec_if {:exec '(item1 item2) :boolean '(true)})
         {:exec '(item1) :boolean '()}))
  (is (= (core/exec_if {:exec '(item1 item2) :boolean '(false)})
         {:exec '(item2) :boolean '()}))
  (is (= (core/exec_if {:exec '(item1 item2) :boolean '()})
         {:exec '(item1 item2) :boolean '()}))
  (is (= (core/exec_if {:exec '() :boolean '()})
         {:exec '() :boolean '()})))

(deftest exec-dup-test
  (is (= (core/exec_dup {:exec '(item1 item2 item3) :other-stacks '(...)})
         {:exec '(item1 item1 item2 item3) :other-stacks '(...)}))
  ;only one item
  (is (= (core/exec_dup {:exec '(item1) :other-stacks '(...)})
         {:exec '(item1 item1) :other-stacks '(...)}))

  (is (= (core/exec_dup {:exec '() :other-stacks '(...)})
         {:exec '() :other-stacks '(...)}))
  (is (= (core/exec_dup {:exec '(item1 item2) :integer '(1 2 3) :boolean '(true false)})
         {:exec '(item1 item1 item2) :integer '(1 2 3) :boolean '(true false)})))

(deftest play-game-test
  (let [player1 {:genome '(1 2 3) :elo 1000}
        player2 {:genome '(4 5 6) :elo 1000}
        result (core/play-game core/empty-board player1 player2)]
    (is (contains? result :winner))
    (is (contains? result :loser))
    (is (contains? result :draw))
    (is (contains? result :win))))

(deftest compete-test
  (let [player1 {:genome '(1 2 3) :elo 1000}
        player2 {:genome '(4 5 6) :elo 1000}
        [updated-player1 updated-player2] (core/compete player1 player2)]
    (is (= 2 (count [updated-player1 updated-player2])))
    (is (contains? updated-player1 :genome))
    (is (contains? updated-player1 :elo))
    (is (contains? updated-player2 :genome))
    (is (contains? updated-player2 :elo))))

(deftest compete-all-test
  (let [population (vec (repeat 10 {:genome '(1 2 3) :elo 1000}))
        result (core/compete-all (first population) population)]
    (is (= 10 (count result)))
    (is (every? #(contains? % :genome) result))
    (is (every? #(contains? % :elo) result))))

(deftest round-robin-test
  (let [population (vec (repeat 10 {:genome '(1 2 3) :elo 1000}))
        result (core/round-robin population)]
    (is (= 10 (count result)))
    (is (every? #(contains? % :genome) result))
    (is (every? #(contains? % :elo) result))))

(comment
  "I would continue one with writing the tests for interpreter but!
   I think someone else had the issue that while testing interpreter,
   symbols from default instructions are not being recognized."
  
  (deftest interpreter
    (is (core/interpret-one-step example-push-state) example-push-state-interpret-one)
    (is (core/interpret-push-program example-push-state empty-push-state) example-push-state-interpret-all))
)

(deftest add-move-to-board-test
  (is (= (core/add-move-to-board [0 0 0 0 0 0 0 0 0] 0) [1 0 0 0 0 0 0 0 0]))
  (is (= (core/add-move-to-board [0 1 0 0 0 0 0 0 0] 2) [0 1 1 0 0 0 0 0 0]))
  (is (= (core/add-move-to-board [1 2 1 2 1 2 1 2 1] 0) [1 2 1 2 1 2 1 2 1])))

(deftest check-win-test
  (is (core/check-win [1 1 1 0 0 0 0 0 0]))  ; Row win
  (is (core/check-win [1 0 0 1 0 0 1 0 0]))  ; Column win
  (is (core/check-win [1 0 0 0 1 0 0 0 1]))  ; Diagonal win
  (is (not (core/check-win [1 2 1 2 1 2 0 0 0])))  ; No win
  (is (not (core/check-win [0 0 0 0 0 0 0 0 0])))  ; Empty board
  )

(deftest inverse-board-test
  (is (= (core/inverse-board [1 2 1 2 0 0 0 0 0]) [2 1 2 1 0 0 0 0 0]))  ; Normal inversion
  (is (= (core/inverse-board [0 0 0 0 0 0 0 0 0]) [0 0 0 0 0 0 0 0 0]))  ; Empty board
  (is (= (core/inverse-board [1 1 1 1 1 1 1 1 1]) [2 2 2 2 2 2 2 2 2]))  ; Full board
  )

(deftest valid-move 
  (is (= (core/valid-move? [0 0 0 0 0 0 0 0 0] 0) true))
  (is (= (core/valid-move? [0 0 0 0 0 0 0 0 0] -1) false))
  (is (= (core/valid-move? [0 0 0 0 0 0 0 0 0] 9) false)))

(deftest adjust-elo-test
  (is (= (core/adjust-elo 1000 1000 1) [1016.0 984.0]))  
  (is (= (core/adjust-elo 1000 1000 2) [984.0 1016.0]))  
  (is (= (core/adjust-elo 1000 1000 0) [1000.0 1000.0]))  
  (is (= (core/adjust-elo 1500 1300 1) [1507.6880983472654 1292.3119016527346])) ; how did I find out... ahah
  )

(deftest update-individ-elo-test
  (is (= (core/update-individ-elo {:elo 1000 :genome '(1 2)} 1100) {:elo 1100 :genome '(1 2)}))
  (is (= (core/update-individ-elo {:elo 950 :genome '(3 4)} 900) {:elo 900 :genome '(3 4)})))

(deftest select-and-vary-elo-inherit-test
  (let [population (repeat 10 {:genome '(1 2 3) :elo 1000})
        child (core/select-and-vary-elo-inherit population instructions)]
    (is (contains? child :genome))
    (is (contains? child :elo))
    (is (number? (:elo child)))))


(deftest GP 
  (is (type (core/make-random-plushy-genome instructions 10)) list)
  (is (<= (count (core/make-random-plushy-genome instructions 10)) 10))
  (is (<= (count (core/make-random-plushy-genome instructions 50)) 50))
  (is (type (core/tournament-selection example-population)) hash-map)
  (is (type (core/crossover example-genome1 example-genome2)) list)
  (is (<= (count (core/crossover example-genome1 example-genome2)) 8))
  (is (>= (count (core/uniform-addition example-genome2 instructions)) 8))
  (is (<= (count (core/uniform-deletion example-genome2)) 8)))
