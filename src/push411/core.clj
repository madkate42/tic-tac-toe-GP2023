(ns push411.core)

(comment
  "Kate Bondarenko")

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(exec_if integer_+ integer_-)
   :integer '(100 10 20 3 4 5 6 7)
   :string '("abc" "def")
   :boolean (list true true)
   :input {:in1 4 :in2 6}})

(def example-player-state
  {:exec '(exec_if integer_+ integer_-)
     :integer '(2 10 20 3 4 5 6 7)
     :string '("abc" "def")
     :boolean (list true true)
     :board [0 0 1 0 0 1 0 0 2]})

(def example-state-exec-if
  {:exec '(exec_if integer_+ integer_-)
   :integer '(10 6)
   :string '("abc" "def")
   :boolean (list true)
   :board [0 0 1 0 0 1 0 0 2]})

(def individ-exec-if
  {:genome '(true 2 2 exec_if integer_+ integer_-)
   :elo 1000})

(def individ-exec-if2
  {:genome '(true exec_if)
   :elo 1000})

; An example Plushy genome
(def example-plushy-genome
  '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close))

; An example Push program
; This is the program that would result from the above Plushy genome
(def example-push-program
  '(integer_* exec_dup ("hello" 4 "world" integer_-)))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:genome '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close)
   :program '(3 5 integer_* exec_dup ("hello" 4 "world" integer_-))
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

(def example-great-player1
  {
   :genome '(4
             empty-square?
             exec_if
             4
             5)
   :elo 1000
  })

(def example-great-player2
  {:genome '(4
             empty-square?
             exec_if
             integer_+
             close
             integer_%
             2
             close)
   :elo 1000})


;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push

(def default-instructions
  (list
   'in1
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   'boolean_=
   'boolean_and
   'boolean_or
   'exec_dup
   'exec_if
   'close
   'empty-square?
   'my-square?
   'enemy-square?
   0 1 2 3 4 5 6 7 8 9
   true 
   false))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1
   'exec_if 2})

;;;;;;;;;
;; Utilities

;; Board utilities 

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :input {}
   :board []})

(def empty-board [0 0 0 0 0 0 0 0 0])

(defn inverse-board 
  [board]
  (loop [new-board []
         index 0]
    (cond  
      (= index 9) new-board
      (= (nth board index) 0) (recur (conj new-board 0) (inc index))
      (= (nth board index) 1) (recur (conj new-board 2) (inc index))
      (= (nth board index) 2) (recur (conj new-board 1) (inc index)))))

(defn add-move-to-board
  "puts a 1 in the position 0-8"
  [board position]
  (assoc board position 1))

(defn valid-move? 
  [board position]
  (if (= (type position) Long)
   (if (and (>= position 0) (< position 9) )
     (if (= 0 (nth board position)) 
       true 
       false) 
    false)
  false))

; https://clojuredocs.org/clojure.core/doseq
(defn print-board
  "Prints a vector representing a 3x3 grid in a formatted way"
  [board]
  (println "_________")
  (doseq [row (partition 3 board)]
    (println "|" (clojure.string/join " " row) "|"))
  (println "---------"))


; https://clojuredocs.org/clojure.core/some
(defn check-win
  "Takes a vector representing a 3x3 grid and checks for a win 
   (three 1s in a row, column, or diagonal)"
  [board]
  (let [rows [(subvec board 0 3) (subvec board 3 6) (subvec board 6 9)]
        cols [(mapv nth rows [0 0 0]) (mapv nth rows [1 1 1]) (mapv nth rows [2 2 2])]
        diags [(mapv nth rows [0 1 2]) (mapv nth rows [2 1 0])]
        lines (concat rows cols diags)]
    ;; (println "-----------------------------")
    ;; (println "| GAME COMPLETED WITH A WIN |")
    ;; (println "-----------------------------")
    (some #(= [1 1 1] %) lines)))

;; elo score utilities

(defn adjust-elo
  "d is either 1 if rating1 wins, 
   2 if rating2 wins
   0 if draw."
  [rating1 rating2 d]
  (if (or (= rating1 nil) (= rating2 nil))
    [rating1 rating2]
    (let [exp-score1 (/ 1 (+ 1 (Math/pow 10 (/ (- rating2 rating1) 400))))
          exp-score2 (/ 1 (+ 1 (Math/pow 10 (/ (- rating1 rating2) 400))))
          score1 (cond (= d 1) 1
                       (= d 2) 0
                       :else 0.5)
          score2 (cond (= d 1) 0
                       (= d 2) 1
                       :else 0.5)
          new-rating1 (+ rating1 (* 32 (- score1 exp-score1)))
          new-rating2 (+ rating2 (* 32 (- score2 exp-score2)))]
      [new-rating1 new-rating2])))

(defn update-individ-elo
  [player new-elo]
  (conj player (hash-map :elo new-elo)))

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state.
    Params:
   - state: map of keywords with lists"
  [state stack item]
  (let [returnStack (conj (state stack) item)
        newState (dissoc state stack)]
    (conj newState (hash-map stack returnStack))))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (let [returnStack (rest (state stack))
        newState (dissoc state stack)]
    (conj newState (hash-map stack returnStack))))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (let [topItem (first (state stack))]
    (if (empty? (state stack))
      :no-stack-item
      topItem)))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map with keys {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))


(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))


;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (if (= ((state :input) :in1) nil)
    state
    (push-to-stack state :exec ((state :input) :in1))))

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the
  second, the result pushed to the stack should be (second - first)."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (let [protected-division (fn
                             [num den]
                             (if (= den 0)
                               num
                               (quot num den)))]
    (make-push-instruction state protected-division [:integer :integer] :integer)))

(defn integer_=
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

(defn integer_>
  [state]
  (let [greater (fn
                  [num1 num2]
                  (if (> num1 num2)
                    true
                    false))]
  (make-push-instruction state greater [:integer :integer] :boolean)))

(defn integer_<
  [state]
  (let [less (fn
                  [num1 num2]
                  (if (> num1 num2)
                    false
                    true))]
    (make-push-instruction state less [:integer :integer] :boolean)))

(defn boolean_=
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))
 
(defn boolean_and
  [state]
  (let [and-f (fn [a b]
                (if (and a b)
                  true
                  false))]
   (make-push-instruction state and-f [:boolean :boolean] :boolean)))

(defn boolean_or
  [state]
  (make-push-instruction state 'or [:boolean :boolean] :boolean))


(defn check-board-position 
  [board position]
  ;; (println position)
  (if (= (type position) Long)
    (if (and (>= position 0) (< position 9))
      (nth board position)
      false)
    false))

(defn empty-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   What happens if the value is not 1-9??"
  [state] 
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))] 
    ;; (println what-on-board)
    (if (not= what-on-board false)
       (if (= what-on-board 0) 
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))


(defn enemy-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   What happens if the value is not 1-9??"
  [state]
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))]
    (if (not= what-on-board false)
      (if (= what-on-board 2)
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))

(defn my-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   What happens if the value is not 1-9??"
  [state]
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))]
    (if (not= what-on-board false)
      (if (= what-on-board 1)
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

(defn exec_if 
  "EXEC.IF: If the top item of the BOOLEAN stack is TRUE 
   then this removes the second item on the EXEC stack, 
   leaving the first item to be executed.
   ^ from PUSH description page"
  [state]
  (if (and (> (count (state :exec)) 1) (not (empty-stack? state :boolean)))
    (let [top-boolean (peek-stack state :boolean)
          new-state (pop-stack state :boolean)]
        (if top-boolean ;if top is true, we have the second item
          (update new-state :exec #(cons (first %) (nthrest % 2)))
          (update new-state :exec #(rest %))))
    state))

;; Interpreter

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Or, if the next element is a nested list, needs to unwrap that list onto
  the exec stack.
  Returns the new Push state."
  [push-state]
  (let [newState (pop-stack push-state :exec)
        topExecItem (peek-stack push-state :exec)
        topExecItemType (type topExecItem)]
    ;; (println push-state)
    (cond
      (= topExecItemType Boolean) (push-to-stack newState :boolean topExecItem)
      (= topExecItemType Long) (push-to-stack newState :integer topExecItem)
      (= topExecItemType String) (push-to-stack newState :string topExecItem)
      (= topExecItemType clojure.lang.PersistentList)
      (conj (dissoc newState :exec) (hash-map :exec (concat topExecItem (newState :exec))))
      (= topExecItemType clojure.lang.PersistentList$EmptyList) newState
      :else ((eval topExecItem) newState))))

(comment
  "Interpret-push-program
   
  newState is created by attaching program to :exec 
   (pushes program onto stack)
  and loops, interprets one step at a time until :exec stack is empty
  or limit (100) over."
  )
(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing.
   To avoid infinite execution, you will need to enforce some maximum number
  of interpreter steps before terminating the program. You can choose this limit."
  [program start-state]
  (let [newState (conj (dissoc start-state :exec) ; delete :exec from start state
                       (hash-map :exec (concat program (start-state :exec)))) ; add program to :exec
        limit 100]
    (loop [state newState
           index 0]
      (if (or (empty-stack? state :exec) (>= index limit))
        state
        (recur (interpret-one-step state) (inc index))))))


;;;;;;;;;
;; Translation from Plushy genomes to Push programs

(defn translate-plushy-to-push
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opened-blocks
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opened-blocks %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else


;;;;;;;;;
;; GP

(defn make-random-plushy-genome
  "Creates and returns a new plushy genome. Takes a list of instructions and
  a maximum initial Plushy genome size."
  [instructions max-initial-plushy-size]
  (let [size (+ 1 (rand-int max-initial-plushy-size))]
    (repeatedly size #(rand-nth instructions))))

; updated to using elo scoring.
(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (let [competing (take 7 (shuffle population))]
    (first (reverse (sort-by :elo competing)))))

(defn make-move
  "Runs program on board, returns the board with a new move
   the new move is 1
   after the program is interpreted, there will be an integer on the stack
   if it's valid and "
  [individ board]
  (let [start-state (conj empty-push-state (hash-map :board board))
        program (translate-plushy-to-push (:genome individ))
        return-state (interpret-push-program program start-state)
        position (peek-stack return-state :integer) 
        new-board board]
    (print-board new-board)
    ;; (println individ)
    (if (valid-move? board position)
      (do 
        (print-board (add-move-to-board board position))
        (add-move-to-board board position))
      false)))

(defn play-game 
  [board current other]
  (let [new-board (make-move current board)] 
    (cond
      (false? new-board) ;; Current player failed to make a valid move, other player wins
      {:winner other :loser current :draw false}

      (check-win new-board) ;; Current player wins
      (do 
        (println "We have a win, individuals:"
                 current 
                 other)
        {:winner current :loser other :draw false :win true})

      ;; Check for draw
      (not-any? zero? new-board)
      {:winner nil :loser nil :draw true}

      :else ;; Continue the game, switch players
      (recur (inverse-board new-board) other current))))

(defn compete-helper
  "Takes two individuals and they compete, returning them with updated elo-scores."
  [player1 player2]
    (let [{:keys [winner loser draw win]} (play-game empty-board player1 player2)
          result (cond
                   draw 0
                   (= winner player1) 1
                   :else 2)
          [new-rating1 new-rating2] (adjust-elo (:elo player1) (:elo player2) result)
          updated-player1 (update-individ-elo player1 new-rating1)
          updated-player2 (update-individ-elo player2 new-rating2)]
      [updated-player1 updated-player2]))

(defn compete
  "Does it at chance"
  [player1 player2]
  (let [chance (rand-int 2)]
    (if (= chance 0) ; first starts first
      (compete-helper player1 player2)
      (reverse (compete-helper player2 player1)))))


(defn compete-all
  "The individual competes with all others in the population. 
   The individual and population are updated after each game."
  [individual population]
  (loop [individual individual
         remaining (rest population)
         updated-population []]
    (if (empty? remaining)
      (concat [individual] updated-population)
      (let [[updated-individual updated-competitor] (compete individual (first remaining))]
        (recur updated-individual (rest remaining) (conj updated-population updated-competitor))))))

(defn round-robin-bloated
  "Every individual competes against every other
   individal. Population is returned in the same order"
  [population]
  (loop [updated-pop (compete-all (first population) population)
         new-pop (vector (first updated-pop))]
    (let [remaining (rest updated-pop)]
      (if (empty? remaining)
        new-pop
        (recur (compete-all (first remaining) remaining) (conj new-pop (first remaining)))))
    ))

(defn round-robin
  "Applies compete-all to each individual in the population. After each individual
   competes against the entire population, they are added to a new population list."
  [population]
  (loop [remaining population
         new-population []]
    (if (empty? remaining)
      new-population
      (let [updated-pop (compete-all (first remaining) remaining)
            updated-individual (first updated-pop)
            rest-of-pop (rest updated-pop)]
        (recur rest-of-pop (conj new-population updated-individual))))))


;; (defn conduct-scoring
;;   "distributes by elo, and conducts scoring for everyone"
;;   [population]
;;   (let [new-pop population
;;         ]))



(defn crossover
  "Crosses over two Plushy genomes (note: not individuals) using uniform crossover.
  Returns child Plushy genome."
  [prog-a prog-b]
  (let [min-len (min (count prog-a) (count prog-b))] 
    (concat (map (fn [pair] (rand-nth pair)) ; select random from pair
                 (map vector ; put in pairs instuctions from both programs at the same index
                      prog-a prog-b))
            ; below, in a program with bigger size for every element either add or not
            (filter (fn [_] (= (rand-int 2) 0)) 
                    (drop min-len (if (<= (count prog-a)
                                          (count prog-b))
                                    prog-b
                                    prog-a))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the Plushy genomes) with some probability. Returns child Plushy genome."
  [prog instructions]
  (mapcat (fn [inst]
            (if (<= (rand) 0.1) ; 10% chance of addition
              (let [inst-toAdd (rand-nth instructions) ; choose instruction
                    pos (if (> (rand) 0.5) 1 0)] ; choose before or after
                (if (= pos 1) ; add instruction before of after
                  [inst-toAdd inst] 
                  [inst inst-toAdd]))
              [inst])) prog))

(defn uniform-deletion
  "Randomly deletes instructions from Plushy genomes at some rate. Returns
   child Plushy genome."
  [prog]
  (filter (fn [n] (>= (rand) 0.1)) prog))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population instructions]
  (let [parent1-genome ((tournament-selection population) :genome)
        parent2-genome ((tournament-selection population) :genome)
        chance (rand)]
    (cond
      (>= chance 0.5) (hash-map :genome (crossover parent1-genome parent2-genome))
      (>= chance 0.25) (hash-map :genome (uniform-addition parent1-genome instructions))
      :else (hash-map :genome (uniform-deletion parent1-genome)))))

(defn select-and-vary-elo-inherit
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population instructions]
  (let [parent1 (tournament-selection population)
        parent2 (tournament-selection population) ; this will become Elo tournament
        parent1-elo (parent1 :elo)
        parent2-elo (parent2 :elo)
        parent1-genome (parent1 :genome)
        parent2-genome (parent2 :genome)
        chance (rand)]
    (cond
      (>= chance 0.5) (hash-map :genome (crossover parent1-genome parent2-genome)
                                :elo (/ (+ parent2-elo parent1-elo) 2)) ; think if it should be average between the two???
      (>= chance 0.25) (hash-map :genome (uniform-addition parent1-genome instructions)
                                 :elo parent1-elo)
      :else (hash-map :genome (uniform-deletion parent1-genome)
                      :elo parent1-elo))))
















(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

-------------------------------------------------------
               Report for Generation 3
-------------------------------------------------------
Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
Best program size: 33
Best total error: 727
Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation] ;; since I pass sorted pop from GP, I don't sort here
  (let [best (first population)]
    (println "----------------------------------------------")
    (println "             Generation" generation "         ")
    (println "----------------------------------------------")
    (println "Best program:" (best :program))
    (println "Best total error:" (best :total-error))
    (println "Best program size:" (count (best :program)))
    (println "Best errors:" (best :errors))
    (println "----------------------------------------------")))

(defn initialize-population
  [population-size max-initial-plushy-size]
  (repeatedly population-size
              #(hash-map :genome 
                         (make-random-plushy-genome default-instructions 
                                                    max-initial-plushy-size),
                         :elo 1000)))

;; https://clojuredocs.org/clojure.core/sort-by
(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
    generates and evaluates new populations. Stops if it finds an
    individual with 0 error (and should return :SUCCESS, or if it
    exceeds the maximum generations (and should return nil). Should print
    report each generation.
    --
    The only argument should be a map containing the core parameters to
    push-gp. The format given below will decompose this map into individual
    arguments. These arguments should include:
     - population-size
     - max-generations
     - error-function
     - instructions (a list of instructions)
     - max-initial-plushy-size (max size of randomly generated Plushy genomes)"
  [{:keys [population-size max-generations error-function instructions max-initial-plushy-size]
    :as argmap}]
  (loop [population (initialize-population population-size max-initial-plushy-size) ; start pop
         generation 1] 
     (let [sorted-population (sort-by :total-error (map error-function population)) ; sort pop from best to worst
           new-child (select-and-vary sorted-population instructions)] ; make a new child
       (report sorted-population generation)
       (if (or (= ((first sorted-population) :total-error) 0)
               (= generation max-generations))
         sorted-population
         ; below, delete worst individ (last in sorted list) and add a child to list
         (recur (concat (butlast sorted-population) (list new-child))
                (inc generation))))))

(defn tic-tac-toe-push-gp
  "Let's see real quick how it does."
  [{:keys [population-size max-generations instructions max-initial-plushy-size]
    :as argmap}]
  (loop [population (initialize-population population-size max-initial-plushy-size) ; start pop
         generation 1]
    (if (< generation max-generations)
      (do
        (println population)
        (let [sorted-population (reverse (sort-by :elo (round-robin population))) ; sort pop from best to worst
              new-child (select-and-vary-elo-inherit sorted-population instructions)] ; make a new child 
          (println (first sorted-population) (nth sorted-population 1))
          (recur (concat (butlast sorted-population) (list new-child))
                 (inc generation))))
      population)))


(defn main
  "Runs push-gp, giving it a map of arguments."
  ([] (main {}))
  ([args]
   (tic-tac-toe-push-gp {:instructions default-instructions
             :max-generations 200
             :population-size 100
             :max-initial-plushy-size 120})))

(defn sum-elo [my-list]
  (reduce #(+ %1 (get %2 :elo 0)) 0 my-list))



(def winners '({:genome (integer_% 5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 integer_+ boolean_= 8 4 integer_% boolean_and exec_if integer_- 8 integer_% empty-square? integer_- boolean_and 4 close 0 integer_* 6 8 1 integer_- true 4 exec_if true 8 close integer_% 8 4 close in1 0 true 8 boolean_= 3 boolean_and empty-square? 8 3 7 boolean_= 8 empty-square? 0 exec_if enemy-square? 5 integer_* in1 integer_% 1 boolean_or true 8 integer_+ empty-square? 7 exec_dup exec_dup my-square? 4 boolean_or 8 exec_if true true 4 0), :elo 2113.67054830246} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 exec_if 9 2 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true empty-square? boolean_= boolean_or true boolean_or empty-square? boolean_or true 7 integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 enemy-square? 2 0 false 7 exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true integer_% 5 my-square? boolean_= enemy-square? 4 3 integer_+ 7 2 7 boolean_or in1 boolean_or integer_+ 7 4 7 exec_dup exec_dup in1 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2064.2531027147515}))

(def winners1 '({:genome (integer_% 5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 integer_+ boolean_= 8 4 integer_% boolean_and exec_if integer_- 8 integer_% empty-square? integer_- boolean_and 4 close 0 integer_* 6 8 1 integer_- true 4 exec_if true 8 close integer_% 8 4 close in1 0 true 8 boolean_= 3 boolean_and empty-square? 8 3 7 boolean_= 8 empty-square? 0 exec_if enemy-square? 5 integer_* in1 integer_% 1 boolean_or true 8 integer_+ empty-square? 7 exec_dup exec_dup my-square? 4 boolean_or 8 exec_if true true 4 0), :elo 2273.1055868798308} {:genome (integer_- true 3 empty-square? enemy-square? close 1 integer_- in1 boolean_or boolean_= integer_* enemy-square? 2 boolean_and boolean_= 0 7 integer_% boolean_and in1 empty-square? boolean_or 1 0 4 0 1 integer_* enemy-square? 4 0 exec_if false false false integer_* integer_+ integer_+ in1 integer_% exec_dup 9 boolean_and integer_* my-square? false 2 7 integer_- integer_% close integer_* close 7 exec_dup 6 integer_+ boolean_and close 3 integer_* 0 integer_% 8 integer_- 9 false boolean_= integer_* 6 4 false close true enemy-square? integer_- 3 boolean_or exec_dup exec_dup integer_+ in1 9 empty-square? 0 2 integer_% boolean_= integer_+ exec_dup 3 exec_if boolean_and 0 2), :elo 1880.4719177966222}))

(defn print-pop 
  [pop]
  (loop [pops pop
         index 0]
    (if (not (empty? pops))
     (do 
       (println (:elo (first pops)) "with index" index)
       (recur (rest pops) (inc index)))
      "There they are, pussy babies.")))


long-run
(def long-run '({:genome (7 0 enemy-square? true 2 close empty-square? 7 enemy-square? integer_+ true in1 0 0 empty-square? boolean_and 2 enemy-square? 6 integer_% boolean_= in1 6 integer_- integer_% boolean_= enemy-square? false 5 boolean_and boolean_or integer_* boolean_= empty-square? 5 exec_if close 4 true 1 9 6 empty-square? false true my-square? integer_* enemy-square? 8 true 0 integer_% 0 integer_* exec_dup 6 true true 6 0 true 8 enemy-square? boolean_or 0 9 boolean_or integer_* 8 enemy-square? boolean_= 5 7 5 true 3 exec_dup 6 3 in1 boolean_and my-square? my-square? my-square? exec_if boolean_or 9 close 1 1 in1 2 5 3 integer_- 0 6 1 false boolean_or integer_+ 8 empty-square? false integer_- 3), :elo 2246.7613529173427} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 exec_if boolean_= 9 2 integer_- 4 boolean_or 5 exec_if 1 enemy-square? integer_* true boolean_= 6 0 boolean_or empty-square? boolean_or true 7 my-square? integer_% integer_* 4 boolean_= 2 integer_- empty-square? 3 4 0 in1 enemy-square? 0 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= enemy-square? 4 false 3 enemy-square? integer_+ 8 7 2 exec_dup boolean_or boolean_or integer_+ 7 4 7 exec_dup exec_dup in1 0 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2227.4742555852467} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? boolean_or 5 9 3 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 1 integer_* true boolean_= 6 empty-square? boolean_or empty-square? integer_+ boolean_or true true 7 my-square? integer_% 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 0 false exec_dup integer_* boolean_or enemy-square? integer_% true 5 my-square? boolean_= enemy-square? 4 3 enemy-square? 7 2 boolean_or boolean_or integer_+ 4 7 exec_dup exec_dup in1 close 3 integer_+ boolean_= enemy-square? close boolean_or), :elo 2183.1786843031527} {:genome (integer_% 5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 integer_+ boolean_= 8 4 integer_% boolean_and exec_if integer_- 8 integer_% empty-square? integer_- boolean_and 4 close 0 integer_* 6 8 1 integer_- true 4 exec_if true 8 close integer_% 8 4 close in1 0 true 8 boolean_= 3 boolean_and empty-square? 8 3 7 boolean_= 8 empty-square? 0 exec_if enemy-square? 5 integer_* in1 integer_% 1 boolean_or true 8 integer_+ empty-square? 7 exec_dup exec_dup my-square? 4 boolean_or 8 exec_if true true 4 0), :elo 2178.0544062200183} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? boolean_or 4 5 9 3 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true boolean_= 6 empty-square? boolean_or empty-square? integer_+ boolean_or true true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= boolean_or enemy-square? 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_or integer_+ 7 4 7 7 exec_dup exec_dup in1 6 close 3 integer_+ boolean_= enemy-square? close boolean_or), :elo 2151.392297347261} {:genome (integer_% 5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 boolean_= 8 exec_if 4 boolean_and exec_if integer_- 8 integer_% integer_- boolean_and 4 integer_- 0 integer_* 6 8 1 true 4 exec_if true 8 close integer_% 4 close in1 0 boolean_= my-square? 3 boolean_and 8 7 boolean_= 8 1 0 exec_if enemy-square? 5 boolean_and integer_* in1 integer_% 1 boolean_or true 8 integer_+ empty-square? 7 exec_dup my-square? 4 boolean_or in1 8 exec_if true true 4 0), :elo 2146.0635255040943} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? boolean_or 4 5 9 3 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true boolean_= 6 empty-square? boolean_or empty-square? boolean_or true true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or integer_% 3 true 5 my-square? boolean_= boolean_or enemy-square? 3 enemy-square? integer_+ 7 2 boolean_or boolean_or integer_+ 7 4 7 7 exec_dup exec_dup in1 6 close 3 integer_+ boolean_= enemy-square? close boolean_or), :elo 2120.013777589313} {:genome (integer_* 0 9 exec_if in1 exec_dup integer_% my-square? boolean_or 4 5 9 3 exec_if 9 integer_% 2 1 in1 integer_- 5 in1 4 boolean_or 5 integer_+ 1 integer_* true boolean_= 6 empty-square? boolean_or empty-square? integer_+ boolean_or true true 7 my-square? 8 integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup integer_* integer_* 9 boolean_or enemy-square? integer_% 3 true 5 my-square? integer_- 5 boolean_= boolean_or enemy-square? 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_or integer_+ 7 4 7 5 7 exec_dup 9 exec_dup in1 6 close 3 integer_+ boolean_= enemy-square? boolean_or close boolean_or), :elo 2094.6206710709434} {:genome (5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 integer_+ boolean_= 8 4 integer_% boolean_and exec_if integer_- 8 integer_% empty-square? integer_- 4 close integer_* 6 8 1 integer_- true 4 exec_if true 8 close integer_% 8 4 close in1 0 true 8 boolean_= 3 boolean_and empty-square? 8 3 7 boolean_= 8 empty-square? 0 exec_if enemy-square? 5 integer_* in1 integer_% 1 boolean_or true 8 integer_+ empty-square? 7 exec_dup exec_dup my-square? 4 boolean_or 8 true true 0), :elo 2092.6986396413904} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? boolean_or 4 5 9 3 exec_if 9 2 in1 integer_- 5 4 5 integer_+ 1 integer_* boolean_= 6 empty-square? empty-square? boolean_or true true 7 my-square? integer_% integer_* 2 integer_- 3 6 4 1 0 in1 enemy-square? false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= enemy-square? 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_or 7 4 7 7 exec_dup exec_dup in1 6 close 3 integer_+ boolean_= enemy-square? close boolean_or), :elo 2091.6701532175343} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 exec_if 9 2 integer_- 4 boolean_or 5 1 integer_* true boolean_= 6 boolean_or empty-square? boolean_or true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 4 0 in1 enemy-square? 0 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= enemy-square? 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_or integer_+ 7 4 7 exec_dup exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2081.256571197323} {:genome (integer_% 5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 integer_+ boolean_= 8 4 integer_% boolean_and exec_if integer_- 8 integer_% empty-square? integer_- boolean_and 4 close 0 integer_* 6 8 1 integer_- true 4 exec_if true 8 close integer_% 8 4 close in1 0 true 8 boolean_= 3 boolean_and empty-square? 8 3 7 boolean_= 8 empty-square? 0 exec_if enemy-square? 5 integer_* in1 integer_% 1 boolean_or true 8 integer_+ empty-square? 7 exec_dup exec_dup my-square? 4 boolean_or 8 exec_if true true 4 0), :elo 2068.365781258094} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true boolean_= 6 boolean_or empty-square? boolean_or true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= enemy-square? 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_or integer_+ 7 4 7 7 exec_dup exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2061.9537869402325} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true empty-square? boolean_= 6 boolean_or true boolean_or empty-square? boolean_or true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 2 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true integer_% 5 my-square? boolean_= enemy-square? 4 3 enemy-square? integer_+ 7 2 7 boolean_or in1 boolean_or integer_+ 7 4 7 7 exec_dup exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2053.336910158867} {:genome (integer_* exec_if 0 9 exec_if 1 exec_dup integer_% my-square? 4 5 7 9 9 3 exec_if boolean_= 9 boolean_and 2 integer_- 4 boolean_or true 5 exec_if 1 enemy-square? 2 integer_* true integer_* boolean_= 6 0 boolean_or empty-square? integer_- boolean_or true integer_+ 7 my-square? integer_% integer_* 4 boolean_= 2 integer_- empty-square? 3 4 0 integer_- 0 in1 4 enemy-square? 0 7 6 exec_if boolean_and exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 7 5 my-square? boolean_= enemy-square? false 4 false 3 enemy-square? integer_+ boolean_and 8 7 2 exec_dup boolean_or boolean_or integer_+ 7 4 integer_% 7 8 exec_dup exec_dup in1 0 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2028.293658290693} {:genome (integer_* 0 9 exec_if 1 exec_dup integer_% my-square? 4 5 7 9 9 3 exec_if boolean_= 9 2 integer_- 4 boolean_or 5 exec_if 1 enemy-square? integer_* true integer_* boolean_= 6 0 boolean_or empty-square? integer_- boolean_or true integer_+ 7 my-square? integer_% integer_* 4 boolean_= 2 integer_- empty-square? 3 4 integer_- 0 in1 enemy-square? 0 7 6 exec_if boolean_and exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= enemy-square? false 4 false 3 enemy-square? integer_+ 8 7 2 exec_dup boolean_or boolean_or integer_+ 7 4 7 8 exec_dup exec_dup in1 0 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2016.5467475131459} {:genome (enemy-square? boolean_and true integer_- 8 boolean_or close integer_* integer_+ in1 close enemy-square? 4 integer_+ integer_% 5 exec_dup true boolean_and 1 exec_if 3 integer_+ integer_% 4 boolean_or 0 boolean_and integer_+ empty-square? exec_if integer_* my-square? exec_if my-square? integer_% empty-square? 3 integer_% empty-square? boolean_= 7 5 my-square?), :elo 2004.8049337397326} {:genome (exec_if empty-square? boolean_or boolean_or false boolean_and boolean_and false integer_% exec_if true 2 true 8 my-square? 4 boolean_or close enemy-square? my-square? enemy-square? 1 integer_+ true 6 5 5 integer_% integer_* enemy-square? exec_if 1 integer_+ close 2 2 false exec_dup true empty-square? my-square? 3 6 integer_% 1 integer_+ empty-square? integer_% empty-square? 2 8 1 in1 true in1 integer_* boolean_= exec_dup 6 integer_- exec_if 4 9 integer_% in1 0 boolean_and 6 9 in1 7 9 8 integer_% 6 2 integer_* 9 7 my-square? exec_if empty-square? integer_% in1 integer_* boolean_= boolean_= 2 my-square? 4 4 integer_+ true integer_+ boolean_or enemy-square? 3 true boolean_and in1 enemy-square? 2 in1 2 empty-square? boolean_and 3 4 integer_* in1 exec_dup 4 1), :elo 1982.2065909159871} {:genome (7 enemy-square? enemy-square? true 3 exec_if close 5 exec_dup true integer_- exec_if 4 integer_% 9 2 exec_if in1 6 integer_- integer_% 1 1 integer_+ exec_if 4 boolean_or in1 2 integer_% true 8 0 0 enemy-square? enemy-square? enemy-square? 5 exec_dup 6 integer_* 3 2 3 integer_+ 7 exec_if 3), :elo 1927.5516039810566} {:genome (integer_* 0 9 exec_if exec_dup integer_% 8 4 5 1 9 true 3 exec_if 9 2 exec_if in1 integer_- 5 4 boolean_or 5 close integer_+ integer_* true boolean_= 6 boolean_or empty-square? boolean_or true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 boolean_= 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= 4 3 enemy-square? integer_+ 7 2 boolean_or integer_+ 7 4 7 true 7 exec_dup empty-square? exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 1914.611086562444} {:genome (integer_+ 7 0 enemy-square? true 6 exec_if integer_% 0 integer_+ true exec_if in1 integer_* integer_% 8 1 exec_if in1 6 integer_- integer_% integer_+ false 1 boolean_and 9 boolean_= enemy-square? integer_+ 5 integer_% 4 my-square? 1 integer_- 6 false 6 5 integer_* integer_% 8 true 0 integer_% 0 integer_* enemy-square? 0 enemy-square? 5 7 1 5 exec_dup 6 3 in1 my-square? close my-square? exec_if boolean_or 9 1 2 integer_% 6 5 0 6 1 integer_- boolean_or integer_+ my-square? integer_- 3), :elo 1890.7247808194904} {:genome (7 0 enemy-square? true 6 exec_if integer_% 0 integer_+ true exec_if in1 integer_* integer_% 8 1 exec_if in1 6 integer_- integer_% integer_+ false 1 boolean_and 9 boolean_= integer_+ 5 integer_% 4 my-square? integer_- 6 false 6 5 integer_* integer_% 8 true 0 integer_% 0 integer_* enemy-square? 0 enemy-square? 5 7 5 exec_dup 6 3 in1 my-square? my-square? exec_if boolean_or 9 1 2 5 0 6 1 boolean_or integer_+ integer_- 3), :elo 1890.666649777724} {:genome (integer_- true 3 empty-square? enemy-square? close 1 integer_- in1 boolean_or boolean_= integer_* enemy-square? 2 boolean_and boolean_= 0 7 integer_% boolean_and in1 empty-square? boolean_or 4 0 1 integer_* enemy-square? 0 exec_if false false false integer_* integer_+ integer_+ in1 integer_% exec_dup 9 boolean_and integer_* my-square? false 2 7 integer_- integer_% close integer_* 7 exec_dup 6 integer_+ boolean_and close 3 integer_* 0 integer_% 8 integer_- 9 false boolean_= integer_* 6 4 false close true enemy-square? integer_- 3 boolean_or exec_dup exec_dup in1 9 empty-square? 0 2 integer_% boolean_= integer_+ exec_dup 3 exec_if boolean_and 0 2), :elo 1883.4378952052807} {:genome (7 0 enemy-square? true exec_dup exec_if 8 0 1 true 8 exec_if 4 boolean_and 9 2 exec_if in1 6 boolean_and integer_% integer_+ 0 close boolean_and integer_* 1 integer_+ 4 exec_if 4 boolean_or true 6 false 6 in1 2 integer_% 8 true 0 8 0 0 enemy-square? enemy-square? enemy-square? false enemy-square? 5 exec_dup 6 integer_* integer_* boolean_or my-square? integer_% 3 9 1 2 boolean_= my-square? 3 enemy-square? integer_+ 7 exec_if 3 6), :elo 1882.20528055114} {:genome (false integer_- exec_if true integer_- boolean_or 6 empty-square? 1 close integer_% integer_+ close integer_- exec_if 5 enemy-square? 8 5 8 4 false boolean_and boolean_or 0 false empty-square? true enemy-square? close 6 exec_dup integer_- 1 4 7 true integer_+ 9 2 exec_if exec_if 3 false 8 9 5 in1 boolean_= 7 exec_if integer_% empty-square? 3 2 empty-square? integer_- 9 boolean_and 4 2 6 5 1 2 enemy-square? integer_% true 1 integer_- close 6 4 7 9 8 8 3 false boolean_or 1 enemy-square? exec_if integer_+ 5), :elo 1879.4967946871054} {:genome (false integer_- exec_if close true integer_- boolean_or 6 empty-square? empty-square? 1 close integer_% integer_+ close in1 integer_- exec_if 5 enemy-square? 8 5 8 4 false boolean_and boolean_or 0 false empty-square? true enemy-square? close 6 exec_dup integer_- 1 4 7 true integer_+ 9 2 exec_if exec_if 3 false 8 9 5 in1 boolean_= 7 exec_if integer_% empty-square? 3 2 empty-square? integer_- 9 boolean_and 4 2 6 5 1 2 enemy-square? integer_% true 1 integer_- close 6 4 7 9 8 8 3 false boolean_or 1 enemy-square? enemy-square? exec_if integer_+ 5), :elo 1877.9709752915542} {:genome (7 0 enemy-square? true 6 exec_if integer_% 0 enemy-square? integer_+ true exec_if in1 0 integer_* integer_% 8 enemy-square? 1 exec_if enemy-square? in1 6 integer_- integer_% in1 integer_+ false 1 boolean_and integer_* 9 boolean_= integer_+ 5 exec_if integer_% 4 8 my-square? integer_- 6 false false 6 5 integer_* integer_% 8 true 0 integer_% 0 integer_* enemy-square? 0 enemy-square? 5 7 5 exec_dup 6 3 in1 my-square? my-square? exec_if boolean_or 9 1 1 2 5 0 6 1 boolean_or integer_+ integer_- 3), :elo 1874.1484508758697} {:genome (integer_- true 3 empty-square? enemy-square? close 1 integer_- in1 boolean_or boolean_= integer_* enemy-square? 2 boolean_and boolean_= 0 7 integer_% boolean_and in1 empty-square? boolean_or 1 0 4 0 1 integer_* enemy-square? 4 0 exec_if false false false integer_* integer_+ integer_+ in1 integer_% exec_dup 9 boolean_and integer_* my-square? false 2 7 integer_- integer_% close integer_* close 7 exec_dup 6 integer_+ boolean_and close 3 integer_* 0 integer_% 8 integer_- 9 false boolean_= integer_* 6 4 false close true enemy-square? integer_- 3 boolean_or exec_dup exec_dup integer_+ in1 9 empty-square? 0 2 integer_% boolean_= integer_+ exec_dup 3 exec_if boolean_and 0 2), :elo 1869.8436801960006} {:genome (false exec_if true integer_- 6 empty-square? 1 close integer_% integer_+ exec_if 5 enemy-square? 8 8 4 false boolean_and boolean_or 9 empty-square? true enemy-square? close exec_dup integer_- 3 4 7 true integer_+ 9 2 exec_if 3 false 8 5 in1 9 boolean_= 7 exec_if integer_% empty-square? 3 2 empty-square? 9 4 2 5 1 enemy-square? 2 true 1 6 4 7 9 8 8 3 false boolean_or 1 enemy-square? integer_+ 5), :elo 1868.4472565106814} {:genome (7 0 3 exec_dup exec_if 7 7 enemy-square? integer_+ 2 exec_if 0 empty-square? my-square? boolean_and integer_% boolean_and boolean_or in1 6 enemy-square? 5 boolean_= false 5 5 exec_dup integer_* 0 integer_* exec_if 8 my-square? 4 true 1 5 6 integer_+ false integer_% exec_if 1 1 8 integer_* 0 integer_% 0 2 exec_dup 6 integer_% 4 6 true 8 boolean_or boolean_or 8 boolean_= 7 true 3 my-square? 9 1 2 5 3 0 8 empty-square? 3), :elo 1857.5683295403608} {:genome (enemy-square? boolean_and true integer_- 8 boolean_or close integer_* integer_+ in1 close enemy-square? 4 integer_+ integer_% 5 exec_dup true boolean_and 1 exec_if 3 integer_+ integer_% boolean_or 0 boolean_and integer_+ empty-square? exec_if integer_* my-square? my-square? integer_% empty-square? 3 integer_% empty-square? boolean_= 7 5 my-square?), :elo 1854.9672868697073} {:genome (3 integer_* boolean_and 1 integer_- boolean_or exec_if 4 empty-square? 0 2 1 true true enemy-square? my-square? in1 enemy-square? 0 integer_% exec_if enemy-square? 9 integer_% 5 false exec_if 0 exec_if 3 empty-square? 8 exec_if), :elo 1849.040000895141} {:genome (integer_- true 3 empty-square? enemy-square? close 1 integer_- in1 boolean_or boolean_= integer_* enemy-square? 2 boolean_and boolean_= 0 7 integer_% boolean_and in1 empty-square? boolean_or 1 0 4 0 1 integer_* enemy-square? 4 0 exec_if false false false integer_* integer_+ integer_+ in1 integer_% exec_dup 9 boolean_and integer_* my-square? false 2 7 integer_- integer_% close integer_* close 7 exec_dup 6 integer_+ boolean_and close 3 integer_* 0 integer_% 8 integer_- 9 false boolean_= integer_* 6 4 false close true enemy-square? integer_- 3 boolean_or exec_dup exec_dup integer_+ in1 9 empty-square? 0 2 integer_% boolean_= integer_+ exec_dup 3 exec_if boolean_and 0 2), :elo 1844.1715713317467} {:genome (8 integer_* in1 integer_* 0 empty-square? exec_if boolean_= integer_+ true integer_+ my-square? 6), :elo 1842.2276263659712} {:genome (in1 enemy-square? 6 enemy-square? 3 empty-square? close 5 exec_dup 5 integer_- in1 6 integer_% integer_+ 2 exec_if empty-square? in1 integer_- empty-square? 1), :elo 1841.8193281006302} {:genome (integer_+ boolean_= exec_if exec_if integer_- boolean_or 6 empty-square? 2 close 8 integer_+ close integer_- enemy-square? integer_+ integer_+ 4 8 false my-square? 0 close 6 integer_- 1 3 integer_+ 2 exec_if exec_if 3 8 5 9 7 exec_if empty-square? 3 2 empty-square? 2 6 2 true close 4 7 9 8 8 8 boolean_or enemy-square? exec_if 5), :elo 1838.7500746179667} {:genome (boolean_and 9 6 6 9 true 4 boolean_or 4 true 9 integer_+ 0 exec_if integer_+ 1 enemy-square? close 4 6 2 integer_* false close my-square? 2 3 exec_dup integer_- empty-square? in1 exec_if 8 exec_dup), :elo 1838.0699025887156} {:genome (4 8 false 3 2 exec_dup false 3 6 integer_+ enemy-square? integer_% empty-square? integer_* 6 in1 true false 8 true 3 boolean_or exec_if exec_if 1 boolean_and integer_% my-square? boolean_= true 6 integer_+ empty-square? close 3 boolean_or empty-square? true integer_* 6 1 boolean_or integer_+ 5 1 7 exec_dup in1 false 1 enemy-square? close integer_+ integer_- my-square? 5 true true 8 6 close my-square? boolean_= 7 my-square? integer_+ 6 exec_dup integer_* boolean_= integer_* enemy-square? 8 close 4 exec_dup integer_* exec_if true boolean_= integer_* boolean_and 8 5 6 integer_+ 4 2 in1 integer_+ boolean_or 2 3 2 boolean_or 7 boolean_and true enemy-square? integer_% in1 empty-square? close 3 6 boolean_= exec_dup my-square?), :elo 1837.6096732788085} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? boolean_or 5 9 3 9 2 in1 integer_- 4 boolean_or 5 1 integer_* true boolean_= 6 empty-square? boolean_or empty-square? integer_+ boolean_or true true 7 integer_% 2 integer_- empty-square? 3 6 4 1 0 in1 enemy-square? 0 false exec_dup integer_* boolean_or enemy-square? integer_% true 5 my-square? boolean_= enemy-square? 4 3 enemy-square? 7 2 boolean_or boolean_or integer_+ 4 7 exec_dup exec_dup close 3 integer_+ boolean_= enemy-square? close boolean_or), :elo 1834.6192913229129} {:genome (false exec_if true integer_- 6 empty-square? 1 close integer_% integer_+ exec_if 5 enemy-square? 8 8 4 false boolean_and boolean_or 9 empty-square? close exec_dup integer_- 3 4 7 true integer_+ 9 2 exec_if 3 false 8 9 boolean_= 7 exec_if integer_% empty-square? 3 2 empty-square? 9 4 2 5 1 enemy-square? 2 true 1 6 4 7 9 8 8 3 false boolean_or 1 enemy-square? integer_+ 5), :elo 1833.9220626869535} {:genome (exec_dup integer_+ integer_* 3 boolean_= boolean_and integer_% 7 integer_+ 9 4 7 boolean_or boolean_or 1 4 integer_+ 3 exec_if 5 8 7 boolean_and integer_% 2 my-square? exec_if 1 false my-square? my-square? empty-square? enemy-square? boolean_or true exec_if 2 1 false 1 close integer_+ false 6 3 6 false my-square? boolean_= boolean_and integer_- 2 exec_dup false), :elo 1831.3368988498896} {:genome (false integer_- exec_if true integer_- 6 empty-square? 1 4 close integer_% integer_+ close exec_if 5 enemy-square? 8 5 8 4 false boolean_and boolean_or 0 false 9 empty-square? true enemy-square? 0 close exec_dup integer_- 3 4 7 true integer_+ 9 2 exec_if exec_if 3 false 8 9 5 in1 9 boolean_= 7 exec_if integer_% empty-square? 3 2 9 empty-square? 9 boolean_and 4 2 5 1 empty-square? enemy-square? integer_% 2 integer_% true 9 1 integer_- close 6 4 7 9 8 8 8 3 false boolean_or 1 enemy-square? integer_+ 5), :elo 1829.9590607741363} {:genome (integer_- true 3 empty-square? enemy-square? close 1 integer_- in1 boolean_or boolean_= integer_* enemy-square? 2 boolean_and boolean_= 0 7 integer_% boolean_and in1 empty-square? boolean_or 1 0 4 0 1 integer_* enemy-square? 4 4 exec_if 0 exec_if false false false integer_+ integer_* integer_+ integer_+ in1 integer_% exec_dup 9 boolean_and integer_* my-square? false 2 7 integer_- integer_% close integer_* close 7 exec_dup 6 integer_+ boolean_and close 3 integer_* 0 integer_% 8 integer_- 9 false boolean_= integer_* 6 4 false close exec_if true enemy-square? my-square? integer_- 3 3 boolean_or exec_dup exec_dup integer_+ in1 9 empty-square? 0 2 integer_% boolean_= integer_+ exec_dup 2 3 exec_if boolean_and 0 2), :elo 1829.4577849686946} {:genome (integer_% close 3 exec_dup 3 exec_if 7 enemy-square? 5 boolean_and 2 exec_if false empty-square? my-square? false integer_% 3 boolean_and 0 boolean_or exec_if 6 enemy-square? 5 5 boolean_= my-square? 1 5 exec_dup my-square? 0 integer_* exec_if 8 my-square? boolean_and 9 integer_- 5 boolean_= integer_+ my-square? integer_% exec_if 1 1 empty-square? integer_* integer_% 0 boolean_or 2 exec_dup 5 integer_% 4), :elo 1811.7721813880316} {:genome (false 5 in1 integer_+ integer_- exec_dup 6 3 1 integer_+ boolean_= 4 integer_% integer_- exec_if 5 8 8 empty-square? 8 4 false boolean_and boolean_or 0 false 8 true enemy-square? true 4 exec_if true 1 4 8 4 close in1 0 true exec_if boolean_= false 8 8 3 in1 boolean_= 7 empty-square? integer_% empty-square? 5 integer_* empty-square? integer_% boolean_or true 4 integer_+ empty-square? 7 1 exec_dup enemy-square? integer_% true 8 exec_if true 6 4 0 8 3 false boolean_or integer_+ 5), :elo 1811.7389598455622} {:genome (false integer_- exec_if integer_+ integer_- exec_dup 6 3 1 close 8 4 integer_+ close integer_- 8 5 enemy-square? 8 5 8 4 false boolean_and 1 boolean_or 4 false true 8 true integer_% enemy-square? close in1 0 boolean_= 1 4 8 true boolean_= enemy-square? 0 exec_if enemy-square? exec_if integer_* in1 integer_% 9 boolean_or true 8 7 exec_if 7 exec_dup my-square? 4 boolean_or integer_- 9 boolean_and 4 4 0 5 0 4 enemy-square? true 1 integer_- close 7 8 false enemy-square? exec_if integer_+), :elo 1811.1306860160669} {:genome (enemy-square? boolean_and true integer_+ 8 boolean_or integer_+ 3 1 integer_+ boolean_= 8 4 integer_% integer_% 5 integer_- true integer_% 1 integer_- boolean_and 4 integer_% boolean_or integer_* 6 8 1 exec_if integer_* my-square? exec_if integer_% empty-square? 3 integer_% 8 boolean_= 7 in1 my-square? 8 boolean_= empty-square? 8 8 0 enemy-square? in1 integer_% boolean_or true 8 empty-square? exec_dup my-square? true 4), :elo 1811.1056613124324} {:genome (false integer_- exec_if true integer_- 6 empty-square? 1 4 close integer_% integer_+ close exec_if 5 enemy-square? 8 5 8 4 enemy-square? false boolean_and boolean_or 0 false 9 empty-square? true enemy-square? 0 close exec_dup integer_- 3 4 2 7 true integer_+ 9 2 exec_if exec_if 2 3 false 8 9 5 in1 9 boolean_= 7 exec_if integer_% empty-square? 3 2 9 empty-square? 9 my-square? boolean_and 4 2 5 1 empty-square? enemy-square? integer_% 2 integer_% true false 9 1 integer_- boolean_or close 6 4 7 9 8 8 8 3 false boolean_or 1 enemy-square? integer_+ boolean_or 5), :elo 1810.4195538625459} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true boolean_= 6 boolean_or empty-square? true 7 my-square? integer_% 2 empty-square? 3 6 1 0 in1 enemy-square? false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= enemy-square? 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_or integer_+ 7 4 7 exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 1807.9756604693544} {:genome (exec_if empty-square? boolean_or boolean_or false boolean_and boolean_and false integer_% exec_if true 2 true 8 my-square? 4 boolean_or close enemy-square? my-square? enemy-square? 1 integer_+ true 5 integer_% integer_* enemy-square? exec_if 1 integer_+ close 2 2 false exec_dup true empty-square? my-square? 3 6 integer_% 1 integer_+ empty-square? integer_% empty-square? 2 8 1 in1 true integer_* boolean_= exec_dup 6 integer_- exec_if 4 9 integer_% in1 0 6 9 in1 7 9 8 integer_% 6 2 integer_* 9 7 my-square? exec_if empty-square? integer_% in1 integer_* boolean_= boolean_= 2 my-square? 4 4 integer_+ true integer_+ enemy-square? 3 boolean_and in1 enemy-square? 2 in1 2 empty-square? boolean_and 3 4 integer_* in1 exec_dup 4 1), :elo 1804.489720325213} {:genome (false integer_- exec_if true integer_- boolean_or 6 empty-square? 1 close integer_% integer_+ close integer_- exec_if 5 enemy-square? 8 5 8 4 false my-square? boolean_and boolean_or 0 false 9 empty-square? true enemy-square? close 6 exec_dup integer_- 1 3 4 7 true integer_+ 9 2 exec_if exec_if 3 false 8 9 5 in1 9 boolean_= 7 exec_if integer_% empty-square? 3 2 9 empty-square? integer_- 9 boolean_and 4 2 6 5 1 2 enemy-square? integer_% 2 true 1 integer_- close 6 4 7 9 8 8 8 3 false boolean_or 1 enemy-square? exec_if integer_+ 5), :elo 1796.3552832198554} {:genome (integer_% 5 in1 integer_+ boolean_= boolean_= exec_dup integer_+ 3 1 boolean_= 8 exec_if 4 boolean_and exec_if integer_- 8 integer_% integer_- integer_- boolean_and 4 integer_- 0 integer_* 6 8 1 true 9 4 exec_if true 8 close integer_% 4 close in1 0 boolean_= my-square? 3 boolean_and 8 7 boolean_= 8 1 1 0 boolean_or exec_if enemy-square? 5 boolean_and integer_* in1 integer_% 1 boolean_or true 8 boolean_or integer_+ empty-square? 7 exec_dup my-square? exec_dup 4 0 boolean_or in1 8 exec_if true true exec_if 4 0), :elo 1795.3784285413446} {:genome (in1 enemy-square? enemy-square? true 3 exec_if integer_% 5 integer_+ true integer_- in1 integer_* integer_% integer_+ 2 exec_if empty-square? in1 integer_- empty-square? 1 false 9 integer_% integer_- false 5 integer_% 8 true 0 0 0 6 3 my-square? 2 0 6 1 boolean_or), :elo 1787.9805392521862} {:genome (5 in1 integer_+ exec_if exec_dup integer_% 3 8 4 5 1 4 integer_% 3 exec_if 9 8 exec_if in1 0 integer_- close integer_* 6 5 close integer_+ integer_* true exec_if 6 8 close empty-square? 8 4 true in1 my-square? integer_% integer_* 2 3 boolean_and empty-square? 6 3 7 boolean_= 0 empty-square? 0 exec_if enemy-square? 5 6 exec_if exec_dup 1 integer_* integer_* 8 integer_+ integer_% 3 exec_dup exec_dup my-square? 4 boolean_= 8 3 enemy-square? 0 7 boolean_or 4 exec_dup close enemy-square? close true), :elo 1787.7881987779704} {:genome (7 0 enemy-square? true boolean_and 8 empty-square? 7 6 boolean_and 2 in1 boolean_= 0 empty-square? boolean_or 2 7 6 integer_% boolean_= boolean_= exec_if integer_- exec_if my-square? boolean_or empty-square? 5 boolean_and integer_- exec_dup integer_% boolean_= 5 enemy-square? close close 1 in1 9 6 2 7 enemy-square? integer_- exec_if enemy-square? exec_dup exec_dup empty-square? integer_* 5 integer_* exec_dup 6 integer_% 2 boolean_and 6 true boolean_= enemy-square? true 0 9 boolean_or integer_% close enemy-square? boolean_= integer_% integer_* 5 true 3 close 6 3 boolean_and boolean_and 2 my-square? 2 2 boolean_or 9 close 1 1 in1 integer_- 5 integer_- 7 enemy-square? exec_dup boolean_= integer_- boolean_or integer_+ 8 empty-square? in1 integer_- close), :elo 1787.4611286124245} {:genome (integer_* 0 9 exec_if exec_dup integer_% 4 5 9 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ 1 empty-square? boolean_= 6 boolean_or true boolean_or boolean_or 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 in1 2 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 integer_% 5 my-square? boolean_= enemy-square? 4 3 enemy-square? integer_+ 7 2 7 boolean_or in1 boolean_or integer_+ 7 4 7 exec_dup exec_dup in1 close integer_+ boolean_= enemy-square? close boolean_or), :elo 1786.9619528760302} {:genome (5 in1 integer_+ boolean_= exec_dup 3 1 boolean_= 8 4 exec_if integer_- integer_- 4 integer_- 0 integer_* 6 8 1 true 4 exec_if true close integer_% 4 in1 0 boolean_= my-square? 3 boolean_and 8 7 boolean_= 8 1 0 exec_if enemy-square? 5 boolean_and integer_* integer_% 1 boolean_or true 8 empty-square? 7 exec_dup my-square? 4 boolean_or in1 8 exec_if true true 4 0), :elo 1786.3525062882368} {:genome (5 5 7 1 integer_* 6 3 boolean_and in1 boolean_= 7), :elo 1784.536497168975} {:genome (4 boolean_or 8 4 6 exec_if integer_% 0 integer_* 5 exec_dup exec_if in1 integer_* integer_* integer_% 8 boolean_and 1 exec_if enemy-square? 4 exec_if in1 integer_% in1 integer_+ integer_- 1 boolean_or integer_* 9 integer_- integer_+ integer_* 9 integer_% integer_% 8 my-square? integer_- 8 false 7 6 5 true integer_% boolean_and 2 integer_+), :elo 1783.7308777228527} {:genome (5 my-square? enemy-square? integer_+ boolean_= boolean_or empty-square? 3 1 boolean_= true in1 true 8 empty-square? boolean_and exec_if 7 integer_- 8 3 integer_% integer_- boolean_and 4 0 integer_* enemy-square? 6 empty-square? 5 integer_* boolean_and empty-square? 5 true 8 boolean_and false 4 close in1 integer_* boolean_= 3 boolean_and 4 1 exec_dup integer_% boolean_= integer_% 0 integer_* enemy-square? 5 integer_* 3 integer_% true 1 boolean_or 6 false 0 integer_+ 0 integer_* 9 exec_dup my-square? 4 9 5 true true exec_dup true 3 0 enemy-square? 0 my-square? exec_if 2 0 false integer_- empty-square? boolean_and exec_dup integer_* boolean_or), :elo 1779.7902324896343} {:genome (4 8 false 3 2 exec_dup false 3 6 integer_+ enemy-square? integer_% empty-square? integer_* 6 in1 true false 8 true 3 boolean_or exec_if exec_if 1 boolean_and integer_% my-square? boolean_= true 6 integer_+ empty-square? close 3 boolean_or empty-square? true integer_* 6 1 boolean_or integer_+ 5 1 7 exec_dup in1 false 1 enemy-square? close integer_+ integer_- my-square? 5 true true 8 6 close my-square? boolean_= 7 my-square? integer_+ 6 exec_dup integer_* boolean_= integer_* enemy-square? 8 close 4 exec_dup integer_* exec_if true boolean_= integer_* boolean_and 8 5 6 integer_+ 4 2 in1 integer_+ boolean_or 2 3 2 boolean_or 7 boolean_and true enemy-square? integer_% in1 empty-square? close 3 6 boolean_= exec_dup my-square?), :elo 1779.1347103891762} {:genome (integer_+ 8 2 my-square? integer_+ integer_- boolean_= exec_if in1 false 0 integer_* exec_dup true integer_% exec_if 1 true boolean_=), :elo 1778.985295433599} {:genome (integer_* 0 9 exec_if exec_dup integer_% 4 5 9 3 exec_if 9 2 in1 integer_- 5 4 boolean_or 5 integer_+ integer_* true boolean_= 6 boolean_or empty-square? boolean_or true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 4 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_= 4 3 enemy-square? integer_+ 7 2 boolean_or integer_+ 7 4 7 7 exec_dup exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or), :elo 1774.0042705978235} {:genome (integer_+ boolean_= 7 exec_if integer_- 2 empty-square? 2 empty-square? 2 false 8 close boolean_and boolean_= 3 enemy-square? integer_+ integer_+ 4), :elo 1769.6457773292846} {:genome (6 empty-square? 8 enemy-square? boolean_and true boolean_= boolean_= true 9 exec_dup 5 9 in1 8 boolean_= 3 0 true integer_+ 6 close 6 false 0 my-square? boolean_= exec_dup boolean_and integer_% 9 0 5 6 close integer_* true close 6 exec_if 8 boolean_or integer_- 7 0 my-square? boolean_and boolean_or 1 close 4 5 integer_% boolean_and integer_+), :elo 1769.0223464334397} {:genome (integer_* 0 9 exec_if exec_dup integer_% integer_+ 8 4 5 1 9 true 3 exec_if 9 2 exec_if in1 0 integer_- 5 4 boolean_or 5 close integer_+ integer_* true boolean_= 6 boolean_or integer_% empty-square? boolean_or false true 7 my-square? integer_% integer_* 2 integer_- empty-square? 3 6 enemy-square? 4 boolean_= 0 in1 enemy-square? 0 false 7 6 exec_if exec_dup 8 integer_* integer_* boolean_or enemy-square? integer_% 3 true 5 my-square? boolean_or boolean_= 4 3 enemy-square? integer_+ 7 2 boolean_or boolean_and integer_+ 7 4 7 true 7 exec_dup empty-square? exec_dup in1 6 close integer_+ boolean_= enemy-square? close boolean_or true), :elo 1766.4411074154032} {:genome (8 integer_* integer_* 0 empty-square? exec_if boolean_= integer_+ true integer_+ my-square? 6), :elo 1766.439492729688} {:genome (8 empty-square? 5 exec_if close boolean_or boolean_= integer_- boolean_= 5 close exec_if exec_dup boolean_or exec_dup), :elo 1764.5186756577446} {:genome (false exec_if true integer_- 6 empty-square? close integer_% integer_+ exec_if 5 enemy-square? 8 8 4 false boolean_and boolean_or 9 close integer_- 4 7 true integer_+ 2 exec_if 3 false 8 9 boolean_= 7 exec_if integer_% empty-square? 3 2 empty-square? 9 4 2 5 1 enemy-square? 2 true 1 6 4 9 8 8 3 false boolean_or 1 enemy-square? integer_+ 5), :elo 1757.4738366011604} {:genome (3 integer_* 9 exec_if exec_dup boolean_or exec_if 4 empty-square? 9 1 exec_if 1 true true integer_- enemy-square? boolean_or 5 exec_if enemy-square? 0 integer_% exec_if boolean_= 6 0 boolean_or empty-square? boolean_or true 0 exec_if integer_% integer_* 4 boolean_= empty-square? 4 in1 0 7 exec_dup boolean_or enemy-square? integer_% 3 5 my-square? boolean_= enemy-square? 4 3 enemy-square? 8 7 2 exec_dup 7 4 0 close enemy-square? boolean_or), :elo 1757.4004542080017} {:genome (4 boolean_and false 3 2 boolean_or false 3 6 integer_+ enemy-square? enemy-square? 4 integer_+ integer_% in1 exec_dup true 8 1 exec_if boolean_or exec_if exec_if 1 boolean_or integer_% my-square? boolean_= empty-square? 6 integer_+ my-square? exec_if 3 boolean_or empty-square? 3 integer_* empty-square? 1 7 integer_+ 5 exec_dup 1 close true true close my-square? boolean_= 7 my-square? 6 exec_dup integer_* close exec_dup integer_* exec_if true integer_* boolean_and 5 6 integer_+ integer_+ boolean_or 3 true integer_% in1 boolean_= my-square?), :elo 1755.934162078966} {:genome (enemy-square? boolean_and integer_- 8 boolean_or close integer_* integer_+ in1 close enemy-square? 4 integer_+ integer_% 5 exec_dup true boolean_and 1 exec_if 3 integer_+ integer_% 4 boolean_or 0 integer_+ exec_if integer_* my-square? exec_if my-square? integer_% empty-square? 3 integer_% empty-square? boolean_= 7 5 my-square?), :elo 1753.0044760583148} {:genome (exec_dup integer_+ integer_* 3 boolean_= boolean_and integer_% 7 empty-square? integer_+ integer_- 9 4 7 boolean_or boolean_or 1 4 boolean_and integer_+ 3 4 exec_if 2 5 8 7 boolean_and integer_% 2 my-square? exec_if 1 false my-square? my-square? empty-square? enemy-square? boolean_or true exec_if 2 1 false 1 close 6 integer_+ false 6 3 6 close false my-square? boolean_= in1 boolean_and integer_- 2 exec_dup false), :elo 1748.9849290478899} {:genome (6 boolean_and true integer_- 8 8 5 my-square? integer_+ in1 close in1 4 my-square? empty-square? integer_% 5 false 7 boolean_and 1 exec_if boolean_= integer_+ integer_% 4 my-square? boolean_or empty-square? integer_+ empty-square? exec_if exec_dup my-square? exec_if my-square? integer_% in1 empty-square? 3 1 empty-square? boolean_= 7 2 my-square? false empty-square? integer_* 1 5 false integer_% boolean_and 8 integer_% close enemy-square? 2 2 1 5 7 boolean_= close), :elo 1747.0884750773453} {:genome (1 my-square? in1 boolean_and boolean_or true true exec_if integer_+ my-square? boolean_or in1 8 5 exec_if empty-square? 6 6 integer_+ false exec_dup 9 8 integer_- 4 3 close integer_% my-square? 9 integer_- 4 boolean_= 3 integer_- 8 integer_+ enemy-square? true exec_dup empty-square? 0 boolean_and 8 boolean_= false exec_dup 5 9 boolean_or exec_dup empty-square? integer_% boolean_= boolean_= enemy-square? 9 7 boolean_= boolean_or enemy-square? 8 in1 boolean_and integer_+ boolean_and integer_- 9 my-square? integer_+ true 9 true empty-square? 6 boolean_= 9 my-square? in1 1 8 2), :elo 1746.660616773165} {:genome (false integer_- exec_if integer_+ integer_- exec_dup 6 3 1 close 8 integer_+ integer_- 8 5 enemy-square? 8 5 8 4 false boolean_or 4 true 8 true integer_% enemy-square? close in1 0 boolean_= 1 4 8 true boolean_= enemy-square? 0 exec_if enemy-square? exec_if integer_* in1 integer_% 9 true 8 7 exec_if 7 exec_dup my-square? 4 boolean_or integer_- 9 boolean_and 4 4 0 5 0 enemy-square? true 1 integer_- close 8 false enemy-square? exec_if integer_+), :elo 1742.7992349845672} {:genome (integer_* 0 9 integer_+ exec_dup exec_dup 8 3 1 1 8 exec_if 4 boolean_and 9 2 8 in1 integer_- boolean_and 4 boolean_or 0 close 6 integer_* 1 boolean_= 4 exec_if true boolean_or true integer_% my-square? integer_% in1 2 boolean_= my-square? 3 boolean_and 8 boolean_= 0 in1 enemy-square? 0 false enemy-square? 5 exec_if exec_dup integer_* integer_* boolean_or boolean_or integer_% 3 true empty-square? 7 boolean_= my-square? 3 enemy-square? integer_+ 7 exec_if true true 4 4 7 7 exec_dup in1 6 close boolean_=), :elo 1741.8876379344488} {:genome (false integer_- exec_if true integer_- 6 empty-square? 1 close integer_% integer_+ close exec_if 5 enemy-square? 8 5 8 4 false boolean_and boolean_or 0 false 9 empty-square? true enemy-square? close exec_dup integer_- 3 4 7 true integer_+ 9 2 exec_if exec_if 3 false 8 9 5 in1 9 boolean_= 7 exec_if integer_% empty-square? 3 2 9 empty-square? 9 boolean_and 4 2 5 1 enemy-square? integer_% 2 true 1 integer_- close 6 4 7 9 8 8 8 3 false boolean_or 1 enemy-square? integer_+ 5), :elo 1741.7366434492037} {:genome (7 6 enemy-square? true 6 integer_- 9 boolean_or my-square? integer_+ exec_if in1 0 integer_% 8 1 exec_if in1 6 integer_- boolean_= integer_+ false 1 1 9 boolean_= integer_+ 5 integer_% 4 my-square? 9 close false 9 5 boolean_and integer_% 8 2 0 integer_% 9 integer_* enemy-square? 0 8 exec_dup 7 5 enemy-square? 6 3 in1 integer_* my-square? exec_if boolean_or 3 1 2 5 0 6 9 boolean_or integer_+ enemy-square? exec_dup enemy-square?), :elo 1739.8016257265988} {:genome (5 boolean_= 1 7 enemy-square? 7 exec_if 5 false enemy-square? empty-square? 9 enemy-square? 8 close boolean_or), :elo 1735.9825309545606} {:genome (integer_* 0 9 exec_if exec_dup integer_% integer_+ 8 4 5 1 9 true 3 2 9 integer_- 5 4 boolean_or 5 5 4 boolean_or 5 boolean_= integer_+ integer_* true empty-square? integer_+ boolean_or integer_% true 7 false integer_% 7 my-square? integer_% integer_* 2 6 4 3 0 enemy-square? 4 0 0 in1 enemy-square? exec_if exec_dup integer_* 6 boolean_or enemy-square? integer_% integer_* true boolean_or enemy-square? boolean_= 3 true 4 my-square? enemy-square? boolean_= 4 3 enemy-square? integer_+ integer_+ 7 4 7 7 exec_dup 4 7 true close exec_dup empty-square? boolean_= in1 close close integer_+ enemy-square? close boolean_or), :elo 1735.1375870936624} {:genome (false integer_- close exec_if true integer_- 6 empty-square? 1 my-square? close integer_% integer_+ close exec_if 5 enemy-square? 8 5 8 4 false boolean_and boolean_or 0 false 9 empty-square? true enemy-square? close exec_dup integer_+ integer_- 3 4 7 true integer_+ 9 2 exec_if exec_if 3 false 8 9 5 in1 9 boolean_= 7 exec_if integer_% empty-square? 8 3 2 9 empty-square? 2 9 boolean_and 6 4 2 5 1 enemy-square? integer_% 0 2 true 1 integer_- close 6 4 7 9 8 8 4 8 3 false boolean_or 1 enemy-square? integer_+ 5), :elo 1731.0950410549053} {:genome (3 integer_* boolean_and 1 integer_- boolean_or exec_if 4 empty-square? 0 1 2 1 true true boolean_= enemy-square? my-square? 4 in1 enemy-square? 0 integer_% exec_if enemy-square? 9 integer_% 5 false exec_if 9 0 exec_if 3 empty-square? 8 exec_if), :elo 1726.723639965284} {:genome (4 close 8 boolean_and false 3 2 exec_dup false 3 6 integer_+ enemy-square? integer_% empty-square? integer_* 6 in1 true false 8 true 3 boolean_or exec_if exec_if 1 boolean_and integer_% my-square? 6 boolean_= true integer_% 6 integer_+ empty-square? close enemy-square? 3 boolean_or empty-square? true integer_* 6 1 boolean_or 7 integer_+ 5 1 7 exec_dup in1 false 1 enemy-square? close integer_+ integer_- my-square? 5 true true 8 integer_* 6 close my-square? boolean_= 7 my-square? integer_+ 6 exec_dup integer_* integer_* boolean_= integer_* enemy-square? 8 close 4 exec_dup integer_* exec_if true boolean_= integer_* boolean_and 8 5 boolean_= 6 integer_+ 4 2 in1 integer_+ boolean_or 2 3 2 boolean_or 7 boolean_and true enemy-square? 2 integer_% in1 empty-square? boolean_and close 3 6 boolean_= exec_dup my-square?), :elo 1721.0434566722722} {:genome (7 0 3 exec_dup 2 exec_if 7 7 enemy-square? integer_+ 2 exec_if 0 empty-square? my-square? boolean_and integer_% enemy-square? boolean_and integer_% boolean_or in1 6 enemy-square? 5 5 boolean_= false 5 5 exec_dup integer_* 0 integer_* exec_if 8 my-square? 4 true 1 5 6 integer_+ false integer_% exec_if 1 1 8 integer_* 0 integer_% 0 2 exec_dup 6 integer_% 4 6 true 8 boolean_or boolean_or 8 boolean_= 7 true 3 my-square? 9 1 2 5 3 integer_- 0 boolean_or 8 empty-square? 3), :elo 1712.4412421526868} {:genome (7 0 in1 integer_+ boolean_= exec_dup empty-square? 7 enemy-square? integer_+ true 8 4 0 empty-square? boolean_and integer_- 8 integer_% empty-square? integer_- in1 6 integer_- integer_% boolean_= enemy-square? 8 1 integer_- boolean_or integer_* boolean_= empty-square? 8 close integer_% 4 4 close 9 6 true 8 true my-square? boolean_and empty-square? 8 true 0 boolean_= 0 empty-square? 0 6 enemy-square? 5 integer_* in1 true 1 enemy-square? true 8 9 boolean_or 7 exec_dup exec_dup boolean_= 5 boolean_or 8 exec_if true true 4 0 in1 boolean_and my-square? my-square? exec_if boolean_or 9 5 integer_- 1 integer_- 3), :elo 1706.4367054580023} {:genome (integer_* 0 enemy-square? true exec_dup close empty-square? 7 5 integer_+ 3 in1 0 0 integer_- 4 2 enemy-square? 1 integer_% boolean_= in1 6 integer_- empty-square? boolean_or true false my-square? boolean_and boolean_or integer_* integer_- empty-square? 3 4 close in1 enemy-square? 1 7 6 empty-square? false true integer_* integer_* enemy-square? 8 3 true integer_% my-square? integer_* enemy-square? 4 3 enemy-square? integer_+ 7 true 8 boolean_or integer_+ 0 4 7 integer_* 8 in1 6 5 integer_+ 5 enemy-square? close exec_dup 3 boolean_and my-square? exec_if 9 close 2 3 6 1 false boolean_or integer_+ 8), :elo 1706.1016263969468} {:genome (integer_* 0 9 exec_if 1 integer_% integer_% boolean_or 5 5 3 9 9 3 in1 boolean_= 5 2 integer_- 4 boolean_or integer_* true 1 enemy-square? integer_* boolean_or empty-square? boolean_= 6 true true 7 my-square? boolean_or true integer_+ 7 3 integer_% 4 4 boolean_= 2 integer_- 0 false 4 integer_- boolean_or enemy-square? integer_% 0 5 6 exec_if boolean_and 4 integer_* enemy-square? 7 2 boolean_or 3 true 4 my-square? exec_dup exec_dup in1 4 3 3 boolean_= enemy-square? 8 7 2 exec_dup boolean_or boolean_or 7 4 exec_dup in1 6 close integer_+), :elo 1705.1671612420178} {:genome (integer_+ 8 2 my-square? integer_+ exec_dup 6 3 1 close 8 integer_* integer_- true 5 enemy-square? 8 true 8 4 boolean_or 4 integer_% enemy-square? close 0 1 8 true boolean_= enemy-square? 0 exec_if enemy-square? true 8 7 exec_dup 4 integer_- 5 0 enemy-square? 1 enemy-square?), :elo 1703.7079893215362} {:genome (false integer_- exec_if integer_+ integer_- exec_dup 6 3 1 close 8 4 integer_+ close 8 integer_- 8 5 enemy-square? 8 5 false 8 4 false boolean_and boolean_or 1 boolean_or 4 integer_+ false true 8 true integer_% enemy-square? my-square? close in1 0 boolean_= 1 4 8 true boolean_= enemy-square? 0 close exec_if enemy-square? exec_if integer_* in1 integer_% 9 boolean_or true 8 7 exec_if 7 exec_dup my-square? 4 boolean_or integer_- 9 boolean_and 4 4 0 5 0 boolean_and 4 integer_* enemy-square? true 1 integer_- close false 7 8 false enemy-square? exec_if integer_+), :elo 1702.4008457545735} {:genome (8 integer_* in1 integer_* 0 empty-square? exec_if boolean_= true integer_+ 6), :elo 1691.7893281670056} {:genome (6 7 integer_- exec_dup boolean_and 8 5 my-square? 6 boolean_and 2 in1 boolean_= my-square? 2 boolean_or false 7 1 5 enemy-square? boolean_= exec_if 5 exec_if my-square? boolean_or empty-square? 7 in1 integer_- exec_dup integer_% boolean_= 3 enemy-square? false close 1 in1 1 5 2 7 enemy-square? integer_- exec_if false exec_dup exec_dup empty-square? integer_* 5 exec_dup 5 false integer_% 2 boolean_and 6 boolean_and boolean_= 5 true integer_% 6 exec_if integer_% close 8 6 integer_% integer_* integer_% boolean_or my-square? close integer_- 7 boolean_and enemy-square? 2 exec_if 2 2 7 close enemy-square? 1 exec_if my-square? integer_- 5 integer_- 7 enemy-square? exec_dup boolean_= integer_- close empty-square? integer_% enemy-square? in1 exec_dup close), :elo 1684.2586940761055} {:genome (5 in1 in1 true exec_dup 6 3 9 empty-square? enemy-square? 8 false enemy-square? enemy-square? 7 empty-square? in1 integer_* in1 exec_if 0 8 in1 9 2 false enemy-square? 1 true 4 8 integer_+ boolean_= 8 exec_dup boolean_= true in1 true boolean_or), :elo 1683.2486641109635} {:genome (7 integer_- exec_if true close 6 7 1 close integer_% in1 close integer_- exec_if 5 enemy-square? enemy-square? 5 integer_% 4 false 6 integer_- integer_% false empty-square? false 5 close 6 integer_* boolean_= empty-square? 4 7 true 4 true 9 exec_if 3 false 8 my-square? 5 in1 true exec_if integer_% 0 2 empty-square? integer_- true 6 0 2 8 5 1 0 9 boolean_or 8 enemy-square? boolean_= 5 4 7 9 3 exec_dup 6 3 boolean_or boolean_and my-square? my-square? my-square? exec_if boolean_or 1 in1 3 integer_- boolean_or empty-square?), :elo 1678.7249945690446} {:genome (integer_- my-square? 3 integer_+ boolean_= integer_- empty-square? 3 boolean_= enemy-square? true in1 boolean_= 8 7 integer_% exec_if 7 empty-square? 8 1 0 integer_- boolean_and 1 integer_* integer_* enemy-square? 0 exec_if 5 false false empty-square? 5 in1 8 exec_dup false 4 close my-square? false boolean_= 7 integer_- integer_% 1 exec_dup close 7 exec_dup 6 integer_+ enemy-square? close integer_* 3 integer_% integer_- 9 boolean_or boolean_= integer_* 0 integer_+ 0 close 9 exec_dup integer_- 3 boolean_or exec_dup true integer_+ in1 true empty-square? 0 2 integer_% my-square? integer_+ exec_dup exec_if boolean_and 2 empty-square? exec_dup), :elo 1678.4197897119718} {:genome (integer_* integer_- exec_if true integer_- integer_% 6 empty-square? 1 5 integer_% integer_+ true integer_- exec_if 9 2 8 5 0 4 false 4 boolean_or boolean_or close integer_+ 9 empty-square? true enemy-square? boolean_or 6 empty-square? integer_- false true 4 my-square? integer_% integer_* 2 integer_- exec_if 3 6 false 8 boolean_= 5 in1 9 0 false 7 integer_% exec_if exec_dup 2 9 integer_* integer_- 9 boolean_and 4 2 5 my-square? boolean_or boolean_= enemy-square? integer_% 2 integer_+ 7 2 close boolean_and 4 7 4 7 true 8 3 false exec_dup in1 enemy-square? exec_if integer_+ boolean_=), :elo 1677.0120871852832} {:genome (false integer_- exec_if integer_+ integer_- exec_dup 6 3 1 close 8 4 close 8 integer_- 8 5 enemy-square? 8 5 false 8 4 false boolean_and boolean_or 1 boolean_or 4 integer_+ false true 8 true integer_% enemy-square? my-square? 0 boolean_= 1 4 8 true boolean_= enemy-square? 0 exec_if enemy-square? exec_if integer_* in1 integer_% 9 boolean_or true 8 7 exec_if 7 exec_dup my-square? 4 boolean_or integer_- 9 boolean_and 4 4 0 0 boolean_and 4 integer_* enemy-square? true 1 integer_- close false 7 8 false enemy-square? integer_+), :elo 1676.8842310999412} {:genome (integer_% 5 in1 integer_+ boolean_= exec_dup integer_+ 3 1 integer_+ boolean_= 4 integer_% boolean_and exec_if integer_- 8 integer_% empty-square? integer_- boolean_and 4 close 0 integer_* 6 8 1 integer_- true 4 exec_if true 8 integer_% 8 4 close in1 0 true 8 boolean_= 3 boolean_and 8 3 7 boolean_= 8 empty-square? 0 exec_if 5 integer_* in1 integer_% boolean_or true 8 integer_+ empty-square? 7 exec_dup exec_dup my-square? 4 boolean_or 8 exec_if true true 4 0), :elo 1669.8749314911056} {:genome (false 0 true integer_- 6 empty-square? empty-square? 7 enemy-square? integer_+ true in1 0 0 empty-square? 4 false enemy-square? boolean_or 9 boolean_= in1 exec_dup integer_- integer_% boolean_= enemy-square? true integer_+ 9 2 integer_* 3 empty-square? 5 9 close 4 exec_if integer_% 9 6 2 false 9 4 integer_* enemy-square? 8 enemy-square? 2 true 0 integer_* 4 7 true true 6 3 true boolean_or enemy-square? enemy-square? integer_+ 9 boolean_or integer_* 8 enemy-square? 5 7 true 3 6 3 my-square? my-square? my-square? exec_if boolean_or 9 close 1 3 0 false boolean_or integer_+ empty-square? integer_- 3), :elo 1669.2344499065052} {:genome (integer_* 0 9 exec_if exec_dup integer_% my-square? 4 5 9 3 exec_if 9 2 integer_- 5 4 boolean_or 5 integer_+ 1 integer_* true empty-square? boolean_= boolean_or true boolean_or empty-square? boolean_or true 7 integer_% integer_* 2 integer_- empty-square? 3 6 4 1 0 enemy-square? 2 0 false 7 exec_dup integer_* integer_* boolean_or enemy-square? integer_% 3 true integer_% 5 my-square? boolean_= enemy-square? 4 3 integer_+ 7 2 7 boolean_or in1 boolean_or integer_+ 7 4 7 exec_dup exec_dup in1 close integer_+ boolean_= enemy-square? close boolean_or), :elo 2053.336910158867}))


(comment

  ;; No arguments
  (main)

  ;; Use args as a map
  (main {:selection :lexicase}))
  
  
