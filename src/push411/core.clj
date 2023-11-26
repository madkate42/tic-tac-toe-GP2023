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
   'close
   'empty-square?
   'my-square?
   'enemy-square?
   0 1 2 3 4 5 6 7 8 9 10
   true 
   false))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1})

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
    (some #(= [1 1 1] %) lines)))

(if (check-win [1 1 1 0 0 0 0 0 0])
  true 
  false)

;; elo score utilities

(defn adjust-elo
  "d is either 1 if rating1 wins, 
   2 if rating2 wins
   0 if draw."
  [rating1 rating2 d]
  ;; (println rating1 rating2 d))
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
    [new-rating1 new-rating2]))

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

(defn exec_if 
  [state]
  state) ; it's simple

(defn check-board-position 
  [board position]
  (if (= (type position) Long)
    (if (and (>= position 0) (< position 9))
      (nth board position)
      false)
    false))


;; (defn empty-square?
;;   "Pop the integer, checks if board at that integer is empty,
;;    pushes true if empty, false if not empty.
;;    What happens if the value is not 1-9??"
;;   [state]
;;   (if (empty-stack? state :integer)
;;     (do (println "Hello, stack empty")
;;         false)
;;     (let [board (state :board)
;;         position (peek-stack state :integer)
;;         new-state (pop-stack state :integer)]
;;     (if (or (not= 0 (nth board position)) (> position 8))
;;       (push-to-stack new-state :boolean false) 
;;       (push-to-stack new-state :boolean true)))))

(defn empty-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   What happens if the value is not 1-9??"
  [state] 
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))] 
    (if (not what-on-board)
      (if (= what-on-board 0) 
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))

(pop-stack empty-push-state :integer)

;; (defn enemy-square?
;;   "Pop the integer, checks if board at that integer is empty,
;;    pushes true if empty, false if not empty.
;;    What happens if the value is not 1-9??"
;;   [state]
;;   (if (empty-stack? state :integer)
;;     false
;;    (let [board (state :board)
;;         position (peek-stack state :integer)
;;         new-state (pop-stack state :integer)]
;;     (if (and (= (nth board position) 2) (not (> position 8)))
;;       (push-to-stack new-state :boolean true)
;;       (push-to-stack new-state :boolean false)))))

(defn enemy-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   What happens if the value is not 1-9??"
  [state]
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))]
    (if (not what-on-board)
      (if (= what-on-board 2)
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))

;; (defn my-square?
;;   "Pop the integer, checks if board at that integer is empty,
;;    pushes true if empty, false if not empty.
;;    What happens if the value is not 1-9??"
;;   [state]
;;   (if (empty-stack? state :integer)
;;     false
;;     (let [board (state :board)
;;           position (peek-stack state :integer)
;;           new-state (pop-stack state :integer)]
;;       (if (and (= (nth board position) 1) (not (> position 8)))
;;         (push-to-stack new-state :boolean true)
;;         (push-to-stack new-state :boolean false)))))

(defn my-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   What happens if the value is not 1-9??"
  [state]
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))]
    (if (not what-on-board)
      (if (= what-on-board 1)
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))


(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

;; (defn exec-if 
;;   [state]
;;   (if (< (count (state :exec)) 3)
;;     state
;;     false))

;; (exec-if example-push-state)


;;;;;;;;;
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

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (let [competing (take 7 (shuffle population))]
    (first (sort-by :total-error competing))))


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
    (if (valid-move? board position)
      (add-move-to-board board position)
      false)))

(defn play-game 
  [board current other]
  (let [new-board (make-move current board)] 
    (cond
      (false? new-board) ;; Current player failed to make a valid move, other player wins
      {:winner other :loser current :draw false}

      (check-win new-board) ;; Current player wins
      {:winner current :loser other :draw false}

      ;; Check for draw
      (not-any? zero? new-board)
      {:winner nil :loser nil :draw true}

      :else ;; Continue the game, switch players
      (recur (inverse-board new-board) other current))))

(defn compete-helper
  "Takes two individuals and they compete, returning them with updated elo-scores."
  [player1 player2]
    (let [{:keys [winner loser draw]} (play-game empty-board player1 player2)
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


(defn round-robin 
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


;;;;;;;;;;
;; The error function
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ 3 (+ x (* (* x x) x))))

;; these are the training cases that I use. There can be different ones.
(def training-cases '(0 1 2 3 5 10 15 1253))
;; (def training-cases '(-25 -13 1 17 29 57 111 167))
;; (def training-cases (range -50 50))


(comment
  "Regression-error-function
   
   First, translates genome to push-program.
   Loops through all test cases carrying errors list with the loop. 
   Within an iteration:
     if all test cases have been through, return a new individual by attaching
     errors list to the individual.
     else, go to the next test-case.")

(defn regression-error-function
  "Takes an individual and evaluates it on some training cases.
    This will need to translate each individual's Plushy genome into a Push
    program before executing the Push program (see translate-plushy-to-push).
    For each training case,
    runs program with the input set to :in1 in the :input map part of the Push state.
    Then, the output is the integer on top of the integer stack in the Push state
    returned by the interpreter. Computes each error by comparing output of
    the program to the correct output.
    Returns the individual with :errors set to the list of errors on each case,
    and :total-error set to the sum of the errors. You may also want to set
    :program to be the Push program translated from the Plushy genome, though
    this isn't mandatory.
    Note: You must consider what to do if the program doesn't leave anything
    on the integer stack."
  [individual]
  (let [push-program (translate-plushy-to-push (individual :genome)) ; interpet genome
        new-individual individual
        cases-total (- (count training-cases) 1)]
    (loop [index 0
           errors []] ; initialize errors list
      (if (= index cases-total)
        (conj
         (conj
          (conj (dissoc new-individual :errors) (hash-map :errors errors)) ; add errors to individ
          (hash-map :total-error (apply + errors))) ; add total-error (sum of all errors)
         (hash-map :program push-program)) ; also add program to individ
        (recur (inc index)
               (conj
                errors
                (let [test-case (nth training-cases index) ; next case
                      start-state (conj empty-push-state ; add input to new start-state
                                        (hash-map :input (hash-map :in1 test-case)))
                      result-state (interpret-push-program push-program start-state) ; get new result
                      result (peek-stack result-state :integer)] ; see the result in the stack
                  (if (= result :no-stack-item)
                    100000 ; arbitrary big error if integer stack is empty
                    (abs (- (target-function test-case) result)))))))))) ; calculate error


; my preferred config to call.
;; (push-gp {:instructions default-instructions
;;           :error-function regression-error-function
;;           :max-generations 300
;;           :population-size 120
;;           :max-initial-plushy-size 50})

;;;;;;;;;;
;; The main function call
;; You can call this in a REPL, or alternatively from the command line
;; by running:
;;   clj -X push411.core/main
;; Additionally, if you want to pass command line arguments as a map to args,
;; you can run something like:
;;   clj -X push411.core/main :selection :lexicase

(defn main
  "Runs push-gp, giving it a map of arguments."
  ([] (main {}))
  ([args]
   (push-gp {:instructions default-instructions
             :error-function regression-error-function
             :max-generations 500
             :population-size 200
             :max-initial-plushy-size 100})))

(comment

  ;; No arguments
  (main)

  ;; Use args as a map
  (main {:selection :lexicase}))
  
  
