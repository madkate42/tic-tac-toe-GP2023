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
   :boolean (list true false)
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

(def example-execdorange-state
  {:exec '(exec_do*range exec_if integer_+)
   :integer '(5 1)
   :string '("abc" "def")
   :boolean (list true false)
   :input {:in1 4 :in2 6}})

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
   :elo 1000})
  

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
   'integer_=
   'integer_>
   'integer_< 
   'boolean_=
   'boolean_and
   'boolean_or
   'boolean_not
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
   (if (and (>= position 0) (< position 9))
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

(defn calculate-elo-pop-range 
  [population]
  (let [sorted-pop (reverse (sort-by :elo population))]
    {:elo-pop-range (- (:elo (first sorted-pop)) (:elo (last sorted-pop)))
     :minimum (:elo (last sorted-pop))
     :maximum (:elo (first sorted-pop))}))

(defn filter-by-elo-range
  [population minimum maximum]
  (filter #(and (>= (:elo %) minimum) (< (:elo %) maximum)) population))

(defn adjust-elo-scores-to-target
  [individuals target-average]
  (let [current-total (reduce + (map :elo individuals))
        num-individuals (count individuals)
        current-average (/ current-total num-individuals)
        adjustment (- target-average current-average)]
    (map (fn [individual]
           (update individual :elo #(+ % adjustment)))
         individuals)))




; Stack utilities

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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defn boolean_not
  [state]
  (let [not-f (fn [a]
                (not a))]
   (make-push-instruction state not-f [:boolean] :boolean)))


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

(defn check-win?
  "Check for its own potential win???"
  [state]
  :STUB
  )

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

(defn exec_do*range
  "idkekkk...
   An iteration instruction that executes the top item on the EXEC stack a number of times that depends on the top two integers, while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop. This is similar to CODE.DO*COUNT except that it takes its code argument from the EXEC stack. The top integer is the 'destination index' and the second integer is the 'current index.' First the code and the integer arguments are saved locally and popped. Then the integers are compared. If the integers are equal then the current index is pushed onto the INTEGER stack and the code (which is the 'body' of the loop) is pushed onto the EXEC stack for subsequent execution. If the integers are not equal then the current index will still be pushed onto the INTEGER stack but two items will be pushed onto the EXEC stack -- first a recursive call to EXEC.DO*RANGE (with the same code and destination index, but with a current index that has been either incremented or decremented by 1 to be closer to the destination index) and then the body code. Note that the range is inclusive of both endpoints; a call with integer arguments 3 and 5 will cause its body to be executed 3 times, with the loop counter having the values 3, 4, and 5. Note also that one can specify a loop that 'counts down' by providing a destination index that is less than the specified current index."
  [state])
  

(defn exec_do*range
  [state]
  (if (and
       (not (empty-stack? state :integer))
       (not (empty-stack? state :exec))
       (> (count (state :integer)) 1))
    (let [top-integer (peek-stack state :integer)
          second-integer (peek-stack (pop-stack state :integer) :integer)
          new-state (pop-stack (pop-stack state :integer) :integer)
          top-exec (peek-stack state :exec)
          new-state (pop-stack new-state :exec)
          next-index (if (> second-integer top-integer) (dec second-integer) (inc second-integer))
          updated-exec (if (= second-integer top-integer)
                         '(top-exec)
                         (list 'exec_do*range next-index top-integer 'top-exec))]
      (-> new-state
          (update :integer #(cons next-index %))
          (update :exec #(cons updated-exec %))))
    state))

;; (interpret-one-step example-execdorange-state)
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
  or limit (100) over.")
  
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
    ;; (print-board new-board)
    ;; (println individ)
    (if (valid-move? board position)
      (do 
        ;; (print-board (add-move-to-board board position))
        (add-move-to-board board position))
      false)))

(defn play-game 
  [board current other moves-played]
  (let [new-board (make-move current board)] 
    (cond
      (false? new-board) ;; Current player failed to make a valid move, other player wins
      {:winner other :loser current :draw false :win false}

      (check-win new-board) ;; Current player wins
      (do 
        (println "We have a win, individuals:"
                 current 
                 other)
        {:winner current :loser other :draw false :win true})

      ;; Check for draw
      (not-any? zero? new-board)
      {:winner nil :loser nil :draw true :win false}

      :else ;; Continue the game, switch players
      (recur (inverse-board new-board) other current (inc moves-played)))))

(defn compete-helper
  "Takes two individuals and they compete, returning them with updated elo-scores."
  [player1 player2]
  (let [{:keys [winner loser draw win moves-played]} (play-game empty-board player1 player2 0)
        result (cond
                 draw 0
                 (= winner player1) 1
                 :else 2)
        [new-rating1 new-rating2] (adjust-elo (:elo player1) (:elo player2) result)
        ; 50 is a weird number. EXPERIMENT.
        winner-rating1 (if (and (= winner player1) win) (+ new-rating1 50) new-rating1)
        winner-rating2 (if (and (= winner player2) win) (+ new-rating2 50) new-rating2)
        updated-player1 (update-individ-elo player1 winner-rating1)
        updated-player2 (update-individ-elo player2 winner-rating2)]
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
        (recur (compete-all (first remaining) remaining) (conj new-pop (first remaining)))))))
    

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



(defn divided-tournament 
  "Splits into n ranges and round robin inside them"
  [population n]
  (let [{:keys [elo-pop-range minimum maximum]} (calculate-elo-pop-range population) 
        margin (/ elo-pop-range n)]
    (loop [new-pop '[]
           current-start-elo minimum
           index 0]
      (if (= index (+ 1 n))
        (do 
          ;; (println "Current new-pop count" (count new-pop))
          new-pop)
        (do 
          ;; (println "Divided Tournament " index new-pop)
          ;; (println current-start-elo margin)
          (recur (flatten (conj new-pop
                              (round-robin
                               (filter-by-elo-range population
                                                    current-start-elo
                                                    (+ current-start-elo margin)))))
               (+ current-start-elo margin)
               (inc index)))))))


(defn evaluate-population 
  "On gen 0 always round-robin. 
   Generation start with 1. Frequency is in how many generation round-robin is conducted
   For example, frquencey 50 means it's conducted at 0, 50, 100, etc."
  [population frequency current-gen stratas]
  (if (= (rem current-gen frequency) 0)
    (round-robin population)
    ; divide into ranges, conduct round robin on those ranges. 
    (divided-tournament population stratas))) ; this value can be changed
  

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


(defn mutate-population 
  "Population with evaluated elos should be passed"
  [population instructions]
  (loop [new-pop '()]
    (if (not= (count new-pop) (count population)) 
      (recur (conj new-pop (select-and-vary-elo-inherit population instructions)))
      new-pop)))





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
  [{:keys [population-size max-generations instructions max-initial-plushy-size round-robin-frequency stratas]
    :as argmap}]
  (loop [population (initialize-population population-size max-initial-plushy-size) ; start pop
         generation 0]
    (if (< generation max-generations)
      (do
        (println population)
        (let [sorted-population (reverse (sort-by :elo (evaluate-population population
                                                                            round-robin-frequency
                                                                            generation
                                                                            stratas))) ; sort pop from best to worst
              new-pop (adjust-elo-scores-to-target (mutate-population sorted-population instructions) 1000)] ; make a new child 
          (println "Best ones:" (first sorted-population) (nth sorted-population 1))
          (println "Highest elo: "(:elo (first sorted-population)) ", worst elo:" (:elo (last sorted-population)))
          (println "-----------------")
          (println "Generation #" generation)
          (println "-----------------")
          (recur new-pop
                 (inc generation))))
      population)))


(defn main
  "Runs push-gp, giving it a map of arguments."
  ([] (main {}))
  ([args]
   (tic-tac-toe-push-gp {:instructions default-instructions
                         :max-generations 10
                         :population-size 10
                         :max-initial-plushy-size 100
                         :round-robin-frequency 5
                         :stratas 5})))


(def long-run2 (main))
(def short-run2 (main))


;; (defn sum-elo [my-list]
;;   (reduce #(+ %1 (get %2 :elo 0)) 0 my-list))

;; (def hello (main))

;; (sum-elo hello)

;; (:elo (first (sort-by :elo hello)))

;; (compete (first (reverse (sort-by :elo hello))) (last (reverse (sort-by :elo hello))))

;; (def advanced (main))
;; (compete (first (reverse (sort-by :elo advanced))) (last (reverse (sort-by :elo advanced))))

;; (def today-long-run (main))

(compete (first (reverse (sort-by :elo long-run2))) (second (reverse (sort-by :elo long-run2))))
(def inds '({:genome (exec_dup my-square? integer_% 4 7 5 exec_if exec_if 5 boolean_and boolean_and exec_if 1 1 6 6 2 4 boolean_not 2 0 2 exec_if 5 boolean_= boolean_= my-square? exec_if exec_if 8 3 3 exec_if 6 exec_if 5 4 2 enemy-square? boolean_not 2 4 enemy-square? in1 integer_% integer_+ 7 8 5 8 8 integer_% exec_if 4 my-square? integer_= in1 boolean_and close integer_+ exec_dup integer_% boolean_or boolean_not exec_dup integer_< 3 boolean_= integer_* my-square? integer_> my-square? integer_- 9 integer_% true true integer_+ 4 integer_= boolean_and false exec_dup integer_- boolean_or boolean_or integer_< exec_dup integer_+ 8 5 6 4 6 integer_% integer_+ integer_> 9 close 4 empty-square? 1 0 7 integer_> true exec_dup 8 7 4 integer_+ 8 integer_* integer_+ boolean_= integer_< integer_< integer_* integer_+ 7 exec_dup boolean_not true integer_< 6 false close 4 integer_> enemy-square? boolean_= 6 6 5 boolean_or 8 5 0 boolean_not 7 6 2 in1 exec_dup integer_> 9 6 9 integer_- 9 boolean_not integer_< 5 in1 7), :elo 38170.14312884259} {:genome (exec_dup my-square? boolean_and 1 4 3 exec_if 3 true exec_dup integer_% 2 boolean_or exec_if integer_% exec_if my-square? empty-square? exec_if 8 1 exec_if true 0 integer_* false 2 enemy-square? 2 my-square? boolean_not true integer_- in1 8 integer_- my-square? 2 exec_if 6 boolean_or boolean_or integer_- exec_if 4 exec_if 7 2 3 5 4 3 2 boolean_and exec_if 6 enemy-square? 2 enemy-square? boolean_and boolean_and 5 enemy-square? boolean_or 2 2 9 7 integer_> 4 integer_+ integer_* integer_+ close integer_+ 6 my-square? boolean_or my-square? true close boolean_or integer_> empty-square? integer_> 6 8 boolean_= true 4 true integer_- exec_if integer_< integer_- integer_> 9 integer_> 0 integer_* integer_% exec_if exec_if enemy-square? 5 boolean_or 6 enemy-square? integer_= false 5 1 3 8 exec_if 0 integer_< boolean_= integer_= 5 enemy-square? boolean_not 9 integer_% empty-square? empty-square? 6 true 8 integer_= 3 enemy-square? false exec_dup close boolean_not close 9 6 in1 boolean_or exec_dup integer_+ 6 boolean_= 5 integer_< close 2 1 8 exec_if boolean_= 2 integer_< 1 boolean_or boolean_= integer_> false enemy-square? integer_< 8 8 boolean_and exec_dup integer_< close close 8 8 integer_> 8 integer_< integer_= boolean_or integer_= true 5 8 0 boolean_or 9 empty-square? integer_- 4 9 integer_> 2 boolean_= integer_% boolean_not boolean_= close boolean_not boolean_or empty-square?), :elo 38314.21094425386}))

(compete (first inds) (second inds))
;; (def today-long-run2 (main))

(defn print-pop 
  [pop]
  (loop [pops pop
         index 0]
    (if (not (empty? pops))
     (do 
       (println (:elo (first pops)) "with index" index)
       (recur (rest pops) (inc index)))
     "This is populaition.")))


(comment

  ;; No arguments
  (main)

  ;; Use args as a map
  (main {:selection :lexicase}))
  
  