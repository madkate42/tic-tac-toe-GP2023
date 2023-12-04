(ns push411.core)

(comment
  "Kate Bondarenko")


;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Puts a 1 in the position [0-8] on the board"
  [board position]
  (assoc board position 1))

(defn valid-move? 
  "Checks whether the move is valid on the board
   at position [0-8]"
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
  "Prints a vector representing a 3x3 grid in a formatted way
   Utility to see the games!"
  [board]
  (println "_________")
  (doseq [row (partition 3 board)]
    (println "|" (clojure.string/join " " row) "|"))
  (println "---------"))

; https://clojuredocs.org/clojure.core/some
; https://clojuredocs.org/clojure.core/subvec
;https://cljs.github.io/api/cljs.core/mapv
(defn check-win
  "Takes a vector representing a 3x3 grid and checks for a win 
   (three 1s in a row, column, or diagonal)"
  [board]
  (let [rows [(subvec board 0 3) (subvec board 3 6) (subvec board 6 9)]
        cols [(mapv nth rows [0 0 0]) (mapv nth rows [1 1 1]) (mapv nth rows [2 2 2])]
        diags [(mapv nth rows [0 1 2]) (mapv nth rows [2 1 0])]
        lines (concat rows cols diags)]
    (some #(= [1 1 1] %) lines)))

(defn check-board-position
  "Checks what's on the board. 
   Returns the value on the board at the position [0-8],
   returns false if the position is invalid (not in the range)"
  [board position]
  (if (= (type position) Long)
    (if (and (>= position 0) (< position 9))
      (nth board position)
      false)
    false))

(defn print-pop
  "Prints population.
   Utility for experiments!"
  [pop]
  (loop [pops pop
         index 0]
    (if (not (empty? pops))
      (do
        (println (:elo (first pops)) "with index" index)
        (recur (rest pops) (inc index)))
      "This is populaition.")))


;; Elo score utilities | Elo utilities

(defn adjust-elo
  "Takes two ratings - rating1 and rating2 and a value d:
   d is 1 if rating1 wins, 
   d is 2 if rating2 wins
   0 if draw.
   Returns updated ratings based on Elo-scoring system with k = 32 *chosen as a classic*"
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
  "Changes the elo score in a player dictionary to new-elo"
  [player new-elo]
  (conj player (hash-map :elo new-elo)))

(defn calculate-elo-pop-range 
  "Takes a population and calculates the range of elo scores. Returns the range itself
   for information, minimum and maximum elos in the population. 
   Used for dividing population for mini tournaments."
  [population]
  (let [sorted-pop (reverse (sort-by :elo population))]
    {:elo-pop-range (- (:elo (first sorted-pop)) (:elo (last sorted-pop)))
     :minimum (:elo (last sorted-pop))
     :maximum (:elo (first sorted-pop))}))

(defn filter-by-elo-range
  "Takes a population and two elo values and returns 
   the subset of the populations whose elo vales are in the range."
  [population minimum maximum]
  (filter #(and (>= (:elo %) minimum) (< (:elo %) maximum)) population))

(defn adjust-elo-scores-to-target
  "Balances the average elo score of the population. 
   The elo scores tend to get bloated due to mutation and inheritance, this 
   function brings them back to earth to target-average elo score.
   Returns the population with updated *balanced* elo-scores."
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
  "Checks if two top integers are equal and pushes true if they are equal,
   and pushed false to boolean stack if they are not equal"
  [state]
  (make-push-instruction state = [:integer :integer] :boolean))

(defn integer_>
  "Checks if top integer is greater than the second integer
  and pushes true to boolean stack if that's the case, 
  and pushed false to boolean stack if not"
  [state]
  (let [greater (fn
                  [num1 num2]
                  (if (> num1 num2)
                    true
                    false))]
   (make-push-instruction state greater [:integer :integer] :boolean)))



(defn integer_<
  "Checks if top integer is less than the second integer
    and pushes true to boolean stack if that's the case, 
    and pushed false to boolean stack if not"
  [state]
  (let [less (fn
                  [num1 num2]
                  (if (>= num1 num2)
                    false
                    true))]
    (make-push-instruction state less [:integer :integer] :boolean)))

(defn boolean_=
  "Checks if top two boolean values are equal, pushed true if they are
   equal, pushed false if they are not"
  [state]
  (make-push-instruction state = [:boolean :boolean] :boolean))
 
(defn boolean_and
  "Operaiton 'and' on the top two boolean values."
  [state]
  (let [and-f (fn [a b]
                (if (and a b)
                  true
                  false))]
   (make-push-instruction state and-f [:boolean :boolean] :boolean)))

(defn boolean_or
  "Operaiton 'or' on the top two boolean values."
  [state]
  (make-push-instruction state 'or [:boolean :boolean] :boolean))

(defn boolean_not
  "Operaiton 'not' on the top two boolean values."
  [state]
  (let [not-f (fn [a]
                (not a))]
   (make-push-instruction state not-f [:boolean] :boolean)))


(defn empty-square?
  "Pop the integer, checks if board at that integer is empty,
   pushes true if empty, false if not empty.
   Pushes false if the value on integer stack is an invalid position"
  [state] 
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))] 
    ;; (println what-on-board)
    (if (not= what-on-board false)
       (if (= what-on-board 0) 
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))


(defn enemy-square?
  "Pop the integer, checks if board at that integer is enemy (2),
   pushes true if enemy, false otherwise
   Pushes false if the value on integer stack is an invalid position"
  [state]
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))]
    (if (not= what-on-board false)
      (if (= what-on-board 2)
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))

(defn my-square?
  "Pop the integer, checks if board at that integer is mine (1),
   pushes true if mine, false otherwise
   Pushes false if the value on integer stack is an invalid position"
  [state]
  (let [what-on-board (check-board-position (state :board) (peek-stack state :integer))]
    (if (not= what-on-board false)
      (if (= what-on-board 1)
        (push-to-stack (pop-stack state :integer) :boolean true)
        (push-to-stack (pop-stack state :integer) :boolean false))
      (push-to-stack (pop-stack state :integer) :boolean false))))

(defn exec_dup
  "Copies the top exec item and pushes on top of exec stack"
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

;; https://faculty.hampshire.edu/lspector/push3-description.html#Type
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


(def default-instructions
  (list
   #'in1
   #'integer_+
   #'integer_-
   #'integer_*
   #'integer_%
   #'integer_=
   #'integer_>
   #'integer_<
   #'boolean_=
   #'boolean_and
   #'boolean_or
   #'boolean_not
   #'exec_dup
   #'exec_if
   'close
   #'empty-square?
   #'my-square?
   #'enemy-square?
   0 1 2 3 4 5 6 7 8 9
   true
   false))

(def opened-blocks
  {'exec_dup 1
   'exec_if 2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  tournament size.
   In this case, 7 (chosen arbitrarily) are selected at random from the population,
   and the biggest elo score owner is returned to be a parent."
  [population]
  (let [competing (take 7 (shuffle population))]
    (first (reverse (sort-by :elo competing)))))

(defn make-move
  "Takes an individual and the current board (where 1's are ally, and 2's are enemy's).
   The individual's program is run and the top of the integer stack is checked. 
   If it's a valid position, the move is made on the board symboled as a '1',
   if it's an invalid position, return false."
  [individ board]
  (let [start-state (conj empty-push-state (hash-map :board board))
        program (translate-plushy-to-push (:genome individ))
        return-state (interpret-push-program program start-state)
        position (peek-stack return-state :integer)]
    (if (valid-move? board position)
      (do
        ; Uncomment below to see the game progress!
        ;; (print-board (add-move-to-board board position)) 
        (add-move-to-board board position))
      false)))

(defn play-game 
  "Takes a board and two players. Players take turns to make a move, 
   when a new move is made, the function checks whether 
   1) It was an invalid move, then return the player who didn't 
   go as a loser, the other player as a winner. Not draw.
   2) It was a win, then return the winner and the loser, and set parameter
   :win to true to reward the player with some extra points!
   3) The board is filled and no winner, then no one is a winner or a loser,
   set parameter :draw to true.
   Keep making turns between the players until one of the conditions is met!"
  [board current other]
  (let [new-board (make-move current board)] 
    (cond
      (false? new-board) ;; Current player failed to make a valid move, other player wins
      {:winner other :loser current :draw false :win false}

      (check-win new-board) ;; Current player wins
      (do 
        ; This is an important moment, so we want to see it in the console:)
        (println "We have a win! Individuals:"
                 current 
                 other)
        {:winner current :loser other :draw false :win true})

      ;; Check for draw
      (not-any? zero? new-board)
      {:winner nil :loser nil :draw true :win false}

      :else ;; Continue the game, switch players
      (recur (inverse-board new-board) other current))))

(defn compete-helper
  "Takes two individuals and they compete, returning them with updated elo-scores.
   The player who actually won a game is rewarded with 50 points."
  [player1 player2]
  (let [{:keys [winner loser draw win]} (play-game empty-board player1 player2)
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
  "Takes two players that are going to compete. 
   Choses the starting player at random and uses a helper function to conduct
   the game.
   Returns the individuals in the same order but with updated elo scores."
  [player1 player2]
  (let [chance (rand-int 2)]
    (if (= chance 0) ; first starts first
      (compete-helper player1 player2)
      (reverse (compete-helper player2 player1)))))


(defn compete-all
  "Takes an individual and the population.
   The individual competes with all others in the population. 
   The individual and population are updated after each game.
   Returns the population with new scores after this tournament."
  [individual population]
  (loop [individual individual
         ; the individual is actually the first in the population, since
         ; round-robin uses 'compete-all' by calling on the first individual in a pop
         remaining (rest population) 
         updated-population []]
    (if (empty? remaining)
      (concat [individual] updated-population)
      (let [[updated-individual updated-competitor] (compete individual (first remaining))]
        (recur updated-individual (rest remaining) (conj updated-population updated-competitor))))))

(defn round-robin
  "Takes a population and conducts a round-robin. Returns the population with new elo
   scores. 
   In particular, applies compete-all to each individual in the population. 
   After each individual competes against the entire population, they are
   added to a new population list."
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
  "Takes the population and n number of 'stratas' or number of competitions that are going
   to be conducted. Each competition has an eligible range of elo scores. All individuals 
   within that range have a round-robin in the league. 
   The overall range of elo scores is divided by n (competitions) and individuals in that range
   are chosen to participate. 
   For example, if the range is 500-1500 and n = 4, there will be 4 
   competitions: 500-750, 750-1000, 1000-1250, 1250-1500. 
   Returns the population with updated scores."
  [population n]
  (let [{:keys [elo-pop-range minimum maximum]} (calculate-elo-pop-range population)
        margin (/ elo-pop-range n)]
    (loop [new-pop '[]
           current-start-elo minimum
           index 0]
      (if (= index (+ 1 n))
        new-pop
        (recur (flatten (conj new-pop
                              (round-robin
                               (filter-by-elo-range population
                                                    current-start-elo
                                                    (+ current-start-elo margin)))))
               (+ current-start-elo margin)
               (inc index))))))


(defn evaluate-population 
  "Takes 
   1) Population 
   2) Frequence - in how many generations round-robin is conducted.
      For example, frequency 25 means that round-robin will be conducted on 
      generations 0, 25, 50, 75..
   3) Current-gen: current generation index
   4) Stratas - how many sub tournaments are going to happen, or in how many 
      parts the elo score range of the population is going to be divided. 
   
   On generation 0 always conducts a round-robin first.
   Returns the population with updated elo-scores."
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

(defn select-and-vary-elo-inherit
  "Takes population and list of instructions to be mutated from.
   Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion.
   The elo scores are passed down to children. In case of crossover, 
   the average elo of the two parents is passed down to the baby.

   Returns a mutated baby program!"
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
  "Takes a population and a list of instructions.
   This function is applied to the whole population - to mutate everyone!
   The select-and-vary-elo-inherit function is applied to each individual with 
   the basis of parents from previous generation. 
   Returns a new fresh mutated population."
  [population instructions]
  (loop [new-pop '()]
    (if (not= (count new-pop) (count population)) 
      (recur (conj new-pop (select-and-vary-elo-inherit population instructions)))
      new-pop)))



(defn print-individual
  [individual]
  (println "Elo:" (:elo individual))
  (println "Genome:"
           (clojure.string/join " "
                     (map (fn [x]
                            (if (fn? x)
                              (-> x meta :name str (clojure.string/replace "#'push411.core/" ""))
                              (clojure.string/replace x "#'push411.core/" "")))
                          (:genome individual)))))
(defn report
  "Reports information on the population each generation.
   Takes a sorted (!!!) population!"
  [sorted-population generation] ;; since I pass sorted pop from GP, I don't sort here
  (println "      BEST ONES:")
  (print-individual (first sorted-population)) 
  (println)
  (print-individual (nth sorted-population 1))
  (println "Highest elo: " (:elo (first sorted-population)) ", worst elo:" (:elo (last sorted-population)))
  (println "-----------------")
  (println "Generation #" generation)
  (println "-----------------"))

(defn initialize-population
  "Initializes randomly a population of size 'population-size' 
   and the genomes can be at most 'max-initial-plushy-size'.
   Returns a new random population."
  [population-size max-initial-plushy-size]
  (repeatedly population-size
              #(hash-map :genome 
                         (make-random-plushy-genome default-instructions 
                                                    max-initial-plushy-size),
                         :elo 1000)))

(defn tic-tac-toe-push-gp
  "This is the system tic-tac-toe-push-gp. 
   Parameters:
   1) population-size: the size of the population
   2) max-generations: how many generations are going to be developed 
      (in our case, we will never have THE solution, so we keep going forever)
   3) instruction: set of default instructions that genomes will use
   4) max-initial-plushy-size: the size of genomes that programs will start with
   5) round-robin-frequency: how often round-robin tournament is conducted on
      the population. 
      For example, frequency of 10 means that round-robin will hapen on generations 
      0, 10, 20, 30..
   6) stratas: how many sub tournaments there are at each generation. The population is 
      split into 'stratas' amount of groups and round-robin is conducted within each one 
      of them.
   
   The loop:
    - Conduct tournament, either divided or round-robin depending on the generation
    - Mutate each individual 
    - Adjust elo scores since they get bloated and we want to maintain a comprehensible average"
  [{:keys [population-size max-generations instructions max-initial-plushy-size round-robin-frequency stratas]
    :as argmap}]
  (loop [population (initialize-population population-size max-initial-plushy-size) ; start pop
         generation 0]
    (if (< generation max-generations)
      (let [sorted-population (reverse (sort-by :elo (evaluate-population population
                                                                          round-robin-frequency
                                                                          generation
                                                                          stratas))) ; sort pop from best to worst
            new-pop (adjust-elo-scores-to-target (mutate-population sorted-population instructions) 1000)] ; make a new child 
        (report sorted-population generation)
        (recur new-pop (inc generation)))
      population)))

; overflow (-> #'nothing meta :name)



;; (print-individual (first monday))
;; (first monday)

;; (def another-run (main))

(defn main
  "Runs push-gp, giving it a map of arguments."
  ([] (main {}))
  ([args]
   (tic-tac-toe-push-gp {:instructions default-instructions
                         :max-generations 10
                         :population-size 50
                         :max-initial-plushy-size 100
                         :round-robin-frequency 5
                         :stratas 5})))

(comment

  ;; No arguments
  (main)

  ;; Use args as a map
  (main {:selection :lexicase}))
  
  