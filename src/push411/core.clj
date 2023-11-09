(ns push411.core)

(comment
  "Kate Bondarenko:

   Implementation of GP system using Push language. 

   I found that running push-gp with smaller maximum genome length 
   works better for this specific problem. 
   My best results were obtained using this configuration:

   {:instructions default-instructions
    :error-function regression-error-function
    :max-generations 500
    :population-size 100
    :max-initial-plushy-size 50}
   
   For the training cases, I wrote a couple small ones for faster runs; one
   that I like very much is (0 1 2 3 5 10 15 1253). Since the first few values are smal
   and not far from each other, (in my opinion) it helps the evolution find where to go.. 
   The last big value is to affirm that the solution is working for a far away value. 
   Evolutions went better on smaller and simpler training cases! But there are also ways
   to generate a long a complicated trainin case; one way could be (range -100 100). 

   Everything else was pretty straighforward and my test file shows that most of the 
   stuff works as intended
   ")


;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(integer_+ integer_-)
   :integer '(10 20 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

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
   'exec_dup
   'close
   0
   1))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1})


;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :input {}})

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

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

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
(map vector '(1 2 3) '(4 5 6))
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
                                                    max-initial-plushy-size))))
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
  
  
