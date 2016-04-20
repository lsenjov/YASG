(ns yasg.core
  (:require [clojure.set :refer [union]])
  (:gen-class)
  )

;; 0,0 is the top left of the board

(defn create-board
  "Creates a blank board, with a possibility space, of the chosen size"
  ([x y]
   (let [square (* x y)
         possibles (vec (range 1 (inc square)))]
     {:x x
      :y y
      :board (vec (repeat square (vec (repeat square nil))))
      :rows (vec (repeat square possibles))
      :columns (vec (repeat square possibles))
      :squares (vec (repeat x (vec (repeat y possibles)))) ;; TODO is this the right way around?
      }
     )
   )
  )

(defn remove-from-board
  "Given an x and y coord, and a number, returns the board with the number removed from the possibles.
  Does not assert they are there to begin with."
  ([board x y n]
   (let [rm (partial remove #(= % n))]
     (-> board
         (update-in [:rows y] rm)
         (update-in [:columns x] rm)
         (update-in [:squares
                     (int (java.lang.Math/floor (/ y (:y board))))
                     (int (java.lang.Math/floor (/ x (:x board))))] rm)))))

(defn add-to-board
  "Given an x and y coord, and a number, returns the board with the number added in the correct place.
  Does not assert the spot is empty"
  ([board x y n]
   ;(println "add-to-board:" x " " y " " n " " board)
   (-> board
       (update-in [:board y] assoc x n))))

(defn modify-board
  "Given an x and y coord, and a number, removes the number from the possibles and adds the number to the board.
  Does not assert the spot is empty"
  ([board x y n]
   (-> board
       (add-to-board x y n)
       (remove-from-board x y n))))

(defn get-value
  "Gets a value from a board"
  ([board x y]
   (-> board (:board) (get y) (get x))))

(defn check-location
  "Given an x and y coord, and a number, checks if the number exists in all the possibles for that location,
  and if the spot is nil. Returns :okay if spot is okay, :taken if the spot is taken,
  :rows, :columns, or :squares in that order if they are not in the possibles."
  ([board x y n]
   (let [search (comp not (partial some #{n}))] ;; search returns true if the item is NOT in the problem space
     (if (-> board (:board) (get y) (get x))
       :taken
       (if (-> board (:rows) (get y) (search))
         :rows
         (if (-> board (:columns) (get x) (search))
           :columns
           (if (-> board
                   (:squares)
                   (get (int (java.lang.Math/floor (/ y (:y board)))))
                   (get (int (java.lang.Math/floor (/ x (:x board)))))
                   (search))
             :squares
             :okay)))))))

(defn fill-single-square
  "Given an x and y coord, and an ordered list of numbers, attempts to fill the spot in the board.
  If successful, returns the filled board, otherwise throws an exception."
  ([board x y nums]
   (if (= :okay (check-location board x y (first nums)))
     (modify-board board x y (first nums))
     (if (<= (count nums) 1)
       (throw (Exception. "Could not find a location in board:" board))
       (recur board x y (rest nums)))))
  ([board x y]
   (fill-single-square board x y (shuffle (range 1 (inc (* (:y board) (:x board)))))))
  )

(defn fill-full-square
  "Given a board, attempt to fill each square randomly, either return the board or return nil on fail"
  ([board]
   (try
     (let [square (* (:x board) (:y board))]
       ;(println "square:" square) ;; DEBUG
       (loop [x 0 y 0 b board]
         ;(println "loop. x:" x " y:" y " board:" b) ;; DEBUG
         (if (= y square) ;; Just gone off the very bottom of the board
           b ;; End
           (if (= x square) ;; End of line
             (recur 0 (inc y) b)
             (recur (inc x) y (fill-single-square b x y)) ;; Not at the end of anything
             ))))
     (catch Exception e nil))))

(defn gen-square
  "Keep trying till a square is created"
  ([x y]
   (let [board (fill-full-square (create-board x y))]
     (if board
       board
       (do
         (print ".")
         (recur x y))))))

(defn create-coords
  "Creates a set of coords n items long"
  ([xsize ysize n items]
   (if (< (count items) n)
     (recur xsize ysize n (union #{} items #{[(rand-int xsize) (rand-int ysize)]}))
     items))
  ([xsize ysize n]
   (create-coords xsize ysize n #{})))

(defn translate-square
  "Takes a complete puzzle, takes num items from the square and puts it in a new square"
  ([full part coords]
   ;(println coords)
   (if (= 0 (count coords))
     part
     (let [item (first coords)
           x (first item)
           y (second item)]
       (recur full (add-to-board part x y (get-value full x y)) (rest coords)))))
  ([full coords]
   (translate-square full (create-board (:x full) (:y full)) (into '() coords))))

(defn create-puzzle
  "Creates a puzzle, returns both the :answer and :puzzle in a map"
  ([x y diff answer]
   (let [square (* x y)]
     {:answer answer :puzzle (translate-square answer (create-coords square square diff))}))
  ([x y diff]
   (create-puzzle x y diff (gen-square x y)))
  ([]
   (create-puzzle 3 3 20)))

(defn- replace-print-line
  "Changes a line from numbers and nil to a pretty line"
  [line xsize]
  (concat (flatten
              (interpose "|"
                         (partition xsize
                                    (map #(if %
                                            (format "%3d" %)
                                            "   ")
                                         line))))
       "\n"))

(defn print-puzzle-from-array
  "Print a puzzle to stdout"
  [puzzleBoard]
  (println
    (apply str
           (flatten
             (interpose
               (concat
                 (flatten
                   (take
                     (+
                      (*
                       3
                       (:x puzzleBoard)
                       (:y puzzleBoard))
                      (dec (:y puzzleBoard)))
                     (repeat "-")))
                 "\n") ;; Makes a horisontal dividing line
               (partition (:y puzzleBoard)
                          (map replace-print-line
                               (:board puzzleBoard)
                               (repeat (:x puzzleBoard)))))))))

(defn print-puzzle-and-answer
  "Generate and print a puzzle and answer"
  [x y diff]
  (let [puzz (create-puzzle x y diff)]
    (println)
    (print-puzzle-from-array (:puzzle puzz))
    (println)
    (print-puzzle-from-array (:answer puzz))))

(defn print-puzzle
  "Generate and print a puzzle, no answer"
  [x y diff]
  (let [puzz (create-puzzle x y diff)]
    (println)
    (print-puzzle-from-array (:puzzle puzz))
    ))

(defn -main
  "Do things"
  [& args]
  (try
    (let [x (Integer/parseInt (nth args 0))
          y (Integer/parseInt (nth args 1))
          revealed (Integer/parseInt (nth args 2))]
      (case (count args)
        3 (print-puzzle x y revealed)
        4 (print-puzzle-and-answer x y revealed)
        (println "Usage: sudoko x y revealed [showAnswer]")
        )
      )
    (catch Exception e (println "Usage: sudoko x y revealed [showAnswer]"))
    )
  )
