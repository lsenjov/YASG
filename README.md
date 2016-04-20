# YASG
Yet another Sudoku Generator.  
  
  
Written in Clojure, requires Leiningen.  
`lein run x y revealed [show]`  
`lein run 3 3 20` will create a 3x3x3x3 grid, with 20 numbers revealed  
`lein run 3 3 20 y` will create a 3x3x3x3 grid, with 20 numbers revealed, and also prints the answer  
`lein run 2 4 18 y` will create a 2x4x4x2 grid, with 18 numbers revealed, and also prints the answer  
If `x * y > 10` the program may take a very long time to find an solution grid to create a puzzle from
