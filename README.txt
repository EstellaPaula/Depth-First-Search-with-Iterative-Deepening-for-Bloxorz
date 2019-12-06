Depth First Search with Iterative Deepening for Bloxorz | HASKELL

Implemented the Bloxorz game mechanics and the solution finding algorithm for the game with DFS Iterative, while using a heuristic based on ordering the child nodes in each state by their probability of leading to a solution.
For running tests:
      stack exec ghci BloxorzTest.hs
 
      * BloxorzTest.hs> main ---> runs all tests
      * BloxorzTest.hs> runTestPP test_name ---> runs a specific test,
      with a value of:
[testAddObject
, testShowLevels
, testMove
, testActivate
, testLimitedDfs
, testIterativeDeepening
, testExtractPath
, testSuccessors
, testIsGoal
, testSolve
, testHeuristic
]

To play on the command line:
      - set workingOs to Windows or Linux
       
      stack exec ghci Interactive.hs
 
      * Interactvie.hs> play level0 ---> starts in the line
                                                      command level0
 
      * Interactvie.hs> visualize level0 False ---> simulates the sequence of moves found
                                                      which leads to a winning level