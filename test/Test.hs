module Main where

    import Test.Tasty
    import Test.Tasty.HUnit

    import Lib
    import Kmeans

    main :: IO ()
    main = do
      defaultMain (testGroup "Our Library Tests" [testTuples, testDistancePtoP,
        testNearest, testConverg, testNotConverg, testReplace])

    testTuples :: TestTree
    testTuples = testGroup "Testing Tuples" [testFirst, testSecond, testThird, testAdd, testDiv]

    testAdd :: TestTree
    testAdd = testCase "Testing add tuples"
        (assertEqual "Add Tuples:" (15, 15, 15) (add_tuples (5, 5, 5) (10, 10, 10)))

    testDiv :: TestTree
    testDiv = testCase "Testing div tuples"
        (assertEqual "div Tuples:" (3, 3, 3) (div_tuples (6, 6, 6) 2))

    testFirst :: TestTree
    testFirst = testCase "Testing first Tuples"
        (assertEqual "Return the first tuples" 5 (first (5, 10, 15)))

    testSecond :: TestTree
    testSecond = testCase "Testing second Tuples"
        (assertEqual "Return the second tuples" 10 (second (5, 10, 15)))

    testThird :: TestTree
    testThird = testCase "Testing third Tuples"
        (assertEqual "Return the third tuples" 15 (third (5, 10, 15)))

    testDistancePtoP :: TestTree
    testDistancePtoP = testCase "Testing the distance between two points"
        (assertEqual "Distance :" 8.6602545 (distance_p_to_p (5, 5, 5) (10, 10, 10)))

    testNearest :: TestTree
    testNearest = testCase "Testing nearest point from the list"
        (assertEqual "nearest point:" 0 (nearest [5, 10, 15] 5 0 0))

    testConverg :: TestTree
    testConverg = testCase "Testing if the convergence is ok"
        (assertEqual "isConverg:" True (verif_converg [(5, 10, 15)] [(5.1, 10.1, 15.1)] 0 0.5))

    testNotConverg :: TestTree
    testNotConverg = testCase "Testing if the convergence is ko"
        (assertEqual "isNotConverg:" False (verif_converg [(4, 10, 15)] [(5.1, 10.1, 15.1)] 0 0.5))

    testReplace :: TestTree
    testReplace = testCase "Testing replacement of char in string"
        (assertEqual "replacing char :" "(5 10 15)" (replace "(5,10,15)" "" ',' ' '))