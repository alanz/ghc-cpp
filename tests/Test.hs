module Main where

import qualified Data.Map as Map
import GHC.Cpp
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1, 2 :: Integer) (foo 3))

test2 :: Test
test2 =
    TestCase
        ( do
            (x, y) <- partA 3
            assertEqual "for the first result of partA," 5 x
            b <- partB y
            assertBool ("(partB " ++ show y ++ ") failed") b
        )

tests :: Test
tests =
    TestList
        [ TestLabel "test1" test1
        , TestLabel "test2" test2
        , TestLabel "state" m1
        ]

foo :: Integer -> (Integer, Integer)
foo x = (x - 2, x - 1)

partA :: Integer -> IO (Integer, Integer)
partA _n = return (5, 3)

partB :: Integer -> IO Bool
partB _n = return True

m0 :: (MacroState, Output)
m0 = do
    let (s0, _) = process initMacroState "#define FOO 3"
    let (s1, _) = process s0 "#ifdef FOO"
    process s1 "# if FOO == 4"

m1 :: Test
m1 =
    TestCase
        ( do
            let (s0, _) = process initMacroState "#define FOO 3"
            assertEqual
                "s0"
                ( MacroState
                    { pp_defines = Map.fromList [(MacroName "FOO" Nothing, ["3"])]
                    , pp_accepting = True
                    }
                )
                s0
            let (s1, _) = process s0 "#ifdef FOO"
            assertEqual
                "s1"
                MacroState
                    { pp_defines = Map.fromList [(MacroName "FOO" Nothing, ["3"])]
                    , pp_accepting = True
                    }
                s1
            let (s2, _) = process s1 "# if FOO == 4"
            assertEqual
                "s2"
                MacroState
                    { pp_defines = Map.fromList [(MacroName "FOO" Nothing, ["3"])]
                    , pp_accepting = False
                    }
                s2
            let (s3, _) = process s2 "#ifndef FOO"
            assertEqual
                "s3"
                MacroState
                    { pp_defines = Map.fromList [(MacroName "FOO" Nothing, ["3"])]
                    , pp_accepting = False
                    }
                s3
        )
