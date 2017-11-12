import Demo3
import Test.HUnit
import Control.Lens

-- For testing
import Control.Exception (try)
import Data.Generics (typeRep, Proxy(Proxy), constrs)
import Data.Map (Map, fromList)
import Data.Serialize (encode)
import System.Process
import System.Posix.Types
import GHC.IO.Handle

main = runTestTT tests

tests :: Test
tests =
  TestList
  [ TestCase (assertEqual "withHopType 1"
                (withHopType (Proxy :: Proxy CreateProcess) (Field 0 1) (show . typeRep) goIxedTest)
                "CmdSpec")
  , TestCase (assertEqual "withHopType 2"
                (withHopType (Proxy :: Proxy CmdSpec) (Field 2 2) (show . typeRep) goIxedTest)
                "[[Char]]")
  , TestCase (assertEqual "withHopType 3"
                (withHopType (Proxy :: Proxy (Int, Float, String)) (TupleHop 2) (show . typeRep) goIxedTest)
                "Float")
  , TestCase (assertEqual "withHopType 4"
                (withHopType (Proxy :: Proxy (Int, Float, String)) (TupleHop 3) (show . typeRep) goIxedTest)
                "[Char]")
  , TestCase (assertEqual "withHopType 5"
                (withHopType (Proxy :: Proxy [Int]) (IndexHop (encode (123 :: Int))) (show . typeRep) goIxedTest)
                "Int")
  , TestCase (assertEqual "withHopType 6"
                (withHopType (Proxy :: Proxy (Map Char Float)) (IndexHop (encode 'x')) (show . typeRep) goIxedTest)
                "goIxed - unsupported Ixed type: Map Char Float")
  , TestCase (assertEqual "traversalFromPath 1"
                [["arg1", "arg2"]]
                (toListOf (traversalFromPath goIxedTest goIxed2Test (TraversalPath [Field 2 2]) :: Traversal' CmdSpec [String]) (RawCommand "cmd" ["arg1", "arg2"])))
  , TestCase (assertEqual ""
                ["arg2"]
                (toListOf (traversalFromPath goIxedTest goIxed2Test (TraversalPath [Field 2 2, IndexHop (encode (1 :: Int))]) :: Traversal' CmdSpec String) (RawCommand "cmd" ["arg1", "arg2"])))
  , TestCase (assertEqual "withHopTypeIndexed 1"
                (withHopTypeIndexed (Proxy :: Proxy [Int]) (IndexHop (encode (123 :: Int))) (show . typeRep))
                "Int")
  , TestCase (assertEqual "withHopTypeIndexed 2"
                (withHopTypeIndexed (Proxy :: Proxy (Map Char Float)) (IndexHop (encode 'x')) (show . typeRep))
                "Float")
  , TestCase (assertEqual "traversalFromHop 1"
                (toListOf (traversalFromHop goIxed2Test (Field 2 2) :: Traversal' CmdSpec [[Char]]) (constrs !! 1))
                [[]])
  , TestCase (assertEqual "traversalFromHop 2"
                (toListOf (traversalFromHop goIxed2Test (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0))
                [""])
  , TestCase (assertEqual "traversalFromHop 3"
                (toListOf (traversalFromHop goIxed2Test (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x'))
                [1.5])
  , TestCase (assertEqual "traversalFromHop 4"
                (toListOf (traversalFromHop goIxed2Test (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"])
                ["c"])
  , TestCase (assertEqual "traversalFromHop 5"
                (toListOf (traversalFromHop goIxed2Test (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6])
                [7])
  , TestCase (assertEqual "traversalFromHop 6"
                (toListOf (traversalFromHop goIxed2Test (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)]))
                []) -- "traversalFromHop - unknown or unsupported Ixed type: t=Map Char Float, Index t= Float"
  , TestCase (assertEqual "traversalFromHopIndexed 1"
                (toListOf (traversalFromHopIndexed (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6])
                [7])
  , TestCase (assertEqual "traversalFromHopIndexed 2"
                (toListOf (traversalFromHopIndexed (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"])
                ["c"])
  , TestCase (assertEqual "traversalFromHopIndexed 3"
                (toListOf (traversalFromHopIndexed (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)] :: Map Char Float))
                [1.5])
  ]
