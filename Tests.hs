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
  [ TestCase (assertEqual "withHopType 1" "CmdSpec"
                (withHopType (Proxy :: Proxy (CreateProcess, CmdSpec)) (Field 0 1) (show . typeRep) goIxedTest))
  , TestCase (assertEqual "withHopType 2" "[[Char]]"
                (withHopType (Proxy :: Proxy (CmdSpec, String)) (Field 2 2) (show . typeRep) goIxedTest))
  , TestCase (assertEqual "withHopType 3" "Float"
                (withHopType (Proxy :: Proxy ((Int, Float, String), Float)) (TupleHop 2) (show . typeRep) goIxedTest))
  , TestCase (assertEqual "withHopType 4" "[Char]"
                (withHopType (Proxy :: Proxy ((Int, Float, String), String)) (TupleHop 3) (show . typeRep) goIxedTest))
  , TestCase (assertEqual "withHopType 5" "Int"
                (withHopType (Proxy :: Proxy ([Int], Int)) (IndexHop (encode (123 :: Int))) (show . typeRep) goIxedTest))
  , TestCase (assertEqual "withHopType 6" "goIxed - unsupported Ixed type: Map Char Float"
                (withHopType (Proxy :: Proxy ((Map Char Float), Float)) (IndexHop (encode 'x')) (show . typeRep) goIxedTest))
  , TestCase (assertEqual "traversalFromPath 1" [["arg1", "arg2"]]
                (toListOf (traversalFromPath goIxedTest goIxed2Test (TraversalPath [Field 2 2]) :: Traversal' CmdSpec [String]) (RawCommand "cmd" ["arg1", "arg2"])))
  , TestCase (assertEqual "" ["arg2"]
                (toListOf (traversalFromPath goIxedTest goIxed2Test (TraversalPath [Field 2 2, IndexHop (encode (1 :: Int))]) :: Traversal' CmdSpec String) (RawCommand "cmd" ["arg1", "arg2"])))
  , TestCase (assertEqual "withHopTypeIndexed 1" "Int"
                (withHopTypeIndexed (Proxy :: Proxy ([Int], Int)) (IndexHop (encode (123 :: Int))) (show . typeRep)))
  , TestCase (assertEqual "withHopTypeIndexed 2" "Float"
                (withHopTypeIndexed (Proxy :: Proxy (Map Char Float, Float)) (IndexHop (encode 'x')) (show . typeRep)))
  , TestCase (assertEqual "withHopTraversal 1" [[]]
                (toListOf (withHopTraversal goIxed2Test (Field 2 2) id :: Traversal' CmdSpec [[Char]]) (constrs !! 1)))
  , TestCase (assertEqual "withHopTraversal 2" [""]
                (toListOf (withHopTraversal goIxed2Test (Field 1 1) id :: Traversal' CmdSpec [Char]) (constrs !! 0)))
  , TestCase (assertEqual "withHopTraversal 3" [1.5]
                (toListOf (withHopTraversal goIxed2Test (TupleHop 2) id :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')))
  , TestCase (assertEqual "withHopTraversal 4" ["c"]
                (toListOf (withHopTraversal goIxed2Test (IndexHop (encode (2 :: Int))) id :: Traversal' [String] String) ["a","b","c","d"]))
  , TestCase (assertEqual "withHopTraversal 5" [7]
                (toListOf (withHopTraversal goIxed2Test (IndexHop (encode (2 :: Int))) id :: Traversal' [Int] Int) [9,8,7,6]))
  , TestCase (assertEqual "withHopTraversal 6" []
                (toListOf (withHopTraversal goIxed2Test (IndexHop (encode 'b')) id :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)]))) -- "withHopTraversal - unknown or unsupported Ixed type: t=Map Char Float, Index t= Float"
  , TestCase (assertEqual "withHopTraversalIndexed 1" [7]
                (toListOf (withHopTraversalIndexed (IndexHop (encode (2 :: Int))) id :: Traversal' [Int] Int) [9,8,7,6]))
  , TestCase (assertEqual "withHopTraversalIndexed 2" ["c"]
                (toListOf (withHopTraversalIndexed (IndexHop (encode (2 :: Int))) id :: Traversal' [String] String) ["a","b","c","d"]))
  , TestCase (assertEqual "withHopTraversalIndexed 3" [1.5]
                (toListOf (withHopTraversalIndexed (IndexHop (encode 'b')) id :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)] :: Map Char Float)))
  ]
