import Control.Exception (try)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Generics (Data, extQ, mkQ, Typeable, typeRep, Proxy(Proxy), constrs)
import Data.Map (Map, fromList)
import Data.Serialize (encode)
import Demo3
import GHC.IO.Handle
import System.Process
import System.Posix.Types
import Test.HUnit

main :: IO ()
main = do
  cts <- runTestTT tests
  case cts of
    Counts {errors = 0, failures = 0} -> pure ()
    _ -> error (showCounts cts)

goFieldTest ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
goFieldTest p h go = (mkQ e f1 `extQ` f2) (Proxy :: Proxy (s, a))
    where
      e :: r
      e = error $ "goIxed - unsupported Ixed type: " ++ show (typeRep (Proxy :: Proxy s))
      f1 :: Proxy ([Int], Int) -> r
      f1 p = withHopTypeIndexed h go p
      f2 :: Proxy ([String], String) -> r
      f2 p = withHopTypeIndexed h go p

goIxedTest ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (Traversal' s a -> r)
    -> r
goIxedTest p h f = f $ (mkQ e f1 `extQ` f2) (Proxy :: Proxy (s, a))
    where
      e :: Traversal' s a
      e = error $ "traversalFromHop - unknown or unsupported Ixed type: t=" ++ show (typeRep (Proxy :: Proxy s)) ++
                  ", Index t= " ++ show (typeRep (Proxy :: Proxy a))
      f1 :: Proxy ([Int], Int) -> Traversal' s a
      f1 _ = withHopTraversalIndexed (castTraversal (Proxy :: Proxy ([Int], s, Int, a))) h
      f2 :: Proxy ([String], String) -> Traversal' s a
      f2 _ = withHopTraversalIndexed (castTraversal (Proxy :: Proxy ([String], s, String, a))) h

tests :: Test
tests =
  TestList
  [ TestCase (assertEqual "withHopType 1" "CmdSpec"
                (withHopType (Proxy :: Proxy (CreateProcess, CmdSpec)) (Field 0 1) (show . typeRep) goFieldTest))
  , TestCase (assertEqual "withHopType 2" "[[Char]]"
                (withHopType (Proxy :: Proxy (CmdSpec, String)) (Field 2 2) (show . typeRep) goFieldTest))
  , TestCase (assertEqual "withHopType 3" "Float"
                (withHopType (Proxy :: Proxy ((Int, Float, String), Float)) (TupleHop 2) (show . typeRep) goFieldTest))
  , TestCase (assertEqual "withHopType 4" "[Char]"
                (withHopType (Proxy :: Proxy ((Int, Float, String), String)) (TupleHop 3) (show . typeRep) goFieldTest))
  , TestCase (assertEqual "withHopType 5" "Int"
                (withHopType (Proxy :: Proxy ([Int], Int)) (IndexHop (encode (123 :: Int))) (show . typeRep) goFieldTest))
  , TestCase (assertEqual "withHopType 6" "goField - unsupported Ixed type: Map Char Float"
                (withHopType (Proxy :: Proxy ((Map Char Float), Float)) (IndexHop (encode 'x')) (show . typeRep) goFieldTest))
  , TestCase (assertEqual "traversalFromPath 1" [["arg1", "arg2"]]
                (toListOf (traversalFromPath goFieldTest goIxedTest (TraversalPath [Field 2 2]) :: Traversal' CmdSpec [String]) (RawCommand "cmd" ["arg1", "arg2"])))
  , TestCase (assertEqual "" ["arg2"]
                (toListOf (traversalFromPath goFieldTest goIxedTest (TraversalPath [Field 2 2, IndexHop (encode (1 :: Int))]) :: Traversal' CmdSpec String) (RawCommand "cmd" ["arg1", "arg2"])))
  , TestCase (assertEqual "withHopTypeIndexed 1" "Int"
                (withHopTypeIndexed (IndexHop (encode (123 :: Int))) (show . typeRep) (Proxy :: Proxy ([Int], Int))))
  , TestCase (assertEqual "withHopTypeIndexed 2" "Float"
                (withHopTypeIndexed (IndexHop (encode 'x')) (show . typeRep) (Proxy :: Proxy (Map Char Float, Float))))
  , TestCase (assertEqual "withHopTraversal 1" [[]]
                (toListOf (withHopTraversal id goIxedTest (Field 2 2) :: Traversal' CmdSpec [[Char]]) (constrs !! 1)))
  , TestCase (assertEqual "withHopTraversal 2" [""]
                (toListOf (withHopTraversal id goIxedTest (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0)))
  , TestCase (assertEqual "withHopTraversal 3" [1.5]
                (toListOf (withHopTraversal id goIxedTest (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')))
  , TestCase (assertEqual "withHopTraversal 4" ["c"]
                (toListOf (withHopTraversal id goIxedTest (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"]))
  , TestCase (assertEqual "withHopTraversal 5" [7]
                (toListOf (withHopTraversal id goIxedTest (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]))
  , TestCase (assertEqual "withHopTraversal 6" []
                (toListOf (withHopTraversal id goIxedTest (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)]))) -- "withHopTraversal - unknown or unsupported Ixed type: t=Map Char Float, Index t= Float"
  , TestCase (assertEqual "withHopTraversalIndexed 1" [7]
                (toListOf (withHopTraversalIndexed id (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]))
  , TestCase (assertEqual "withHopTraversalIndexed 2" ["c"]
                (toListOf (withHopTraversalIndexed id (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"]))
  , TestCase (assertEqual "withHopTraversalIndexed 3" [1.5]
                (toListOf (withHopTraversalIndexed id (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)] :: Map Char Float)))
  ]
