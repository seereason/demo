{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Demo3 where

import Control.Lens
import Data.Data.Lens
import Data.ByteString hiding (drop, empty, intercalate)
import Data.Generics
import Data.List (intercalate)
import Data.Maybe
import Data.Serialize

-- For instances
import Data.Monoid (mempty)
import Data.Map (Map)

-- For testing
import System.Process
import System.Posix.Types
import GHC.IO.Handle

import Debug.Show

data TraversalPath s a = TraversalPath {_traversalPathHops :: [Hop ByteString]}

data Hop key
    = Field {_cpos :: Int, _fpos :: Int}
      -- ^ Hop to one of the fields of a record - the constructor and
      -- field number are specified.
    | TupleHop {_tpos :: Int} -- ^ Hop to nth element of a tuple
    | IndexHop {_key :: key}
    -- ^ Hop from an instance of 'Ixed', such as @[]@ or @Map@, via
    -- some key of the corresponding 'Index' type, to a value of the
    -- corresponding 'IxValue' type.  For serialization the
    -- 'Data.Serialize.encode' function is applied to the key to make
    -- this a monomorphic type @Hop ByteString@.
    deriving (Show, Functor, Data, Typeable)

-- | Convert a sequence of Hops into a traversal:
-- @@
-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
-- @@
-- λ> toListOf (traversalFromPath (TraversalPath [Field 2 2]) :: Traversal' CmdSpec [String]) (RawCommand "cmd" ["arg1", "arg2"])
-- [["arg1", "arg2"]]
-- λ> toListOf (traversalFromPath (TraversalPath [Field 2 2, IndexHop (encode (1 :: Int))]) :: Traversal' CmdSpec String) (RawCommand "cmd" ["arg1", "arg2"])
-- ["arg2"]
-- @@
traversalFromPath ::
    forall s a. (Data s, Typeable a)
    => (forall s r. (Typeable s) => Proxy (s, a) -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r)
    -> (forall s a r. (Typeable s, Typeable a) => Proxy (s, a) -> Hop ByteString -> Traversal' s a)
    -> TraversalPath s a
    -> Traversal' s a
traversalFromPath _ _ (TraversalPath []) = castTraversal (Proxy :: Proxy (s, s, s, a)) id
traversalFromPath goIxed goIxed2 (TraversalPath (h : hs)) =
    withHopType (Proxy :: Proxy (s, a)) h go goIxed
    where
      go :: forall b. Data b => Proxy b -> Traversal' s a
      go _ =
          (withHopTraversal goIxed2 id h :: Traversal' s b) .
          (traversalFromPath goIxed goIxed2 (TraversalPath hs) :: Traversal' b a)

instance Show (V TypeRep) where
    show (V t) =
        "TypeRep (" ++
          (intercalate ") ("
           [show (typeRepFingerprint t),
            show (typeRepTyCon t),
            "[KindRep list]",
            show (V (typeRepArgs t))] :: String) ++ ")"

instance Show (V a) => Show (V [a]) where
    show (V xs) = "[" ++ intercalate ", " (fmap (show . V) xs) ++ "]"

-- | Given a type @s@, determine the type @a@ resulting from doing some
-- hop and apply it to a function @go@.
-- @@
-- λ> withHopType (Proxy :: Proxy (CreateProcess, CmdSpec)) (Field 0 1) typeRep goIxedTest
-- CmdSpec
-- λ> withHopType (Proxy :: Proxy (CmdSpec, String)) (Field 2 2) typeRep goIxedTest
-- [[Char]]
-- λ> withHopType (Proxy :: Proxy ((Int, Float, String), Float)) (TupleHop 2) typeRep goIxedTest
-- Float
-- λ> withHopType (Proxy :: Proxy ((Int, Float, String), String)) (TupleHop 3) typeRep goIxedTest
-- [Char]
-- @@
-- We can also handle any Ixed types explicitly handled by goIx.
-- @@
-- λ> withHopType (Proxy :: Proxy ([Int], Int)) (IndexHop (encode (123 :: Int))) typeRep goIxedTest
-- Int
-- λ> withHopType (Proxy :: Proxy (Map Char Float, Float)) (IndexHop (encode 'x')) typeRep goIxedTest
-- *** Exception: goIxed - unsupported Ixed type: Map Char Float
-- @@
withHopType ::
    forall s a r. (Data s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> (forall s r. (Typeable s) => Proxy (s, a) -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r)
    -> r
withHopType _p (Field cpos fpos) go _ =
    maybe (error $ "withHopType - " ++ show (typeRep (Proxy :: Proxy s)) ++ " constructor index out of range: " ++ show cpos)
          (gmapQi (pred fpos) go')
          constr
    where
      -- Get a value from 'constrs' corresponding to the constructor
      -- at position cpos.  Will be Nothing if cpos is out of range.
      constr :: Maybe s
      constr = listToMaybe $ drop (pred cpos) (constrs :: [s])
      -- Turn the empty hop destination value into a type and apply to go
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
withHopType _p (TupleHop n) go _ =
    gmapQi (pred n) go' (empty :: s)
    where
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
withHopType p h go goIxed =
    goIxed p h go

goIxedTest ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
goIxedTest p h go = (mkQ e f1 `extQ` f2) (Proxy :: Proxy (s, a))
    where
      e :: r
      e = error $ "goIxed - unsupported Ixed type: " ++ show (typeRep (Proxy :: Proxy s))
      f1 :: Proxy ([Int], Int) -> r
      f1 _ = withHopTypeIndexed (Proxy :: Proxy ([Int], Int)) h go
      f2 :: Proxy ([String], String) -> r
      f2 _ = withHopTypeIndexed (Proxy :: Proxy ([String], String)) h go

goIxedNull ::
    forall s r. (Typeable s)
    => Proxy s
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
goIxedNull p h go = error "goIxedNull"

goIxed2Test ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> Traversal' s a
goIxed2Test p h = (mkQ e f1 `extQ` f2) (Proxy :: Proxy (s, a))
    where
      e :: Traversal' s a
      e = error $ "traversalFromHop - unknown or unsupported Ixed type: t=" ++ show (typeRep (Proxy :: Proxy s)) ++
                  ", Index t= " ++ show (typeRep (Proxy :: Proxy a))
      f1 :: Proxy ([Int], Int) -> Traversal' s a
      f1 _ = withHopTraversalIndexed (castTraversal (Proxy :: Proxy ([Int], s, Int, a))) h
      f2 :: Proxy ([String], String) -> Traversal' s a
      f2 _ = withHopTraversalIndexed (castTraversal (Proxy :: Proxy ([String], s, String, a))) h

{-
goIxed3Test ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
goIxed3Test p h go = (mkQ e f1 `extQ` f2) (Proxy :: Proxy (s, a))
-}

goIxed2Null ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> Traversal' s a
goIxed2Null _ _ = error "goIxed2Null"

-- | 'withHopType', extended to use the IxValue instance of s to get the hop type.
-- @@
-- λ> withHopTypeIndexed (Proxy :: Proxy ([Int], Int)) (IndexHop (encode (123 :: Int))) typeRep
-- Int
-- λ> withHopTypeIndexed (Proxy :: Proxy (Map Char Float, Float)) (IndexHop (encode 'x')) typeRep
-- Float
-- @@
withHopTypeIndexed ::
    forall s r. (Data s, Data (IxValue s))
    => Proxy (s, IxValue s)
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
withHopTypeIndexed _p (IndexHop _) go = go (Proxy :: Proxy (IxValue s))
withHopTypeIndexed _p h go = withHopType _p h go goIxedNull

-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
-- @@
-- λ> toListOf (withHopTraversal goIxed2Test id (Field 2 2) :: Traversal' CmdSpec [[Char]]) (constrs !! 1)
-- [[]]
-- λ> toListOf (withHopTraversal goIxed2Test id (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0)
-- [""]
-- λ> toListOf (withHopTraversal goIxed2Test id (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')
-- [1.5]
-- λ> toListOf (withHopTraversal goIxed2Test id (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"]
-- ["c"]
-- λ> toListOf (withHopTraversal goIxed2Test id (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]
-- [7]
-- λ> toListOf (withHopTraversal goIxed2Test id (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)])
-- *** Exception: withHopTraversal - unknown or unsupported Ixed type: t=Map Char Float, Index t= Float
-- @@
withHopTraversal ::
    forall s a r. (Data s, Typeable a)
    => (forall s a r. (Data s, Typeable s, Typeable a) => Proxy (s, a) -> Hop ByteString -> Traversal' s a)
    -> (Traversal' s a -> r)
    -> Hop ByteString
    -> r
withHopTraversal _ go h@(Field _cpos fpos) =
    go (onceUpon' field)
    where
      -- Build a function to extract the requested field
      field :: s -> a
      field s = gmapQi (pred fpos) go' s
      -- Use cast to verify that the extracted field type is @a@
      go' :: forall b. Data b => b -> a
      go' b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
withHopTraversal _ go h@(TupleHop n) =
    go (onceUpon' field)
    where
      field :: s -> a
      field s = gmapQi (pred n) go' s
      go' :: forall b. Data b => b -> a
      go' b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
withHopTraversal goIxed3 go h@(IndexHop bytes) = go (goIxed3 (Proxy :: Proxy (s, a)) h)

-- | withHopTraversal, extended to use the IxValue instance of s to get the hop type.
-- @@
-- λ> toListOf (withHopTraversalIndexed id (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]
-- [7]
-- λ> toListOf (withHopTraversalIndexed id (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"]
-- ["c"]
-- λ> toListOf (withHopTraversalIndexed id (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)] :: Map Char Float)
-- [1.5]
-- @@
withHopTraversalIndexed ::
    forall s r. (Data s, Ixed s, Typeable (Index s), Serialize (Index s), Typeable (IxValue s))
    => (Traversal' s (IxValue s) -> r)
    -> Hop ByteString
    -> r
withHopTraversalIndexed go (IndexHop bytes) =
    go (idx (decode bytes :: Either String (Index s)))
    where
      idx :: Either String (Index s) -> Traversal' s (IxValue s)
      idx (Left s) = error $ "Error decoding " ++ show bytes ++ " to " ++ show (typeRep (Proxy :: Proxy (Index s))) ++ ": " ++ s
      idx (Right k) = go' k
      go' :: Index s -> Traversal' s (IxValue s)
      go' k = castTraversal (Proxy :: Proxy (s, s, IxValue s, a)) (ix k)
withHopTraversalIndexed go h = withHopTraversal goIxed2Null go h

-- | Type safe cast of a traversal
castTraversal ::
    forall a b f s t proxy. (Applicative f, Typeable a, Typeable b, Typeable s, Typeable t)
    => proxy (s, t, a, b)
    -> ((a -> f a) -> s -> f s) -- Traversal' s a
    -> ((b -> f b) -> t -> f t) -- (Traversal' t b)
castTraversal _ l =
    case (eqT :: Maybe (s :~: t), eqT :: Maybe (a :~: b)) of
      (Just Refl, Just Refl) -> l
      _ -> const pure

-- For testing

deriving instance Data CUid
deriving instance Data CGid
deriving instance Data CmdSpec
deriving instance Data StdStream
deriving instance Data CreateProcess
