{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
    | IndexHop {_key :: key} -- ^ Hop from an instance of 'Index', such as Map, via some key value
    deriving (Show, Functor, Data, Typeable)

-- | Convert a sequence of Hops into a traversal:
-- @@
-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
-- @@
-- λ> toListOf (traversalFromPath (TraversalPath [Field 2 2, IndexHop (encode (1 :: Int))]) :: Traversal' CmdSpec [[Char]]) (constrs !! 1)
-- [[]]
-- λ> toListOf (traversalFromHop (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0)
-- [""]
-- λ> toListOf (traversalFromHop (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')
-- [1.5]
-- λ> toListOf (traversalFromHopIndexed (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]
-- [7]
-- @@
traversalFromPath ::
    forall s a. (Data s, Typeable a)
    => TraversalPath s a
    -> Traversal' s a
traversalFromPath (TraversalPath []) = castTraversal (Proxy :: Proxy (s, s, s, a)) id
traversalFromPath (TraversalPath (h : hs)) =
    withHopType (Proxy :: Proxy s) h go (error "traversalFromPath")
    where
      go :: forall b. Data b => Proxy b -> Traversal' s a
      go _ =
          (traversalFromHop h :: Traversal' s b) .
          (traversalFromPath (TraversalPath hs) :: Traversal' b a)

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
-- λ> withHopType (Proxy :: Proxy CreateProcess) (Field 0 1) typeRep empty
-- CmdSpec
-- λ> withHopType (Proxy :: Proxy CmdSpec) (Field 2 2) typeRep empty
-- [[Char]]
-- λ> withHopType (Proxy :: Proxy (Int, Float, String)) (TupleHop 2) typeRep empty
-- Float
-- λ> withHopType (Proxy :: Proxy (Int, Float, String)) (TupleHop 3) typeRep empty
-- [Char]
-- @@
-- We can also handle any Ixed types explicitly handled by goIx.
-- @@
-- λ> withHopType (Proxy :: Proxy [Int]) (IndexHop (encode (123 :: Int))) typeRep empty
-- Int
-- λ> withHopType (Proxy :: Proxy (Map Char Float)) (IndexHop (encode 'x')) typeRep empty
-- *** Exception: goIx - unsupported Ixed type: Map Char Float
-- @@
withHopType ::
    forall s r. (Data s)
    => Proxy s
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r) -> r -> r
withHopType _p (Field cpos fpos) go r0 =
    maybe r0 (gmapQi (pred fpos) go') constr
    where
      -- Get a value from 'constrs' corresponding to the constructor
      -- at position cpos.  Will be Nothing if cpos is out of range.
      constr :: Maybe s
      constr = listToMaybe $ drop (pred cpos) (constrs :: [s])
      -- Turn the empty hop destination value into a type and apply to go
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
withHopType _p (TupleHop n) go r0 =
    gmapQi (pred n) go' (empty :: s)
    where
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
withHopType p h go r0 =
    fromMaybe (error $ "Unexpected hop " ++ show (typeRep p)) (goIx p h go r0)

goIx :: forall s r. (Typeable s) => Proxy s -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r -> Maybe r
goIx p h go r0 | isJust (eqT :: Maybe (s :~: [Int])) = Just $ withHopTypeIndexed (Proxy :: Proxy [Int]) h go r0
goIx p h go r0 | isJust (eqT :: Maybe (s :~: [String])) = Just $ withHopTypeIndexed (Proxy :: Proxy [String]) h go r0
goIx p h go r0 = error $ "goIx - unsupported Ixed type: " ++ show (typeRep (Proxy :: Proxy s))

-- | 'withHopType', extended to use the IxValue instance of s to get the hop type.
-- @@
-- λ> withHopTypeIndexed (Proxy :: Proxy [Int]) (IndexHop (encode (123 :: Int))) typeRep empty
-- Int
-- λ> withHopTypeIndexed (Proxy :: Proxy (Map Char Float)) (IndexHop (encode 'x')) typeRep empty
-- Float
-- @@
withHopTypeIndexed ::
    forall s r. (Data s, Data (IxValue s))
    => Proxy s -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r -> r
withHopTypeIndexed _p (IndexHop _) go r0 = go (Proxy :: Proxy (IxValue s))
withHopTypeIndexed _p h go r0 = withHopType _p h go r0

-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
-- @@
-- λ> toListOf (traversalFromHop (Field 2 2) :: Traversal' CmdSpec [[Char]]) (constrs !! 1)
-- [[]]
-- λ> toListOf (traversalFromHop (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0)
-- [""]
-- λ> toListOf (traversalFromHop (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')
-- [1.5]
-- λ> toListOf (traversalFromHop (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]
-- [7]
-- λ> toListOf (traversalFromHop (IndexHop (encode (2 :: Int))) :: Traversal' [String] Int) ["a","b","c","d"]
-- ["c"]
-- @@
traversalFromHop ::
    forall s a. (Data s, Typeable a)
    => Hop ByteString -> Traversal' s a
traversalFromHop h@(Field _cpos fpos) =
    onceUpon' field
    where
      -- Build a function to extract the requested field
      field :: s -> a
      field s = gmapQi (pred fpos) go s
      -- Use cast to verify that the extracted field type is @a@
      go :: forall b. Data b => b -> a
      go b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
traversalFromHop h@(TupleHop n) =
    onceUpon' field
    where
      field :: s -> a
      field s = gmapQi (pred n) go s
      go :: forall b. Data b => b -> a
      go b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
traversalFromHop h@(IndexHop bytes) =
    fromMaybe (error $ "traversalFromHop - unexpected hop from " ++ show (typeRep (Proxy :: Proxy s)) ++ ": " ++ show h)
              (goIx (Proxy :: Proxy s) h undefined undefined)

traversalFromHopIndexed ::
    forall s a. (Data s, Ixed s, Typeable (Index s), Serialize (Index s), Typeable (IxValue s), Typeable a)
    => Hop ByteString -> Traversal' s a
traversalFromHopIndexed (IndexHop bytes) =
    idx (decode bytes :: Either String (Index s))
    where
      idx :: Either String (Index s) -> Traversal' s a
      idx (Left s) = error $ "Error decoding " ++ show bytes ++ " to " ++ show (typeRep (Proxy :: Proxy (Index s))) ++ ": " ++ s
      idx (Right k) = go k
      go :: Index s -> Traversal' s a
      go k = castTraversal (Proxy :: Proxy (s, s, IxValue s, a)) (ix k)
traversalFromHopIndexed h = traversalFromHop h

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
