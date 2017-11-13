{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Demo3
    ( Hop(..)
    , TraversalPath(..)
    , traversalFromPath
    , withHopType
    , withHopTypeIndexed
    , withHopTraversal
    , withHopTraversalIndexed
    , castTraversal
    , makeFieldQuery
    ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Data.Lens
import Data.Generics
import Data.List (intercalate)
import Data.Maybe
import Data.Serialize
import Debug.Show
import Language.Haskell.TH

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
    => (forall s a r. (Typeable s, Typeable a) => Proxy (s, a) -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r)
    -> (forall s a r. (Typeable s, Typeable a) => Proxy (s, a) -> Hop ByteString -> (Traversal' s a -> r) -> r)
    -> TraversalPath s a
    -> Traversal' s a
traversalFromPath _ _ (TraversalPath []) = castTraversal (Proxy :: Proxy (s, s, s, a)) id
traversalFromPath goField goIxed (TraversalPath (h : hs)) =
    withHopType (Proxy :: Proxy (s, a)) h go goField
    where
      go :: forall b. Data b => Proxy b -> Traversal' s a
      go _ =
          (withHopTraversal id goIxed h :: Traversal' s b) .
          (traversalFromPath goField goIxed (TraversalPath hs) :: Traversal' b a)

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
    -> (forall s a r. (Typeable s, Typeable a) => Proxy (s, a) -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r)
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

-- | 'withHopType', extended to use the IxValue instance of s to get the hop type.
-- @@
-- λ> withHopTypeIndexed (IndexHop (encode (123 :: Int))) typeRep (Proxy :: Proxy ([Int], Int))
-- Int
-- λ> withHopTypeIndexed (IndexHop (encode 'x')) typeRep (Proxy :: Proxy (Map Char Float, Float))
-- Float
-- @@
withHopTypeIndexed ::
    forall s r. (Data s, Data (IxValue s))
    => Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> Proxy (s, IxValue s)
    -> r
withHopTypeIndexed (IndexHop _) go _p = go (Proxy :: Proxy (IxValue s))
withHopTypeIndexed h go _p = withHopType _p h go goFieldNull

-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
-- @@
-- λ> toListOf (withHopTraversal id goIxedTest (Field 2 2) :: Traversal' CmdSpec [[Char]]) (constrs !! 1)
-- [[]]
-- λ> toListOf (withHopTraversal id goIxedTest (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0)
-- [""]
-- λ> toListOf (withHopTraversal id goIxedTest (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')
-- [1.5]
-- λ> toListOf (withHopTraversal id goIxedTest (IndexHop (encode (2 :: Int))) :: Traversal' [String] String) ["a","b","c","d"]
-- ["c"]
-- λ> toListOf (withHopTraversal id goIxedTest (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [9,8,7,6]
-- [7]
-- λ> toListOf (withHopTraversal id goIxedTest (IndexHop (encode 'b')) :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)])
-- *** Exception: withHopTraversal - unknown or unsupported Ixed type: t=Map Char Float, Index t= Float
-- @@
withHopTraversal ::
    forall s a r. (Data s, Typeable a)
    => (Traversal' s a -> r)
    -> (forall s a r. (Data s, Typeable a) => Proxy (s, a) -> Hop ByteString -> (Traversal' s a -> r) -> r)
    -> Hop ByteString
    -> r
withHopTraversal go _ h@(Field _cpos fpos) =
    -- Build a function to extract the requested field
    go (onceUpon' (gmapQi (pred fpos) go'))
    where
      -- Use cast to verify that the extracted field type is @a@
      go' :: forall b. Data b => b -> a
      go' b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
withHopTraversal go _ h@(TupleHop n) =
    -- Build a function to extract the requested tuple field
    go (onceUpon' (gmapQi (pred n) go'))
    where
      go' :: forall b. Data b => b -> a
      go' b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
withHopTraversal go goIxed3 h@(IndexHop bytes) = goIxed3 (Proxy :: Proxy (s, a)) h go

-- | withHopTraversal, extended to use the IxValue instance of s to get the hop type.
-- @@
-- λ> toListOf (withHopTraversalIndexed (IndexHop (encode (2 :: Int))) id :: Traversal' [Int] Int) [9,8,7,6]
-- [7]
-- λ> toListOf (withHopTraversalIndexed (IndexHop (encode (2 :: Int))) id :: Traversal' [String] String) ["a","b","c","d"]
-- ["c"]
-- λ> toListOf (withHopTraversalIndexed (IndexHop (encode 'b')) id :: Traversal' (Map Char Float) Float) (fromList [('a', 1.2), ('b', 1.5)] :: Map Char Float)
-- [1.5]
-- @@
withHopTraversalIndexed ::
    forall s r. (Data s, Ixed s, Typeable (Index s), Serialize (Index s), Typeable (IxValue s))
    => Hop ByteString
    -> (Traversal' s (IxValue s) -> r)
    -> r
withHopTraversalIndexed (IndexHop bytes) go =
    go (idx (decode bytes :: Either String (Index s)))
    where
      idx :: Either String (Index s) -> Traversal' s (IxValue s)
      idx (Left s) = error $ "Error decoding " ++ show bytes ++ " to " ++ show (typeRep (Proxy :: Proxy (Index s))) ++ ": " ++ s
      idx (Right k) = go' k
      go' :: Index s -> Traversal' s (IxValue s)
      go' k = castTraversal (Proxy :: Proxy (s, s, IxValue s, a)) (ix k)
withHopTraversalIndexed h go = withHopTraversal go goIxedNull h

goFieldNull ::
    forall s r. (Typeable s)
    => Proxy s
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
goFieldNull p h go = error "goIxedNull"

{-
goIxed3Test ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (forall d. Data d => Proxy d -> r)
    -> r
goIxed3Test p h go = (mkQ e f1 `extQ` f2) (Proxy :: Proxy (s, a))
-}

goIxedNull ::
    forall s a r. (Typeable s, Typeable a)
    => Proxy (s, a)
    -> Hop ByteString
    -> (Traversal' s a -> r)
    -> r
goIxedNull _ _ _ = error "goIxed2Null"

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

-- * Template Haskell

makeFieldQuery :: Name -> Q [Dec]
makeFieldQuery name = do
  hop <- newName "hop"
  go <- newName "go"
  s <- newName "s"
  r <- newName "r"
  (ClassI _ ixedInstances) <- reify ''Ixed
  ixedTypes <- mapM (\(n, InstanceD _ _ (AppT (ConT _) typ) _) -> (,) <$> newName ("f" ++ show n) <*>  pure typ) (zip ([1..] :: [Int]) ixedInstances)
  fieldDecs <- makeFieldDecs (varE hop) (varE go) (varT r) ixedTypes
  sequence
    [ sigD name [t| forall s a r. (Typeable s, Typeable a) => Proxy (s, a) -> Hop ByteString -> (forall d. Data d => Proxy d -> r) -> r |]
    , funD name [clause [] (normalB [|mkQ (error "No Ixed instance for " ++ show (typeRep (Proxy :: Proxy s))) $(buildQuery ixedTypes)|])
                   (fmap pure fieldDecs)] ]
    where
      buildQuery :: [(Name, Type)] -> ExpQ
      buildQuery [] = error "No Ixed instances found!"
      buildQuery [(fn, _)] = varE fn
      buildQuery ((fn, _) : ts) = [|$(varE fn) `extQ` $(buildQuery ts)|]

      makeFieldDecs :: ExpQ -> ExpQ -> TypeQ -> [(Name, Type)] -> Q [Dec]
      makeFieldDecs hop go r types = mapM (makeFieldFun hop go r) types

      makeFieldFun :: ExpQ -> ExpQ -> TypeQ -> (Name, Type) -> Q Dec
      makeFieldFun hop go r (fname, ixed) = do
        funD fname [clause [] (normalB [|withHopTypeIndexed $hop $go :: Proxy ($(pure ixed), IxValue $(pure ixed)) -> $r|]) []]

makeIxedFun :: TypeQ -> TypeQ -> ExpQ -> TypeQ -> Q Dec
makeIxedFun s a hop ixed = do
  f <- newName "f"
  p <- newName "p"
  funD f [clause [varP p] (normalB [|withHopTraversalIndexed $hop (castTraversal ($(varE p) :: Proxy ($ixed, IxValue $ixed))) :: Traversal' $s $a|]) []]
