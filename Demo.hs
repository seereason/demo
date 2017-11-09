{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}

import Control.Lens hiding (Strict)
import Data.Data.Lens (onceUpon')
import Data.Generics -- (cast, Data, empty, Typeable, typeRep)
import Data.Generics.Uniplate.Operations
import Data.Maybe (fromMaybe, listToMaybe)
import Language.Haskell.TH (Name, mkName)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (OccName(..), NameFlavour(..))
import Test.HUnit hiding (Testable)

data Key = K_Int Int | K_String String | K_Void () deriving (Show, Data)

unKey :: Biplate Key a => Key -> Maybe a
unKey x =
    case childrenBi x of
      [a] -> Just a
      _ -> Nothing

data Hop key
    = Field {_cpos :: Int, _fpos :: Int}
      -- ^ Hop to one of the fields of a record - the constructor and
      -- field number are specified.
    | TupleHop {_tpos :: Int} -- ^ Hop to nth element of a tuple
    | IndexHop {_key :: key} -- ^ Hop from an instance of 'Index', such as Map, via some key value
    deriving (Show, Functor)

-- | The ByteString is a serialized value of type @Index s@
data TraversalPath s a = TraversalPath {_traversalPathHops :: [Hop Key]}

traversalFromPath ::
    forall s a. (Data s, Data a)
    => TraversalPath s a -> Traversal' s a
traversalFromPath (TraversalPath []) =
    castTraversal (Proxy :: Proxy (s, s, s, a)) id
traversalFromPath (TraversalPath (h : hs)) =
    withHopType (Proxy :: Proxy s) h go
      (error $ "bad hop from " ++ show (typeRep (Proxy :: Proxy s)) ++ " : " ++ show h)
    where
      go :: forall b. Data b => Proxy b -> Traversal' s a
      go _ =
          (traversalFromHop h :: Traversal' s b) .
          (traversalFromPath (TraversalPath hs) :: Traversal' b a)

{-
withDecodedHop ::
    forall s r. (Data s, Data (Index s), Serialize (Index s), Show (Index s))
    => Proxy s -> Hop Key -> (Maybe (Hop (Index s)) -> r) -> r
withDecodedHop proxy h f = f (key :: Maybe (Hop (Index s)))
-}

-- | Apply the proxy type @a@ resulting from doing a hop from @s@ to a
-- function.
withHopType ::
    forall s r. (Data s)
    => Proxy s
    -> Hop Key
    -> (forall a. Data a => Proxy a -> r)
    -> r -> r
withHopType _p (Field cpos fpos) go r0 =
    fromMaybe r0 (fmap (gmapQi (pred fpos) go') c)
    where
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
      -- Get the value from 'constrs' corresponding to constructor
      -- at position cpos
      c :: Maybe s
      c = listToMaybe $ drop (pred cpos) (constrs :: [s])
withHopType _p (TupleHop n) go r0 =
    fromMaybe r0 $ fmap (gmapQi (pred n) go') (empty :: Maybe s)
    where
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
withHopType _p h@(IndexHop key) go r0 =
    case (show (typeRep (Proxy :: Proxy s)), key) of
      ("[Char]", K_Int n) -> go (Proxy :: Proxy Char)
      ("Name", K_Void ()) -> go (Proxy :: Proxy ())
      _ -> error $ "withHopType (Proxy " ++ show (typeRep (Proxy :: Proxy s)) ++ ") " ++ show h

-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
traversalFromHop :: forall s a. (Data s, Data a) => Hop Key -> Traversal' s a
traversalFromHop h@(Field _cpos fpos) =
    -- gmapQi will do a map on the specified field value.  If the cast
    -- fails to produce the type that gmapQi expects it means the hop
    -- value does not match the field type.
    onceUpon' (gmapQi
                 (pred fpos)
                 (\b -> fromMaybe
                          (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h)
                          ((cast :: forall b. Data b => b -> Maybe a) b)))
traversalFromHop (TupleHop n) =
    onceUpon' field
    where
      field :: s -> a
      field = gmapQi (pred n) (fromMaybe (error "invalid hop 1") . cast)
traversalFromHop (IndexHop key) =
    case (show (typeRep (Proxy :: Proxy s)), key) of
      ("[Char]", K_Int n) -> castTraversal (Proxy :: Proxy ([Char], s, Char, a)) (ix n)
      _ -> error "traversalFromHop"

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

-- TESTS

h1, h2 :: Hop Key -- Int
[h1, h2] = recordHops (Proxy :: Proxy Name)
h3 :: Hop Key -- Int
[h3] = recordHops (Proxy :: Proxy OccName)
h4 :: Hop Key
h4 = fmap K_Int (IndexHop (0 :: Int))

-- | Return a list of possible hops from a record type.  The key type
-- parameter is not used here.  Because the return type is not an
-- IndexHop the key type can be anything.
recordHops :: forall t key. (Data t) => Proxy t -> [Hop key]
recordHops _ =
  concatMap (uncurry fieldHops) cs
    where
      fieldHops :: Int -> t -> [Hop key]
      fieldHops ci t =
          let fs = gmapQ dataTypeOf t in
          fmap (uncurry (fieldHop ci t)) (zip [1..] fs)
      fieldHop :: forall key. Int -> t -> Int -> DataType -> Hop key
      fieldHop ci _t fi _f = Field ci fi
      cs :: [(Int, t)]
      cs = zip [1..] constrs

testCase :: (Eq a, Show a) => String -> a -> a -> Test
testCase name actual expected =
    TestLabel name (TestCase (assertEqual name expected actual))

tests :: Test
tests =
    TestList
    [ testCase "traversalFromHops1" (toListOf (traversalFromPath (TraversalPath [h1]) :: Traversal' Name OccName) (mkName "a")) [OccName "a"]
    , testCase "traversalFromHops2" (toListOf (traversalFromPath (TraversalPath [h2]) :: Traversal' Name NameFlavour) (mkName "a")) [NameS]
    , testCase "traversalFromHops3" (toListOf (traversalFromPath (TraversalPath [h1, h3]) :: Traversal' Name String) (mkName "a")) ["a"]
    , testCase "traversalFromHops4" (toListOf (traversalFromPath (TraversalPath [h1, h3, h4]) :: Traversal' Name String) (mkName "a")) ["a"]
    ]
