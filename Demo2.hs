-- | Example setup:
-- @@
-- % ghci
-- λ> :set -XStandaloneDeriving
-- λ> :set -XDeriveDataTypeable
-- λ> :m +System.Process
-- λ> :m +System.Posix.Types
-- λ> :m +Data.Generics
-- λ> deriving instance Data CUid
-- λ> deriving instance Data CGid
-- λ> deriving instance Data CmdSpec
-- λ> deriving instance Data StdStream
-- λ> deriving instance Data CreateProcess
-- λ> take 80 $ show $ empty :: CreateProcess
-- "CreateProcess {cmdspec = ShellCommand \"\", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = "
-- λ> constrs :: [CmdSpec]
-- [ShellCommand "",RawCommand "" []]
-- @@
-- Some warmups:
-- @@
-- λ> take 8 (gfoldMap @Data typeNameList (head (constrs :: [CreateProcess])))
-- ["CmdSpec","Maybe [Char]","Maybe [([Char],[Char])]","StdStream","StdStream","StdStream","Bool","Bool"]
-- λ> map (gfoldMap @Data typeNameList) (constrs :: [CmdSpec])
-- [["[Char]"],["[Char]","[[Char]]"]]
-- λ> map (gfoldMap @Data typeNameList) (constrs :: [CmdSpec])
-- [["[Char]"],["[Char]","[[Char]]"]]
-- @@

{-# OPTIONS -Wall #-}
{-# LANGUAGE CPP, ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Data.Lens
import Data.Maybe (fromMaybe, listToMaybe)
import Data.ByteString (ByteString)
import Data.Generics (Data, eqT, (:~:)(Refl), Proxy(Proxy), typeRep, Typeable, (:~:), cast, gshow, empty, constrs)
import Data.Generics.Traversable -- (gfoldr)
import Data.Generics.Traversable.TH
import Data.Serialize (decode, encode, Serialize)

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant

-- * Imports and instances for the examples

import System.Process
import System.Posix.Types
import GHC.IO.Handle

deriving instance Data CUid
deriving instance Data CGid
deriving instance Data CmdSpec
deriving instance Data StdStream
deriving instance Data CreateProcess

$(deriveGTraversable ''CreateProcess)
$(deriveGTraversable ''CmdSpec)
$(deriveGTraversable ''StdStream)

typeNameList :: forall a. Typeable a => a -> [String]
typeNameList _ = [show (typeRep (Proxy :: Proxy a))]

-- | Show the names of (t, Index t, IxValue t)
-- @@
-- λ> fmap (gfoldMap @Pathy ixedTypeNames) (constrs :: [StdStream])
-- [("","",""),("Handle","()","Handle"),("","",""),("","","")]
-- @@
ixedTypeNames :: forall a. (Pathy a, Typeable a, Typeable (PathIndex a), Typeable (PathIxValue a)) => a -> (String, String, String)
ixedTypeNames _ = (show (typeRep (Proxy :: Proxy a)),
                   show (typeRep (Proxy :: Proxy (PathIndex a))),
                   show (typeRep (Proxy :: Proxy (PathIxValue a))))

-- λ> map (gfoldMap @Data typeNameList) (constrs :: [CmdSpec])
-- [["[Char]"],["[Char]","[[Char]]"]]

data Hop key
    = Field {_cpos :: Int, _fpos :: Int}
      -- ^ Hop to one of the fields of a record - the constructor and
      -- field number are specified.
    | TupleHop {_tpos :: Int} -- ^ Hop to nth element of a tuple
    | IndexHop {_key :: key} -- ^ Hop from an instance of 'Index', such as Map, via some key value
    deriving (Show, Functor)

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

-- | A type that can participate in a path due to its 'Ixed' instance and
-- accompanying 'Index' and 'IxValue' type family instances.
-- @@
-- λ> gfoldr @Pathy (\x r -> ixedTypeNames x : r) [] (empty :: CmdSpec)
-- [("[Char]","Int","Char")]
-- λ> gfoldr @Pathy (\x r -> ixedTypeNames x : r) [] (empty :: CreateProcess)
-- [("CmdSpec","()","CmdSpec"),("Maybe [Char]","()","[Char]"),("Maybe [([Char],[Char])]","()","[([Char],[Char])]"),
--  ("StdStream","()","StdStream"),("StdStream","()","StdStream"),("StdStream","()","StdStream"),("Bool","()","Bool"),
--  ("Bool","()","Bool"),("Bool","()","Bool"),("Bool","()","Bool"),("Bool","()","Bool"),("Bool","()","Bool"),
--  ("Maybe CGid","()","CGid"),("Maybe CUid","()","CUid")]
-- @@
#if 0
class (Data a, Typeable a, Ixed a,
       Data (Index a), Typeable (Index a),
       Data (IxValue a), Typeable (IxValue a), Ixed (IxValue a)) => Pathy a

instance Pathy CreateProcess
instance Ixed CreateProcess where ix () = id
type instance Index CreateProcess = ()
type instance IxValue CreateProcess = CreateProcess

-- instance Pathy a => Pathy [a]
instance Pathy [(String, String)]
instance Pathy [String]
instance Pathy String
instance Pathy a => Pathy (Maybe a)

instance Pathy CmdSpec
instance Ixed CmdSpec where ix () = id
type instance Index CmdSpec = ()
type instance IxValue CmdSpec = CmdSpec

instance Pathy StdStream
instance Ixed StdStream where ix () = id
type instance Index StdStream = ()
type instance IxValue StdStream = StdStream

instance Pathy Char
instance Ixed Char where ix () = id
type instance Index Char = ()
type instance IxValue Char = Char

instance Pathy Bool
instance Ixed Bool where ix () = id
type instance Index Bool = ()
type instance IxValue Bool = Bool

instance Pathy CGid
instance Ixed CGid where ix () = id
type instance Index CGid = ()
type instance IxValue CGid = CGid

instance Pathy CUid
instance Ixed CUid where ix () = id
type instance Index CUid = ()
type instance IxValue CUid = CUid

instance Pathy Handle
instance Ixed Handle where ix () = id
type instance Index Handle = ()
type instance IxValue Handle = Handle

instance (Pathy a, Pathy b) => Pathy (a, b)
-- instance (Pathy a, Pathy b, Pathy c) => Pathy (a, b, c)
-- instance (Pathy a, Pathy b, Pathy c, Pathy d) => Pathy (a, b, c, d)
-- instance (Pathy a, Pathy b, Pathy c, Pathy d, Pathy e) => Pathy (a, b, c, d, e)
#else
class (Data p, Typeable p,
       Data (PathIndex p), Typeable (PathIndex p), Serialize (PathIndex p),
       Data (PathIxValue p), Typeable (PathIxValue p)) => Pathy p where
  type PathIndex p
  type PathIxValue p
  pix :: PathIndex p -> Traversal' p (PathIxValue p)

instance Pathy CreateProcess where
    type PathIndex CreateProcess = ()
    type PathIxValue CreateProcess = CreateProcess
    pix () = id

instance (Pathy a, Pathy b) => Pathy (a, b) where
    type PathIndex (a, b) = ()
    type PathIxValue (a, b) = (a, b)
    -- pix 1 = view _1
    -- pix 2 = view _2

instance (Pathy a, Pathy b, Pathy c) => Pathy (a, b, c) where
    type PathIndex (a, b, c) = ()
    type PathIxValue (a, b, c) = (a, b, c)
    -- pix 1 = view _1
    -- pix 2 = view _2
    -- pix 3 = view _3

instance Pathy a => Pathy (Maybe a) where
    type PathIndex (Maybe a) = ()
    type PathIxValue (Maybe a) = a
    pix = ix

instance (Pathy a, Pathy (PathIxValue a))  => Pathy [a] where
    type PathIndex [a] = Int
    type PathIxValue [a] = a
    pix = ix

instance Pathy Char where
    type PathIndex Char = ()
    type PathIxValue Char = Char
    pix () = id

instance Pathy Int where
    type PathIndex Int = ()
    type PathIxValue Int = Int
    pix () = id

instance Pathy Float where
    type PathIndex Float = ()
    type PathIxValue Float = Float
    pix () = id

instance Pathy Bool where
    type PathIndex Bool = ()
    type PathIxValue Bool = Bool
    pix () = id

instance Pathy CGid where
    type PathIndex CGid = ()
    type PathIxValue CGid = CGid
    pix () = id

instance Pathy CUid where
    type PathIndex CUid = ()
    type PathIxValue CUid = CUid
    pix () = id

instance Pathy CmdSpec where
    type PathIndex CmdSpec = ()
    type PathIxValue CmdSpec = CmdSpec
    pix () = id

instance Pathy StdStream where
    type PathIndex StdStream = ()
    type PathIxValue StdStream = StdStream
    pix () = id

instance Pathy Handle where
    type PathIndex Handle = ()
    type PathIxValue Handle = Handle
    pix () = id
#endif

-- | Map over the nth subterm.
-- @@
-- λ> gmapi @Pathy 0 ixedTypeNames (empty :: CreateProcess)
-- Just ("CmdSpec","()","CmdSpec")
-- λ> gmapi @Pathy 1 ixedTypeNames (empty :: CreateProcess)
-- Just ("Maybe [Char]","()","[Char]")
-- λ> gmapi @Pathy 6 ixedTypeNames (empty :: CreateProcess)
-- Just ("Bool","()","Bool")
-- λ> gmapi @Pathy 14 ixedTypeNames (empty :: CreateProcess)
-- Nothing
-- @@
gmapi ::
    forall c a r. (GTraversable c a)
    => Int -> (forall d. (c d) => d -> r)
    -> a -> Maybe r
gmapi n f x =
  snd $ gfoldl' @c g (0 :: Int, Nothing) x
  where
    g :: forall d. (c d) => (Int, Maybe r) -> d -> (Int, Maybe r)
    g (i, r@(Just _)) _ = (i, r)
    g (i, Nothing) a | i == n = (succ i, Just (f a))
    g (i, Nothing) _ = (succ i, Nothing)

-- | The ByteString is a serialized value of type @Index s@.  The
-- Hop type parameter is ByteString, a serialized value.  The type of
-- that value is different for each hop, depending on the type of
-- value we are hoping from.  Specifically, if the hop is an IndexHop
-- and the type we are hopping from is @b@, the hop type parameter is
-- 'Index' @b@, the key type family of the 'Ixed' class.
data TraversalPath s a = TraversalPath {_traversalPathHops :: [Hop ByteString]}

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
-- λ> withHopType (Proxy :: Proxy [Int]) (IndexHop (encode (123 :: Int))) typeRep empty
-- Int
-- @@
withHopType ::
    forall s r. (Pathy s, GTraversable Data s)
    => Proxy s
    -> Hop ByteString
    -> (forall a. (Data a, Typeable a) => Proxy a -> r) -- ^ Hop destination type is applied to this function
    -> r -> r
withHopType _p (Field cpos fpos) go r0 =
    maybe r0 (fromMaybe r0 . gmapi @Data (pred fpos) go') c
    -- fromMaybe r0 (fmap (gmapi @Data (pred fpos) go') c)
    where
      -- Get a value from 'constrs' corresponding to the constructor
      -- at position cpos.  Will be Nothing if cpos is out of range.
      c :: Maybe s
      c = listToMaybe $ drop (pred cpos) (constrs :: [s])
      -- Turn the empty hop destination value into a type and apply to go
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
-- Tuples with types like (a, a, a) are Ixed instances, but (a, b, c)
-- are not.
withHopType _p (TupleHop n) go r0 =
    fromMaybe r0 $ gmapi @Data (pred n) go' (empty :: s)
    where
      go' :: forall b. Data b => b -> r
      go' _ = go (Proxy :: Proxy b)
withHopType _p (IndexHop _) go _ =
    go (Proxy :: Proxy (PathIxValue s))

-- | Turn a hop which we know will go from @s -> a@ into a Traversal'.
-- @@
-- λ> toListOf (traversalFromHop (Field 2 2) :: Traversal' CmdSpec [[Char]]) (constrs !! 1)
-- [[]]
-- λ> toListOf (traversalFromHop (Field 1 1) :: Traversal' CmdSpec [Char]) (constrs !! 0)
-- [""]
-- λ> toListOf (traversalFromHop (TupleHop 2) :: Traversal' (Int, Float, Char) Float) (1,1.5,'x')
-- [1.5]
-- λ> toListOf (traversalFromHop (IndexHop (encode (2 :: Int))) :: Traversal' [Int] Int) [0,1,2,3]
-- Int
-- @@
traversalFromHop :: forall s a. (Pathy s, Data a, GTraversable Data s) => Hop ByteString -> Traversal' s a
traversalFromHop h@(Field _cpos fpos) =
    -- gmapi will do a map on the specified field value.  If the cast
    -- fails to produce the type that gmapi expects it means the hop
    -- value does not match the field type.
    onceUpon' field
    where
      -- Build a function to extract the requested field
      field :: s -> a
      field s =
          fromMaybe (error $ "invalid field hop for " ++ show (typeRep (Proxy :: Proxy s)) ++
                             " -> " ++ show (typeRep (Proxy :: Proxy a)) ++ ": " ++ show h)
                    (gmapi @Data (pred fpos) go s)
      -- Use cast to verify that the extracted field type is @a@
      go :: forall b. Data b => b -> a
      go b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
traversalFromHop h@(TupleHop n) =
    onceUpon' field
    where
      field :: s -> a
      field s =
          fromMaybe (error $ "invalid hop for " ++ show (typeRep (Proxy :: Proxy s)) ++
                             " -> " ++ show (typeRep (Proxy :: Proxy a)) ++ ": " ++ show h)
                    (gmapi @Data (pred n) go s)
      go :: forall b. Data b => b -> a
      go b = fromMaybe (error $ "invalid field hop for " ++ gshow b ++ ": " ++ show h) (cast b)
traversalFromHop (IndexHop key) =
    idx (decode key :: Either String (PathIndex s))
    where
      idx :: Either String (PathIndex s) -> Traversal' s a
      idx (Left s) = error $ "Error decoding " ++ show key ++ " to " ++ show (typeRep (Proxy :: Proxy (PathIndex s))) ++ ": " ++ s
      idx (Right k) = go k
      go :: PathIndex s -> Traversal' s a
      go k = castTraversal (Proxy :: Proxy (s, s, PathIxValue s, a)) (pix k)

traversalFromPath ::
    forall s a. (Pathy s, Pathy a, Typeable a, GTraversable Data s, GTraversable Pathy s)
    => TraversalPath s a -> Traversal' s a
traversalFromPath (TraversalPath []) =
    castTraversal (Proxy :: Proxy (s, s, s, a)) id
traversalFromPath (TraversalPath (h : hs)) =
    withHopType (Proxy :: Proxy s) h go (error "traversalFromPath")
    where
      go :: forall b. (Pathy b, GTraversable Data b, GTraversable Pathy b) => Proxy b -> Traversal' s a
      go _ =
          (traversalFromHop h :: Traversal' s b) .
          (traversalFromPath (TraversalPath hs) :: Traversal' b a)
