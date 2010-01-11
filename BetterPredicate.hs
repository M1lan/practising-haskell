-- -*- haskell -*-
-- BetterPredicate.hs slightly modified tutorial code from Real World Haskell, Chapter 9

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.OldException (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath        -- path to directory entry
               -> Permissions    -- permissions
               -> Maybe Integer -- file size
               -> ClockTime      -- last modified
               -> Bool

-- "soon to be defined"(???)

getFileSize :: FilePath -> IO (Maybe Integer)

betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

--

simpleFileSize :: FilePath -> IO Integer

simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

--

saferFileSize :: FilePath -> IO (Maybe Integer)

saferFileSize path = handle (\_ -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

--

getFileSize path = handle (\_ -> return Nothing) $ 
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

-- A small test: search for all c++ source files!
myTest path _ (Just size) _ = 
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False 
-- The predicate takes four arguments, always ignores two of them and requires two equations to define :-(



-- An embedded domain specific language for Predicates!

type InfoP a = FilePath         -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size
             -> ClockTime       -- last modified
             -> a

pathP :: InfoP FilePath
-- This function extracts the path from the arguments passed to a Predicate
pathP path _ _ _ = path

-- 

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

-- Anonymous function passes arguments to f and compares results to k

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

-- This function behaves identically, but makes use of currying

equalP'  :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

-- Example of Lifting

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- Combine predicates simple way
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

-- Use lifting to do the same and even more
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

-- LiftP in terms of LiftP2:
constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z

-- Another test, this time with our combitators
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

-- defining new infix operators
(==?) = equalP
(&&?) = andP
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

-- paren free with fixities
infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
