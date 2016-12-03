module Lujvo where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.List (minimumBy, sort)
import Lojban
import Rafsi

-- While making, fixing, and scoring lujvo, we will treat them
-- as lists of alternating rafsi and hyphens. For example,
-- `pairkamnycmi` is represented as
--
--     [("pai", "r"), ("kamn", "y"), ("cmi", "")]
--
-- Let's call such a representation a "proto-lujvo".

type ProtoLujvo = [(Rafsi, Hyphen)]

------------------------------------------------------------
-- Hyphens
------------------------------------------------------------

type Hyphen = String

-- Is it illegal for one letter to follow another?
-- (The second one is always a consonant.)
clash :: Char -> Char -> Bool
clash a b =
  not (isVowel a || isMiddleCluster [a, b])

-- Which consonant hyphen should be used *before* this rafsi?
consonantHyphen :: Rafsi -> Hyphen
consonantHyphen r =
  case head r of
    'r' -> "n"
    _   -> "r"

hyphenate :: [Rafsi] -> ProtoLujvo
hyphenate rs@(r1:rs'@(r2:_:_))
  | rafsiForm r1 == CVV
  = (r1, consonantHyphen r2) : hyphenateRest rs'
hyphenate [r1, r2]
  | rafsiForm r1 == CVV && rafsiForm r2 /= CCV
  = [(r1, consonantHyphen r2), (r2, "")]
hyphenate rs = hyphenateRest rs

hyphenateRest :: [Rafsi] -> ProtoLujvo
hyphenateRest [r] = [(r, "")]
hyphenateRest (r1:(rs'@(r2:_))) =
  let
    needsHyphen =
      clash (last r1) (head r2)
      || rafsiForm r1 == FourLetter
    hyphen =
      if needsHyphen then "y" else ""

  in (r1, hyphen) : hyphenateRest rs'

glueLujvo :: ProtoLujvo -> Lujvo
glueLujvo p = concat [r ++ h | (r, h) <- p]

------------------------------------------------------------
-- The "tosmabru test"
------------------------------------------------------------

-- 1. Verify that the word starts with one or more CVC rafsi, and either
--
--	    (a) the final such rafsi has a y hyphen, or
--
--	    (b) these rafsi are followed by a CVCCV rafsi whose cluster is a
--	    valid initial pair.
--
--    If this is not the case, stop here.
--
-- 2. Consider all the rafsi-hyphen pairs in the entire word up to and
--    including the first one with a y hyphen (or until the end of the word).
--
-- 3. Find all consonant pairs across un-hyphenated rafsi boundaries in this
--    portion of the word. (These are "joints".)
--
-- 4. If all of those pairs are permissible initials, add a y hyphen to the
--    first rafsi.

-- | Determine if a proto-lujvo passes step 1 of the test.
tosmabruVerify :: ProtoLujvo -> Bool
tosmabruVerify [_] = False
tosmabruVerify ((r1, h1):t@((r2, h2):_)) =
  case (rafsiForm r1, rafsiForm r2) of
    (CVC, CVC) ->
      tosmabruVerify t
    (CVC, _) ->
      map letterClass r2 == [Consonant, Vowel, Consonant, Consonant, Vowel]
        && isInitialCluster [r2 !! 2, r2 !! 3]
        || h1 == "y"
    (_, _) ->
      False

-- | Fix a proto-lujvo, if possible and necessary.
tosmabruFix :: ProtoLujvo -> ProtoLujvo
tosmabruFix p =
  let
    takeWhileIncluding f [] = []
    takeWhileIncluding f (x:xs) =
      if f x then x : takeWhileIncluding f xs
             else [x]
    pairs x = x `zip` tail x
    untilY = takeWhileIncluding (\(_, h) -> h /= "y") p
    joints = [j | ((r1, h), (r2, _)) <- pairs untilY,
                  let j = [last r1, head r2],
                  null h && all isConsonant j]
    needsFix = all isInitialCluster joints
  in
    if tosmabruVerify p && needsFix then
      case p of ((r, h):t) -> (r, "y"):t
    else
      p

------------------------------------------------------------
-- Rafsi picking
------------------------------------------------------------

rafsiChoices :: Tanru -> [[Rafsi]]
rafsiChoices gs =
  let
    isValidInInit r = rafsiForm r `elem` [CVV, CCV, CVC, FourLetter]
    isValidInLast r = rafsiForm r `elem` [CVV, CCV, FiveLetter]
    rafsiChoices =
         [filter isValidInInit $ rafsiFor g | g <- init gs]
      ++ [filter isValidInLast $ rafsiFor $ last gs]
  in
    sequence rafsiChoices   -- Cartesian product of n lists

------------------------------------------------------------
-- Lujvo scoring
------------------------------------------------------------

rafsiScore :: Rafsi -> Int
rafsiScore r =
  case rafsiForm r of
    FiveLetter -> if isVowel (r !! 1) then 1 else 3
    FourLetter -> if isVowel (r !! 2) then 2 else 4
    CVC        -> 5
    CCV        -> 7
    CVV        -> if '\'' `elem` r then 6 else 8

protoLujvoScore :: ProtoLujvo -> Int
protoLujvoScore p =
  let lujvo = glueLujvo p
      l = length lujvo
      a = length $ filter (== '\'') lujvo
      h = sum $ map (length . snd) p
      r = sum $ map (rafsiScore . fst) p
      v = length $ filter isVowel lujvo

  in 1000 * l - 500 * a + 100 * h - 10 * r - v

------------------------------------------------------------
-- Lujvo-making!
------------------------------------------------------------

allProtoLujvo :: Tanru -> [ProtoLujvo]
allProtoLujvo =
  map (tosmabruFix . hyphenate) . rafsiChoices

bestProtoLujvo :: Tanru -> ProtoLujvo
bestProtoLujvo =
  minimumBy (comparing protoLujvoScore) . allProtoLujvo

scoredLujvoList :: Tanru -> [(Int, Lujvo)]
scoredLujvoList t =
  sort [(protoLujvoScore p, glueLujvo p) | p <- allProtoLujvo t]

bestLujvo :: Tanru -> Lujvo
bestLujvo =
  glueLujvo . bestProtoLujvo

