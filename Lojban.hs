module Lojban where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import qualified Data.Set as S
import Data.Set (Set)

type Valsi = String
type Gismu = String
type Rafsi = String
type Lujvo = String
type Tanru = [Gismu]

vowels :: Set Char
vowels = S.fromList "aeiou"

isVowel :: Char -> Bool
isVowel = (`S.member` vowels)

consonants :: Set Char
consonants = S.fromList "bcdfgjklmnprstvxz"

isConsonant :: Char -> Bool
isConsonant = (`S.member` consonants)

initialClusters :: Set String
initialClusters =
  S.fromList
    ["ct","cp","cf","ck","cm","cn","cl","cr","jd","jb","jv","jg"
    ,"jm","st","sp","sf","sk","sm","sn","sl","sr","zd","zb","zv"
    ,"zg","zm","tc","ts","tr","dj","dz","dr","pl","pr","bl","br"
    ,"fl","fr","vl","vr","kl","kr","gl","gr","xl","xr","ml","mr"]

isInitialCluster :: String -> Bool
isInitialCluster = (`S.member` initialClusters)

middleClusters :: Set String
middleClusters =
  S.fromList
    ["ct","cp","cf","ck","cm","cn","cl","cr","jd","jb","jv","jg"
    ,"jm","jn","jl","jr","st","sp","sf","sk","sx","sm","sn","sl"
    ,"sr","zd","zb","zv","zg","zm","zn","zl","zr","tc","ts","tp"
    ,"tf","tk","tx","tm","tn","tl","tr","dj","dz","db","dv","dg"
    ,"dm","dn","dl","dr","pc","ps","pt","pf","pk","px","pm","pn"
    ,"pl","pr","bj","bz","bd","bv","bg","bm","bn","bl","br","fc"
    ,"fs","ft","fp","fk","fx","fm","fn","fl","fr","vj","vz","vd"
    ,"vb","vg","vm","vn","vl","vr","kc","ks","kt","kp","kf","km"
    ,"kn","kl","kr","gj","gz","gd","gb","gv","gm","gn","gl","gr"
    ,"xs","xt","xp","xf","xm","xn","xl","xr","mc","mj","ms","mt"
    ,"md","mp","mb","mf","mv","mk","mg","mx","mn","ml","mr","nc"
    ,"nj","ns","nz","nt","nd","np","nb","nf","nv","nk","ng","nx"
    ,"nm","nl","nr","lc","lj","ls","lz","lt","ld","lp","lb","lf"
    ,"lv","lk","lg","lx","lm","ln","lr","rc","rj","rs","rz","rt"
    ,"rd","rp","rb","rf","rv","rk","rg","rx","rm","rn","rl"]

isMiddleCluster :: String -> Bool
isMiddleCluster = (`S.member` middleClusters)

data LetterClass
  = Consonant
  | Vowel
  | Yhybu
  deriving (Eq, Ord, Show)

letterClass :: Char -> LetterClass
letterClass c | isVowel c     = Vowel
letterClass c | isConsonant c = Consonant
letterClass '\''              = Yhybu
letterClass c                 = error ("Invalid letter " ++ show c)

data RafsiForm
  = CVV
  | CCV
  | CVC
  | FourLetter
  | FiveLetter
  deriving (Eq, Ord, Show)

rafsiForm :: Rafsi -> RafsiForm
rafsiForm r =
  case map letterClass r of
    [Consonant, Vowel, Vowel]        -> CVV
    [Consonant, Vowel, Yhybu, Vowel] -> CVV
    [Consonant, Consonant, Vowel]    -> CCV
    [Consonant, Vowel, Consonant]    -> CVC
    [_, _, _, _]                     -> FourLetter
    [_, _, _, _, _]                  -> FiveLetter
    _                                -> error ("Invalid rafsi " ++ show r)

