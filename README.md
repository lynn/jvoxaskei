# la jvoxaskei
Haskell implementation of the Lojban lujvo-making algorithm.

## Usage

```haskell
-- % ghci Lujvo.hs
-- GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
-- [1 of 3] Compiling Lojban           ( Lojban.hs, interpreted )
-- [2 of 3] Compiling Rafsi            ( Rafsi.hs, interpreted )
-- [3 of 3] Compiling Lujvo            ( Lujvo.hs, interpreted )
-- Ok, modules loaded: Lujvo, Lojban, Rafsi.
>>> let tanru = ["lujvo", "xamsi", "kelci"]
>>> mapM_ print $ take 5 $ scoredLujvoList tanru
(8796,"jvoxaskei")
(9916,"luvyxaskei")
(10866,"jvoxaskelci")
(10906,"jvoxamsykei")
(10926,"lujvyxaskei")
>>> bestLujvo tanru
"jvoxaskei"
>>> bestLujvo ["tisna", "mabru"]
"tisymabru"
```
