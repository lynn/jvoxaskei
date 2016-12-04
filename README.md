# la jvoxaskei
A Haskell implementation of the Lojban lujvo-making and lujvo-scoring algorithms.

## The Lojban *what?*
[**Lojban**](http://lojbo.org/) is a [constructed human language](https://en.wikipedia.org/wiki/Constructed_language). The root [content words](https://en.wikipedia.org/wiki/Content_word) in Lojban, such as `cabna` (current) and `djedi` (day), can be smashed together into [compound words](https://en.wikipedia.org/wiki/Compound_(linguistics)) called *lujvo*, such as `cabdei` (today). This process follows a non-deterministic algorithm called the *lujvo-making algorithm*. Another algorithm exists to score its outputs for brevity and *niceness*: the lujvo that is assigned the lowest score is often chosen as the dictionary form of a new compound word.

My program takes list of content words, generates all possible *lujvo*, optionally scores each of them, and then optionally selects the best result.

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
(10926,"jvoxamsykei")
(10946,"lujvyxaskei")
>>> bestLujvo tanru
"jvoxaskei"
>>> bestLujvo ["tisna", "mabru"]
"tisymabru"
```
