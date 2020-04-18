# egison-trs

技術書典7 Egison Journal vol.2「Egisonで項書き換え系」のレポジトリです

## Contents
* `src-egi` : Egison implementation of Knuth-Bendix completion
* `src-hs` : Haskell implementation of Knuth-Bendix completion

## How to run completion
### Egison

Requirement: Egison v4.0.0

```
$ cd src-egi
$ egison
> loadFile "completion.egi"
> showEquations (complete ["e", "*", "i"] axiomsOfGroup) ; takes about 5 minutes
```

### Haskell
```
$ cd src-hs
$ ghci
> :l Completion.hs
> complete ["e", "*", "i"] axiomsOfGroup
```

## Acknowledgement
Part of the egison program was authored by [Yuichi Nishiwaki](https://github.com/nyuichi).
