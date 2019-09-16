# egison-trs

Egison Journal vol.2「Egisonで項書き換え系」のレポジトリです

## Contents
* `src-egi/` : Egison implementation of Knuth-Bendix completion
* `src-hs/` : Haskell implementation of Knuth-Bendix completion

## How to run completion
### Egison
```
$ egison
> (load-file "completion.egi")
> (show-equations (complete axioms-of-group)) ; takes about 5 minutes
```

### Haskell
```
$ ghci
> :l Completion.hs
> complete ["e", "*", "i"] axiomsOfGroup
```

## Acknowledgement
Part of the egison program was authored by [Yuichi Nishiwaki](https://github.com/nyuichi).
