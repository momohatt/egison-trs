# egison-trs

Egison Journal vol.2「Egisonで項書き換え系」のレポジトリです

## Knuth-Bendix Completion
### How to run
```
$ egison
> (load-file "completion.egi")
> (complete axioms-of-groups) ; takes about 5 minutes
```

#### Instant completion result of `axioms-of-groups`
```
> (load-file "completion.egi")
> (show-equations axioms-of-groups-completion-result)
```

## Acknowledgement
Part of the program was authored by [Yuichi Nishiwaki](https://github.com/nyuichi).
