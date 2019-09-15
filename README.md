# egison-trs

Egison Journal vol.2「Egisonで項書き換え系」のレポジトリです

## Knuth-Bendix Completion
### How to run
```
$ egison
> (load-file "completion.egi")
> (complete axioms-of-groups) ; takes about 5 minutes
```

### Result
interreduced:
```
> (load-file "completion.egi")
> (show-equations axioms-of-groups-completion-result-interreduced)
```

not interreduced:
```
> (load-file "completion.egi")
> (show-equations axioms-of-groups-completion-result)
```
