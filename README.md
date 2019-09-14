# egison-trs

Egison Journal vol.2「Egisonで項書き換え系」のレポジトリです

## Knuth-Bendix Completion
```
$ egison -l completion.egi
> (complete axioms-of-groups) ; takes about 30 minutes
```

Completion result:
```
$ egison -l completion.egi
> (show-equations axioms-of-groups-completion-result)
"{e * x = x,
 i(x) * x = e,
 (x * y) * z = x * y * z,
 i(x) * x * z = z,
 i(e) * w = w,
 i(i(w)) * e = w,
 i(x4 * x5) * x4 * x5 * w = w,
 i(i(x)) * z = x * z,
 i(x * e) * x * x5 = x5,
 i(x * i(x5)) * x * e = x5,
 i(i(z * x4) * z) * e = x4,
 i(x6 * x7 * x5) * x6 * x7 * x5 * x4 = x4,
 i(x * i(x5)) * x * x7 = x5 * x7,
 i(i(x * z) * x) * x4 = z * x4,
 y * i(y) = e,
 y * i(y) * x4 = x4,
 y * e = y,
 i(x4 * x5 * i(z)) * x4 * x5 = z,
 i(w * i(z)) * w = z,
 i(i(x4 * x5 * w) * x4 * x5) = w,
 i(i(x5) * i(w)) = w * x5,
 i(x * i(x4 * x6) * x4) * x = x6,
 i(x * x8 * x9 * x7) * x * x8 * x9 * x7 * x6 = x6,
 i(x * z * i(x7)) * x * z * x9 = x7 * x9,
 i(x * i(x7 * x9) * x7) * x * x11 = x9 * x11,
 w * i(y * w) * y = e,
 w * i(y * w) * y * x6 = x6,
 w * i(i(x6) * w) = x6,
 i(w * i(y * w)) = y,
 w * i(x6 * x8 * w) * x6 * x8 * x10 = x10,
 w * i(i(x7) * w) * x5 = x7 * x5,
 i(e) = e,
 w * x4 * i(w * x4) = e,
 i(z * x) * z = i(x),
 w * i(i(i(w))) = e,
 i(z * x4 * x) * z * x4 = i(x),
 i(x * i(x5)) = x5 * i(x),
 x4 * x5 * i(x4 * x5) * y = y,
 i(w * x) * w * y = i(x) * y,
 i(i(y) * x) = i(x) * y,
 i(i(x)) = x,
 i(y * x) = i(x) * i(y)}"
```
