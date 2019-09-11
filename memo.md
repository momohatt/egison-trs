# 完備化のメモ

* PLARの実装で群の公理を完備化した時の結果

```
4 equations and 8 pending critical pairs + 0 deferred
5 equations and 12 pending critical pairs + 0 deferred
6 equations and 16 pending critical pairs + 0 deferred
7 equations and 27 pending critical pairs + 0 deferred
8 equations and 51 pending critical pairs + 0 deferred
9 equations and 70 pending critical pairs + 0 deferred
10 equations and 81 pending critical pairs + 0 deferred
11 equations and 78 pending critical pairs + 0 deferred
12 equations and 85 pending critical pairs + 0 deferred
13 equations and 114 pending critical pairs + 0 deferred
14 equations and 151 pending critical pairs + 0 deferred
15 equations and 180 pending critical pairs + 0 deferred
16 equations and 247 pending critical pairs + 0 deferred
17 equations and 298 pending critical pairs + 0 deferred
18 equations and 356 pending critical pairs + 0 deferred
19 equations and 404 pending critical pairs + 0 deferred
20 equations and 485 pending critical pairs + 0 deferred
21 equations and 530 pending critical pairs + 0 deferred
22 equations and 583 pending critical pairs + 0 deferred
23 equations and 642 pending critical pairs + 0 deferred
24 equations and 730 pending critical pairs + 0 deferred
25 equations and 779 pending critical pairs + 0 deferred
26 equations and 794 pending critical pairs + 0 deferred
27 equations and 819 pending critical pairs + 1 deferred
28 equations and 918 pending critical pairs + 1 deferred
29 equations and 901 pending critical pairs + 1 deferred
30 equations and 1005 pending critical pairs + 1 deferred
31 equations and 1086 pending critical pairs + 1 deferred
32 equations and 1155 pending critical pairs + 1 deferred
32 equations and 1000 pending critical pairs + 1 deferred
32 equations and 0 pending critical pairs + 1 deferred
32 equations and 0 pending critical pairs + 0 deferred

[<<(x * y) * z = x * y * z>>;
 <<i(x) * x = 1>>;
 <<1 * x = x>>;
 <<i(x1) * x1 * x2 = x2>>;
 <<i(i(x0)) * x1 = x0 * x1>>;
 <<i(1) * x1 = x1>>;
 <<x1 * 1 = x1>>;
 <<i(x2 * x3) * x2 * x3 * x1 = x1>>;
 <<x0 * i(x0) * x3 = x3>>;
 <<x0 * i(x0) = 1>>;
 <<i(1) = 1>>;
 <<i(i(x1)) = x1>>;
 <<i(x4 * i(x1 * x2)) * x4 * x0 = x1 * x2 * x0>>;
 <<i(i(x5 * x2) * x5) * x0 = x2 * x0>>;
 <<i(x1 * i(x3)) * x1 * x4 = x3 * x4>>;
 <<i(x1 * x3 * x4) * x1 * x3 * x4 * x0 = x0>>;
 <<i(x3 * x4) * x3 * x1 = i(x4) * x1>>;
 <<x2 * x3 * i(x2 * x3) * x1 = x1>>;
 <<x1 * x2 * i(x1 * x2) = 1>>;
 <<i(i(x3 * i(x1 * x2)) * i(x5 * x6)) * x1 * x2 * x0 = x5 * x6 * x3 * x0>>;
 <<i(x3 * i(x1 * x2)) = x1 * x2 * i(x3)>>;
 <<x1 * i(i(x4 * x5) * x1) * x3 = x4 * x5 * x3>>;
 <<x1 * i(x2 * x1) * x2 = 1>>;
 <<i(i(x4) * x2) * x0 = i(x2) * x4 * x0>>;
 <<i(i(x2 * x1) * x2) = x1>>;
 <<i(x0 * i(x1)) = x1 * i(x0)>>;
 <<i(x4 * x5 * x6 * x3) * x0 = i(x3) * i(x4 * x5 * x6) * x0>>;
 <<i(x3 * x5) * x0 = i(x5) * i(x3) * x0>>;
 <<x1 * i(i(x4) * i(x3) * x1) = x3 * x4>>;
 <<i(x4) * x1 * i(x3 * x1) = i(x4) * i(x3)>>;
 <<x1 * i(x5 * x1) = i(x5)>>;
 <<i(x4 * x5) = i(x5) * i(x4)>>]
```

* PLARの実装をHaskellに移植したもので完備化した結果
```
>>> complete ["e", "i", "*"] axiomsOfGroup
4 equations and 10 pending critical pairs; 0 deferred
i(x2) * x2 * x3 = x3
5 equations and 12 pending critical pairs; 0 deferred
i(i(x2)) * x3 = x2 * x3
6 equations and 16 pending critical pairs; 0 deferred
i(e) * x1 = x1
7 equations and 27 pending critical pairs; 0 deferred
x1 * e = x1
8 equations and 52 pending critical pairs; 0 deferred
i(x2 * x3) * x2 * x3 * x1 = x1
9 equations and 70 pending critical pairs; 0 deferred
x0 * i(x0) * x3 = x3
10 equations and 81 pending critical pairs; 0 deferred
x0 * i(x0) = e
11 equations and 78 pending critical pairs; 0 deferred
i(e) = e
12 equations and 85 pending critical pairs; 0 deferred
i(i(x1)) = x1
13 equations and 115 pending critical pairs; 0 deferred
i(x0 * i(x3 * x4)) * x0 * x5 = x3 * x4 * x5
14 equations and 152 pending critical pairs; 0 deferred
i(i(x1 * x4) * x1) * x5 = x4 * x5
15 equations and 181 pending critical pairs; 0 deferred
i(x0 * i(x3)) * x0 * x4 = x3 * x4
16 equations and 248 pending critical pairs; 0 deferred
i(x0 * x3 * x4) * x0 * x3 * x4 * x2 = x2
17 equations and 299 pending critical pairs; 0 deferred
i(x2 * x3) * x2 * x1 = i(x3) * x1
18 equations and 356 pending critical pairs; 0 deferred
x2 * x3 * i(x2 * x3) * x1 = x1
19 equations and 404 pending critical pairs; 0 deferred
x1 * x2 * i(x1 * x2) = e
20 equations and 486 pending critical pairs; 0 deferred
i(i(x4 * i(x5 * x6)) * i(x1 * x2)) * x5 * x6 * x7 = x1 * x2 * x4 * x7
21 equations and 565 pending critical pairs; 1 deferred
i(x0 * i(x1 * x2)) * x5 = x1 * x2 * i(x0) * x5             <- ここから違ってきてしまっている
22 equations and 623 pending critical pairs; 1 deferred
x1 * i(i(x4 * x5) * x1) * x6 = x4 * x5 * x6
23 equations and 685 pending critical pairs; 1 deferred
x1 * i(x0 * x1) * x0 = e
24 equations and 778 pending critical pairs; 1 deferred
i(i(x4) * x0) * x2 = i(x0) * x4 * x2
25 equations and 822 pending critical pairs; 1 deferred
i(i(x0 * x1) * x0) = x1
26 equations and 882 pending critical pairs; 2 deferred
i(x0 * i(x1)) * x4 = x1 * i(x0) * x4
27 equations and 915 pending critical pairs; 3 deferred
i(x3 * x4 * x5 * x6) * x2 = i(x6) * i(x3 * x4 * x5) * x2
28 equations and 1018 pending critical pairs; 3 deferred
i(x3 * x5) * x2 = i(x5) * i(x3) * x2
29 equations and 964 pending critical pairs; 4 deferred
i(x3 * i(x2 * x3)) = x2
30 equations and 1074 pending critical pairs; 4 deferred
i(x3) * x1 * i(x2 * x1) = i(x3) * i(x2)
31 equations and 1159 pending critical pairs; 4 deferred
x1 * i(x4 * x1) = i(x4)
32 equations and 1304 pending critical pairs; 5 deferred
x3 * x4 * x5 * i(x3 * x4 * x5) = e
33 equations and 1465 pending critical pairs; 6 deferred
x3 * x4 * x0 * x5 * i(x3 * x4 * x0 * x5) = e
34 equations and 1552 pending critical pairs; 6 deferred
x4 * x1 * x2 * i(x0) * i(x4 * i(x0 * i(x1 * x2))) = e
35 equations and 1541 pending critical pairs; 8 deferred
x2 * i(x4) * i(x3) * i(x2 * i(x3 * x4)) = e
36 equations and 1786 pending critical pairs; 8 deferred
x3 * x4 * i(x2) * i(x6) * i(x5) * i(x3 * x4 * i(x2) * i(x5 * x6)) = e
37 equations and 1868 pending critical pairs; 8 deferred
x3 * i(x1) * x0 * i(x3 * i(i(x0) * x1)) = e
38 equations and 1863 pending critical pairs; 20 deferred
x3 * x1 * i(x0) * i(x3 * i(x0 * i(x1))) = e
39 equations and 2051 pending critical pairs; 23 deferred
i(x3) * i(x2) * i(x1) * i(x0) * x6 * i(i(x3) * i(x2) * i(x1) * i(x0) * x6) = e
40 equations and 2332 pending critical pairs; 23 deferred
x5 * i(x3) * i(x2) * i(x1) * i(x0) * i(x5 * i(x0 * x1 * x2 * x3)) = e
```

問題の危険対は
```
i(x0 * i(x1 * x2)) * x0 * x3 = x1 * x2 * x3
x4 * i(x4) * x5 = x5
```
の2つの式から生成される
