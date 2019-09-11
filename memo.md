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
