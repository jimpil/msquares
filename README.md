# msquares

Clojure code that produces magic squares of any order

## Example

`lein run 3`

```
Odd magic square
Magic Constant: 15
========================

| 1 | 2 | 3 |
|---+---+---|
| 8 | 1 | 6 |
| 3 | 5 | 7 |
| 4 | 9 | 2 |
```


`lein run 6`

```
Singly-even magic square
Magic Constant: 111
========================

|  1 |  2 |  3 |  4 |  5 |  6 |
|----+----+----+----+----+----|
| 35 |  1 |  6 | 26 | 19 | 24 |
|  3 | 32 |  7 | 21 | 23 | 25 |
| 31 |  9 |  2 | 22 | 27 | 20 |
|  8 | 28 | 33 | 17 | 10 | 15 |
| 30 |  5 | 34 | 12 | 14 | 16 |
|  4 | 36 | 29 | 13 | 18 | 11 |
```


`lein run 12`

```
Doubly-even magic square
Magic Constant: 870
========================

|   1 |   2 |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 |  11 |  12 |
|-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----|
| 144 | 143 | 142 |   4 |   5 |   6 |   7 |   8 |   9 | 135 | 134 | 133 |
| 132 | 131 | 130 |  16 |  17 |  18 |  19 |  20 |  21 | 123 | 122 | 121 |
| 120 | 119 | 118 |  28 |  29 |  30 |  31 |  32 |  33 | 111 | 110 | 109 |
|  37 |  38 |  39 | 105 | 104 | 103 | 102 | 101 | 100 |  46 |  47 |  48 |
|  49 |  50 |  51 |  93 |  92 |  91 |  90 |  89 |  88 |  58 |  59 |  60 |
|  61 |  62 |  63 |  81 |  80 |  79 |  78 |  77 |  76 |  70 |  71 |  72 |
|  73 |  74 |  75 |  69 |  68 |  67 |  66 |  65 |  64 |  82 |  83 |  84 |
|  85 |  86 |  87 |  57 |  56 |  55 |  54 |  53 |  52 |  94 |  95 |  96 |
|  97 |  98 |  99 |  45 |  44 |  43 |  42 |  41 |  40 | 106 | 107 | 108 |
|  36 |  35 |  34 | 112 | 113 | 114 | 115 | 116 | 117 |  27 |  26 |  25 |
|  24 |  23 |  22 | 124 | 125 | 126 | 127 | 128 | 129 |  15 |  14 |  13 |
|  12 |  11 |  10 | 136 | 137 | 138 | 139 | 140 | 141 |   3 |   2 |   1 |

```

