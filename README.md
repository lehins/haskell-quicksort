# quicksort


Benchmarks for the quicksort implemented for `massiv` vs introsort in `vector-algorithms`:

```
benchmarking random/Array Seq
time                 84.74 ms   (84.53 ms .. 85.00 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 84.72 ms   (84.48 ms .. 84.87 ms)
std dev              338.2 μs   (187.5 μs .. 539.7 μs)

benchmarking random/Array Par
time                 23.37 ms   (22.99 ms .. 23.90 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 23.53 ms   (23.33 ms .. 23.84 ms)
std dev              546.5 μs   (410.5 μs .. 775.9 μs)

benchmarking random/Vector Algorithms
time                 100.3 ms   (95.05 ms .. 112.0 ms)
                     0.988 R²   (0.971 R² .. 1.000 R²)
mean                 98.18 ms   (96.47 ms .. 102.1 ms)
std dev              4.051 ms   (1.511 ms .. 6.364 ms)

benchmarking sorted/Array Seq
time                 31.25 ms   (31.17 ms .. 31.37 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 31.51 ms   (31.41 ms .. 31.66 ms)
std dev              279.4 μs   (179.9 μs .. 371.0 μs)

benchmarking sorted/Array Par
time                 11.56 ms   (11.28 ms .. 11.87 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 11.26 ms   (11.13 ms .. 11.39 ms)
std dev              384.4 μs   (325.5 μs .. 516.5 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking sorted/Vector Algorithms
time                 27.46 ms   (27.31 ms .. 27.67 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 27.82 ms   (27.56 ms .. 28.52 ms)
std dev              992.7 μs   (234.3 μs .. 1.888 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking reversed sorted/Array Seq
time                 31.52 ms   (31.04 ms .. 31.86 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 31.59 ms   (31.41 ms .. 32.13 ms)
std dev              590.2 μs   (203.7 μs .. 1.162 ms)

benchmarking reversed sorted/Array Par
time                 11.05 ms   (10.80 ms .. 11.32 ms)
                     0.995 R²   (0.989 R² .. 0.998 R²)
mean                 11.38 ms   (11.13 ms .. 11.75 ms)
std dev              840.6 μs   (546.6 μs .. 1.317 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking reversed sorted/Vector Algorithms
time                 27.76 ms   (27.46 ms .. 27.90 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 28.02 ms   (27.84 ms .. 28.30 ms)
std dev              471.4 μs   (225.8 μs .. 705.3 μs)

benchmarking replicated/Array Seq
time                 2.994 ms   (2.954 ms .. 3.027 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 3.065 ms   (3.027 ms .. 3.201 ms)
std dev              262.2 μs   (104.8 μs .. 472.9 μs)
variance introduced by outliers: 58% (severely inflated)

benchmarking replicated/Array Par
time                 4.024 ms   (3.908 ms .. 4.206 ms)
                     0.989 R²   (0.982 R² .. 0.995 R²)
mean                 3.958 ms   (3.862 ms .. 4.027 ms)
std dev              278.3 μs   (237.4 μs .. 358.6 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking replicated/Vector Algorithms
time                 27.73 ms   (27.57 ms .. 27.92 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 27.88 ms   (27.78 ms .. 28.01 ms)
std dev              261.6 μs   (152.7 μs .. 352.2 μs)
```
