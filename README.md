## serial-bench

[![Build Status](https://travis-ci.org/fpco/serial-bench.svg?branch=master)](https://travis-ci.org/fpco/serial-bench)

Blog post pending explaining this. In the meanwhile,
[benchmarks](https://s3.amazonaws.com/download.fpcomplete.com/michael/serial-bench-2016-03-13.html)!

```
benchmarking encode/encodeSimpleRaw
time                 516.9 ns   (514.3 ns .. 519.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 517.5 ns   (514.3 ns .. 524.0 ns)
std dev              14.99 ns   (8.780 ns .. 27.52 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking encode/encodeSimplePoke
time                 511.9 ns   (509.7 ns .. 514.0 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 512.2 ns   (509.6 ns .. 515.6 ns)
std dev              9.718 ns   (7.981 ns .. 14.68 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking encode/encodeSimplePokeMonad
time                 549.9 ns   (534.2 ns .. 567.7 ns)
                     0.993 R²   (0.986 R² .. 0.998 R²)
mean                 539.5 ns   (530.3 ns .. 555.0 ns)
std dev              41.45 ns   (25.22 ns .. 67.95 ns)
variance introduced by outliers: 84% (severely inflated)

benchmarking encode/encodeSimplePokeRef
time                 798.9 ns   (795.1 ns .. 802.3 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 804.5 ns   (799.7 ns .. 811.5 ns)
std dev              19.62 ns   (14.37 ns .. 28.68 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking encode/encodeSimplePokeRefMonad
time                 815.9 ns   (810.5 ns .. 822.1 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 814.7 ns   (809.7 ns .. 820.3 ns)
std dev              17.07 ns   (13.93 ns .. 20.71 ns)
variance introduced by outliers: 26% (moderately inflated)

benchmarking encode/encodeBuilderLE
time                 3.079 μs   (3.064 μs .. 3.093 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.092 μs   (3.073 μs .. 3.117 μs)
std dev              73.09 ns   (55.90 ns .. 98.13 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarking encode/encodeBuilderBE
time                 3.596 μs   (3.556 μs .. 3.646 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.573 μs   (3.553 μs .. 3.597 μs)
std dev              76.89 ns   (61.84 ns .. 106.8 ns)
variance introduced by outliers: 24% (moderately inflated)

benchmarking encode/encodeCereal
time                 17.82 μs   (17.69 μs .. 17.94 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 17.80 μs   (17.66 μs .. 17.99 μs)
std dev              514.9 ns   (423.8 ns .. 695.0 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking encode/binary
time                 69.49 μs   (68.92 μs .. 70.27 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 69.88 μs   (69.38 μs .. 70.40 μs)
std dev              1.730 μs   (1.371 μs .. 2.151 μs)
variance introduced by outliers: 22% (moderately inflated)

benchmarking decode/decodeSimplePeek
time                 1.036 μs   (1.029 μs .. 1.043 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.039 μs   (1.033 μs .. 1.046 μs)
std dev              23.82 ns   (18.78 ns .. 31.98 ns)
variance introduced by outliers: 29% (moderately inflated)

benchmarking decode/decodeSimplePeekEx
time                 1.710 μs   (1.699 μs .. 1.720 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.696 μs   (1.685 μs .. 1.707 μs)
std dev              36.97 ns   (31.98 ns .. 43.32 ns)
variance introduced by outliers: 26% (moderately inflated)

benchmarking decode/decodeRawLE
time                 1.787 μs   (1.774 μs .. 1.801 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.786 μs   (1.775 μs .. 1.798 μs)
std dev              38.31 ns   (31.72 ns .. 47.10 ns)
variance introduced by outliers: 25% (moderately inflated)

benchmarking decode/decodeRawBE
time                 2.667 μs   (2.652 μs .. 2.685 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.675 μs   (2.661 μs .. 2.694 μs)
std dev              54.38 ns   (41.63 ns .. 83.72 ns)
variance introduced by outliers: 22% (moderately inflated)

benchmarking decode/decodeCereal
time                 9.228 μs   (9.185 μs .. 9.275 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 9.264 μs   (9.189 μs .. 9.351 μs)
std dev              283.5 ns   (220.9 ns .. 373.7 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarking decode/binary
time                 71.68 μs   (70.66 μs .. 73.40 μs)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 70.93 μs   (70.30 μs .. 72.39 μs)
std dev              3.175 μs   (1.665 μs .. 5.724 μs)
variance introduced by outliers: 48% (moderately inflated)
```
