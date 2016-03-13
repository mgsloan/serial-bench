## serial-bench

[![Build Status](https://travis-ci.org/snoyberg/serial-bench.svg?branch=master)](https://travis-ci.org/snoyberg/serial-bench)

Ridiculously oversimplified serialization benchmark.

```
serial-bench-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark serial-bench-bench: RUNNING...
benchmarking encode/encode
time                 6.346 μs   (6.321 μs .. 6.366 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.307 μs   (6.268 μs .. 6.344 μs)
std dev              133.2 ns   (102.4 ns .. 209.3 ns)
variance introduced by outliers: 22% (moderately inflated)

benchmarking encode/simpleEncode
time                 4.035 μs   (3.823 μs .. 4.283 μs)
                     0.987 R²   (0.981 R² .. 0.997 R²)
mean                 3.928 μs   (3.843 μs .. 4.041 μs)
std dev              325.1 ns   (242.9 ns .. 430.2 ns)
variance introduced by outliers: 83% (severely inflated)

benchmarking decode/binary
time                 12.00 μs   (11.87 μs .. 12.16 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 11.85 μs   (11.79 μs .. 11.96 μs)
std dev              258.6 ns   (154.9 ns .. 469.3 ns)
variance introduced by outliers: 22% (moderately inflated)

benchmarking decode/cereal
time                 12.88 μs   (11.20 μs .. 14.80 μs)
                     0.868 R²   (0.783 R² .. 0.944 R²)
mean                 15.74 μs   (14.17 μs .. 18.71 μs)
std dev              7.091 μs   (5.133 μs .. 11.13 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking decode/simple
time                 7.195 μs   (6.721 μs .. 7.862 μs)
                     0.933 R²   (0.890 R² .. 0.975 R²)
mean                 8.683 μs   (7.749 μs .. 12.02 μs)
std dev              4.467 μs   (1.638 μs .. 9.269 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking decode/simpleLE
time                 4.962 μs   (4.848 μs .. 5.125 μs)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 4.911 μs   (4.879 μs .. 4.970 μs)
std dev              156.7 ns   (96.56 ns .. 277.3 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking decode/simpleClass
time                 5.702 μs   (5.404 μs .. 6.196 μs)
                     0.942 R²   (0.893 R² .. 0.977 R²)
mean                 8.254 μs   (6.524 μs .. 13.96 μs)
std dev              8.799 μs   (4.262 μs .. 16.99 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking decode/simpleClassEx
time                 2.740 μs   (2.619 μs .. 2.878 μs)
                     0.990 R²   (0.983 R² .. 0.997 R²)
mean                 2.732 μs   (2.683 μs .. 2.825 μs)
std dev              224.4 ns   (154.6 ns .. 344.8 ns)
variance introduced by outliers: 83% (severely inflated)

Benchmark serial-bench-bench: FINISH
```
