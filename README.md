## serial-bench

[![Build Status](https://travis-ci.org/snoyberg/serial-bench.svg?branch=master)](https://travis-ci.org/snoyberg/serial-bench)

Ridiculously oversimplified serialization benchmark.

```
benchmarking binary
time                 21.64 μs   (18.14 μs .. 25.39 μs)
                     0.905 R²   (0.848 R² .. 0.998 R²)
mean                 19.71 μs   (18.86 μs .. 21.49 μs)
std dev              3.833 μs   (1.577 μs .. 7.268 μs)
variance introduced by outliers: 96% (severely inflated)
             
benchmarking cereal
time                 15.21 μs   (13.42 μs .. 17.20 μs)
                     0.911 R²   (0.872 R² .. 0.961 R²)
mean                 13.32 μs   (12.47 μs .. 14.85 μs)
std dev              3.524 μs   (2.404 μs .. 5.556 μs)
variance introduced by outliers: 98% (severely inflated)
             
benchmarking simple
time                 7.845 μs   (7.525 μs .. 8.184 μs)
                     0.981 R²   (0.970 R² .. 0.991 R²)
mean                 8.175 μs   (7.853 μs .. 8.612 μs)
std dev              1.246 μs   (1.007 μs .. 1.866 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking simpleLE
time                 5.888 μs   (5.732 μs .. 6.093 μs)
                     0.987 R²   (0.980 R² .. 0.993 R²)
mean                 6.444 μs   (6.197 μs .. 6.803 μs)
std dev              1.027 μs   (780.0 ns .. 1.380 μs)
variance introduced by outliers: 95% (severely inflated)
```
