Ridiculously oversimplified serialization benchmark.

```
Running 1 benchmarks...
Benchmark serial-bench-bench: RUNNING...
benchmarking binary
time                 19.88 μs   (18.80 μs .. 21.16 μs)
                     0.978 R²   (0.966 R² .. 0.993 R²)
mean                 18.96 μs   (18.40 μs .. 19.74 μs)
std dev              2.170 μs   (1.610 μs .. 3.038 μs)
variance introduced by outliers: 88% (severely inflated)
             
benchmarking cereal
time                 10.02 μs   (10.00 μs .. 10.05 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.03 μs   (10.01 μs .. 10.06 μs)
std dev              79.96 ns   (67.74 ns .. 101.8 ns)
             
benchmarking simple
time                 6.220 μs   (6.185 μs .. 6.271 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 6.236 μs   (6.209 μs .. 6.282 μs)
std dev              114.7 ns   (76.41 ns .. 158.0 ns)
variance introduced by outliers: 17% (moderately inflated)
```
