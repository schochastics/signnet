# Exact frustration index of a signed network

Computes the exact frustration index of a signed network using linear
programming

## Usage

``` r
frustration_exact(g, ...)
```

## Arguments

- g:

  signed network

- ...:

  additional parameters for the ompr solver

## Value

list containing the frustration index and the bipartition of nodes

## Details

The frustration index indicates the minimum number of edges whose
removal results in a balance network. The function needs the following
packages to be installed: `ompr`, `ompr.roi`,`ROI`, and
`ROI.plugin.glpk`. The function Implements the AND model in Aref et al.,
2020

## References

Aref, Samin, Andrew J. Mason, and Mark C. Wilson. "Computing the line
index of balance using linear programming optimisation." Optimization
problems in graph theory. Springer, Cham, 2018. 65-84.

Aref, Samin, Andrew J. Mason, and Mark C. Wilson. "A modeling and
computational study of the frustration index in signed networks."
Networks 75.1 (2020): 95-110.

## Author

David Schoch
