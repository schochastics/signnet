# Check if network is a signed network

Check if network is a signed network

## Usage

``` r
is_signed(g)
```

## Arguments

- g:

  igraph object

## Value

logical scalar

## Examples

``` r
g <- sample_islands_signed(2, 5, 1, 5)
is_signed(g)
#> [1] TRUE
```
