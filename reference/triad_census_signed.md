# signed triad census

triad census for signed graphs

## Usage

``` r
triad_census_signed(g)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

## Value

counts for all 138 signed directed triangle types

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_full_graph(4, directed = TRUE)
E(g)$sign <- rep(c(-1, 1, 1, -1, -1, 1), 2)
triad_census_signed(g)
#>  003-000000  012-0000P0  012-0000N0  102-0000PP  102-0000NP  102-0000NN 
#>           0           0           0           0           0           0 
#> 021C-0PP000 021C-0NP000 021C-0PN000 021C-0NN000 021U-0P0P00 021U-0N0P00 
#>           0           0           0           0           0           0 
#> 021U-0N0N00 021D-P0P000 021D-N0P000 021D-N0N000 111U-0P00PP 111U-0N00PP 
#>           0           0           0           0           0           0 
#> 111U-0P00NP 111U-0P00PN 111U-0N00NP 111U-0N00PN 111U-0P00NN 111U-0N00NN 
#>           0           0           0           0           0           0 
#> 111D-P000PP 111D-N000PP 111D-P000NP 111D-P000PN 111D-N000NP 111D-N000PN 
#>           0           0           0           0           0           0 
#> 111D-P000NN 111D-N000NN  201-00PPPP  201-00NPPP  201-00PNPP  201-00NNPP 
#>           0           0           0           0           0           0 
#>  201-00NPNP  201-00NPPN  201-00PNPN  201-00NNNP  201-00NNPN  201-00NNNN 
#>           0           0           0           0           0           0 
#> 030C-P00PP0 030C-N00PP0 030C-N00PN0 030C-N00NN0 030T-0P0PP0 030T-0N0PP0 
#>           0           0           0           0           0           0 
#> 030T-0P0NP0 030T-0P0PN0 030T-0N0NP0 030T-0N0PN0 030T-0P0NN0 030T-0N0NN0 
#>           0           0           0           0           0           0 
#> 120U-0P0PPP 120U-0N0PPP 120U-0P0PNP 120U-0N0NPP 120U-0N0PNP 120U-0N0PPN 
#>           0           0           0           0           0           0 
#> 120U-0P0PNN 120U-0N0NNP 120U-0N0PNN 120U-0N0NNN 120D-P0P0PP 120D-N0P0PP 
#>           0           0           0           0           0           0 
#> 120D-P0P0NP 120D-N0N0PP 120D-N0P0NP 120D-P0N0NP 120D-P0P0NN 120D-N0N0NP 
#>           0           0           0           0           0           0 
#> 120D-N0P0NN 120D-N0N0NN 120C-0PP0PP 120C-0NP0PP 120C-0PN0PP 120C-0PP0NP 
#>           0           0           0           0           0           0 
#> 120C-0PP0PN 120C-0NN0PP 120C-0NP0NP 120C-0NP0PN 120C-0PN0NP 120C-0PN0PN 
#>           0           0           0           0           0           0 
#> 120C-0PP0NN 120C-0NN0NP 120C-0NN0PN 120C-0NP0NN 120C-0PN0NN 120C-0NN0NN 
#>           0           0           0           0           0           0 
#>  210-PPP0PP  210-NPP0PP  210-PNP0PP  210-PPN0PP  210-PPP0NP  210-PPP0PN 
#>           0           0           0           0           0           0 
#>  210-NNP0PP  210-NPN0PP  210-NPP0NP  210-NPP0PN  210-PNN0PP  210-PNP0NP 
#>           0           0           0           0           0           0 
#>  210-PNP0PN  210-PPN0NP  210-PPN0PN  210-PPP0NN  210-NNN0PP  210-NNP0NP 
#>           0           0           0           0           0           0 
#>  210-NNP0PN  210-NPN0NP  210-NPN0PN  210-NPP0NN  210-PNN0NP  210-PNN0PN 
#>           0           0           0           0           0           0 
#>  210-PNP0NN  210-PPN0NN  210-NNN0NP  210-NNN0PN  210-NNP0NN  210-NPN0NN 
#>           0           0           0           0           0           0 
#>  210-PNN0NN  210-NNN0NN  300-PPPPPP  300-NPPPPP  300-NNPPPP  300-NPNPPP 
#>           0           0           0           0           0           0 
#>  300-PNNPPP  300-PPNPNP  300-NNNPPP  300-NNPNPP  300-NPNPNP  300-PNNPPN 
#>           1           1           0           0           0           0 
#>  300-NNNNPP  300-NNNPNP  300-NNNPPN  300-NPNPNN  300-NNNNNP  300-NNNNNN 
#>           0           0           1           1           0           0 
```
