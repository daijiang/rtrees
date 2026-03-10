# Convert a vector of species names to a data frame

Convert a vector of species names to a data frame

## Usage

``` r
sp_list_df(sp_list, taxon)
```

## Arguments

- sp_list:

  A string vector or a data frame with at least one column named
  "species".

- taxon:

  The taxon group of this species list. If not specified, only species
  and genus will be returned.

## Value

A data frame with columns: species, genus, and family (if `taxon` is
specified).

## Examples

``` r
sp_list_df(sp_list = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii"),
           taxon = "fish")
#> # A tibble: 3 × 3
#>   species                genus        family       
#>   <chr>                  <chr>        <chr>        
#> 1 Serrasalmus_geryi      Serrasalmus  Serrasalmidae
#> 2 Careproctus_reinhardti Careproctus  Liparidae    
#> 3 Gobiomorphus_coxii     Gobiomorphus Eleotridae   
```
