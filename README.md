
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtrees

The goal of `rtrees` is to remove the barriers to derive phylogenies for
a list of species from mega-trees. Basically, `Phylomatic` and more.

# Installation

``` r
devtools::install_github("daijiang/rtrees")
```

``` r
library(rtrees)
```

# Mega-trees

Currently, `rtrees` provides mega-trees for four taxon groups: plants,
fishes, birds, and
mammals.

| Taxon  | Mega\_tree                              | Reference                                              |
| :----- | :-------------------------------------- | :----------------------------------------------------- |
| Plant  | `tree_plant_otl`                        | Open Tree of Life, Smith & Brown 2018; Jin & Qian 2019 |
| Fish   | `tree_fish`                             | Fish Tree of Life, Rabosky et al. 2018                 |
| Bird   | `tree_bird_ericon`, `tree_bird_hackett` | Bird Tree of Life, Jetz et al. 2018                    |
| Mammal | `tree_mammal`                           | PHYLACINE, Faurby et al. 2018                          |

# Usage examples

The species lists which we want to have a phylogeny should be provided
as a data frame with at least 3 columns: `family`, `genus`, and
`species`. Their order does not matter. Here is an example for fish.

``` r
test_fish_list
#> # A tibble: 12 x 3
#>    species                 genus          family        
#>    <chr>                   <chr>          <chr>         
#>  1 Serrasalmus_geryi       Serrasalmus    Serrasalmidae 
#>  2 Careproctus_reinhardti  Careproctus    Liparidae     
#>  3 Gobiomorphus_coxii      Gobiomorphus   Eleotridae    
#>  4 Periophthalmus_barbarus Periophthalmus Gobiidae      
#>  5 Prognichthys_glaphyrae  Prognichthys   Exocoetidae   
#>  6 Barathronus_bicolor     Barathronus    Aphyonidae    
#>  7 Knipowitschia_croatica  Knipowitschia  Gobiidae      
#>  8 Rhamphochromis_lucius   Rhamphochromis Cichlidae     
#>  9 Neolissochilus_tweediei Neolissochilus Cyprinidae    
#> 10 Haplochromis_nyanzae    Haplochromis   Cichlidae     
#> 11 Astronesthes_micropogon Astronesthes   Stomiidae     
#> 12 Sanopus_reticulatus     Sanopus        Batrachoididae
```

For plant, fish, bird, and mammal, it is possible to prepare `sp_list`
with function
`sp_list_df()`.

``` r
sp_list_df(sp_list = c("Periophthalmus_barbarus", "Barathronus_bicolor"),
           taxon = "fish")
#> # A tibble: 2 x 3
#>   species                 genus          family    
#>   <chr>                   <chr>          <chr>     
#> 1 Periophthalmus_barbarus Periophthalmus Gobiidae  
#> 2 Barathronus_bicolor     Barathronus    Aphyonidae
```

Then we can derive a phylogeny from `tree_fish`.

``` r
test_tree = get_tree(sp_list = test_fish_list,
                     tree = tree_fish, # either 
                     taxon = "fish", # or
                     scenario = "S1",
                     show_grafted = TRUE)
#> These species have no species in the same family in the mega-tree, skipped: 
#> Barathronus_bicolor
library(ape)
plot(ladderize(test_tree), no.margin = T)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Some notes:

  - If `tree` is specified, then `taxon` can be ignored and *vice
    versa*.

  - If a species does not have a co-family species in the mega-tree, it
    will be skipped.

  - If `show_grafted = TRUE`, species that are grafted will have one or
    two `*` at the end of their names.
    
      - If it is grafted at the genus level, one `*`.
      - If it is grafted at the family level, two `*`s.

  - The default scenario is `S1`, which will graft species at the
    genus/family basal node.
    
      - `S2` will randomly select a downstream node to attach the new
        tip.
      - `S3` will graft the new tip above the genus/family basal node.
      - If only one species in the mega-tree that is in the same
        genus/family of the new tip, then the new tip will be grafted at
        the middle of this species’ branch for all scenarioes.

  - See `?get_tree` for more details.
    
    # Note
    
    This package is still in very early stage. Contributions are
    welcome.
