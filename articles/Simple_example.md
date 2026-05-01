# Simple_example

In this vignette, I provide code for some simple examples. The main
function to use is
[`rtrees::get_tree()`](https://daijiang.github.io/rtrees/reference/get_tree.md).
Users can type
[`?rtrees::get_tree`](https://daijiang.github.io/rtrees/reference/get_tree.md)
in the R console to see details.

To get a phylogeny for a list of species using an existing synthetic
megatree, we can use the following code:

``` r

# create a species list
species <- c('Meliosma laui', 'Acer cordatum', 'Fraxinus mandshurica',
            'Ormosia pinnata', 'Aglaia dasyclada', 'Sphagnum_subnitens',
            'Stephanomeria_cichoriacea', 'Taraxacum_schroeterianum', 
            'Humiria_balsamifera', 'Salix_cinerea', 'Floerkea_proserpinacoides')
# get a phylogeny
sp_tree <- rtrees::get_tree(sp_list = species, taxon = 'plant')
#> 
#> 5 species added at genus level (*)
#> 1 species added at family level (**)
#> 1 species have no co-family species in the mega-tree, skipped
#> (if you know their family, prepare and edit species list with `rtrees::sp_list_df()` may help): 
#> Sphagnum_subnitens
```

In the code chunk above, the vector of species names can have either
space or underscore in the names; spaces will be replaced by underscores
internally within
[`rtrees::get_tree()`](https://daijiang.github.io/rtrees/reference/get_tree.md).
If no megatree was set by the `tree =` argument, then the `taxon =`
argument must be one of the supported taxonomic groups (“amphibian”,
“bird”, “fish”, “mammal”, “plant”, “reptile”, and “shark_ray”). Function
[`rtrees::get_tree()`](https://daijiang.github.io/rtrees/reference/get_tree.md)
will print out messages about the number of species been grafted at
genus and family level as well as the number of species been skipped if
neither congeneric nor co-family species were found in the megatree. In
the example above, the skipped species is a moss and the megatree does
not have any moss species from the Sphagnaceae family. When the argument
`show_grafted` of
[`rtrees::get_tree()`](https://daijiang.github.io/rtrees/reference/get_tree.md)
is set to ‘TRUE’ (default is ‘FALSE’), the tip labels of the generated
phylogeny will have a trailing \* if it is grafted at genus level or
\*\* if it is grafted at family level. No matter whether `show_grafted`
is ‘TRUE’ or ‘FALSE’, the grafting information was saved along with the
phylogeny and can be extracted with the following code:

``` r

# or use rtrees::get_graft_status()
sp_tree$graft_status
#> # A tibble: 11 × 3
#>    tip_label                 species                   status                   
#>    <chr>                     <chr>                     <chr>                    
#>  1 Taraxacum_schroeterianum  Taraxacum_schroeterianum  exisiting species in the…
#>  2 Stephanomeria_cichoriacea Stephanomeria_cichoriacea exisiting species in the…
#>  3 Fraxinus_mandshurica      Fraxinus_mandshurica      grafted at genus level   
#>  4 Ormosia_pinnata           Ormosia_pinnata           grafted at genus level   
#>  5 Salix_cinerea             Salix_cinerea             exisiting species in the…
#>  6 Humiria_balsamifera       Humiria_balsamifera       exisiting species in the…
#>  7 Floerkea_proserpinacoides Floerkea_proserpinacoides grafted at family level  
#>  8 Aglaia_dasyclada          Aglaia_dasyclada          grafted at genus level   
#>  9 Acer_cordatum             Acer_cordatum             grafted at genus level   
#> 10 Meliosma_laui             Meliosma_laui             grafted at genus level   
#> 11 NA                        Sphagnum_subnitens        skipped as no co-family …
```

When users already have a phylogeny for most of their species (i.e., the
second and third scenarios described in the Usage scenarios section
above), we can use the same code as above, with the argument
`tree_by_user = TRUE`. And here is an example using the phylogeny
generated above as a pretended megatree that we already have.

``` r

more_sp_to_add = c('Ormosia_sp.', 'Fraxinus_americana')
new_species = c(species, more_sp_to_add)
sp_tree_2 = rtrees::get_tree(sp_list = new_species, tree = sp_tree, 
                             taxon = 'plant', tree_by_user = TRUE)
#> Not all genus can be found in the phylogeny.
#> 
#> 2 species added at genus level (*)
#> 1 species have no co-family species in the mega-tree, skipped
#> (if you know their family, prepare and edit species list with `rtrees::sp_list_df()` may help): 
#> Sphagnum_subnitens
```

In the code above, as there is a genus (Sphagnum) not included in the
user provided phylogeny, we need to specify the `taxon` argument to
extract the correct classification information; note that this requires
the taxonomic group is one of those supported by `rtrees`. However, if
all genus of the species list are already in the user provided
phylogeny, then we can ignore the `taxon` argument:

``` r

# remove Sphagnum_subnitens so that all genus are in the megatree
new_species_all_in = setdiff(new_species, 'Sphagnum_subnitens')
sp_tree_3 = rtrees::get_tree(sp_list = new_species_all_in, tree = sp_tree, 
                             tree_by_user = TRUE)
#> 
#> 2 species added at genus level (*)
```

The function
[`rtrees::get_tree()`](https://daijiang.github.io/rtrees/reference/get_tree.md)
can also work with **a set of posterior megatrees** with the option to
use parallel computing for the whole process. The default number of
cores to be used will be the available number of cores minus 2 (so that
users can still perform other tasks on their computers at the same
time). The output will be a set of generated phylogenies with class
‘multiPhylo’; the number of derived phylogenies will be the same as the
input megatrees. For this scenario, we can use exactly the same code
described above.

``` r

# bird species
bird_species =  c('Brachypteryx_major', 'Asthenes_perijana', 'Ciridops_anna', 
                   'Leiothlypis_ruficapilla', 'Reinwardtoena_reinwardti',
                   'Garrulax_caerulatus', 'Buteo_rufofuscus', 'Sylvia_mystacea',
                   'Telophorus_viridis', 'Trachyphonus_darnaudii')
sp_tree_4 = rtrees::get_tree(sp_list = bird_species, taxon = 'bird')
#> 
#> 3 species added at genus level (*)
#> 2 species added at family level (**)
sp_tree_4
#> 100 phylogenetic trees
```
