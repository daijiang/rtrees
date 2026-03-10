# Get one or multiple trees from megatree(s)

For some taxa groups, there are multiple posterior megatrees. It is a
common task to derive a phylogeny from each of these (or a random subset
of) megatrees.

## Usage

``` r
get_tree(
  sp_list,
  tree,
  taxon = NULL,
  scenario = c("at_basal_node", "random_below_basal"),
  show_grafted = FALSE,
  tree_by_user = FALSE,
  mc_cores = future::availableCores() - 2,
  .progress = "text",
  fish_tree = c("timetree", "all-taxon"),
  mammal_tree = c("vertlife", "phylacine"),
  bee_tree = c("maximum-likelihood", "bootstrap"),
  dt = TRUE
)
```

## Arguments

- sp_list:

  A character vector or a data frame with at least three columns:
  species, genus, family. Species column holds the species for which we
  want to have a phylogeny. It can also have two optional columns:
  close_sp and close_genus. We can specify the closest species/genus of
  the species based on expert knowledge. If specified, the new species
  will be grafted to that particular location.

  It can also be a string vector if `taxon` is specified. Though it
  probably is a better idea to prepare your data frame with
  [`sp_list_df()`](https://daijiang.github.io/rtrees/reference/sp_list_df.md).
  The string vector can also have the same format as that required by
  Phylomatic (i.e., family/genus/genus_sp).

- tree:

  A mega-tree with class `phylo` or a list of mega-trees with class
  `multiPhylo`. Optional if `taxon` is specified, in which case, a
  default mega-phylogeny (or a set of 100 randomly selected posterior
  phylogenies) will be used (see their own documentations from the
  `megatrees` package).

  - For amphibian, the mega-trees are
    [megatrees::tree_amphibian_n100](https://rdrr.io/pkg/megatrees/man/tree_amphibian_n100.html).

  - For bee, the mega-tree is
    [megatrees::tree_bee](https://rdrr.io/pkg/megatrees/man/tree_bee.html),
    with
    [megatrees::tree_bee_n100](https://rdrr.io/pkg/megatrees/man/tree_bee_n100.html)
    be the other option.

  - For butterfly, the mega-tee is
    [megatrees::tree_butterfly](https://rdrr.io/pkg/megatrees/man/tree_butterfly.html).

  - For bird, the mega-trees are
    [megatrees::tree_bird_n100](https://rdrr.io/pkg/megatrees/man/tree_bird_n100.html).

  - For fish, the mega-tree is
    [megatrees::tree_fish_12k](https://rdrr.io/pkg/megatrees/man/tree_fish_12k.html),
    with
    [megatrees::tree_fish_32k_n50](https://rdrr.io/pkg/megatrees/man/tree_fish_32k_n50.html)
    be the other option.

  - For mammal, the default mega-trees are
    [megatrees::tree_mammal_n100_vertlife](https://rdrr.io/pkg/megatrees/man/tree_mammal_n100_vertlife.html),
    with
    [megatrees::tree_mammal_n100_phylacine](https://rdrr.io/pkg/megatrees/man/tree_mammal_n100_phylacine.html)
    be the other option.

  - For plant, the mega-tree is
    [megatrees::tree_plant_otl](https://rdrr.io/pkg/megatrees/man/tree_plant_otl.html).

  - For reptile, the mega-trees are
    [megatrees::tree_reptile_n100](https://rdrr.io/pkg/megatrees/man/tree_reptile_n100.html).

  - For shark, ray, and chimaeras, the mega-trees are
    [megatrees::tree_shark_ray_n100](https://rdrr.io/pkg/megatrees/man/tree_shark_ray_n100.html).

- taxon:

  The taxon of species in the `sp_list`. Currently, can be `amphibian`,
  `bird`, `fish`, `mammal`, `plant`, `reptile`, or `shark_ray`.

- scenario:

  How to insert a species into the mega-tree?

  - In both scenarioes, if there is only 1 species in the genus or
    family, a new node will be inserted to the middle point of this only
    species' branch length and the new species will be attached to this
    new node.

  - If `scenario = "at_basal_node"`, a species is attached to the basal
    node of the same genus or the same family if the mega-tree does not
    have any species of this genus.

  - If `scenario = "random_below_basal"`, a species is attached to a
    randomly selected node that is at or below the basal node of the
    same genus of the same family if the mega-tree does not have any
    species in this genus. The probability of node been selected is
    proportional to its branch length. Because of the random sampling
    involved, you may want to run several times to get a collection of
    derived phylogenies.

- show_grafted:

  Whether to indicate which species was grafted onto the mega-tree. If
  `TRUE`, a `*` will be appended to the species name on the tip if it
  was grafted within the same genus; `**` will be appended if it was
  grafted within the same family.

- tree_by_user:

  Is the mega-tree provided by user? Default is `FALSE` but it will be
  automatically set to `TRUE` when the class of `tree` is `multiPhylo`
  since we don't provide any such mega-trees here.

- mc_cores:

  Number of cores to parallel processing when `tree` is a list of large
  number of trees. The default is the number of available cores minus 2.

- .progress:

  Form of progress bar, default to be text.

- fish_tree:

  Which fish tree do you want to use? If it is "timetree" (default), it
  will be the smaller time tree with 11638 species that all have
  sequence data; if it is "all-taxon", then it will be the 100 larger
  posterior phylogenies with 31516 soecues.

- mammal_tree:

  Which set of mammal trees to use? If it is "vertlife" (default), then
  100 randomly selected posterior phylogenies provided by Vertlife will
  be used; if it is "phylacine", then 100 randomly selected posterior
  phylogenies provided by PHYLACINE will be used.

- bee_tree:

  Which bee tree to use? If it is "maximum-likelihood" (default), the a
  single maximum likelihood tree will be used. If it is "bootstrap",
  then a set of 100 randomly selected posterior phylogenies will be
  used. All trees are provided by the [Bee Tree of
  Life](http://beetreeoflife.org).

- dt:

  Whether to use data.table version to bind tips
  [bind_tip](https://daijiang.github.io/rtrees/reference/bind_tip.md).
  The default is `TRUE` as it maybe slightly faster.

## Value

A phylogeny for the species required, with class `phylo`; or a list of
phylogenies with class `multiPhylo` depends on the input `tree`. Within
each phylogeny, the grafted status of all species was saved as a data
frame named as "graft_status".

## Details

Derive a phylogeny from a mega-tree

For a list of species, generate a phylogeny or multiple phylogenies from
a provided mega-tree or mega-trees. If a species is not in the
mega-tree, it will be grafted to the mega-tree with two scenarios.

## Examples

``` r
test_sp = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii", 
"Periophthalmus_barbarus", "Prognichthys_glaphyrae", "Barathronus_bicolor", 
"Knipowitschia_croatica", "Rhamphochromis_lucius", "Neolissochilus_tweediei", 
"Haplochromis_nyanzae", "Astronesthes_micropogon", "Sanopus_reticulatus")
test_tree = get_tree(sp_list = test_sp,
                     taxon = "fish",
                     show_grafted = TRUE)
#> 
#> 6 species added at genus level (*) 
#> 1 species have no co-family species in the mega-tree, skipped
#> (if you know their family, prepare and edit species list with `rtrees::sp_list_df()` may help): 
#> Barathronus_bicolor
```
