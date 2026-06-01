# Fetching phylogenies from mega-trees

Provides tools to derive species-level phylogenies from large synthesis
mega-trees for a wide range of taxonomic groups, including plants,
birds, mammals, amphibians, reptiles, fish, bees, butterflies, and
sharks. When a queried species is absent from the mega-tree, it is
grafted onto the tree using one of two placement strategies: attachment
at the basal node of the most closely related genus or family
('at_basal_node'), or random attachment below that basal node with
probability proportional to branch length ('random_below_basal'). See Li
(2023) [doi:10.1111/ecog.06643](https://doi.org/10.1111/ecog.06643) for
details. Multiple species from a genus not represented in the mega-tree
are placed as a polytomy to preserve clade coherence. The package
interfaces with the 'megatrees' data package, which bundles or downloads
on demand curated mega-trees. Users can also provide their own
mega-trees.

## See also

Useful links:

- <https://daijiang.github.io/rtrees/>

## Author

Daijiang Li <daijianglee@gmail.com>
