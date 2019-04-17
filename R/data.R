#' Mega-tree of plants based on Open Tree of Life
#' 
#' This tree was based on Brown and Smith (2018), which in turn was based on the Open Tree of Life. 
#' It was copied from `V.PhyloMaker::GBOTB.extended`. After then, node labels were added for empty ones.
#'
#' @format A phylogeny with class "phylo". It is also a list. Compare with a normal phylo object, it has another data frame `tree_plant_GBOTB$genus_family_root`, which provides the root nodes information for every unique genus and family in the phylogeny. Such information can be used to insert new tips onto the phylogeny later.
#' @source <https://github.com/jinyizju/V.PhyloMaker/tree/master/data>
#' 
"tree_plant_GBOTB"

