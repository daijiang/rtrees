#' Mega-tree of plants
#' 
#' This tree was based on Brown and Smith (2018), which in turn was based on the Open Tree of Life. 
#'   It was copied from \code{V.PhyloMaker::GBOTB.extended}. After then, I added node labels for empty ones.
#'
#' @format A phylogeny with class "phylo". It is also a list. Compare with a normal phylo object, it has another data frame \code{tree_plant_GBOTB$genus_family_root}, which provide the root nodes information for every unique genus and family in the phylogeny. Such information can be later used to insert new tips onto the phylogeny.
#' @source \url{https://github.com/jinyizju/V.PhyloMaker/tree/master/data}
"tree_plant_GBOTB"

