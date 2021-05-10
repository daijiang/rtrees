#' Get one or multiple trees from megatree(s)
#' 
#' For some taxa groups, there are multiple posterior megatrees. It is a common
#' task to derive a phylogeny from each of these (or a random subset of) megatrees.
#' 
#' 
#' 
#' 

get_tree = function(sp_list, tree, taxon, 
                    # multiple_trees = TRUE,
                    scenario = c("at_basal_node", "random_below_basal", "at_or_above_basal"), 
                    show_grafted = FALSE,
                    tree_by_user = FALSE){
  scenario = match.arg(scenario)
  if(length(tree) > 1)
}