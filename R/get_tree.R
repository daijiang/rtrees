#' Get one or multiple trees from megatree(s)
#' 
#' For some taxa groups, there are multiple posterior megatrees. It is a common
#' task to derive a phylogeny from each of these (or a random subset of) megatrees.
#' 
#' 
#' 
#' Derive a phylogeny from a mega-tree
#' 
#' For a list of species, generate a phylogeny or multiple phylogenies from a 
#' provided mega-tree or mega-trees. If a species is
#' not in the mega-tree, it will be grafted to the mega-tree with three scenarios.
#' 
#' @param sp_list A data frame with at least three columns: species, genus, family. Species column
#' holds the species for which we want to have a phylogeny. It can also have two optional columns:
#' close_sp and close_genus. We can specify the closest species/genus of the species based on
#' expert knowledge. If specified, the new species will be grafted to that particular location.
#' 
#' It can also be a string vector if `taxon` is specified. Though it probably is a better idea
#' to prepare your data frame with [sp_list_df()].
#' 
#' In all cases, species names should be separated by `_` between genus and species, i.e. genus_sp.
#' @param tree A mega-tree with class `phylo` or a list of mega-trees with class `multiPhylo`.
#'  Optional if `taxon` is specified, in which case, a default
#' mega-phylogeny will be used (see their own documentations for details).
#' 
#' - For plant, the mega-tree is [tree_plant_otl].
#' - For fish, the mega-tree is [tree_fish].
#' - For bird, the mega-trees are [tree_bird_ericson] and [tree_bird_hackett]. [tree_bird_ericson] will be the 
#' default if no `tree` is specified and `taxon` is `bird`.
#' - For mammal, the mega-tree is [tree_mammal].
#' 
#' @param taxon The taxon of species in the `sp_list`. Currently, can be `plant`, `fish`, `bird`, or `mammal`.
#' @param scenario How to insert a species into the mega-tree? 
#' - In all scenarioes, if there is only 1 species in the genus or family, a new node will be inserted to
#' the middle point of this only species' branch length and the new species will be attached to this new 
#' node.
#' - If `scenario = "at_basal_node"`, a species is attached to the basal node of the same genus or the same family 
#' if the mega-tree does not have any species of this genus. 
#' - If `scenario = "random_below_basal"`, a species is attached to a randomly selected node that is at or below the 
#' basal node of the same genus of the same family if the mega-tree does not have any species in this genus.
#' The probability of node been selected is proportional to its branch length.
#' Because of the random sampling involved, you may want to run several times to get a collection of 
#' derived phylogenies.
#' - If `scenario = "at_or_above_basal"`, a species is attached to the basal node of the same genus if the mega-tree has species 
#' in the same genus; otherwise, a species is inserted to a new node above the basal node of the family.
#' If the age of the basal node is less than 2/3 of the node above it (root node of the family), the new node will
#' be added so that its age will be 2/3 of the root node of the family. Otherwise, a new node will be inserted 
#' into the middle point of the basal node and the root node of the family. I probably won't use this scenario.
#' @param show_grafted Whether to indicate which species was grafted onto the mega-tree. 
#' If `TRUE`, a `*` will be appended to the species name on the tip if it was grafted within
#' the same genus; `**` will be appended if it was grafted within the same family.
#' @param tree_by_user Is the mega-tree provided by user? Default is `FALSE` but it will be automatically set to `TRUE` when the class of `tree` is 
#' `multiPhylo` since we don't provide any such mega-trees here.
#' @param mc_cores Number of cores to parallel processing when `tree` is a list of large number of trees.
#' @return A phylogeny for the species required, with class `phylo`; 
#' or a list of phylogenies with class `multiPhylo` depends on the input `tree`. 
#' @export
#' @examples 
#' test_sp = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii", 
#' "Periophthalmus_barbarus", "Prognichthys_glaphyrae", "Barathronus_bicolor", 
#' "Knipowitschia_croatica", "Rhamphochromis_lucius", "Neolissochilus_tweediei", 
#' "Haplochromis_nyanzae", "Astronesthes_micropogon", "Sanopus_reticulatus")
#' test_tree = get_tree(sp_list = test_sp,
#'                      tree = tree_fish, # either 
#'                      taxon = "fish", # or
#'                      scenario = "at_basal_node",
#'                      show_grafted = TRUE)

get_tree = function(sp_list, tree, taxon = NULL, 
                    # multiple_trees = TRUE,
                    scenario = c("at_basal_node", "random_below_basal", "at_or_above_basal"), 
                    show_grafted = FALSE,
                    tree_by_user = FALSE,
                    mc_cores = 1){
  scenario = match.arg(scenario)
  
  if(missing(tree) & is.null(taxon))
    stop("Please specify at least a tree or a taxon group.")
  if(missing(tree) & !is.null(taxon)){# pick default tree
    tree = switch(taxon,
                  plant = rtrees::tree_plant_otl,
                  fish = rtrees::tree_fish,
                  bird = rtrees::tree_bird_ericson,
                  mammal = rtrees::tree_mammal
    )
  }
  
  if(inherits(tree, "phylo")){ # one phylo
    return(get_one_tree(sp_list, tree, taxon, scenario, show_grafted, tree_by_user))
  }
  
  if((inherits(tree, "multiPhylo") | inherits(tree, "list")) & 
     length(tree) > 1 &
     inherits(tree[[1]], "phylo") # a list of multiple phylo
     ){
    # if(.Platform$OS.type == "windows" & mc_cores > 1){
    #   warning("parallel does not work on Windows")
    # }
    
    if(is.null(tree[[1]]$genus_family_root)) tree_by_user = TRUE
    
    if(mc_cores > 1){
      if(mc_cores > future::availableCores()) 
        stop("mc_cores is larger than available cores")
      future::plan(future::multisession, workers = mc_cores)
      out = furrr::future_map(tree, function(i){
       rtrees::get_one_tree(sp_list, tree = i, taxon, scenario, show_grafted, tree_by_user)
      })
    } else {
      out = lapply(tree, get_one_tree, sp_list = sp_list, taxon = taxon, 
                   scenario = scenario, show_grafted = show_grafted, 
                   tree_by_user = tree_by_user)
    }
    class(out) = "multiPhylo"
    return(out)
  } 
}


