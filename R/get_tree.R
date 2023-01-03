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
#' @param sp_list A character vector or a data frame with at least three columns: species, genus, family. Species column
#' holds the species for which we want to have a phylogeny. It can also have two optional columns:
#' close_sp and close_genus. We can specify the closest species/genus of the species based on
#' expert knowledge. If specified, the new species will be grafted to that particular location.
#' 
#' It can also be a string vector if `taxon` is specified. Though it probably is a better idea
#' to prepare your data frame with [sp_list_df()]. The string vector can also have the same format
#' as that required by Phylomatic (i.e., family/genus/genus_sp).
#' @param tree A mega-tree with class `phylo` or a list of mega-trees with class `multiPhylo`.
#'  Optional if `taxon` is specified, in which case, a default
#' mega-phylogeny (or a set of 100 randomly selected posterior phylogenies) will be used 
#' (see their own documentations from the `megatrees` package).
#' 
#' - For amphibian, the mega-trees are [megatrees::tree_amphibian_n100].
#' - For bird, the mega-trees are [megatrees::tree_bird_n100].
#' - For fish, the mega-tree is [megatrees::tree_fish_12k], with [megatrees::tree_fish_32k_n50] be the other option.
#' - For mammal, the default mega-trees are [megatrees::tree_mammal_n100_vertlife], with [megatrees::tree_mammal_n100_phylacine] be the other option.
#' - For plant, the mega-tree is [megatrees::tree_plant_otl].
#' - For reptile, the mega-trees are [megatrees::tree_reptile_n100].
#' - For shark, ray, and chimaeras, the mega-trees are [megatrees::tree_shark_ray_n100].
#' 
#' @param taxon The taxon of species in the `sp_list`. Currently, can be `amphibian`, `bird`, `fish`, `mammal`, `plant`, `reptile`, or `shark_ray`.
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
#' @param mc_cores Number of cores to parallel processing when `tree` is a list of large number of trees. The default is the number of available cores minus 2.
#' @param .progress Form of progress bar, default to be text.
#' @param fish_tree Which fish tree do you want to use? If it is "timetree" (default), it will be the smaller time tree with 11638 species
#' that all have sequence data; if it is "all-taxon", then it will be the 100 larger posterior phylogenies with 31516 soecues.
#' @param mammal_tree Which set of mammal trees to use? If it is "vertlife" (default), then 100 randomly selected posterior phylogenies provided
#' by Vertlife will be used; if it is "phylacine", then 100 randomly selected posterior phylogenies provided by PHYLACINE will be used.
#' @param dt Whether to use data.table version to bind tips [bind_tip]. The default is `TRUE` as it maybe slightly faster.
#' @return A phylogeny for the species required, with class `phylo`; 
#' or a list of phylogenies with class `multiPhylo` depends on the input `tree`. Within each phylogeny, the grafted status of all species was saved as a data frame named as "graft_status".
#' @export
#' @examples 
#' test_sp = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii", 
#' "Periophthalmus_barbarus", "Prognichthys_glaphyrae", "Barathronus_bicolor", 
#' "Knipowitschia_croatica", "Rhamphochromis_lucius", "Neolissochilus_tweediei", 
#' "Haplochromis_nyanzae", "Astronesthes_micropogon", "Sanopus_reticulatus")
#' test_tree = get_tree(sp_list = test_sp,
#'                      taxon = "fish",
#'                      show_grafted = TRUE)

get_tree = function(sp_list, tree, taxon = NULL, 
                    # multiple_trees = TRUE,
                    scenario = c("at_basal_node", "random_below_basal", "at_or_above_basal"), 
                    show_grafted = FALSE,
                    tree_by_user = FALSE,
                    mc_cores = future::availableCores() - 2, .progress = "text",
                    fish_tree = c("timetree", "all-taxon"),
                    mammal_tree = c("vertlife", "phylacine"),
                    dt = TRUE
                    ){
  scenario = match.arg(scenario)
  
  if(missing(tree) & is.null(taxon))
    stop("Please specify at least a tree or a taxon group.")
  if(missing(tree) & !is.null(taxon)){# pick default tree
    if(taxon == "plant") tree = megatrees::tree_plant_otl
    if(taxon == "bird") tree = megatrees::tree_bird_n100
    if(taxon == "amphibian") tree = megatrees::tree_amphibian_n100
    if(taxon == "reptile") tree = megatrees::tree_reptile_n100
    if(taxon == "shark_ray") tree = megatrees::tree_shark_ray_n100
    if(taxon == "mammal"){
      mammal_tree = match.arg(mammal_tree)
      if (mammal_tree == "vertlife") tree = megatrees::tree_mammal_n100_vertlife
      if (mammal_tree == "phylacine") tree = megatrees::tree_mammal_n100_phylacine
    }
    if(taxon == "fish"){
      fish_tree = match.arg(fish_tree)
      if (fish_tree == "timetree") tree = megatrees::tree_fish_12k
      if (fish_tree == "all-taxon") tree = megatrees::tree_fish_32k_n50
    }
  }
  
  if(inherits(tree, "phylo")){ # one phylo
    return(get_one_tree(sp_list = sp_list, tree = tree, taxon = taxon, 
                        scenario = scenario, show_grafted = show_grafted,
                        tree_by_user = tree_by_user, .progress = .progress, 
                        dt = dt))
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
      if(mc_cores > future::availableCores()) {
        message("The specified mc_cores is larger than what available")
        mc_cores = future::availableCores() - 1
      }
      future::plan(future::multisession, workers = mc_cores)
      # run it once just to get the messages, not ideal but I did not find another way
      # to only show the message once yet.
      out0 = rtrees::get_one_tree(sp_list, tree = tree[[1]], taxon = taxon, 
                           scenario = scenario, show_grafted = show_grafted,
                           tree_by_user = tree_by_user, 
                           .progress = "none", dt = dt)
      
      out = furrr::future_map(tree[-1], function(i){
        suppressMessages(rtrees::get_one_tree(sp_list, tree = i, taxon = taxon, 
                             scenario = scenario, show_grafted = show_grafted,
                             tree_by_user = tree_by_user, 
                             .progress = "none", dt = dt))
      # .progress = "none" # hide progress bar
      }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
      
      # put the first generated phylogeny back and in order
      out[[length(tree)]] = out0
      names(out)[length(tree)] = names(tree)[1]
      out = c(out[length(tree)], out[-length(tree)])
    } else {
      ## TO DO: test .progress issue with lapply()
      out = lapply(tree, get_one_tree, sp_list = sp_list, taxon = taxon, 
                   scenario = scenario, show_grafted = show_grafted, 
                   tree_by_user = tree_by_user, .progress = .progress, dt = dt)
    }
    
    class(out) = "multiPhylo"
    return(out)
  } 
}


