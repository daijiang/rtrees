#' Captize the first letter of a word.
#' @param x A word.
#' @return The same word with the first letter captized.
#' @noRd
cap_first_letter = function (x) {
  sub("^([a-z])", "\\U\\1", x, perl = TRUE)
}

if(getRversion() >= "2.15.1") 
  utils::globalVariables(c(".", "isTip", "is_tip", "node",
                           "tree_fish", "tree_plant_otl", "classifications",
                           "tree_bird_ericson", "tree_mammal", "taxon",
                           "family", "genus", "species", "grp",
                           "root_node", "basal_node"))

#' Convert a vector of species names to a data frame
#' 
#' @param sp_list A string vector.
#' @param taxon The taxon group of this species list.
#' @return A data frame with 3 columns: species, genus, and family.
#' @export
#' @examples 
#' sp_list_df(sp_list = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii"),
#'            taxon = "fish")
sp_list_df = function(sp_list, taxon){
  groups_supported = c("plant", "fish", "bird", "mammal")
  if(!taxon %in% groups_supported) 
    stop("Sorry but only the following taxon groups are supported: ", 
         paste(groups_supported, collapse = ", "))
  if(!is.vector(sp_list, mode = "character"))
    stop("sp_list must be a character vector.")
  utils::data("classifications", envir = environment())
  clsf = classifications[classifications$taxon == taxon, ]
  out = tibble::tibble(species = sp_list,
                 genus = gsub("^([-A-Za-z]*)_.*$", "\\1", sp_list))
  if(mean(out$genus %in% clsf$genus) < 0.9)
    warning("Are you sure that you specified the right taxon group?", call. = FALSE)
  out = dplyr::left_join(out, clsf, by = "genus")
  out$taxon = NULL
  out
}

#' Add genus and family basal/root node information to a phylogeny
#' 
#' Based on the classification of tips, find where is the basal and root node for
#' each genus and each family. Such information can be later used to graft new 
#' tips onto the phylogeny. This function can be used to process a user provided
#' tree.
#' 
#' @param tree A phylogeny with class "phylo".
#' @param classification A data frame of 2 columns: genus, family. It should include
#' all genus the tips of the tree belong to.
#' @param genus_list An optinoal subset list of genus to find root information.
#' @param family_list An optinoal subset list of family to find root information. 
#' This should be for species that do not have co-genus in the tree.
#' @param show_warning Whether to print warning information or not.
#' @return A phylogeny with basal nodes information attached.
#' @export
#' 
add_root_info = function(tree, classification, 
                         genus_list = NULL, family_list = NULL,
                         show_warning = TRUE){
  if(is.null(tree$node.label))
    tree$node.label = paste0("N", 1:ape::Nnode(tree))
  tree = ape::makeLabel(tree, tips = FALSE, node = TRUE)
  if(any(ww <- grepl("^[0-9]*$", tree$node.label)))
    tree$node.label[ww] = paste0("N", tree$node.label[ww])
  tips = tibble::tibble(species = tree$tip.label, 
                        genus = gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label))
  if(any(!tips$genus %in% classification$genus) & 
     is.null(genus_list) & is.null(family_list) &
     show_warning)
    warning("Some genus are not in the classification.")
  tips = dplyr::left_join(tips, classification, by = "genus")
  if(!is.null(genus_list)) {
    if(any(!genus_list %in% tips$genus) & show_warning)
      warning("Some genus_list are not in the tree.")
    tips_genus = tips[tips$genus %in% genus_list, ]
  }
  if(!is.null(family_list)) {
    if(any(!family_list %in% tips$family) & show_warning)
      warning("Some family_list are not in the tree.")
    tips_family = tips[tips$family %in% family_list, ]
  }
  if(!is.null(genus_list)){
    tips = tips_genus
  }
  if(!is.null(family_list)){
    tips = unique(dplyr::bind_rows(tips, tips_family))
  }
  
  family_summ = dplyr::mutate(
    dplyr::summarise(dplyr::group_by(tips, family), 
                     n_genus = dplyr::n_distinct(genus), 
                     n_spp = dplyr::n_distinct(species)),
    genus = NA)
  family_summ = family_summ[!is.na(family_summ$family), ]
  genus_summ = dplyr::left_join(
    dplyr::summarise(dplyr::group_by(tips, genus), 
                     n_genus = dplyr::n_distinct(genus), 
                     n_spp = dplyr::n_distinct(species)),
    classification, by = "genus")
  gf_summ = dplyr::bind_rows(family_summ, genus_summ)
  
  gf_summ$grp = 1:nrow(gf_summ)
  
  find_root = function(xdf, tips, tree_df){
    target = xdf$genus
    if(fam <- is.na(target)) target = xdf$family
    if(fam){ # members of this genus or family
      sp_names = tips$species[tips$family == target]
    } else {
      sp_names = tips$species[tips$genus == target]
    }
    sp_names = sp_names[!is.na(sp_names)]
    # cat(sp_names)
    tree_df_subset = tree_df[tree_df$label %in% sp_names, ]
    basal_node = tidytree::MRCA(tree_df, min(tree_df_subset$node), max(tree_df_subset$node))
    if(length(sp_names) == 1){ # only 1 sp
      root_node = basal_node
    } else {
      root_node = tidytree::parent(tree_df, basal_node$node)
    }
    tibble::tibble(basal_node = basal_node$label,
                   root_node = root_node$label,
                   only_sp = if(length(sp_names) == 1) sp_names else NA)
  }
  
  # this takes time
  tree_df = tidytree::as_tibble(tree)
  gf_summ2 = dplyr::do(dplyr::group_by(gf_summ, grp), 
                       find_root(., tips, tree_df))
  
  node_heights = ape::branching.times(tree)
  
  gf_summ3 = dplyr::left_join(gf_summ, dplyr::ungroup(gf_summ2), by = "grp")
  gf_summ3 = dplyr::mutate(gf_summ3, 
                           root_time = node_heights[root_node],
                           basal_time = node_heights[basal_node]) 
  gf_summ3 = gf_summ3[, c("family", "genus", "basal_node", "basal_time", 
                          "root_node", "root_time", "n_genus", "n_spp", "only_sp")]
  
  tree$genus_family_root = gf_summ3
  
  tree
}

