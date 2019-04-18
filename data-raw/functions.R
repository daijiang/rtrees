library(tidyverse)
library(ape)
library(tidytree)

#' @param tree A phylogeny with class "phylo".
#' @param classification A data frame of 2 columns: genus, family
#' @noRd
add_root_info = function(tree, classification){
  tree = makeLabel(tree, tips = FALSE, node = TRUE)
  if(any(ww <- grepl("^[0-9]*$", tree$node.label)))
    tree$node.label[ww] = paste0("N", tree$node.label[ww])
  tips = tibble(species = tree$tip.label, 
         genus = gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label)) %>% 
    left_join(classification, by = "genus")
  
  family_summ = tips %>%
    group_by(family) %>%
    summarise(n_genus = n_distinct(genus), n_spp = n_distinct(species)) %>%
    mutate(genus = NA)
  genus_summ = tips %>%
    group_by(genus) %>%
    summarise(n_genus = n_distinct(genus), n_spp = n_distinct(species)) %>%
    left_join(classification, by = "genus")
  gf_summ = bind_rows(family_summ, genus_summ) %>%
    mutate(grp = 1:n())
  
  #' Find basal and root nodes for genus and families in a phylogeny
  #'
  #' @param xdf A data frame with 1 row and columns from the above `gf_summ`.
  #' @param tips A data frame with at least 3 columns: species, genus, family. All tips of the
  #' tree are in the species column.
  #' @param tree_df A data frame of the tree, generated from `tidytree::as_tibble(tree)`.
  find_root = function(xdf, tips, tree_df){
    target = xdf$genus
    if(fam <- is.na(target)) target = xdf$family
    if(fam){ # members of this genus or family
      sp_names = filter(tips, family == target)$species
    } else {
      sp_names = filter(tips, genus == target)$species
    }
    tree_df_subset = filter(tree_df, label %in% sp_names)
    basel_node = tidytree::MRCA(tree_df, min(tree_df_subset$node), max(tree_df_subset$node))
    if(length(sp_names) == 1){ # only 1 sp
      root_node = basel_node
    } else {
      root_node = tidytree::parent(tree_df, basel_node$node)
    }
    tibble(basel_node = basel_node$label,
           root_node = root_node$label,
           only_sp = if(length(sp_names) == 1) sp_names else NA)
  }
  
  # this takes time
  tree_df = tidytree::as_tibble(tree)
  gf_summ2 = gf_summ %>%
    group_by(grp) %>%
    do(find_root(., tips, tree_df))
  
  node_heights = ape::branching.times(tree)
  
  gf_summ3 = left_join(gf_summ, gf_summ2, by = "grp") %>%
    mutate(root_time = node_heights[root_node],
           basel_time = node_heights[basel_node]) %>%
    ungroup() %>%
    select(family, genus, basel_node, basel_time, root_node, root_time, n_genus, n_spp, only_sp)
  
  tree$genus_family_root = gf_summ3
  
  tree
}
