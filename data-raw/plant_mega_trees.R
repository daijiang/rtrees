#' library(tidyverse)
#' library(tidytree)
#' 
#' # plants ----
#' load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/GBOTB.extended.rda")))
#' tree_plant_GBOTB = GBOTB.extended
#' load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/nodes.info.1.rda")))
#' tree_plant_GBOTB = ape::makeLabel(tree_plant_GBOTB, tips = F)
#' tree_plant_GBOTB$node.label[grep("^[0-9]*$", tree_plant_GBOTB$node.label)] =
#'   paste0("N", tree_plant_GBOTB$node.label[grep("^[0-9]*$", tree_plant_GBOTB$node.label)])
#' any(duplicated(tree_plant_GBOTB_df$label))
#' 
#' tree_plant_GBOTB_df = as_tibble(tree_plant_GBOTB)
#' tips = enframe(GBOTB.extended$tip.label, value = "species") %>% 
#'   mutate(genus = gsub("^([-A-Za-z]*)_.*$", "\\1", species)) %>% 
#'   left_join(unique(select(nodes.info.1, genus, family)), by = "genus") %>% 
#'   select(-name)
#' filter(tips, is.na(family))
#' # https://en.wikipedia.org/wiki/Trophis_scandens
#' tips$family[tips$species == "Malaisia_scandens"] = "Euphorbiaceae" # wiki
#' # but the plant list said it is an annoymous...which makes me wonder how 
#' # V.PhyloMaker did their name standardization. I probably should do it by myself.
#' # http://www.theplantlist.org/tpl1.1/record/kew-5837
#' tips$family[tips$species == "Lithraea_molleoides"] = "Anacardiaceae" # wiki
#' # https://en.wikipedia.org/wiki/Lithraea_molleoides
#' 
#' family_summ = tips %>%
#'   group_by(family) %>%
#'   summarise(n_genus = n_distinct(genus), n_spp = n_distinct(species)) %>%
#'   mutate(genus = NA)
#' genus_summ = tips %>%
#'   group_by(genus) %>%
#'   summarise(n_genus = n_distinct(genus), n_spp = n_distinct(species)) %>%
#'   left_join(unique(select(tips, family, genus)), by = "genus")
#' gf_summ = bind_rows(family_summ, genus_summ) %>%
#'   mutate(grp = 1:n())
#' 
#' #' Find basal and root nodes for genus and families in a phylogeny
#' #' 
#' #' @param xdf A data frame with 1 row and columns from the above `gf_summ`.
#' #' @param tips A data frame with at least 3 columns: species, genus, family. All tips of the 
#' #' tree are in the species column.
#' #' @param tree_df A data frame of the tree, generated from `tidytree::as_tibble(tree)`.
#' find_root = function(xdf, tips, tree_df){
#'   target = xdf$genus
#'   if(fam <- is.na(target)) target = xdf$family
#'   if(fam){ # members of this genus or family
#'     sp_names = filter(tips, family == target)$species
#'   } else {
#'     sp_names = filter(tips, genus == target)$species
#'   }
#'   tree_df_subset = filter(tree_df, label %in% sp_names)
#'   basel_node = tidytree::MRCA(tree_df, min(tree_df_subset$node), max(tree_df_subset$node))
#'   if(length(sp_names) == 1){ # only 1 sp
#'     root_node = basel_node
#'   } else {
#'     root_node = tidytree::parent(tree_df, basel_node$node)
#'   }
#'   tibble(basel_node = basel_node$label,
#'          root_node = root_node$label,
#'          only_sp = if(length(sp_names) == 1) sp_names else NA)
#' }
#' # this takes about 10 mins
#' gf_summ2 = gf_summ %>%
#'   group_by(grp) %>%
#'   do(find_root(., tips, tree_plant_GBOTB_df))
#' 
#' node_heights = ape::branching.times(tree_plant_GBOTB)
#' gf_summ3 = left_join(gf_summ, gf_summ2, by = "grp") %>%
#'   mutate(root_time = node_heights[root_node],
#'          basel_time = node_heights[basel_node]) %>% 
#'   ungroup() %>% 
#'   select(family, genus, basel_node, basel_time, root_node, root_time, n_genus, n_spp, only_sp)
#' str(tree_plant_GBOTB)
#' tree_plant_GBOTB$genus_family_root = gf_summ3
#' usethis::use_data(tree_plant_GBOTB, overwrite = T)
#' tools::checkRdaFiles("data/tree_plant_GBOTB.rda")
#' 
#' # sp list of plants
#' sp_list = tibble::as_tibble(read.csv("~/Dropbox/Reading/ECOG-04434/Appendix_3-Example_species_list.csv", stringsAsFactors = F)) 
#' 
#' ape::branching.times(tidytree::as.phylo(tree_df))[where]
#' system.time(tst1 <- get_tree(dplyr::filter(sp_list, !species %in% c("Hicoria_texana")), scenario = "S3"))
#' system.time(tst1 <- get_tree(sp_list, scenario = "S3"))
#' plot(tst1)
#' 
#' library(V.PhyloMaker)
#' system.time(tst1_v <- V.PhyloMaker::phylo.maker(sp_list, scenarios = "S3"))
#' par(mfrow = c(1, 2))
#' plot(ladderize(tst1), cex = 0.8)
#' plot(ladderize(tst1_v$scenario.2$run.1), cex = 0.8)
#' plot(ladderize(tree_sub), cex = 0.8)
#' 
#' View(tidytree::offspring(tree_df, "N74476"))