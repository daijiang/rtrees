#' Derive a phylogeny from a mega-tree
#' 
#' For a list of species, generate a phylogeny from a provided mega-tree. If a species is
#' not in the mega-tree, it will be inserted to the mega-tree with three scenarioes.
#' 
#' @param sp_list A data frame with at least three columns: species, genus, family. Species column
#' holds the species for which we want to have a phylogeny. It can also have two optional columns:
#' close_sp and close_genus. We can specify the cloest species/genus of the species based on
#' expert knowledgement. If specified, the new species will be grafted to this particular location.
#' 
#' It can also be a string vector if `taxon` is specified. Though it probably is a better idea
#' to prepare your data frame with [sp_list_df()].
#' @param tree A mega-tree with class `phylo`. Optional if `taxon` is specified, in which case, a default
#' mega-phylogeny will be used.
#' 
#' - For plant, the mega-tree is [tree_plant_otl].
#' - For fish, the mega-tree is [tree_fish].
#' - For bird, the mega-trees are [tree_bird_ericson] and [tree_bird_hackett]. [tree_bird_ericson] will be the 
#' default if no `tree` is specified and `taxon` is `bird`.
#' - For mammal, the mega-tree is [tree_mammal].
#' 
#' @param taxon The taxon of species in the `sp_list`. Currently, can be `plant`, `fish`, `bird`, or `mammal`.
#' @param scenario How to insert a species into the mega-tree? 
#' - If `scenario = "S1"`, a species is attached to the basal node of the same genus or the same family 
#' if the mega-tree does not have any species of this genus. 
#' - If `scenario = "S2"`, a species is attached to a randomly selected node that is at or below the 
#' basal node of the same genus of the same family if the mega-tree does not have any species in this genus.
#' The probability of node been selected is proportional to its branch length.
#' Because of the random sampling involved, you may want to run several times to get a collection of 
#' derived phylogenies.
#' - If `scenario = "S3"`, a species is attached to the basal node of the same genus if the mega-tree has species 
#' in the same genus; otherwise, a species is inserted to a new node above the basal node of the family.
#' If the age of the basal node is less than 2/3 of the node above it (root node of the family), the new node will
#' be added so that its age will be 2/3 of the root node of the family. Otherwise, a new node will be inserted 
#' into the middle point of the basal node and the root node of the family.
#' - In all scenarioes, if there is only 1 species in the genus or family, a new node will be inserted to
#' the middle point of this only species' branch length and the new species will be attached to this new 
#' node.
#' @param show_grafted Whether to indicate which species was grafted onto the mega-tree. 
#' If `TRUE`, a `*` will be appended to the species name on the tip if it was grafted within
#' the same genus; `**` will be appended if it was grafted within the same family.
#' @param tree_by_user Is the mega-tree provided by user?
#' @return A phylogeny for the species required, with class `phylo`.
#' @export
#' 
get_tree = function(sp_list, tree, taxon, 
                    scenario = c("S1", "S2", "S3"), 
                    show_grafted = FALSE,
                    tree_by_user = FALSE) {
  if(is.vector(sp_list, mode = "character") & !missing(taxon))
    sp_list = sp_list_df(sp_list, taxon)
  if(!inherits(sp_list, "data.frame"))
    stop("sp_list must be a data frame with at least these columns: species, genus, family.")
  if(any(!c("species", "genus", "family") %in% names(sp_list)))
    stop("sp_list must has at least these columns: species, genus, family.")
  if(missing(tree) & missing(taxon))
    stop("Please specify at least a tree or a taxon group.")
  if(missing(tree) & !missing(taxon)){# pick default tree
    tree = switch(taxon,
      plant = tree_plant_otl,
      fish = tree_fish,
      bird = tree_bird_ericson,
      mammal = tree_mammal
    )
  }
  scenario = match.arg(scenario)
  
  sp_list$species = gsub(" +", "_", sp_list$species)
  sp_out_tree = sp_list[!sp_list$species %in% tree$tip.label, ]
  
  if(nrow(sp_out_tree) == 0){
    message("Wow, all species are already in the mega-tree!")
    tree_sub = ape::drop.tip(tree, setdiff(tree$tip.label, sp_list$species))
    return(tree_sub)
  }
  
  sp_out_tree$status = ""
  tree_df = tidytree::as_tibble(tree)
  tree_df$is_tip = !(tree_df$node %in% tree_df$parent)
  node_hts = ape::branching.times(tree)
  node_label_new = NULL
  all_eligible_nodes = unique(c(tree$genus_family_root$basal_node,
                                tree$genus_family_root$root_node))
  
  for(i in 1:nrow(sp_out_tree)){
    # cat(i)
    if(!sp_out_tree$family[i] %in% tree$genus_family_root$family){
      sp_out_tree$status[i] = "No co-family species in the mega-tree"
      next()
    }
    add_above_node = FALSE
    fraction = 1/2
    if(sp_out_tree$genus[i] %in% tree$genus_family_root$genus){
      sp_out_tree$status[i] = "*"
      # tree has species in the same genus
      idx_row = which(tree$genus_family_root$genus == sp_out_tree$genus[i])
      root_sub = tree$genus_family_root[idx_row, ]
      if(root_sub$n_spp == 1) { # but only 1 species in this genus
        where_loc = root_sub$only_sp
        # the new tip will be bind to this species, in the half of its branch length (default frac)
        new_ht = root_sub$basal_time * (1 - fraction)
        node_hts = c(new_ht, node_hts) # update node ages since added 1 new node
        node_label_new = paste0("N", length(node_hts))
        names(node_hts)[1] = node_label_new
        all_eligible_nodes = c(all_eligible_nodes, node_label_new)
        tree$genus_family_root$only_sp[idx_row] = NA # now will be more than 1 sp in this genus
      } else { # more than 1 species in the genus
        where_loc = root_sub$basal_node # scenarioes 1 and 3
        if(scenario == "S2"){ # randomly select a node in the genus and attach to it, no new node added
          tree_df_sub = dplyr::filter(tidytree::offspring(tree_df, where_loc), !is_tip)
          if(nrow(tree_df_sub) > 0){
            potential_locs = c(where_loc, tree_df_sub$label)
            bls = tree_df_sub$branch.length
            names(bls) = tree_df_sub$label
            bls = c(root_sub$root_time - root_sub$basal_time, bls)
            names(bls)[1] = root_sub$basal_node
            prob = bls/sum(bls)
            where_loc = sample(potential_locs, 1, prob = prob)
          }
        }
      }
    } else {
      # no species in the same genus in the tree, go up to family node
      sp_out_tree$status[i] = "**"
      idx_row = which(tree$genus_family_root$family == sp_out_tree$family[i] & 
                        is.na(tree$genus_family_root$genus))
      root_sub = tree$genus_family_root[idx_row, ]
      
      if(root_sub$n_spp == 1) { # but only 1 species in this family
        where_loc = root_sub$only_sp
        # the new tip will be bind to this species, in the half of its branch length (default frac)
        new_ht = root_sub$basal_time * (1 - fraction)
        node_hts = c(new_ht, node_hts) # update node ages since added 1 new node
        node_label_new = paste0("N", length(node_hts))
        names(node_hts)[1] = node_label_new
        all_eligible_nodes = c(all_eligible_nodes, node_label_new)
        tree$genus_family_root = tibble::add_row(tree$genus_family_root,
                                                 family = sp_out_tree$family[i],
                                                 genus = sp_out_tree$genus[i],
                                                 basal_node = node_label_new,
                                                 basal_time = unname(new_ht),
                                                 root_node = node_label_new,
                                                 root_time = unname(new_ht),
                                                 n_genus = 1,
                                                 n_spp = 1, 
                                                 only_sp = sp_out_tree$species[i]
                                                 )
      } else { # more than 1 species; can be the same genus or different genus
        where_loc = root_sub$basal_node # for scenario 1 & 3
        if(scenario == "S2"){ # randomly select a node in the family, no new node added
          tree_df_sub = dplyr::filter(tidytree::offspring(tree_df, where_loc), !is_tip)
          if(nrow(tree_df_sub) > 0){
            # only bind to genus/family basal node, not within genus nodes
            potential_locs = intersect(c(where_loc, tree_df_sub$label), all_eligible_nodes)
            locs_bl = tree_df_sub[tree_df_sub$label %in% potential_locs, ]
            bls = locs_bl$branch.length
            names(bls) = locs_bl$label
            bls = c(root_sub$root_time - root_sub$basal_time, bls)
            names(bls)[1] = root_sub$basal_node
            prob = bls/sum(bls)
            where_loc = sample(potential_locs, 1, prob = prob)
          }
        }
        if(scenario == "S3"){ # insert new node and bind tip above family basal node
          add_above_node = TRUE
          if(2 * root_sub$root_time / 3 > root_sub$basal_time){
            fraction = (2 * root_sub$root_time / 3 - root_sub$basal_time) /
              (root_sub$root_time - root_sub$basal_time)
          }
          new_ht = unname(root_sub$basal_time + (root_sub$root_time - root_sub$basal_time) * (1 - fraction))
          # here is the node height, but in bind_tip, it is the length between a parent and a node,
          # thus 1 - fraction
          node_hts = c(new_ht, node_hts) # update node ages since added 1 new node
          node_label_new = paste0("N", length(node_hts)) 
          names(node_hts)[1] = node_label_new
          all_eligible_nodes = c(all_eligible_nodes, node_label_new)
          tree$genus_family_root$basal_node[idx_row] = node_label_new # new basal node
          tree$genus_family_root$basal_time[idx_row] = new_ht
          tree$genus_family_root = tibble::add_row(tree$genus_family_root,
                                                   family = sp_out_tree$family[i],
                                                   genus = sp_out_tree$genus[i],
                                                   basal_node = node_label_new,
                                                   basal_time = new_ht,
                                                   root_node = root_sub$root_node,
                                                   root_time = root_sub$root_time,
                                                   n_genus = 1,
                                                   n_spp = 1, 
                                                   only_sp = sp_out_tree$species[i])
        }
      }
      # update genus number
      tree$genus_family_root$n_genus[idx_row] = tree$genus_family_root$n_genus[idx_row] + 1
    }
    
    tree_df = bind_tip(tree_tbl = tree_df, node_heights = node_hts, where = where_loc, 
                       new_node_above = add_above_node, tip_label = sp_out_tree$species[i], 
                       frac = fraction, return_tree = FALSE, node_label = node_label_new)
    tree_df$is_tip[tree_df$label == sp_out_tree$species[i]] = TRUE
    tree_df$is_tip[is.na(tree_df$is_tip)] = FALSE
    # update n_spp in tree$genus_family_root
    tree$genus_family_root$n_spp[idx_row] = tree$genus_family_root$n_spp[idx_row] + 1
  }
  
  if(any(sp_out_tree$status == "No co-family species in the mega-tree")) {
    message("These species have no species in the same family in the mega-tree, skipped: \n",
        sp_out_tree$species[sp_out_tree$status == "No co-family species in the mega-tree"])
  }
  
  tree_sub = ape::drop.tip(tidytree::as.phylo(tree_df), setdiff(tree$tip.label, sp_list$species))
  
  if(show_grafted){
    grafted = sp_out_tree[sp_out_tree$status %in% c("*", "**"), ]
    grafted$sp2 = paste0(grafted$species, grafted$status)
    wid = which(tree_sub$tip.label %in% grafted$species)
    tree_sub$tip.label[wid] = dplyr::left_join(tibble::tibble(species = tree_sub$tip.label[wid]),
                                               grafted, by = "species")$sp2
  }
  return(tree_sub)
}
