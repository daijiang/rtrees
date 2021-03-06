#' Derive a phylogeny from a mega-tree
#' 
#' For a list of species, generate a phylogeny from a provided mega-tree. If a species is
#' not in the mega-tree, it will be grafted to the mega-tree with three scenarioes.
#' 
#' @param sp_list A data frame with at least three columns: species, genus, family. Species column
#' holds the species for which we want to have a phylogeny. It can also have two optional columns:
#' close_sp and close_genus. We can specify the cloest species/genus of the species based on
#' expert knowledgement. If specified, the new species will be grafted to that particular location.
#' 
#' It can also be a string vector if `taxon` is specified. Though it probably is a better idea
#' to prepare your data frame with [sp_list_df()].
#' 
#' In all cases, species names should be separated by `_` between genus and species, i.e. genus_sp.
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
#' @param tree_by_user Is the mega-tree provided by user?
#' @return A phylogeny for the species required, with class `phylo`.
#' @export
#' 
get_one_tree = function(sp_list, tree, taxon, 
                    # scenario = c("S1", "S2", "S3"), 
                    scenario = c("at_basal_node", "random_below_basal", "at_or_above_basal"), 
                    show_grafted = FALSE,
                    tree_by_user = FALSE) {
  if(tree_by_user & all(!grepl("_", tree$tip.label)))
    stop("Please change the tree's tip labels to be the format of genus_sp.")
  tree_genus = unique(gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label))
  
  sp_list = unique(sp_list) # remove duplications
  if(is.vector(sp_list, mode = "character")){
    sp_list = sp_list_df(sp_list)
  } else {
    if(!inherits(sp_list, "data.frame"))
      stop("`sp_list` must either be a string vector or a data frame")
    if(any(!c("species", "genus") %in% names(sp_list)))
      stop("`sp_list` must has at least two columns: species, genus.")
    sp_list$species = cap_first_letter(gsub(" +", "_", sp_list$species)) # just in case
  }
  
  all_genus_in_tree = all(unique(sp_list$genus) %in% tree_genus)
  # if TRUE, no taxon is required
  if(!all_genus_in_tree){
    if((!"family" %in% names(sp_list))){
      if(missing(taxon)) stop("Please specify `taxon`.")
      sp_list = sp_list_df(sp_list$species, taxon) # add family information
    }
  }
  sp_list = unique(sp_list) # remove duplications
  
  sp_out_tree = sp_list[!sp_list$species %in% tree$tip.label, ]
  
  # some tree tips has genus_sp_subsp
  subsp_in_tree = grep("^.*_.*_.*$", x = tree$tip.label, value = T)
  if(length(subsp_in_tree)){
    sp_out_tree = dplyr::mutate(sp_out_tree, re_matched = NA, matched_name = NA)
    for(i in 1:length(sp_out_tree$species)){
      name_in_tree = grep(paste0("^", sp_out_tree$species[i], "_"), x = subsp_in_tree,
                          ignore.case = T, value = T) 
      # avoid Allium_sp. match things like Allium_splendens
      # cat(i, name_in_tree, "\n")
      if(length(name_in_tree)) {
        sp_out_tree$re_matched[i] = TRUE
        sp_out_tree$matched_name[i] = sample(name_in_tree, 1)
        # rename the tree tip 
        tree$tip.label[tree$tip.label == sp_out_tree$matched_name[i]] = sp_out_tree$species[i]
        if(!is.null(tree$genus_family_root)) {
          tree$genus_family_root$only_sp[tree$genus_family_root$only_sp == 
                                           sp_out_tree$matched_name[i]] = 
            sp_out_tree$species[i]
        }
      }
    }
    sp_out_tree = dplyr::distinct(sp_list[!sp_list$species %in% tree$tip.label, ])
  }
  
  close_sp_specified = close_genus_specified = FALSE
  if("close_sp" %in% names(sp_out_tree)) close_sp_specified = TRUE
  if("close_genus" %in% names(sp_out_tree)) close_genus_specified = TRUE
  
  if(nrow(sp_out_tree) == 0){
    message("Wow, all species are already in the mega-tree!")
    tree_sub = ape::drop.tip(tree, setdiff(tree$tip.label, sp_list$species))
    return(tree_sub)
  }
  
  if(tree_by_user){
    if(!is.null(tree$genus_family_root)) 
      warning("The phylogeny has basal node information, are you sure this is an user provided tree?")
    if(all_genus_in_tree){
      tree = add_root_info(tree, process_all_tips = FALSE,
                           genus_list = unique(sp_out_tree$genus), show_warning = FALSE)
    } else { # some genus not in the tree
      if(missing(taxon)) stop("Please specify `taxon`.")
      genus_not_in_tree = dplyr::filter(sp_out_tree, !genus %in% tree_genus)
      # add root information for species not in the tree
      tree = add_root_info(
        tree, 
        classification = rtrees::classifications[rtrees::classifications$taxon == taxon, ], 
        process_all_tips = FALSE,
        genus_list = setdiff(sp_out_tree$genus, genus_not_in_tree$genus),
        family_list = ifelse(nrow(genus_not_in_tree) > 0, unique(genus_not_in_tree$family), NULL),
        show_warning = FALSE)
    }
  }
  
  scenario = match.arg(scenario)
  
  if(is.null(tree$genus_family_root))
    stop("Did you use your own phylogeny? If so, please set `tree_by_user = TRUE`.")
  sp_out_tree$status = ""
  tree_df = tidytree::as_tibble(tree)
  tree_df$is_tip = !(tree_df$node %in% tree_df$parent)
  node_hts = ape::branching.times(tree)
  node_label_new = NULL
  all_eligible_nodes = unique(c(tree$genus_family_root$basal_node,
                                tree$genus_family_root$root_node))
  
  if(nrow(sp_out_tree) > 100){
    progbar = utils::txtProgressBar(min = 0, max = nrow(sp_out_tree), initial = 0, style = 3)
  }
  
  for(i in 1:nrow(sp_out_tree)){
    if(nrow(sp_out_tree) > 100){
      utils::setTxtProgressBar(progbar, i)
    }
    # cat(i, "\t")
    where_loc_i = where_loc_i2 = NA
    
    if(close_sp_specified){
      if(!is.na(sp_out_tree$close_sp[i]) &
         sp_out_tree$close_sp[i] %in% tree$tip.label){
        where_loc_i = sp_out_tree$close_sp[i]
      }
    }
    
    if(close_genus_specified){
      if(!is.na(sp_out_tree$close_genus[i]) &
         sp_out_tree$close_genus[i] != "" &
         sp_out_tree$close_genus[i] %in% tree_genus){
        sp_out_tree$genus[i] = sp_out_tree$close_genus[i]
        where_loc_i2 = sp_out_tree$close_genus[i]
      } else {
        if(!is.na(sp_out_tree$close_genus[i]))
          warning("The genus specified for ", sp_out_tree$species[i], 
                  " is not in the phylogeny.")
      }
    }
    
    if(!all_genus_in_tree & is.na(where_loc_i) & is.na(where_loc_i2)){
      if(is.na(sp_out_tree$family[i]) | 
         !sp_out_tree$family[i] %in% tree$genus_family_root$family){
        sp_out_tree$status[i] = "No co-family species in the mega-tree"
        next()
      }
    }
    
    add_above_node = FALSE
    fraction = 1/2
    
    if(sp_out_tree$genus[i] %in% tree$genus_family_root$genus |
       !is.na(where_loc_i2) | !is.na(where_loc_i)){
      sp_out_tree$status[i] = "*"
      # tree has species in the same genus
      idx_row = which(tree$genus_family_root$genus == sp_out_tree$genus[i])
      root_sub = tree$genus_family_root[idx_row, ]
      if(root_sub$n_spp == 1 | !is.na(where_loc_i)) { # but only 1 species in this genus
        if(!is.na(where_loc_i)){# a close sp specified
          where_loc = where_loc_i
          # the new tip will be bind to this species, in the half of its branch length (default frac)
          new_ht = tree_df$branch.length[tree_df$label == where_loc_i] * (1 - fraction)
          node_hts = c(new_ht, node_hts) # update node ages since added 1 new node
          node_label_new = paste0("N", length(node_hts))
          names(node_hts)[1] = node_label_new
          all_eligible_nodes = c(all_eligible_nodes, node_label_new)
          if(!sp_out_tree$genus[i] %in% tree$genus_family_root$genus){
            # this is a new genus that has not in the tree
            # cat("here")
            tree$genus_family_root = tibble::add_row(
              tree$genus_family_root,
              family = sp_out_tree$family[i],
              genus = sp_out_tree$genus[i],
              basal_node = node_label_new,
              basal_time = new_ht,
              root_node = tree_df$lable[tree_df$node == tree_df$parent[tree_df$label == where_loc_i]],
              root_time = tree_df$branch.length[tree_df$node == tree_df$parent[tree_df$label == where_loc_i]],
              n_genus = 1, n_spp = 1, only_sp = sp_out_tree$species[i])
          }
        } else {
          where_loc = root_sub$only_sp
          # the new tip will be bind to this species, in the half of its branch length (default frac)
          new_ht = root_sub$basal_time * (1 - fraction)
          node_hts = c(new_ht, node_hts) # update node ages since added 1 new node
          node_label_new = paste0("N", length(node_hts))
          names(node_hts)[1] = node_label_new
          all_eligible_nodes = c(all_eligible_nodes, node_label_new)
          tree$genus_family_root$only_sp[idx_row] = NA # now will be more than 1 sp in this genus
        }
      } else { # more than 1 species in the genus
        where_loc = root_sub$basal_node # scenarioes 1 and 3, no new node added
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
        where_loc = root_sub$basal_node # for scenario 1, no new node added
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
    
    # when the clade is large, tidytree::offspring() will take a long time
    if(root_sub$n_spp > 100) use_castor = TRUE else use_castor = FALSE
    # cat(where_loc)
    tree_df = bind_tip(tree_tbl = tree_df, node_heights = node_hts, where = where_loc, 
                       new_node_above = add_above_node, tip_label = sp_out_tree$species[i], 
                       frac = fraction, return_tree = FALSE, node_label = node_label_new,
                       use_castor = use_castor)
    tree_df$is_tip[tree_df$label == sp_out_tree$species[i]] = TRUE
    tree_df$is_tip[is.na(tree_df$is_tip)] = FALSE
    # tree_df = dplyr::distinct(tree_df)
    # update n_spp in tree$genus_family_root
    tree$genus_family_root$n_spp[idx_row] = tree$genus_family_root$n_spp[idx_row] + 1
  }
  
  if(nrow(sp_out_tree) > 100){
    close(progbar)
  }
  
  if(any(sp_out_tree$status == "*")) {
    message(sum(sp_out_tree$status == "*"), " species added at genus level (*) \n")
  }
  
  if(any(sp_out_tree$status == "**")) {
    message(sum(sp_out_tree$status == "**"), " species added at family level (**) \n")
  }
  
  if(any(sp_out_tree$status == "No co-family species in the mega-tree")) {
    sp_no_family = sp_out_tree$species[sp_out_tree$status == "No co-family species in the mega-tree"]
    message(length(sp_no_family), " species have no co-family species in the mega-tree, skipped: \n",
            paste(sp_no_family, collapse = ", "))
  }
  
  tree_sub = castor::get_subtree_with_tips(tidytree::as.phylo(tree_df), sp_list$species)$subtree
  
  if(show_grafted){
    grafted = sp_out_tree[sp_out_tree$status %in% c("*", "**"), ]
    grafted$sp2 = paste0(grafted$species, grafted$status)
    wid = which(tree_sub$tip.label %in% grafted$species)
    tree_sub$tip.label[wid] = dplyr::left_join(tibble::tibble(species = tree_sub$tip.label[wid]),
                                               grafted, by = "species")$sp2
  }
  
  return(ape::ladderize(tree_sub))
}
