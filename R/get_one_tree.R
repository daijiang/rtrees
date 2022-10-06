#' Derive a phylogeny from a mega-tree
#' 
#' For a list of species, generate a phylogeny from a provided mega-tree. If a species is
#' not in the mega-tree, it will be grafted to the mega-tree with three scenarioes.
#' 
#' @inheritParams get_tree
#' @return A phylogeny for the species required, with class `phylo`.
#' @export
#' 
get_one_tree = function(sp_list, tree, taxon, 
                        scenario = c("at_basal_node", "random_below_basal", "at_or_above_basal"), 
                        show_grafted = FALSE,
                        tree_by_user = FALSE,
                        .progress = "text", dt = TRUE) {
  if(tree_by_user & all(!grepl("_", tree$tip.label)))
    stop("Please change the tree's tip labels to be the format of genus_sp.")
  tree_genus = unique(gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label))
  
  sp_list = sp_list_df(unique(sp_list)) # remove duplications and prep genus, family
  
  all_genus_in_tree = all(unique(sp_list$genus) %fin% tree_genus)
  # if TRUE, no taxon is required
  if(!all_genus_in_tree){
    if((!"family" %fin% names(sp_list))){ # no family info
      if(missing(taxon) | is.null(taxon)) 
        stop("Please specify `taxon` as not all genus are in the tree.")
      sp_list = sp_list_df(sp_list$species, taxon) # add family information
    }
  }
  
  # add new classification data to classification data frame
  if(!is.null(taxon)){
    if(!taxon %fin% rtrees::taxa_supported & !all_genus_in_tree){
      new_cls = unique(dplyr::select(sp_list, genus, family))
      new_cls$taxon = taxon
      classifications <- dplyr::bind_rows(rtrees::classifications, new_cls)
    }
  } 
  
  sp_out_tree = sp_list[!sp_list$species %fin% tree$tip.label, ]
  
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
    sp_out_tree = dplyr::distinct(sp_list[!sp_list$species %fin% tree$tip.label, ])
  }
  
  close_sp_specified = close_genus_specified = FALSE
  if("close_sp" %fin% names(sp_out_tree)) {
    close_sp_specified = TRUE
    # in case of white spaces in names
    sp_out_tree$close_sp = cap_first_letter(gsub(" +", "_", sp_out_tree$close_sp))
  }
  if("close_genus" %fin% names(sp_out_tree)) close_genus_specified = TRUE
  
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
      message("Not all genus can be found in the phylogeny.")
      if(is.null(tree$genus_family_root)){
        warning("For user provided phylogeny, without a classification for all genus of species in the phylogeny,
              it is unlikely to find the most recent ancestor for genus and family; here we proceed the phylogeny
              by adding root information for genus and family that can be found in the phylogeny or species list but
              we recommend to prepare the phylogeny using `add_root_info()` with a classification
              data frame with all tips first.", call. = FALSE, immediate. = TRUE)
      }
      genus_not_in_tree = dplyr::filter(sp_out_tree, !genus %fin% tree_genus)
      
      # add root information for species not in the tree
      tree = add_root_info(
        tree, 
        classification = if(is.null(taxon) & 
                            inherits(sp_list, "data.frame") & 
                            all(c("genus", "family") %fin% names(sp_list))){
          unique(sp_list[, c("genus", "family")])
        } else {
          unique(classifications[classifications$taxon == taxon, ])
        }, 
        process_all_tips = FALSE,
        genus_list = if(length(setdiff(sp_out_tree$genus, genus_not_in_tree$genus))) setdiff(sp_out_tree$genus, genus_not_in_tree$genus) else NULL,
        family_list = if(nrow(genus_not_in_tree) > 0) unique(genus_not_in_tree$family) else NULL,
        show_warning = FALSE)
    }
  }
  
  scenario = match.arg(scenario)
  
  if(is.null(tree$genus_family_root))
    stop("Did you use your own phylogeny? If so, please set `tree_by_user = TRUE`.")
  sp_out_tree$status = ""
  tree_df = tidytree::as_tibble(tree)
  tree_df$is_tip = !(tree_df$node %fin% tree_df$parent)
  node_hts = ape::branching.times(tree)
  all_eligible_nodes = unique(c(tree$genus_family_root$basal_node,
                                tree$genus_family_root$root_node))
  
  n_spp_to_show_progress = 200
  if(nrow(sp_out_tree) > n_spp_to_show_progress){
    progress <- create_progress_bar(.progress)
    progress$init(nrow(sp_out_tree))
    on.exit(progress$term())
  }
  
  for(i in 1:nrow(sp_out_tree)){
    # if(nrow(sp_out_tree) > 100){
    #   utils::setTxtProgressBar(progbar, i)
    # }
    # cat(i, "\t")
    if(nrow(sp_out_tree) > n_spp_to_show_progress)
      progress$step()
    
    where_loc_i = where_loc_i2 = NA
    
    if(close_sp_specified){
      if(!is.na(sp_out_tree$close_sp[i]) &
         sp_out_tree$close_sp[i] %fin% tree$tip.label){
        where_loc_i = sp_out_tree$close_sp[i]
      }
    }
    
    if(close_genus_specified){
      if(!is.na(sp_out_tree$close_genus[i]) &
         sp_out_tree$close_genus[i] != "" &
         sp_out_tree$close_genus[i] %fin% tree_genus){
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
         !sp_out_tree$family[i] %fin% tree$genus_family_root$family){
        sp_out_tree$status[i] = "No co-family species in the mega-tree"
        next()
      }
    }
    
    node_label_new = NULL
    add_above_node = FALSE
    fraction = 1/2
    
    if(sp_out_tree$genus[i] %fin% tree$genus_family_root$genus |
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
          add_above_node = TRUE
          if(!sp_out_tree$genus[i] %fin% tree$genus_family_root$genus){
            # this is a new genus that has not in the tree
            # cat("here")
            tree$genus_family_root = tibble::add_row(
              tree$genus_family_root,
              family = sp_out_tree$family[i],
              genus = sp_out_tree$genus[i],
              basal_node = node_label_new,
              basal_time = new_ht,
              root_node = tree_df$label[tree_df$node == tree_df$parent[tree_df$label == where_loc_i]],
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
          add_above_node = TRUE
          tree$genus_family_root$only_sp[idx_row] = NA # now will be more than 1 sp in this genus
        }
      } else { # more than 1 species in the genus
        where_loc = root_sub$basal_node # scenarioes 1 and 3, no new node added
        if(scenario == "random_below_basal"){ # randomly select a node in the genus and attach to it, no new node added
          # TO DO: speed up the line below
          tree_df_sub = tidytree::offspring(tree_df, where_loc)
          tree_df_sub= tree_df_sub[tree_df_sub$is_tip == FALSE,]
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
        add_above_node = TRUE
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
        if(scenario == "random_below_basal"){ # randomly select a node in the family, no new node added
          # TO DO: speed up the line below
          tree_df_sub = tidytree::offspring(tree_df, where_loc)
          tree_df_sub= tree_df_sub[tree_df_sub$is_tip == FALSE,]
          if(nrow(tree_df_sub) > 0){
            # only bind to genus/family basal node, not within genus nodes
            potential_locs = intersect(c(where_loc, tree_df_sub$label), all_eligible_nodes)
            locs_bl = tree_df_sub[tree_df_sub$label %fin% potential_locs, ]
            bls = locs_bl$branch.length
            names(bls) = locs_bl$label
            bls = c(root_sub$root_time - root_sub$basal_time, bls)
            names(bls)[1] = root_sub$basal_node
            prob = bls/sum(bls)
            where_loc = sample(potential_locs, 1, prob = prob)
          }
        }
        if(scenario == "at_or_above_basal"){ # insert new node and bind tip above family basal node
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
    if(root_sub$n_spp > 1) use_castor = TRUE else use_castor = FALSE
    # cat(where_loc)
    if(dt){
      tree_df = bind_tip(tree_tbl = tree_df, node_heights = node_hts, where = where_loc, 
                         new_node_above = add_above_node, tip_label = sp_out_tree$species[i], 
                         frac = fraction, return_tree = FALSE, node_label = node_label_new,
                         use_castor = use_castor)
    } else {
      tree_df = bind_tip_df(tree_tbl = tree_df, node_heights = node_hts, where = where_loc, 
                            new_node_above = add_above_node, tip_label = sp_out_tree$species[i], 
                            frac = fraction, return_tree = FALSE, node_label = node_label_new,
                            use_castor = use_castor)
    }
    
    # tree_df$is_tip[tree_df$label == sp_out_tree$species[i]] = TRUE
    # tree_df$is_tip[is.na(tree_df$is_tip)] = FALSE
    # tree_df = dplyr::distinct(tree_df)
    # update n_spp in tree$genus_family_root
    tree$genus_family_root$n_spp[idx_row] = tree$genus_family_root$n_spp[idx_row] + 1
  }
  
  tree_df = dplyr::arrange(tree_df, node)
  
  # if(nrow(sp_out_tree) > 100){
  #   close(progbar)
  # }
  
  if(any(sp_out_tree$status == "*")) {
    message("\n", sum(sp_out_tree$status == "*"), " species added at genus level (*) \n")
  }
  
  if(any(sp_out_tree$status == "**")) {
    message(sum(sp_out_tree$status == "**"), " species added at family level (**) \n")
  }
  
  tree_sub = castor::get_subtree_with_tips(tidytree::as.phylo(tree_df), sp_list$species)$subtree
  
  # add trailing *
  grafted = sp_out_tree[sp_out_tree$status %fin% c("*", "**"), ]
  grafted$sp2 = paste0(grafted$species, grafted$status)
  wid = which(tree_sub$tip.label %fin% grafted$species)
  tree_sub$tip.label[wid] = dplyr::left_join(tibble::tibble(species = tree_sub$tip.label[wid]),
                                             grafted, by = "species")$sp2
  
  graft_status = tibble::tibble(tip_label = tree_sub$tip.label)
  graft_status$species = gsub("\\*", "", graft_status$tip_label)
  graft_status$status = ifelse(grepl("\\*{2}$", graft_status$tip_label), "grafted at family level",
                      ifelse(grepl("[^*]\\*{1}$", graft_status$tip_label), "grafted at genus level",
                             "exisiting species in the megatree"))
  
  if(any(sp_out_tree$status == "No co-family species in the mega-tree")){
    sp_no_family = sp_out_tree$species[sp_out_tree$status == "No co-family species in the mega-tree"]
    message(length(sp_no_family), " species have no co-family species in the mega-tree, skipped\n(if you know their family, prepare and edit species list with `rtrees::sp_list_df()` may help): \n",
            paste(sp_no_family, collapse = ", "))
    graft_status = dplyr::bind_rows(graft_status, 
                                    data.frame(species = sp_no_family, 
                                                  status = rep("skipped as no co-family in the megatree", 
                                                               length(sp_no_family)))
    )
  }
  
  tree_sub$graft_status = graft_status
  
  if(!show_grafted){
    tree_sub = rm_stars(tree_sub)
    graft_status$tip_label = gsub("\\*", "", graft_status$tip_label)
  }
  
  return(ape::ladderize(tree_sub))
}
