#' Derive a phylogeny from a mega-tree
#' 
#' For a list of species, generate a phylogeny from a provided mega-tree. If a species is
#' not in the mega-tree, it will be grafted to the mega-tree with three scenarioes.
#' 
#' @inheritParams get_tree
#' @return A phylogeny for the species required, with class `phylo`.
#' @importFrom stats setNames
#' @export
#' 
get_one_tree = function(sp_list, tree, taxon, 
                        scenario = c("at_basal_node", "random_below_basal"), 
                        show_grafted = FALSE,
                        tree_by_user = FALSE,
                        .progress = "text", dt = TRUE) {
  if(tree_by_user & all(!grepl("_", tree$tip.label)))
    stop("Please change the tree's tip labels to be the format of genus_sp.")
  if(tree_by_user) tree = rm_stars(tree)
  tree_genus = unique(gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label))
  
  sp_list = sp_list_df(unique(sp_list)) # remove duplication and prep genus, family
  
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
          if((!taxon %fin% rtrees::taxa_supported) & !all_genus_in_tree){
            unique(classifications[classifications$taxon == taxon, ])
          } else {
            unique(rtrees::classifications[rtrees::classifications$taxon == taxon, ])
          }
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

  # Use plain integer/numeric vectors instead of a tibble throughout the loop.
  # phylo_to_vecs() is O(Ntip) and avoids all tidytree/[[.tbl_df overhead.
  vecs <- phylo_to_vecs(tree)
  # O(1) label → row-index lookup updated incrementally after each graft.
  label_env <- list2env(
    setNames(as.list(seq_along(vecs$label)), vecs$label),
    hash = TRUE, parent = emptyenv())
  # O(1) node-height (branching time) lookup; new nodes added via [[ <- ]] .
  node_hts_env <- list2env(as.list(ape::branching.times(tree)), hash = TRUE, parent = emptyenv())
  n_internal_initial <- ape::Nnode(tree)
  n_nodes_added <- 0L

  all_eligible_nodes = unique(c(tree$genus_family_root$basal_node,
                                tree$genus_family_root$root_node))

  n_spp_to_show_progress = 200
  if(nrow(sp_out_tree) > n_spp_to_show_progress){
    progress <- create_progress_bar(.progress)
    progress$init(nrow(sp_out_tree))
    on.exit(progress$term())
  }

  # For at_basal_node: pre-allocate graft-parameter arrays; call graft_all_cpp()
  # once after the loop (one C++ allocation instead of N tibble round-trips).
  n_sp <- nrow(sp_out_tree)
  if(scenario == "at_basal_node") {
    gb_where <- character(n_sp)
    gb_nlbl  <- character(n_sp)   # "" = no new internal node needed
    gb_frac  <- rep(0.5, n_sp)
    gb_above <- logical(n_sp)
    gb_ht    <- numeric(n_sp)
    gb_valid <- logical(n_sp)
  }

  for(i in 1:n_sp){
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
    new_genus_entry_added = FALSE  # set TRUE when we register a new genus in family fallback

    if(sp_out_tree$genus[i] %fin% tree$genus_family_root$genus |
       !is.na(where_loc_i2) | !is.na(where_loc_i)){
      sp_out_tree$status[i] = "*"
      # tree has species in the same genus
      idx_row = which(tree$genus_family_root$genus == sp_out_tree$genus[i])
      root_sub = tree$genus_family_root[idx_row, ]
      if(isTRUE(root_sub$n_spp == 1) | !is.na(where_loc_i)) { # but only 1 species in this genus
        if(!is.na(where_loc_i)){# a close sp specified
          where_loc = where_loc_i
          wi <- label_env[[where_loc_i]]
          new_ht = vecs$branch.length[wi] * (1 - fraction)
          n_nodes_added <- n_nodes_added + 1L
          node_label_new = paste0("N", n_internal_initial + n_nodes_added)
          node_hts_env[[node_label_new]] <- new_ht
          all_eligible_nodes = c(all_eligible_nodes, node_label_new)
          add_above_node = TRUE
          if(!sp_out_tree$genus[i] %fin% tree$genus_family_root$genus){
            # this is a new genus that has not in the tree
            par_num <- vecs$parent[wi]
            # For the initial tree vecs$node[j]==j, so par_num IS the row index.
            # Use match() to be safe across random_below_basal grafts.
            par_row <- match(par_num, vecs$node)
            par_label <- vecs$label[par_row]
            nh_par <- node_hts_env[[par_label]]
            if(is.null(nh_par)) nh_par <- 0.0
            tree$genus_family_root = tibble::add_row(
              tree$genus_family_root,
              family = sp_out_tree$family[i],
              genus = sp_out_tree$genus[i],
              basal_node = node_label_new,
              basal_time = new_ht,
              root_node = par_label,
              root_time = nh_par,
              n_genus = 1, n_spp = 1, only_sp = sp_out_tree$species[i])
            idx_row = nrow(tree$genus_family_root)
          }
        } else {
          where_loc = root_sub$only_sp
          # the new tip will be bind to this species, in the half of its branch length (default frac)
          new_ht = root_sub$basal_time * (1 - fraction)
          n_nodes_added <- n_nodes_added + 1L
          node_label_new = paste0("N", n_internal_initial + n_nodes_added)
          node_hts_env[[node_label_new]] <- new_ht
          all_eligible_nodes = c(all_eligible_nodes, node_label_new)
          add_above_node = TRUE
          tree$genus_family_root$only_sp[idx_row] = NA # now will be more than 1 sp in this genus
          tree$genus_family_root$basal_node[idx_row] = node_label_new
          tree$genus_family_root$basal_time[idx_row] = unname(new_ht)
        }
      } else { # more than 1 species in the genus
        where_loc = root_sub$basal_node # scenarioes 1 and 3, no new node added
        if(scenario == "random_below_basal"){ # randomly select a node in the genus and attach to it, no new node added
          wi <- label_env[[where_loc]]
          where_loc_node = vecs$node[wi]
          sub_idx = fast_internal_offspring_cpp(vecs$parent, vecs$node, vecs$is_tip, where_loc_node)
          if(length(sub_idx) > 0){
            sub_labels <- vecs$label[sub_idx]
            sub_bls    <- vecs$branch.length[sub_idx]
            potential_locs = c(where_loc, sub_labels)
            bls = sub_bls
            names(bls) = sub_labels
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
        n_nodes_added <- n_nodes_added + 1L
        node_label_new = paste0("N", n_internal_initial + n_nodes_added)
        node_hts_env[[node_label_new]] <- new_ht
        all_eligible_nodes = c(all_eligible_nodes, node_label_new)
        add_above_node = TRUE
        tree$genus_family_root = tibble::add_row(tree$genus_family_root,
                                                 family = sp_out_tree$family[i],
                                                 genus = sp_out_tree$genus[i],
                                                 basal_node = node_label_new,
                                                 basal_time = unname(new_ht),
                                                 root_node = root_sub$basal_node,
                                                 root_time = root_sub$basal_time,
                                                 n_genus = 1,
                                                 n_spp = 1,
                                                 only_sp = sp_out_tree$species[i]
        )
      } else { # more than 1 species; can be the same genus or different genus
        where_loc = root_sub$basal_node # for scenario 1, no new node added
        if(scenario == "random_below_basal"){ # randomly select a node in the family, no new node added
          wi <- label_env[[where_loc]]
          where_loc_node = vecs$node[wi]
          sub_idx = fast_internal_offspring_cpp(vecs$parent, vecs$node, vecs$is_tip, where_loc_node)
          if(length(sub_idx) > 0){
            sub_labels <- vecs$label[sub_idx]
            sub_bls    <- vecs$branch.length[sub_idx]
            # only bind to genus/family basal node, not within genus nodes
            potential_locs = intersect(c(where_loc, sub_labels), all_eligible_nodes)
            pl_in_sub <- potential_locs[potential_locs %fin% sub_labels]
            bls = sub_bls[match(pl_in_sub, sub_labels)]
            names(bls) = pl_in_sub
            bls = c(root_sub$root_time - root_sub$basal_time, bls)
            names(bls)[1] = root_sub$basal_node
            prob = bls/sum(bls)
            where_loc = sample(potential_locs, 1, prob = prob)
          }
        }
        # if(scenario == "at_or_above_basal"){ # insert new node and bind tip above family basal node
        #   add_above_node = TRUE
        #   if(2 * root_sub$root_time / 3 > root_sub$basal_time){
        #     fraction = (2 * root_sub$root_time / 3 - root_sub$basal_time) /
        #       (root_sub$root_time - root_sub$basal_time)
        #   }
        #   new_ht = unname(root_sub$basal_time + (root_sub$root_time - root_sub$basal_time) * (1 - fraction))
        #   # here is the node height, but in bind_tip, it is the length between a parent and a node,
        #   # thus 1 - fraction
        #   n_nodes_added <- n_nodes_added + 1L
        #   node_label_new = paste0("N", n_internal_initial + n_nodes_added)
        #   node_hts_env[[node_label_new]] <- new_ht
        #   all_eligible_nodes = c(all_eligible_nodes, node_label_new)
        #   tree$genus_family_root$basal_node[idx_row] = node_label_new # new basal node
        #   tree$genus_family_root$basal_time[idx_row] = new_ht
        #   tree$genus_family_root = tibble::add_row(tree$genus_family_root,
        #                                            family = sp_out_tree$family[i],
        #                                            genus = sp_out_tree$genus[i],
        #                                            basal_node = node_label_new,
        #                                            basal_time = new_ht,
        #                                            root_node = root_sub$root_node,
        #                                            root_time = root_sub$root_time,
        #                                            n_genus = 1,
        #                                            n_spp = 1,
        #                                            only_sp = sp_out_tree$species[i])
        # }
        # Register a new genus entry only when there are additional missing
        # species from the same new genus, so they form a polytomy clade above
        # the first attached species rather than scattering at the family basal node.
        if (i < n_sp) {
          genus_i <- sp_out_tree$genus[i]
          remaining_genera <- sp_out_tree$genus[(i + 1L):n_sp]
          n_same_genus_remaining <- sum(
            remaining_genera == genus_i &
            !remaining_genera %fin% tree$genus_family_root$genus
          )
          if (n_same_genus_remaining > 0L) {
            tree$genus_family_root = tibble::add_row(
              tree$genus_family_root,
              family     = sp_out_tree$family[i],
              genus      = genus_i,
              basal_node = root_sub$basal_node,
              basal_time = root_sub$basal_time,
              root_node  = root_sub$root_node,
              root_time  = root_sub$root_time,
              n_genus    = 1L,
              n_spp      = 1L,
              only_sp    = sp_out_tree$species[i]
            )
            new_genus_entry_added = TRUE
          }
        }
      }
      # update genus number
      tree$genus_family_root$n_genus[idx_row] = tree$genus_family_root$n_genus[idx_row] + 1
    }

    # ---- Graft ----
    if(scenario == "at_basal_node") {
      # Collect parameters; the single graft_all_cpp() call happens after the loop.
      nh_val <- node_hts_env[[where_loc]]
      gb_where[i] <- where_loc
      gb_nlbl[i]  <- if (!is.null(node_label_new)) node_label_new else ""
      gb_frac[i]  <- fraction
      gb_above[i] <- add_above_node
      gb_ht[i]    <- if (!is.null(nh_val)) nh_val else 0.0
      gb_valid[i] <- TRUE
    } else {
      # random_below_basal: update vecs in place after each graft.
      # bind_tip_core_cpp() operates on plain vectors — no tibble allocation per step.
      nh_val <- node_hts_env[[where_loc]]
      if(is.null(nh_val)) nh_val <- 0.0
      res <- bind_tip_core_cpp(
        parent        = vecs$parent,
        node          = vecs$node,
        branch_length = vecs$branch.length,
        label         = vecs$label,
        is_tip        = vecs$is_tip,
        where         = where_loc,
        tip_label     = sp_out_tree$species[i],
        node_label_str = if (!is.null(node_label_new)) node_label_new else "",
        frac          = fraction,
        new_node_above = add_above_node,
        node_height   = nh_val
      )
      n_old <- length(vecs$label)
      vecs$parent        <- res$parent
      vecs$node          <- res$node
      vecs$branch.length <- res$`branch.length`
      vecs$label         <- res$label
      vecs$is_tip        <- res$is_tip
      for (j in (n_old + 1L):length(vecs$label)) label_env[[vecs$label[j]]] <- j
      if (new_genus_entry_added) {
        # Correct basal_time to the actual branch length of the first species after
        # random placement (may differ from the family basal time used as initial estimate).
        tree$genus_family_root$basal_time[nrow(tree$genus_family_root)] <-
          vecs$branch.length[label_env[[sp_out_tree$species[i]]]]
      }
    }

    # update n_spp in tree$genus_family_root
    tree$genus_family_root$n_spp[idx_row] = tree$genus_family_root$n_spp[idx_row] + 1
  }

  # ---- Post-loop: materialise the final tree_df ----
  if(scenario == "at_basal_node") {
    valid_i <- which(gb_valid)
    if(length(valid_i) > 0) {
      result <- graft_all_cpp(
        parent0        = vecs$parent,
        node0          = vecs$node,
        bl0            = vecs$branch.length,
        label0         = vecs$label,
        is_tip0        = vecs$is_tip,
        where_labels   = gb_where[valid_i],
        new_tip_labels = sp_out_tree$species[valid_i],
        new_node_labels = gb_nlbl[valid_i],
        fracs          = gb_frac[valid_i],
        above_flags    = gb_above[valid_i],
        node_heights_vec = gb_ht[valid_i]
      )
      tree_df <- tibble::as_tibble(result)
    } else {
      tree_df <- tibble::as_tibble(vecs)
    }
  } else {
    tree_df <- tibble::as_tibble(vecs)
  }
  if(!inherits(tree_df, "tbl_tree")) class(tree_df) <- c("tbl_tree", class(tree_df))

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
  
  # in case of non ultrametric
  if(ape::is.ultrametric(tree) & !ape::is.ultrametric(tree_sub)){
    ntips = ape::Ntip(tree_sub)
    ages_tips = castor::get_all_distances_to_root(tree_sub)[1 : ntips]
    ages_diff = round(ages_tips - stats::median(ages_tips), 3) # the most common age should be the median
    tree_sub_df = tidytree::as_tibble(tree_sub)
    stopifnot(all(!tree_sub_df$node[1:ntips] %in% tree_sub_df$parent))
    tree_sub_df$branch.length[1:ntips] = pmax(0, tree_sub_df$branch.length[1:ntips] - ages_diff)
    tree_sub = tidytree::as.phylo(tree_sub_df)
    # ultrametric
    tree_sub = castor::extend_tree_to_height(tree_sub)$tree
  }
  
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
  
  if(!show_grafted){
    tree_sub = rm_stars(tree_sub)
    graft_status$tip_label = gsub("\\*", "", graft_status$tip_label)
  }
  
  tree_sub$graft_status = graft_status
  
  tree_sub = ape::ladderize(tree_sub)
  
  return(tree_sub)
}
