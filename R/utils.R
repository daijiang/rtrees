#' Faster match of vectors
#'
#' See \code{fastmatch::\link[fastmatch:fmatch]{\%fin\%}} for details.
#'
#' @name %fin%
#' @rdname fmatch
#' @keywords internal
#' @importFrom fastmatch %fin%
#' @import data.table
NULL

#' #' Faster match of character vectors
#' #'
#' #' See \code{data.table::\link[data.table:chmatch]{\%chin\%}} for details.
#' #'
#' #' @name %chin%
#' #' @rdname chmatch
#' #' @keywords internal
#' #' @export
#' #' @importFrom data.table %chin%
#' NULL


#' Captize the first letter of a word.
#' @param x A word.
#' @return The same word with the first letter captized.
#' @noRd
cap_first_letter <- function(x) {
  sub("^([a-z])", "\\U\\1", x, perl = TRUE)
}

# BFS to find the max tip node-number among all descendants of root_node.
# Avoids converting the whole table to a phylo object on every bind_tip call.
fast_max_tip <- function(parent_vec, node_vec, is_tip_vec, root_node) {
  non_root <- which(parent_vec != node_vec)
  children <- split(non_root, parent_vec[non_root])
  frontier <- as.character(root_node)
  max_tip  <- 0L
  while (length(frontier) > 0L) {
    ch_idx <- unlist(children[frontier], use.names = FALSE)
    if (!length(ch_idx)) break
    is_tip    <- is_tip_vec[ch_idx]
    tip_nodes <- node_vec[ch_idx][is_tip]
    if (length(tip_nodes)) max_tip <- max(max_tip, max(tip_nodes))
    frontier <- as.character(node_vec[ch_idx][!is_tip])
  }
  max_tip
}

# BFS returning row indices of all internal-node descendants of root_node.
# Replaces tidytree::offspring() in random_below_basal sampling.
fast_internal_offspring <- function(parent_vec, node_vec, is_tip_vec, root_node) {
  non_root <- which(parent_vec != node_vec)
  children <- split(non_root, parent_vec[non_root])
  frontier   <- as.character(root_node)
  result_idx <- integer(0)
  while (length(frontier) > 0L) {
    ch_idx <- unlist(children[frontier], use.names = FALSE)
    if (!length(ch_idx)) break
    is_tip     <- is_tip_vec[ch_idx]
    result_idx <- c(result_idx, ch_idx[!is_tip])
    frontier   <- as.character(node_vec[ch_idx][!is_tip])
  }
  result_idx
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "isTip", "is_tip", "node", "tree_bee", "tree_butterfly",
    "tree_fish", "tree_plant_otl", "classifications",
    "tree_bird_ericson", "tree_mammal", "taxon",
    "family", "genus", "species", "grp", "parent",
    "root_node", "basal_node", "taxa_supported"
  ))
}

#' Taxonomic groups supported
#'
#' @description Supported taxonomic groups with mega-trees provided in the \pkg{megatrees} package.
#'
"taxa_supported"

#' Convert a vector of species names to a data frame
#'
#' @param sp_list A string vector or a data frame with at least one column named "species".
#' @param taxon The taxon group of this species list. If not specified, only species and
#' genus will be returned.
#' @return A data frame with columns: species, genus, and family (if `taxon` is specified).
#' @export
#' @examples
#' sp_list_df(
#'   sp_list = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii"),
#'   taxon = "fish"
#' )
sp_list_df <- function(sp_list, taxon) {
  if (!is.vector(sp_list, mode = "character") &
    !inherits(sp_list, "data.frame")) {
    stop("`sp_list` must either be a string vector or a data frame")
  }

  if (is.vector(sp_list, mode = "character")) { # vector
    # phylomatic format
    if (all(grepl(pattern = "[/]", x = sp_list))) { # phylomatic format
      sp_list_sep <- strsplit(sp_list, split = "/")
      sp_list <- tibble::tibble(
        species = cap_first_letter(sapply(
          sp_list_sep,
          function(x) gsub(" +", "_", x[3])
        )),
        genus = cap_first_letter(sapply(sp_list_sep, function(x) x[2])),
        family = cap_first_letter(sapply(sp_list_sep, function(x) x[1]))
      )
      return(sp_list)
    }

    sp_list <- unique(cap_first_letter(gsub(" +", "_", sp_list)))
    out <- tibble::tibble(
      species = sp_list,
      genus = gsub("^([-A-Za-z]*)_.*$", "\\1", sp_list)
    )
    if (missing(taxon)) {
      return(out)
    }
  } else { # data frame
    if (!"species" %in% names(sp_list)) {
      stop("`sp_list` must has at least one column named species.")
    }
    sp_list$species <- cap_first_letter(gsub(" +", "_", sp_list$species)) # just in case
    if ("genus" %in% names(sp_list)) {
      sp_list$genus <- cap_first_letter(sp_list$genus)
      if (missing(taxon)) {
        return(sp_list)
      }
    } else { # no genus column
      sp_list$genus <- gsub("^([-A-Za-z]*)_.*$", "\\1", sp_list$species)
      if ("family" %in% names(sp_list) | missing(taxon)) { # already have family, nothing to do
        return(sp_list)
      }
    }
    out <- sp_list
  }

  if (!taxon %fin% rtrees::taxa_supported) {
    stop(
      "Sorry but only the following taxon groups are supported: ",
      paste(rtrees::taxa_supported, collapse = ", "),
      "\n You need to prepare the species list data frame by yourself,",
      "\n which should have at least three columns: species, genus, family"
    )
  }
  # utils::data("classifications", envir = environment())
  clsf <- rtrees::classifications[rtrees::classifications$taxon == taxon, ]
  if (any(!out$genus %fin% clsf$genus)) {
    warning("The following genus are not in our classification database: ",
      paste(setdiff(out$genus, clsf$genus), collapse = ", "),
      call. = FALSE
    )
    # not necessary to be a problem, if tree has same genus species,
    # then no family info needed
  }
  if (mean(out$genus %fin% clsf$genus) < 0.8) {
    warning("Are you sure that you specified the right taxon group?", call. = FALSE)
  }
  out <- dplyr::left_join(out, clsf, by = "genus")
  out$taxon <- NULL
  unique(out)
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
#' @param process_all_tips Whether to find basal nodes for all tips? Default is `TRUE`.
#' @param genus_list An optinoal subset list of genus to find root information.
#' @param family_list An optinoal subset list of family to find root information.
#' This should be for species that do not have co-genus in the tree.
#' @param show_warning Whether to print warning information about non-monophyletic clades or not.
#' @return A phylogeny with basal nodes information attached.
#' @export
#'
add_root_info <- function(tree, classification, process_all_tips = TRUE,
                          genus_list = NULL, family_list = NULL,
                          show_warning = FALSE) {
  if (is.null(tree$node.label)) {
    tree$node.label <- paste0("N", 1:ape::Nnode(tree))
  }
  tree <- ape::makeLabel(tree, tips = FALSE, node = TRUE)
  if (any(ww <- grepl("^[0-9]*$", tree$node.label))) {
    tree$node.label[ww] <- paste0("N", tree$node.label[ww])
  }
  if (any(duplicated(tree$node.label))) {
    tree$node.label <- make.unique(tree$node.label, sep = "_")
  }
  tips <- tibble::tibble(
    species = tree$tip.label,
    genus = gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label)
  )

  if (process_all_tips) {
    if (!is.null(genus_list)) {
      stop("When `process_all_tips = TRUE`, `genus_list` must be NULL.")
    }
    if (any(!tips$genus %fin% classification$genus) & show_warning) {
      warning("Some genus are not in the classification.")
    }
    # add family information
    tips <- dplyr::left_join(tips, classification, by = "genus")
  } else { # only need for a subset of genus/family
    if (!is.null(family_list)) { # both genus and family
      # add family information
      family_list <- family_list[!is.na(family_list)]
      # classification will be filtered to the taxon in `get_one_tree()`
      # need family infor for genus in the phylogeny !!
      tips <- dplyr::left_join(tips, classification, by = "genus")
      if (all(is.na(tips$family))) {
        stop("No tips in the user provided tree can get family information from classification,
             please use `add_root_info()` to prepare the tree first.")
      }
      if (any(!family_list %fin% tips$family) & show_warning) {
        warning("Some family_list are not in the tree; these species will be ignored.")
      }
      tips_family <- tips[tips$family %fin% family_list, ]
      # if(any(!genus_list %fin% tips$genus) & show_warning)
      #   warning("Some genus_list are not in the tree.")
      ## these genus' family will be in the family_list
      tips_genus <- tips[tips$genus %fin% genus_list, ]
      tips <- unique(dplyr::bind_rows(tips_genus, tips_family))
    } else { # only genus
      if (is.null(genus_list)) {
        stop("When `process_all_tips = FALSE`, `genus_list` must be specified.")
      }
      if (any(!genus_list %fin% tips$genus) & show_warning) {
        warning("Some genus_list are not in the tree.")
      }
      tips <- tips[tips$genus %fin% genus_list, ] # no family column
    }
  }

  if ("family" %fin% names(tips)) {
    family_summ <- dplyr::mutate(
      dplyr::summarise(dplyr::group_by(tips, family),
        n_genus = dplyr::n_distinct(genus),
        n_spp = dplyr::n_distinct(species)
      ),
      genus = NA_character_
    )
    family_summ <- family_summ[!is.na(family_summ$family), ]
    genus_summ <- dplyr::left_join(
      dplyr::summarise(dplyr::group_by(tips, genus),
        n_genus = dplyr::n_distinct(genus),
        n_spp = dplyr::n_distinct(species)
      ),
      classification,
      by = "genus"
    )
    gf_summ <- dplyr::bind_rows(family_summ, genus_summ)
  } else {
    gf_summ <- dplyr::summarise(dplyr::group_by(tips, genus),
      n_genus = dplyr::n_distinct(genus),
      n_spp = dplyr::n_distinct(species)
    )
    gf_summ <- dplyr::mutate(gf_summ, family = NA)
  }

  gf_summ$grp <- 1:nrow(gf_summ)

  find_root <- function(xdf, tips, tree_df, show_warning) {
    target <- xdf$genus
    if (fam <- is.na(target)) target <- xdf$family
    sp_names <- if (fam) tips$species[tips$family == target] else tips$species[tips$genus == target]
    sp_names <- sp_names[!is.na(sp_names)]

    if (length(sp_names) == 1) {
      # Single species: its immediate parent is the basal (and root) node for the group.
      tip_row  <- tree_df[tree_df$label %fin% sp_names, ]
      root_row <- tree_df[tree_df$node == tip_row$parent, ]
      return(tibble::tibble(
        basal_node = root_row$label,
        root_node  = root_row$label,
        only_sp    = sp_names
      ))
    }

    # Multiple species: castor's C++ MRCA is ~10x faster than tidytree::MRCA on tbl_tree.
    # `tree` is the phylo object from the enclosing add_root_info() scope.
    mrca_num <- castor::get_mrca_of_set(tree, sp_names)
    mrca_row <- tree_df[tree_df$node == mrca_num, ]

    if (show_warning) {
      descts <- tidytree::offspring(tree_df, mrca_row$node, tiponly = TRUE)$label
      if (!setequal(sp_names, descts)) {
        cat("Caution: Species in", if (fam) "family" else "genus", target,
            "do not form a monophyletic clade.\n")
      }
    }

    if (mrca_row$parent == mrca_row$node) {
      root_row <- mrca_row
    } else {
      root_row <- tree_df[tree_df$node == mrca_row$parent, ]
    }

    tibble::tibble(
      basal_node = mrca_row$label,
      root_node  = root_row$label,
      only_sp    = NA_character_
    )
  }

  # this takes time
  tree_df <- tidytree::as_tibble(tree)
  gf_summ2 <- dplyr::bind_rows(
    lapply(seq_len(nrow(gf_summ)), function(i)
      find_root(gf_summ[i, ], tips, tree_df, show_warning))
  )
  gf_summ2$grp <- seq_len(nrow(gf_summ))

  node_heights <- ape::branching.times(tree)

  gf_summ3 <- dplyr::left_join(gf_summ, dplyr::ungroup(gf_summ2), by = "grp")
  gf_summ3 <- dplyr::mutate(gf_summ3,
    root_time = node_heights[root_node],
    basal_time = node_heights[basal_node]
  )
  gf_summ3 <- gf_summ3[, c(
    "family", "genus", "basal_node", "basal_time",
    "root_node", "root_time", "n_genus", "n_spp", "only_sp"
  )]

  tree$genus_family_root <- gf_summ3

  tree
}

#' Remove trailing *
#'
#' @param tree A phylogeny generated by `get_tree(..., show_grafted = TRUE)` with trailing stars in tip labels.
#' @return A phylogeny after removing trailing stars.
#' @export
#'
rm_stars <- function(tree) {
  if (!inherits(tree, "phylo")) {
    stop("tree must have class of 'phylo'.")
  }
  tree$tip.label <- gsub("[*]*$", "", tree$tip.label)
  tree
}

# copied from plyr
progress_text <- function(style = 3, ...) {
  n <- 0
  txt <- NULL

  list(
    init = function(x) {
      txt <<- utils::txtProgressBar(max = x, style = style, ...)
      utils::setTxtProgressBar(txt, 0)
    },
    step = function() {
      n <<- n + 1
      utils::setTxtProgressBar(txt, n)
    },
    term = function() close(txt)
  )
}

progress_none <- function() {
  list(
    init = function(x) NULL,
    step = function() NULL,
    term = function() NULL
  )
}

create_progress_bar <- function(name = "text", ...) {
  if (!is.character(name)) {
    return(name)
  }
  name <- paste("progress", name, sep = "_")

  if (!exists(name, mode = "function")) {
    warning("Cannot find progress bar ", name, call. = FALSE)
    progress_none()
  } else {
    match.fun(name)(...)
  }
}


# Fast conversion from phylo to the 5-column list used by graft_all_cpp and
# bind_tip_core_cpp.  Avoids all tidytree/dplyr overhead of as_tibble.
phylo_to_vecs <- function(tree) {
  n_tips  <- ape::Ntip(tree)
  n_nodes <- ape::Nnode(tree)
  n_total <- n_tips + n_nodes

  # parent[i] = parent node number of node i (root is self-referential)
  par_vec <- integer(n_total)
  par_vec[tree$edge[, 2L]] <- tree$edge[, 1L]
  par_vec[n_tips + 1L] <- n_tips + 1L  # root (ape convention: always Ntip+1)

  bl_vec <- numeric(n_total)
  bl_vec[tree$edge[, 2L]] <- tree$edge.length

  node_labels <- if (!is.null(tree$node.label)) {
    tree$node.label
  } else {
    paste0("N", seq_len(n_nodes))
  }
  label_vec  <- c(tree$tip.label, node_labels)
  is_tip_vec <- c(rep(TRUE, n_tips), rep(FALSE, n_nodes))

  list(parent       = par_vec,
       node         = seq_len(n_total),
       branch.length = bl_vec,
       label        = label_vec,
       is_tip       = is_tip_vec)
}

## original contributed by Bradley Jones and modified by Guangchuang Yu
## copied from tidytree, modified to make it slightly faster
as_tree <- function(x) {
  x <- x[order(x$node), ] # tip.label must be in node-number order (ape convention)
  edge <- x[, c("parent", "node")]
  i <- which(edge[, 1] != 0 & edge[, 1] != edge[, 2])
  edge <- edge[i, ]
  if (is.null(x[["branch.length"]])) {
    edge.length <- NULL
  } else {
    edge.length <- x$branch.length[i]
  }
  tip.label <- as.character(x$label[x$is_tip])

  phylo <- list(
    edge = as.matrix(edge),
    edge.length = edge.length,
    tip.label = tip.label
  )

  node.label <- as.character(x$label[!x$is_tip])

  if (!all(is.na(node.label))) {
    phylo$node.label <- node.label
  }
  phylo$Nnode <- sum(!x[, "is_tip"])
  class(phylo) <- "phylo"
  return(phylo)
}

as_tree_isTip <- function(x) {
  # x = as.data.frame(x)
  i <- which(x$parent != 0 & x$parent != x$node)
  edge <- x[i, c("parent", "node")]
  if (is.null(x[["branch.length"]])) {
    edge.length <- NULL
  } else {
    edge.length <- x$branch.length[i]
  }
  tip.label <- x$label[x$isTip]

  phylo <- list(
    edge = edge,
    edge.length = edge.length,
    tip.label = tip.label
  )

  node.label <- x$label[!x$isTip]

  if (!all(is.na(node.label))) {
    phylo$node.label <- node.label
  }
  phylo$Nnode <- sum(!x[, "isTip"])
  class(phylo) <- "phylo"
  return(phylo)
}

#' Extract grafting status information as a data frame
#'
#' @param tree A phylogeny generated by `get_tree(...)` with trailing stars in tip labels.
#' @return A tibble with three columns: tip_label, species, and status.
#' @export
#'
get_graft_status <- function(tree) {
  tree$graft_status
}
