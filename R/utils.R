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
cap_first_letter = function (x) {
  sub("^([a-z])", "\\U\\1", x, perl = TRUE)
}

if(getRversion() >= "2.15.1") 
  utils::globalVariables(c(".", "isTip", "is_tip", "node",
                           "tree_fish", "tree_plant_otl", "classifications",
                           "tree_bird_ericson", "tree_mammal", "taxon",
                           "family", "genus", "species", "grp",
                           "root_node", "basal_node", "taxa_supported"))

#' Taxonomic groups supported
#' 
#' @description Supported taxonomic groups with mega-trees provided in the {megatrees} package.
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
#' sp_list_df(sp_list = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii"),
#'            taxon = "fish")
sp_list_df = function(sp_list, taxon){
  if(!is.vector(sp_list, mode = "character") &
     !inherits(sp_list, "data.frame")){
    stop("`sp_list` must either be a string vector or a data frame")
  }
  
  if(is.vector(sp_list, mode = "character")){ # vector
    # phylomatic format
    if(all(grepl(pattern = "[/]", x = sp_list))){ # phylomatic format
      sp_list_sep = strsplit(sp_list, split = "/")
      sp_list = tibble::tibble(species = cap_first_letter(sapply(sp_list_sep, 
                                                                 function(x) gsub(" +", "_", x[3]))),
                               genus = cap_first_letter(sapply(sp_list_sep, function(x) x[2])),
                               family = cap_first_letter(sapply(sp_list_sep, function(x) x[1])))
      return(sp_list)
    } 
    
    sp_list = unique(cap_first_letter(gsub(" +", "_", sp_list)))
    out = tibble::tibble(species = sp_list,
                         genus = gsub("^([-A-Za-z]*)_.*$", "\\1", sp_list))
    if(missing(taxon)) return(out)
  } else { # data frame
    if(!"species" %in% names(sp_list))
      stop("`sp_list` must has at least one column named species.")
    sp_list$species = cap_first_letter(gsub(" +", "_", sp_list$species)) # just in case
    if("genus" %in% names(sp_list)){
      sp_list$genus = cap_first_letter(sp_list$genus)
      if(missing(taxon)) return(sp_list)
    } else{ # no genus column
      sp_list$genus = gsub("^([-A-Za-z]*)_.*$", "\\1", sp_list$species)
      if("family" %in% names(sp_list) | missing(taxon)) # already have family, nothing to do
        return(sp_list)
    }
    out = sp_list
  }

  if(!taxon %fin% taxa_supported) 
    stop("Sorry but only the following taxon groups are supported: ", 
         paste(taxa_supported, collapse = ", "),
         "\n You need to prepare the classification data frame by yourself,", 
         "\n which should have at least three columns: species, genus, family")
  # utils::data("classifications", envir = environment())
  clsf = rtrees::classifications[rtrees::classifications$taxon == taxon, ]
  if(any(!out$genus %fin% clsf$genus)){
    warning("The following genus are not in our classification database: ", 
            paste(setdiff(out$genus, clsf$genus), collapse = ", "),
            call. = FALSE)
    # not necessary to be a problem, if tree has same genus species, 
    # then no family info needed
  }
  if(mean(out$genus %fin% clsf$genus) < 0.8)
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
#' @param process_all_tips Whether to find basal nodes for all tips? Default is `TRUE`.
#' @param genus_list An optinoal subset list of genus to find root information.
#' @param family_list An optinoal subset list of family to find root information. 
#' This should be for species that do not have co-genus in the tree.
#' @param show_warning Whether to print warning information or not.
#' @return A phylogeny with basal nodes information attached.
#' @export
#' 
add_root_info = function(tree, classification, process_all_tips = TRUE,
                         genus_list = NULL, family_list = NULL,
                         show_warning = TRUE){
  if(is.null(tree$node.label))
    tree$node.label = paste0("N", 1:ape::Nnode(tree))
  tree = ape::makeLabel(tree, tips = FALSE, node = TRUE)
  if(any(ww <- grepl("^[0-9]*$", tree$node.label)))
    tree$node.label[ww] = paste0("N", tree$node.label[ww])
  tips = tibble::tibble(species = tree$tip.label, 
                        genus = gsub("^([-A-Za-z]*)_.*$", "\\1", tree$tip.label))
  
  if(process_all_tips){
    if(!is.null(genus_list))
      stop("When `process_all_tips = TRUE`, `genus_list` must be NULL.")
    if(any(!tips$genus %fin% classification$genus) & show_warning)
      warning("Some genus are not in the classification.")
    # add family information
    tips = dplyr::left_join(tips, classification, by = "genus")
  } else { # only need for a subset of genus/family
    if(!is.null(family_list)) { # both genus and family
      # add family information
      family_list = family_list[!is.na(family_list)]
      # classification will be filtered to the taxon in `get_one_tree()`
      # need family infor for genus in the phylogeny !!
      tips = dplyr::left_join(tips, classification, by = "genus") 
      if(all(is.na(tips$family))) 
        stop("No tips in the user provided tree can get family information from classification,
             please use `add_root_info()` to prepare the tree first.")
      if(any(!family_list %fin% tips$family) & show_warning)
        warning("Some family_list are not in the tree; these species will be ignored.")
      tips_family = tips[tips$family %fin% family_list, ]
      # if(any(!genus_list %fin% tips$genus) & show_warning)
      #   warning("Some genus_list are not in the tree.") 
      ## these genus' family will be in the family_list
      tips_genus = tips[tips$genus %fin% genus_list, ]
      tips = unique(dplyr::bind_rows(tips_genus, tips_family))
    } else { # only genus
      if(is.null(genus_list))
        stop("When `process_all_tips = FALSE`, `genus_list` must be specified.")
      if(any(!genus_list %fin% tips$genus) & show_warning)
        warning("Some genus_list are not in the tree.")
      tips = tips[tips$genus %fin% genus_list, ] # no family column
    }
  }
  
  if("family" %fin% names(tips)){
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
  } else {
    gf_summ = dplyr::summarise(dplyr::group_by(tips, genus), 
                     n_genus = dplyr::n_distinct(genus), 
                     n_spp = dplyr::n_distinct(species))
    gf_summ = dplyr::mutate(gf_summ, family = NA)
  }
  
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
    tree_df_subset = tree_df[tree_df$label %fin% sp_names, ]
    basal_node = tidytree::MRCA(tree_df, min(tree_df_subset$node), max(tree_df_subset$node))
    if(basal_node$parent == basal_node$node){
      # root
      root_node = basal_node
    } else {
      if(length(sp_names) == 1){ # only 1 sp
        root_node = basal_node
      } else {
        root_node = tidytree::parent(tree_df, basal_node$node)
      }
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

#' Remove trailing *
#' 
#' @param tree A phylogeny generated by `get_tree()` with trailing stars in tip labels.
#' @return A phylogeny after removing trailing stars.
#' @export
#' 
rm_stars = function(tree){
  if(!inherits(tree, "phylo"))
    stop("tree must have class of 'phylo'.")
  tree$tip.label = gsub("[*]*$", "", tree$tip.label)
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
    step = function()  NULL,
    term = function()  NULL
  )
}

create_progress_bar <- function(name = "text", ...) {
  if (!is.character(name)) return(name)
  name <- paste("progress", name, sep="_")
  
  if (!exists(name, mode = "function")) {
    warning("Cannot find progress bar ", name, call. = FALSE)
    progress_none()
  } else {
    match.fun(name)(...)
  }
}


## original contributed by Bradley Jones and modified by Guangchuang Yu
## copied from tidytree, modified to make it slightly faster
as_tree <- function(x) {
  edge <- x[, c("parent", "node")]
  i <- which(edge[,1] != 0 & edge[,1] != edge[,2])
  edge <- edge[i, ]
  if (is.null(x[["branch.length"]])) {
    edge.length <- NULL
  } else {
    edge.length <- x$branch.length[i]
  }
  tip.label <- as.character(x$label[x$isTip])
  
  phylo <- list(edge = as.matrix(edge),
                edge.length = edge.length,
                tip.label = tip.label)
  
  node.label <- as.character(x$label[!x$isTip])

  if (!all(is.na(node.label))) {
    phylo$node.label <- node.label
  }
  phylo$Nnode <- sum(!x[, "isTip"])
  class(phylo) <- "phylo"
  return(phylo)
}



