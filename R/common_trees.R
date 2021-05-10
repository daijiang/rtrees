# functions to download common but large phylogenies.

# fish ----
#' Get full fish phylogeny provided by the [Fish Tree of Life](https://fishtreeoflife.org/about/)
#' 
#' Get the 100 phylogenies, each with 31516 tips. 
#' The `Complete “all-taxon assembled” chronograms` option from 
#' its [downloads page](https://fishtreeoflife.org/downloads/).
#' 
#' @param path_save_to Where do you want to save the file? The default is `NULL`,
#'   which means that not to save it locally. When specify, specify to the folder/directory.
#' @param read_into_R Do you want to read the loaded phylogenies into R? Default is `TRUE`.
#' 
download_fish_full_tree = function(path_save_to = NULL, read_into_R = TRUE){
  if(is.null(path_save_to) & isFALSE(read_into_R)) {
    warning("Nothing to do here... check your arguments")
    return()
  }
    
  fishurl2 = "https://fishtreeoflife.org/downloads/actinopt_full.trees.xz"
  
  if(is.null(path_save_to)){
    tempf = tempfile()
    download.file(fishurl2, tempf)
    if(read_into_R) fish_tree_full = ape::read.tree(tempf) # 100 phylogenies
    unlink(tempf)
  } else {
    local_file = paste0(path_save_to, "/actinopt_full.trees.xz")
    download.file(fishurl2, local_file)
    if(read_into_R) fish_tree_full = ape::read.tree(local_file) 
  }
  if(read_into_R) return(fish_tree_full)
}


# bird ----




# mammal ---