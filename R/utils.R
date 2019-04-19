#' Captize the first letter of a word.
#' @param x A word.
#' @return The same word with the first letter captized.
#' @noRd
cap_first_letter = function (x) {
  sub("^([a-z])", "\\U\\1", x, perl = TRUE)
}

if(getRversion() >= "2.15.1") 
  utils::globalVariables(c(".", "isTip", "is_tip", "node",
                           "tree_fish", "tree_plant_GBOTB", "classifications",
                           "tree_bird_ericson"))

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
  if(!taxon %in% c("plant", "fish", "bird")) 
    stop("Sorry but only the following taxon groups are supported: plant, fish.")
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
