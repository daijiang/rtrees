# Provenance of data-raw/carruthers_taxonomy.rds -----------------------------
#
# Tip taxonomy of the Carruthers et al. dated plant phylogeny, taken from the
# OSF project that distributes the trees (node 9tbha). The tip labels of
# best_wcvp.tre_dated are formatted as Order_Family_Genus_species.
#
# The result is consumed by classification.R and fix_duplicated_genus.R, which
# add the genera it holds that our classification does not already have. This
# script is provenance only: it hits the network and downloads ~135 MB, so it
# is not part of the data build. Rerun it by hand when a new version of the
# tree is released, then re-run fix_duplicated_genus.R.
#
# Two things to know about the file it produces:
#   - order and family are missing for 47 tips, and 9 tips have a species slot
#     that is not a binomial epithet ("sp.", "sect.", "x"). Both are flagged
#     rather than dropped: filter on is.na(family) and is_binomial downstream.
#   - 49 genera appear under two different families (Dryopteris in both
#     Polypodiaceae and Dryopteridaceae, Muscari in both Asparagaceae and
#     Hyacinthaceae, ...), so it is not safe to join on genus without
#     collapsing those first. See fix_duplicated_genus.R.

xfun::pkg_attach2(c("osfr"))
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

project <- osfr::osf_retrieve_node("9tbha")
tree_files <- osfr::osf_ls_files(project, n_max = Inf)

# best_wcvp.tre_dated is the single best-scoring dated tree, used here for the
# taxonomy; zipped-dated-wcvp-trees.zip holds the posterior set behind
# megatrees::get_tree_plant_n100_Carruthers()
tree_files_to_use <- c("best_wcvp.tre_dated", "zipped-dated-wcvp-trees.zip")
tree_files_to_use <- dplyr::filter(tree_files, name %in% tree_files_to_use)

temp_dir <- tempdir()
tree_d <- osfr::osf_download(tree_files_to_use, temp_dir, conflicts = "overwrite")

pt_1 <- ape::read.tree(filter(tree_d, name == "best_wcvp.tre_dated")$local_path)

## Tip labels are Order_Family_Genus_species. Parse strictly: a label with the
## wrong number of parts should error, not silently yield a garbage genus
## (str_remove() is a no-op once the label runs out of underscores).
parse_carruthers_tips <- function(tip_label) {
  tibble(tip_label = tip_label) |>
    tidyr::separate_wider_delim(
      tip_label,
      delim = "_",
      names = c("order", "family", "genus", "species"),
      too_few = "error", too_many = "error", cols_remove = FALSE
    ) |>
    # order/family are the literal string "NA" for 47 tips, not a real NA
    mutate(
      across(c(order, family), \(x) na_if(x, "NA")),
      tips = paste(genus, species, sep = "_"),
      is_binomial = str_detect(species, "^[a-z][a-z-]+$")
    )
}

pt_1_taxon <- parse_carruthers_tips(pt_1$tip.label)

# counts, previous version of the tree ---> this one
n_distinct(pt_1_taxon$tips) # 123,182 ---> 117,933
n_distinct(pt_1_taxon$genus) # 12,684 ---> 12,236
n_distinct(pt_1_taxon$family) # 515 ---> 475

sum(is.na(pt_1_taxon$family)) # 47
sum(!pt_1_taxon$is_binomial) # 9: "sp.", "sect.", "x"

saveRDS(pt_1_taxon, "data-raw/carruthers_taxonomy.rds")
