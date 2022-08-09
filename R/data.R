#' Classifications of species
#' 
#' @description 
#' Genus and family information of different groups of taxon. 
#' 
#' - Plant classification information. Its sources include:
#'         + based on [`V.PhyloMaker::nodes.info.1`](https://github.com/jinyizju/V.PhyloMaker/tree/master/data)
#'         + based on The Plant List
#'         + [taxonlookup](https://github.com/traitecoevo/taxonlookup)
#'         + [Plants of the World online](http://plantsoftheworldonline.org/)
#'         
#' - Fish classification information was based on FishBase. There are 4,825 genus in this file.
#' <https://fishtreeoflife.org/downloads/PFC_taxonomy.csv.xz>
#' 
#' - Bird classification information was based on BirdLife, which resulted in 2,391 genus.
#' <http://datazone.birdlife.org/species/taxonomy> However, based on the 
#' [taxonomy file](https://data.vertlife.org/birdtree/BLIOCPhyloMasterTax.csv)
#' of the Jetz et al. 2012 phylogeny, there are additional 117 genus that are not in the file
#' of BirdLife. Both are combined here, which leads to 2,508 genus.
#' 
#' - Mammal classification information was based on PHYLACINE, which has 1,400 genus. 
#' <https://github.com/MegaPast2Future/PHYLACINE_1.2/blob/master/Data/Taxonomy/Synonymy_table_valid_species_only.csv>
#' 
#' @format A data frame with three columns: genus, family, and taxon (`plant`, `fish`, `bird`, `mammal`). 
#' 
"classifications"
