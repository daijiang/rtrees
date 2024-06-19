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
#' - Bee classification information was from [Bee Tree of Life](http://beetreeoflife.org/nomenclature/). Note that we used 'Subfamily' in their nomenclature file as "family" here. If a genus' Subfamily is missing, we used its Family.
#' 
#' - Bird classification information was based on BirdLife, which resulted in 2,391 genus.
#' <http://datazone.birdlife.org/species/taxonomy> However, based on the 
#' [taxonomy file](https://data.vertlife.org/birdtree/BLIOCPhyloMasterTax.csv)
#' of the Jetz et al. 2012 phylogeny, there are additional 117 genus that are not in the file
#' of BirdLife. Both are combined here, which leads to 2,508 genus.
#' 
#' - Mammal classification information was based on PHYLACINE, which has 1,400 genus. 
#' <https://github.com/MegaPast2Future/PHYLACINE_1.2/blob/master/Data/Taxonomy/Synonymy_table_valid_species_only.csv> Additional 
#' genus from Vertlife were added too. For the same genus from both PHYLACINE and Vertlife that have different family information,
#' I used the family from Vertlife as I found that they are mostly more accurate.
#' 
#' - Amphibian classification information was from [VertLife](https://data.vertlife.org/amphibiantree/download/amph_shl_new_Classification.csv).
#' 
#' - Reptile classification information was largely from [wikipedia](https://en.wikipedia.org/wiki/List_of_reptile_genera).
#' 
#' - Shark and Ray classification information was largely from NCBI.
#' 
#' - Butterfly classification information was from Kawahara et al. 2023, using the tip labels of their phylogeny.
#' 
#' @format A data frame with three columns: genus, family, and taxon (`plant`, `fish`, `bird`, `mammal`, `amphibian`, `reptile`, `shark_ray`, `bee`, `butterfly`). 
#' 
"classifications"
