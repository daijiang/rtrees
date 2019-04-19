source("data-raw/functions.R")

# plants ----
load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/GBOTB.extended.rda")))
load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/nodes.info.1.rda")))
classification_plants = select(nodes.info.1, genus, family) %>% 
  unique() %>% 
  filter(genus != "") %>% 
  as_tibble()
# by looking at the phylogeny, two species do not have classification info:
# Malaisia_scandens, Lithraea_molleoides
# https://en.wikipedia.org/wiki/Trophis_scandens
# tips$family[tips$species == "Malaisia_scandens"] = "Euphorbiaceae" # wiki
# but the plant list said it is an annoymous...which makes me wonder how
# V.PhyloMaker did their name standardization. I probably should do it by myself.
# http://www.theplantlist.org/tpl1.1/record/kew-5837
# tips$family[tips$species == "Lithraea_molleoides"] = "Anacardiaceae" # wiki
# https://en.wikipedia.org/wiki/Lithraea_molleoides
if(!"Malaisia" %in% classification_plants$genus)
  classification_plants = add_row(classification_plants, 
                                  genus = "Malaisia",
                                  family = "Euphorbiaceae")
if(!"Lithraea" %in% classification_plants$genus)
  classification_plants = add_row(classification_plants, 
                                  genus = "Lithraea",
                                  family = "Anacardiaceae")
# usethis::use_data(classification_plants, overwrite = T, compress = "xz")

# # takes a while, commented out
# tree_plant_GBOTB = add_root_info(tree = GBOTB.extended, classification = classification_plants)

# usethis::use_data(tree_plant_GBOTB, overwrite = T, compress = "xz")
# tools::checkRdaFiles("data/tree_plant_GBOTB.rda")

# test
sp_list = tibble::as_tibble(read.csv("~/Dropbox/Reading/ECOG-04434/Appendix_3-Example_species_list.csv", stringsAsFactors = F))

ape::branching.times(tidytree::as.phylo(tree_df))[where]
system.time(tst1 <- get_tree(dplyr::filter(sp_list, !species %in% c("Hicoria_texana")), scenario = "S3"))
system.time(tst1 <- get_tree(sp_list, scenario = "S3"))
plot(tst1)

library(V.PhyloMaker)
system.time(tst1_v <- V.PhyloMaker::phylo.maker(sp_list, scenarios = "S3"))
par(mfrow = c(1, 2))
plot(ladderize(tst1), cex = 0.8)
plot(ladderize(tst1_v$scenario.2$run.1), cex = 0.8)
plot(ladderize(tree_sub), cex = 0.8)

# fish ----
# time tree
fishurl = "https://fishtreeoflife.org/downloads/actinopt_12k_treePL.tre.xz"
tempf = tempfile()
download.file(fishurl, tempf)
fish_tree_12k = read.tree(tempf) # 11638 species, no node labels
unlink(tempf)
fish_tree_12k$node.label = paste0("N", 1:Nnode(fish_tree_12k))

# # full tree
# fishurl2 = "https://fishtreeoflife.org/downloads/actinopt_full.trees.xz"
# tempf = tempfile()
# download.file(fishurl2, tempf)
# fish_tree_full = read.tree(tempf)
# str(fish_tree_full) # 100 phylogenies
# fish_tree_full[[1]] # 31516 species each
# Nnode(fish_tree_full[[1]])
# Nnode(fish_tree_full[[2]])
# # unlink(tempf)

# fish names
fishurl3 = "https://fishtreeoflife.org/downloads/PFC_taxonomy.csv.xz"
tempf2 = tempfile()
download.file(fishurl3, tempf2)
fish_names = read.csv(tempf2, stringsAsFactors = F) %>% as_tibble() %>% 
  select(genus.species, genus, family)
unlink(tempf2)

fish_names = mutate(fish_names, genus2 = gsub("^([-A-Za-z]*) .*$", "\\1", genus))
all(fish_names$genus == fish_names$genus2)
fish_names = select(fish_names, -genus2) %>% 
  rename(species = genus.species) %>% 
  mutate(species = gsub(" ", "_", species))
classification_fish = select(fish_names, genus, family) %>% 
  unique()
# usethis::use_data(fish_names, compress = "xz", overwrite = T)
# usethis::use_data(classification_fish, compress = "xz", overwrite = T)

# find roots for fish tree
tree_fish = add_root_info(fish_tree_12k, classification_fish)
usethis::use_data(tree_fish, overwrite = T, compress = "xz")

# test
sp_list_fish = tibble(species = c(sample(tree_fish$tip.label, 5), "Barathronus_bicolor",
                 sample(setdiff(fish_names$species, tree_fish$tip.label), 6))) %>% 
  unique() %>% 
  left_join(fish_names, by = "species")
test = get_tree(sp_list = sp_list_fish, tree = tree_fish, scenario = "S3")
test = get_tree(sp_list = sp_list_fish, taxon = "fish", scenario = "S3")
test = get_tree(sp_list = sp_list_fish, tree = tree_fish, show_grafted = T, scenario = "S3")
plot(ladderize(test))

# combine classifications ----
classifications = bind_rows(mutate(classification_plants, taxon = "plant"),
          mutate(classification_fish, taxon = "fish"))
usethis::use_data(classifications, overwrite = T, compress = "xz")
