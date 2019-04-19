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
any(duplicated(classification_fish$genus)) # all genus monophytic? T
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


# birds ----
tempf = tempfile(fileext = ".zip")
download.file("http://datazone.birdlife.org/userfiles/file/Species/Taxonomy/HBW-BirdLife_Checklist_v3_Nov18.zip",
              tempf)
unzip(tempf, list = T)
unzip(tempf, file = "HBW-BirdLife_Checklist_Version_3.xlsx")
bird_names = readxl::read_excel("HBW-BirdLife_Checklist_Version_3.xlsx", skip = 1)
unlink(tempf)
unlink("HBW-BirdLife_Checklist_Version_3.xlsx")
bird_names = unique(dplyr::select(bird_names, species = `Scientific name`, family = `Family name`)) %>% 
  mutate(species = gsub(" ", "_", species))

bird_names = mutate(bird_names, genus = gsub("^([-A-Za-z]*)_.*$", "\\1", species))
classification_bird = select(bird_names, genus, family) %>% unique()

tips = read_csv("https://data.vertlife.org/birdtree/BLIOCPhyloMasterTax.csv") %>% 
  select(species = TipLabel, family = BLFamilyLatin) %>% 
  mutate(genus = gsub("^([-A-Za-z]*)_.*$", "\\1", species))
select(tips, genus, family) %>% unique()
all(tips$genus %in% classification_bird$genus)
setdiff(tips$genus, classification_bird$genus)
setdiff(classification_bird$genus, tips$genus)
full_join(unique(select(tips, genus, family)), classification_bird) 
# filter(classification_bird, genus == "Bias")
# filter(tips, genus == "Bias")
# It seems the online version of birdlife is more accurate, if a genus has different family,
# use birdlife's version.
classification_bird2 = filter(unique(select(tips, genus, family)), 
       !genus %in% classification_bird$genus)
filter(tips, genus %in% 
classification_bird2$genus[duplicated(classification_bird2$genus)])
# double checked, and it should be an error,
# genus Chlorothraupis belongs to family Cardinalidae
classification_bird2 = filter(classification_bird2, !(genus == "Chlorothraupis" & family == "Thraupidae"))
classification_bird = bind_rows(
  classification_bird,
  classification_bird2
)
any(duplicated(classification_bird$genus))

# phylogeny
# downloaded the first 1000 phylogenies
tempf = tempfile(fileext = ".zip")
download.file("https://data.vertlife.org/birdtree/Stage2/EricsonStage2_0001_1000.zip",
              tempf)
unzip(tempf, list = T)
unzip(tempf, file = unzip(tempf, list = T)$Name[1])
tree_bird_ericson = read.tree(unzip(tempf, list = T)$Name[1])
set.seed(123)
nnw = sample(1:1000, 1) # 288
tree_bird_ericson = tree_bird_ericson[[nnw]]
unlink(unzip(tempf, list = T)$Name[1])
unlink(tempf)
tree_bird_ericson$node.label = paste0("N", 1:Nnode(tree_bird_ericson))
tree_bird_ericson0 = tree_bird_ericson
tree_bird_ericson = add_root_info(tree_bird_ericson, classification_bird)
usethis::use_data(tree_bird_ericson, overwrite = T, compress = "xz")

tempf = tempfile(fileext = ".zip")
download.file("https://data.vertlife.org/birdtree/Stage2/HackettStage2_0001_1000.zip",
              tempf)
unzip(tempf, list = T)
unzip(tempf, file = unzip(tempf, list = T)$Name[1])
set.seed(123)
nnw = sample(1:1000, 1) # 288
tree_bird_hackett = read.tree(unzip(tempf, list = T)$Name[1])[[nnw]]
unlink(unzip(tempf, list = T)$Name[1])
unlink(tempf)

tree_bird_hackett$node.label = paste0("N", 1:Nnode(tree_bird_hackett))
tree_bird_hackett0 = tree_bird_hackett
tree_bird_hackett = add_root_info(tree_bird_hackett, classification_bird)
tree_bird_hackett$genus_family_root
tail(tree_bird_hackett$genus_family_root)
usethis::use_data(tree_bird_hackett, overwrite = T, compress = "xz")
setdiff(tree_bird_ericson$tip.label, tree_bird_hackett$tip.label)
setdiff(tree_bird_ericson$tip.label, tips$species)


xx = read.nexus("~/Downloads/tree-pruner-67cdc9cc-3480-4bc8-96f3-0d997b1e4802/output.nex")
plot(xx[[1]])

# mammals ----

# butterfly ----
xb = readLines("https://datadryad.org/bitstream/handle/10255/dryad.170784/Espeland_et_al_Dated_tree_Magallon_2015_root_calibration.tre?sequence=1")
xb[4] = sub("UTREE", "TREE", xb[4])
tempf = tempfile()
cat(paste(xb, collapse = "\n"), file = tempf)
tree_butterfly = ape::read.nexus(tempf)
plot(tree_butterfly, cex = 0.5)
tree_butterfly$tip.label

# combine classifications ----
classifications = bind_rows(mutate(classification_plants, taxon = "plant"),
          mutate(classification_fish, taxon = "fish"))
classifications = bind_rows(classifications, 
                            mutate(classification_bird, taxon = "bird"))
usethis::use_data(classifications, overwrite = T, compress = "xz")
