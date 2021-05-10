library(tidyverse)
library(ape)
library(tidytree)

# mega-trees ===============================================================

# plant ----
load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/GBOTB.extended.rda")))

# # takes a while, commented out
# tree_plant_otl = add_root_info(tree = GBOTB.extended, classification = dplyr::filter(classifications, taxon == "plant"))

# usethis::use_data(tree_plant_otl, overwrite = T, compress = "xz")
# tools::checkRdaFiles("data/tree_plant_otl.rda")

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

# find roots for fish tree
tree_fish = add_root_info(fish_tree_12k, dplyr::filter(classifications, taxon == "fish"))
usethis::use_data(tree_fish, overwrite = T, compress = "xz")

# birds ----
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
unlink(unzip(tempf, list = T)$Name[1]) # need modify
unlink(tempf)
tree_bird_ericson$node.label = paste0("N", 1:Nnode(tree_bird_ericson))
tree_bird_ericson0 = tree_bird_ericson
tree_bird_ericson = add_root_info(tree_bird_ericson, dplyr::filter(classifications, taxon == "bird"))
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
tree_bird_hackett = add_root_info(tree_bird_hackett, dplyr::filter(classifications, taxon == "bird"))
tree_bird_hackett$genus_family_root
tail(tree_bird_hackett$genus_family_root)
usethis::use_data(tree_bird_hackett, overwrite = T, compress = "xz")
setdiff(tree_bird_ericson$tip.label, tree_bird_hackett$tip.label)
setdiff(tree_bird_ericson$tip.label, tips$species)

# mammals ----
m_url = "https://media.githubusercontent.com/media/MegaPast2Future/PHYLACINE_1.2/master/Data/Phylogenies/Complete_phylogeny.nex"
tempf = tempfile()
download.file(m_url, tempf)
# download mammal tree, 1000 trees
tree_mammal = ape::read.nexus(tempf)
unlink(tempf)
class(tree_mammal)

set.seed(123)
nnw = sample(1:1000, 100) # 288
tree_mammal_phylacine = tree_mammal[nnw]
tree_mammal_phylacine = lapply(tree_mammal_phylacine, add_root_info, 
                               classification = dplyr::filter(rtrees::classifications, taxon == "mammal"))
names(tree_mammal_phylacine) = paste0("tree_", nnw)
# tree_mammal = add_root_info(tree_mammal, classification_mammal)
usethis::use_data(tree_mammal, overwrite = T, compress = "xz")
usethis::use_data(tree_mammal_phylacine, overwrite = T, compress = "xz")

# from vertlife
"https://data.vertlife.org/"
"https://data.vertlife.org/mammaltree/Completed_5911sp_topoCons_NDexp.zip"


# butterfly ----
xb = readLines("https://datadryad.org/bitstream/handle/10255/dryad.170784/Espeland_et_al_Dated_tree_Magallon_2015_root_calibration.tre?sequence=1")
xb[4] = sub("UTREE", "TREE", xb[4])
tempf = tempfile()
cat(paste(xb, collapse = "\n"), file = tempf)
tree_butterfly = ape::read.nexus(tempf)
plot(tree_butterfly, cex = 0.5)
tree_butterfly$tip.label
unlink(tempf)

