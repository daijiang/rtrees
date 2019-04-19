source("data-raw/functions.R")

# classifications ----

# plants ----
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
any(duplicated(classification_plants$genus)) # all genus monophytic? T
# fish ----
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

# mammals ----
names_mammal = read_csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Taxonomy/Synonymy_table_valid_species_only.csv")
names_mammal = unique(select(names_mammal, species = Binomial.1.2, genus = Genus.1.2, family = Family.1.2))
classification_mammal = unique(select(names_mammal, genus, family)) %>% mutate(taxon = "mammal")
any(duplicated(classification_mammal$genus))

# combine classifications ----
classifications = bind_rows(mutate(classification_plants, taxon = "plant"),
                            mutate(classification_fish, taxon = "fish"))
classifications = bind_rows(classifications, 
                            mutate(classification_bird, taxon = "bird"))
classifications = bind_rows(classifications, classification_mammal)
usethis::use_data(classifications, overwrite = T, compress = "xz")

# mega-trees ===============================================================

# plant ----
load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/GBOTB.extended.rda")))

# # takes a while, commented out
# tree_plant_GBOTB = add_root_info(tree = GBOTB.extended, classification = classification_plants)

# usethis::use_data(tree_plant_GBOTB, overwrite = T, compress = "xz")
# tools::checkRdaFiles("data/tree_plant_GBOTB.rda")

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
tree_fish = add_root_info(fish_tree_12k, classification_fish)
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

# mammals ----
m_url = "https://github.com/MegaPast2Future/PHYLACINE_1.2/blob/master/Data/Phylogenies/Complete_phylogeny.nex"
browseURL(m_url)
# download mammal tree, 1000 trees
tree_mammal = ape::read.nexus("~/Downloads/Complete_phylogeny.nex")
set.seed(123)
nnw = sample(1:1000, 1) # 288
tree_mammal = tree_mammal[[nnw]]
tree_mammal = add_root_info(tree_mammal, classification_mammal)
usethis::use_data(tree_mammal, overwrite = T, compress = "xz")


# butterfly ----
xb = readLines("https://datadryad.org/bitstream/handle/10255/dryad.170784/Espeland_et_al_Dated_tree_Magallon_2015_root_calibration.tre?sequence=1")
xb[4] = sub("UTREE", "TREE", xb[4])
tempf = tempfile()
cat(paste(xb, collapse = "\n"), file = tempf)
tree_butterfly = ape::read.nexus(tempf)
plot(tree_butterfly, cex = 0.5)
tree_butterfly$tip.label
unlink(tempf)

