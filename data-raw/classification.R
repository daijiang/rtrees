library(tidyverse)
library(ape)
library(tidytree)

# classifications of tips in the mega-trees ----

# # plants ----
# load(rawConnection(RCurl::getBinaryURL("https://raw.githubusercontent.com/jinyizju/V.PhyloMaker/master/data/nodes.info.1.rda")))
# classification_plants = select(nodes.info.1, genus, family) %>%
#   unique() %>%
#   filter(genus != "") %>%
#   as_tibble()
# # by looking at the phylogeny, two species do not have classification info:
# # Malaisia_scandens, Lithraea_molleoides
# # https://en.wikipedia.org/wiki/Trophis_scandens
# # tips$family[tips$species == "Malaisia_scandens"] = "Euphorbiaceae" # wiki
# # but the plant list said it is an annoymous...which makes me wonder how
# # V.PhyloMaker did their name standardization. I probably should do it by myself.
# # http://www.theplantlist.org/tpl1.1/record/kew-5837
# # tips$family[tips$species == "Lithraea_molleoides"] = "Anacardiaceae" # wiki
# # https://en.wikipedia.org/wiki/Lithraea_molleoides
# if(!"Malaisia" %in% classification_plants$genus)
#   classification_plants = add_row(classification_plants,
#                                   genus = "Malaisia",
#                                   family = "Euphorbiaceae")
# if(!"Lithraea" %in% classification_plants$genus)
#   classification_plants = add_row(classification_plants,
#                                   genus = "Lithraea",
#                                   family = "Anacardiaceae")

# # all genus and family from The Plant List ----
# tpl_family = xml2::read_html("http://theplantlist.org/1.1/browse/-/") %>%
#   rvest::html_nodes(".family") %>%
#   rvest::html_text() # 652 families
#
# get_sp_per_family = function(x = "Didiereaceae"){
#   cat(x, "\t")
#   base_url = paste0("http://theplantlist.org/1.1/browse/A/", x, "/", x, ".csv")
#   out = try(read_csv(base_url))
#   if(inherits(out, "try-error")){
#     base_url = paste0("http://theplantlist.org/1.1/browse/B/", x, "/", x, ".csv")
#     out = try(read_csv(base_url))
#   }
#   if(inherits(out, "try-error")){
#     base_url = paste0("http://theplantlist.org/1.1/browse/P/", x, "/", x, ".csv")
#     out = try(read_csv(base_url))
#   }
#   if(inherits(out, "try-error")){
#     base_url = paste0("http://theplantlist.org/1.1/browse/G/", x, "/", x, ".csv")
#     out = try(read_csv(base_url))
#   }
#   out = try(out %>%
#               select(genus = Genus, family = Family) %>%
#               unique())
#   out
# }
# classification_plant = map(tpl_family, get_sp_per_family)
# classification_plant_TPL = bind_rows(classification_plant)
# usethis::use_data(classification_plant_TPL)
# classification_plant = bind_rows(classification_plant_TPL,
#                                  # from Jin & Qian, 2019
#                                  tibble::tribble(~genus,~family,
#                                                  "Davilanthus","Asteraceae",
#                                                  "Ewartiothamnus","Asteraceae",
#                                                  "Myrovernix","Asteraceae",
#                                                  "Gongyloglossa","Asteraceae",
#                                                  "Laevicarpa","Asteraceae",
#                                                  "Monticapra","Asteraceae",
#                                                  "Leucosyris","Asteraceae",
#                                                  "Kieslingia","Asteraceae",
#                                                  "Tephrothamnus","Asteraceae",
#                                                  "Kurziella","Asteraceae",
#                                                  "Sampera","Asteraceae",
#                                                  "Platycarphella","Asteraceae",
#                                                  "Pseudocodon","Campanulaceae",
#                                                  "Pankycodon","Campanulaceae",
#                                                  "Himalacodon","Campanulaceae",
#                                                  "Rivasmartinezia","Apiaceae",
#                                                  "Schultzia","Apiaceae",
#                                                  "Rughidia","Apiaceae",
#                                                  "Szovitsia","Apiaceae",
#                                                  "Spuriopimpinella","Apiaceae",
#                                                  "Trichera","Caprifoliaceae",
#                                                  "Erythranthe","Phrymaceae",
#                                                  "Diceratotheca","Acanthaceae",
#                                                  "Chayamaritia","Gesneriaceae",
#                                                  "Somrania","Gesneriaceae",
#                                                  "Tribounia","Gesneriaceae",
#                                                  "Lesia","Gesneriaceae",
#                                                  "Trichodrymonia","Gesneriaceae",
#                                                  "Johnstonella","Boraginaceae",
#                                                  "Greeneocharis","Boraginaceae",
#                                                  "Foonchewia","Rubiaceae",
#                                                  "Edrastima","Rubiaceae",
#                                                  "Dimetia","Rubiaceae",
#                                                  "Rubiaceae","Rubiaceae",
#                                                  "Rhachicallis","Rubiaceae",
#                                                  "Anemotrochus","Apocynaceae",
#                                                  "Monsanima","Apocynaceae",
#                                                  "Calciphila","Apocynaceae",
#                                                  "Richtersveldia","Apocynaceae",
#                                                  "White-sloanea","Apocynaceae",
#                                                  "Agiortia","Ericaceae",
#                                                  "Acrothamnus","Ericaceae",
#                                                  "Leptecophylla","Ericaceae",
#                                                  "Pleioluma","Sapotaceae",
#                                                  "Bemangidia","Sapotaceae",
#                                                  "Kewa","Kewaceae",
#                                                  "Pseudocherleria","Caryophyllaceae",
#                                                  "Sedobassia","Amaranthaceae",
#                                                  "Bactria","Polygonaceae",
#                                                  "Solori","Fabaceae",
#                                                  "Oberholzeria","Fabaceae",
#                                                  "Gabonius","Fabaceae",
#                                                  "Symbegonia","Begoniaceae",
#                                                  "Synostemon","Phyllanthaceae",
#                                                  "Gitara","Euphorbiaceae",
#                                                  "Hartogiopsis","Celastraceae",
#                                                  "Thelypodieae","Brassicaceae",
#                                                  "Phyllolepidum","Brassicaceae",
#                                                  "Kitaibela","Malvaceae",
#                                                  "Anthocarapa","Meliaceae",
#                                                  "Tetracarpaea","Tetracarpaeaceae",
#                                                  "Eucarpha","Proteaceae",
#                                                  "Oncidiinae","Orchidaceae",
#                                                  "Schlimmia","Orchidaceae",
#                                                  "Orthochilus","Orchidaceae",
#                                                  "Pendulorchis","Orchidaceae",
#                                                  "Neooreophilus","Orchidaceae",
#                                                  "Dracontia","Orchidaceae",
#                                                  "Sansonia","Orchidaceae",
#                                                  "Danxiaorchis","Orchidaceae",
#                                                  "Orchidaceae","Orchidaceae",
#                                                  "Tsaiorchis","Orchidaceae",
#                                                  "Dithrix","Orchidaceae",
#                                                  "Sinocurculigo","Hypoxidaceae",
#                                                  "Dupontiopsis","Poaceae",
#                                                  "Koordersiochloa","Poaceae",
#                                                  "Calliscirpus","Cyperaceae",
#                                                  "Wallisia","Bromeliaceae",
#                                                  "Stigmatodon","Bromeliaceae",
#                                                  "Zizkaea","Bromeliaceae",
#                                                  "Josemania","Bromeliaceae",
#                                                  "Borneocola","Zingiberaceae",
#                                                  "Ripogonum","Ripogonaceae",
#                                                  "Onixotis","Colchicaceae",
#                                                  "Schottarum","Araceae",
#                                                  "Fenestratarum","Araceae",
#                                                  "Hottarum","Araceae",
#                                                  "Guamia","Annonaceae",
#                                                  "Winitia","Annonaceae",
#                                                  "Huberantha","Annonaceae",
#                                                  "Sirdavidia","Annonaceae",
#                                                  "Hypodematium","Hypodematiaceae",
#                                                  "Desmophlebium","Desmophlebiaceae")
# )
# filter(classification_plant_TPL, genus %in% filter(classification_plant_TPL, duplicated(genus))$genus)
# any(duplicated(classification_plant_TPL$genus)) # all genus monophytic? No...


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

# other genus based on later tests
classifications = add_row(classifications,
                          genus = "Epifagus", family = "Orobanchaceae", taxon = "plant") %>% 
  add_row(genus = "Elytrigia", family = "Poaceae", taxon = "plant")

classifications = add_row(classifications,
                          genus = "Rumex", family = "Polygonaceae", taxon = "plant")

# taxonlookup ----
# devtools::install_github("wcornwell/taxonlookup")
# maily from TPL too.
aplant = taxonlookup::plant_lookup() %>% 
  select(genus, family) %>% 
  mutate(taxon = "plant") %>% 
  as_tibble()

classifications = bind_rows(classifications, aplant) %>% 
  unique() %>% 
  arrange(taxon, family, genus)
classifications$family[classifications$family == "Isoëtaceae"] = "Isoetaceae"

classifications = tibble::add_row(classifications,
                                  genus = c("Dasyatis", "Entosphenus", "Ichthyomyzon", "Lampetra", "Lethenteron", "Petromyzon"), 
                                  family = c("Dasyatidae", rep("Petromyzontidae", 5)), taxon = "fish")

classifications = unique(classifications)
classifications = filter(classifications, !(genus == "Nyssa" & family == "Cornaceae"))

usethis::use_data(classifications, overwrite = T, compress = "xz")

# duplicated genera ----
dp = filter(classifications, taxon == "plant") %>% 
  group_by(genus) %>% 
  tally() %>% 
  filter(n > 1) %>% pull(genus)
# # checked with POWO http://www.plantsoftheworldonline.org/
# # several genus not there or have different family
# # used Wiki or TPL instead
# taxize::get_pow_("Balbisia")[[1]] %>% 
#   filter(rank == "Genus", accepted) %>% 
#   pull(family)
# taxize::pow_lookup("urn:lsid:ipni.org:names:7831-1")$meta$taxonomicStatus
# taxize::pow_lookup("urn:lsid:ipni.org:names:329554-2")$meta$taxonomicStatus
# taxize::pow_synonyms(id = 'urn:lsid:ipni.org:names:7831-1')
# taxize::pow_synonyms(id = 'urn:lsid:ipni.org:names:329554-2')
# xb = taxize::pow_search(q = "Balbisia")
# xb$meta
# View(xb$data)

gfam_powo = tribble(
  ~genus, ~family, ~taxon,
  "Alzatea","Alzateaceae","plant",
  "Anthobolus","Santalaceae","plant",
  "Apodytes","Metteniusaceae","plant",
  "Axinandra","Crypteroniaceae","plant",
  "Balbisia","Francoaceae","plant",
  "Batis","Bataceae","plant",
  "Bersama","Francoaceae","plant",
  "Bornmuellerantha","Orobanchaceae","plant",
  "Borthwickia","Resedaceae","plant",
  "Bottegoa","Rutaceae","plant",
  "Brandisia","Orobanchaceae","plant",
  "Calandrinia","Montiaceae","plant",
  "Calatola","Metteniusaceae","plant",
  "Calophyllum","Calophyllaceae","plant",
  "Camptotheca","Nyssaceae","plant",
  "Cedrelopsis","Rutaceae","plant",
  "Chaetocarpus","Peraceae","plant",
  "Chamaescilla","Asphodelaceae","plant",
  "Cissarobryon","Francoaceae","plant",
  "Cistanthe","Montiaceae","plant",
  "Cleoserrata","Cleomaceae","plant",
  "Corrigiola","Molluginaceae","plant",
  "Crypteronia","Crypteroniaceae","plant",
  "Cubitanthus","Gesneriaceae","plant",
  "Cyathobasis","Amaranthaceae","plant",
  "Dactylocladus","Crypteroniaceae","plant",
  "Davidia","Nyssaceae","plant",
  "Dendrobangia","Metteniusaceae","plant",
  "Diplopanax","Nyssaceae","plant",
  "Dodartia","Mazaceae","plant",
  "Emmotum","Metteniusaceae","plant",
  "Euryodendron","Theaceae","plant",
  "Forchhammeria","Resedaceae","plant",
  "Francoa","Francoaceae","plant",
  "Gallesia","Petiveriaceae","plant",
  "Greyia","Francoaceae","plant",
  "Griselinia","Griseliniaceae","plant",
  "Hemidictyum","Aspleniaceae","plant",
  "Hilleria","Petiveriaceae","plant",
  "Kaliphora","Cornaceae","plant",
  "Lancea","Mazaceae","plant",
  "Lapiedra","Amaryllidaceae","plant",
  "Ledenbergia","Petiveriaceae","plant",
  "Lindenbergia","Orobanchaceae","plant",
  "Macarthuria","Macarthuriaceae","plant",
  "Malaisia","Moraceae","plant",
  "Mastixia","Nyssaceae","plant",
  "Maundia","Maundiaceae","plant",
  "Mazus","Mazaceae","plant",
  "Melianthus","Francoaceae","plant",
  "Microtea","Microteaceae","plant",
  "Nuttallia","Rosaceae","plant",
  "Oecopetalum","Metteniusaceae","plant",
  "Ottoschulzia","Metteniusaceae","plant",
  "Peltanthera","Gesneriaceae","plant",
  "Petiveria","Petiveriaceae","plant",
  "Philcoxia","Plantaginaceae","plant",
  "Pittosporopsis","Icacinaceae","plant",
  "Platea","Icacinaceae","plant",
  "Poraqueiba","Metteniusaceae","plant",
  "Purdiaea","Clethraceae","plant",
  "Pyrsonota","Elaeocarpaceae","plant",
  "Rehmannia","Orobanchaceae","plant",
  "Rhaphiostylis","Metteniusaceae","plant",
  "Rhipogonum","Rhipogonaceae","plant",
  "Rhynchotheca","Geraniaceae","plant",
  "Richea","Rhizophoraceae","plant",
  "Rivina","Petiveriaceae","plant",
  "Seguieria","Petiveriaceae","plant",
  "Stemodiopsis","Linderniaceae","plant",
  "Stilbocarpa","Apiaceae","plant",
  "Stixis","Resedaceae","plant",
  "Tetilla","Saxifragaceae","plant",
  "Triaenophora","Plantaginaceae","plant",
  "Trichopodium","Dioscoreaceae","plant",
  "Trichostigma","Petiveriaceae","plant",
  "Trigonopleura","Peraceae","plant",
  "Viviania","Vivianiaceae","plant",
  "Wendtia","Francoaceae","plant"
)

classifications = bind_rows(
  filter(classifications, taxon == "plant", 
         !genus %in% dp),
  gfam_powo
) %>% 
  bind_rows(filter(classifications, taxon != "plant"))

usethis::use_data(classifications, overwrite = T, compress = "xz")

# catalogue of life 2019 ----
## https://www.catalogueoflife.org/content/annual-checklist-archive 
catl_2019 = vroom::vroom("~/Downloads/2019-annual/taxa.txt")
sort(unique(catl_2019$kingdom))
xc = filter(catl_2019, kingdom == "Plantae")
filter(xc, taxonomicStatus == "accepted name")
xc_cls = select(xc, family, genus) %>% 
  drop_na(family, genus) %>% 
  distinct()
n_distinct(xc_cls$family)
n_distinct(xc_cls$genus)
xc_cls2 = left_join(xc_cls, filter(classifications, taxon == "plant"))
filter(xc_cls2, is.na(taxon)) %>% View()
xc_cls3 = filter(xc_cls2, is.na(taxon), family != "Not assigned") %>% 
  mutate(taxon = "plant")
xc_cls3[!xc_cls3$genus %in% classifications$genus, ] # 26 new genus
classifications = bind_rows(classifications,
                            xc_cls3[!xc_cls3$genus %in% classifications$genus, ])

filter(classifications, taxon == "plant") %>% 
  group_by(genus) %>% 
  summarise(n_f = n_distinct(family)) %>% 
  arrange(desc(n_f)) %>% 
  filter(n_f > 1) # no duplicated genus

# Plant of World online data ----
# https://github.com/RBGKew/powo-data/blob/master/data-prod.json
"https://storage.googleapis.com/powop-content/backbone/powoNames.zip" # download and unzip. it is large
powo = data.table::fread("/media/dli/Data/common_data/taxon_powo.txt")
powo2 = filter(powo, V3 == "Genus", V28 == "Accepted")
powo_gf = select(powo2, genus = V6, family = V5) %>% distinct() %>% 
  mutate(taxon = "plant") %>% as_tibble()

setdiff(filter(classifications, taxon == "plant")$genus, powo_gf$genus)
setdiff(powo_gf$genus, filter(classifications, taxon == "plant")$genus)

filter(powo_gf, genus %in% intersect(powo_gf$genus, filter(classifications, taxon == "plant")$genus)) %>% 
  rename(family_powo = family) %>% 
  left_join(filter(classifications, taxon == "plant")) %>% 
  mutate(dame = family_powo == family) -> tst
filter(tst, !dame) %>% View()

class_plant = bind_rows(
  # unique genus from other sources
  filter(classifications, taxon == "plant", !genus %in% powo_gf$genus),
  # common genus between other sources and POWO, use accepted info from POWO instead
  filter(powo_gf, genus %in% intersect(powo_gf$genus, filter(classifications, taxon == "plant")$genus)),
  # unique genus from POWO
  filter(powo_gf, genus %in% setdiff(powo_gf$genus, filter(classifications, taxon == "plant")$genus))
)


classifications = bind_rows(class_plant, 
                            filter(classifications, taxon != "plant")) %>% 
  distinct()
                   
tools::showNonASCII(classifications$genus)
# classifications$genus[15153] = "Leptochloopsis" # "Leptochloöpsis"
# classifications$genus = stringi::stri_trans_general(classifications$genus, "Latin-ASCII")
                          
usethis::use_data(classifications, overwrite = T, compress = "xz")
