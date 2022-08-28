
taxa_supported = c("amphibian", "bird", "fish", "mammal", "plant", "reptile", "shark_ray")
usethis::use_data(taxa_supported, overwrite = T)


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

classifications = bind_rows(classifications, 
                            read_csv("genus,family,taxon\nParacoelops,Hipposideridae,mammal\nDesmalopex,Pteropodidae,mammal\nSubmyotodon,Vespertilionidae,mammal\nEudiscoderma,Megadermatidae,mammal\nParastrellus,Vespertilionidae,mammal\nDryadonycteris,Phyllostomidae,mammal\nHsunycteris,Phyllostomidae,mammal\nPseudalopex,Canidae,mammal\nLeontocebus,Cebidae,mammal\nCallibella,Callitrichidae,mammal\nPipanacoctomys,Octodontidae,mammal\nLiomys,Heteromyidae,mammal\nMusseromys,Muridae,mammal\nMirzamys,Muridae,mammal\nHalmaheramys,Muridae,mammal\nWaiomys,Muridae,mammal\nDrymoreomys,Cricetidae,mammal\nCalassomys,Cricetidae,mammal\nParalomys,Cricetidae,mammal\n")) %>% 
  unique() %>% 
  arrange(taxon, genus)

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

classifications = add_row(classifications,
                          genus = c("Podagrostis", "Hesperostipa"), family = c("Poaceae", "Poaceae"),
                          taxon = c("plant", "plant"))

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


# amphibians ----
amph0 = read_csv("https://data.vertlife.org/amphibiantree/download/amph_shl_new_Classification.csv")
xfun::download_file("https://data.vertlife.org/amphibiantree/download/amph_shl_new_Classification.csv", tempf)
amph = filter(amph0, Taxon != "Outgroup") %>% 
  dplyr::select(sp = `Scientific Name`, family = Family) %>% 
  mutate(genus = str_extract(sp, "^[^ ]*")) %>% 
  select(genus, family) %>% 
  distinct() %>% 
  mutate(taxon = "amphibian") %>% 
  arrange(genus) %>% 
  filter(genus != "")
unlink(tempf)

classifications = bind_rows(amph, classifications) %>% 
  as_tibble() %>% 
  distinct()

# mammal vertlife ---
mammal_class_vertlife = read_csv("https://data.vertlife.org/mammaltree/taxonomy_mamPhy_5911species.csv")
mammal_class_vertlife = dplyr::select(mammal_class_vertlife, genus = gen, family = fam) %>% 
  distinct() %>% 
  mutate(family = cap_first_letter(tolower(family)))
setdiff(mammal_class_vertlife$genus, 
filter(classifications, taxon == "mammal")$genus)
left_join(mammal_class_vertlife,
rename(filter(classifications, taxon == "mammal"), f = family)) %>% 
  mutate(same = family == f) %>% 
  filter(!same) %>% View()
# it seems the vertlife classification is more accurate
classifications = filter(classifications, !(taxon == "mammal" & genus %in% mammal_class_vertlife$genus))
classifications = bind_rows(mutate(mammal_class_vertlife, taxon = "mammal"), 
                            classifications) %>% 
  distinct() %>% 
  arrange(taxon, genus)


# shark ---

sharks = read_csv("https://data.vertlife.org/sharktree/Species.list.csv")
shark_genus = sort(unique(str_extract(sharks$Species_list, "^[^_]*")))
setdiff(shark_genus, classifications$genus) # no...
shark_class = vector("list", length(shark_genus))
for(i in 144:length(shark_genus)){
  shark_class[[i]] = taxize::tax_name(shark_genus[i], get = "family", db = "ncbi")
  if(i %% 10 == 0) Sys.sleep(time = 2)
}

shark_class = bind_rows(shark_class)
filter(shark_class, is.na(family))

x = taxize::tax_name(filter(shark_class, is.na(family))$query, get = "family", db = "itis")
x$family[x$query == "Electrolux"] = "Narkidae"
x$family[x$query == "Makararaja"] = "Dasyatidae"
x$family[x$query == "Spiniraja"] = "Rajidae"
x$family[x$query == "Taeniurops"] = "Dasyatidae"
ss = ape::read.nexus("shark_10.cal.tree.nex")
ss1 = ss[[1]]
setdiff(sharks$Species_list, ss1$tip.label)
setdiff(ss1$tip.label, sharks$Species_list)

shark_class = bind_rows(shark_class, x) %>% 
  filter(!is.na(family))

shark_class = dplyr::select(shark_class, -db, genus = query, family) %>% 
  mutate(taxon = "shark_ray") %>% 
  arrange(genus)

classifications = bind_rows(classifications, shark_class) %>% 
  arrange(taxon, genus) %>% 
  distinct()


# reptile ----
rept = read_csv("https://data.vertlife.org/squamatetree/sqamate_names.csv", col_names = F)
rept_genus = sort(unique(str_extract(rept$X1, "^[^ ]*")))
setdiff(rept_genus, rtrees::classifications$genus) # no...
rept_class = vector("list", length(rept_genus))
for(i in 1:length(rept_genus)){
  rept_class[[i]] = taxize::tax_name(rept_genus[i], get = "family", db = "ncbi", ask = F)
  if(i %% 10 == 0) Sys.sleep(time = 30)
}
rept_class = bind_rows(rept_class)

x_rep = taxize::tax_name(filter(rept_class, is.na(family))$query, get = "family", db = "ncbi")

rept_class = read_csv("~/Documents/rept_class.csv")
n_distinct(rept_class$family) # 62

# get from wikipedia https://en.wikipedia.org/wiki/List_of_reptile_genera
rep_2 = readLines("~/Documents/reptile.txt")
f2 = grep("Family", rep_2, value = T)
rept_class_wiki = vector("list", length = length(f2))
names(rept_class_wiki) = f2
j = 1
for(i in 2:length(rep_2)){
  cat("i = ", i, "\t")
  if(rep_2[i] %in% names(rept_class_wiki)){
    j = j + 1
    cat("j = ", j, "\n")
    next()
  }
  if(!grepl(pattern = "Family", x = rep_2[i])){
    rept_class_wiki[[j]] = c(rept_class_wiki[[j]], rep_2[i])
  }
}



rept_class_wiki = bind_rows(lapply(rept_class_wiki, as.data.frame), .id = "family") %>% 
  set_names(c("family", "genus")) %>% 
  filter(genus != "") %>% 
  as_tibble() %>% 
  mutate(family = gsub("Family ", "", family),
         family = str_trim(family),
         genus = str_trim(genus))

setdiff(rept_class$genus, rept_class_wiki$genus)
setdiff(rept_class_wiki$genus, rept_class$genus)

rept_class_wiki2 = set_names(rept_class_wiki, c("family_wiki", "genus"))

xx = full_join(rept_class, rept_class_wiki2) %>% 
  mutate(same = family == family_wiki)

xx = mutate(xx, family_wiki = ifelse(is.na(family_wiki), family, family_wiki))

reptile_class = select(xx, genus, family = family_wiki) %>% 
  mutate(taxon = "reptile") %>% 
  distinct()
reptile_class[which(duplicated(reptile_class$genus)),]
reptile_class = filter(reptile_class, !genus %in% c("Homoroselaps", "Xylophis"))
reptile_class = add_row(reptile_class,
                        genus = c("Homoroselaps", "Xylophis"),
                        family = c("Atractaspididae", "Pareidae"),
                        taxon = "reptile")

classifications = bind_rows(classifications, reptile_class) %>% 
  arrange(taxon, genus) %>% 
  distinct()

classifications = add_row(classifications,
                          genus = c("Elachistodon", "Thalesius", "Parahelicops", "Pararhabdophis", "Vietnascincus",  
                                    "Haackgreerius", "Geomyersia", "Geoscincus", "Tachygyia",  "Leptoseps", "Chabanaudia",
                                    "Scolecoseps", "Chalcidoseps", "Sepsophis", "Nessia", "Jarujinia", "Barkudia", "Rhinogecko"),
                          family = c("Colubridae", "Colubridae", "Colubridae", "Colubridae", "Scincidae",
                                     "Scincidae",  "Scincidae",  "Scincidae", "Scincidae",  "Scincidae",  "Scincidae", 
                                     "Scincidae",  "Scincidae",  "Scincidae", "Scincidae",  "Scincidae",  "Scincidae", "Gekkonidae"
                          ),
                          taxon = "reptile")

filter(classifications, taxon == "amphibian", genus == "Homo")
o

# plants world flora online ----
xfun::download_file("http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip")
unzip("WFO_Backbone.zip", list = T)
unzip("WFO_Backbone.zip", file = "classification.txt")
pfo = read_delim("classification.txt", delim = "\t")
pfo2 = unique(select(filter(pfo, taxonomicStatus == "ACCEPTED", taxonRank == "SPECIES",
                            majorGroup %in% c("A", "G", "P")), # Angiosperms, Gymnosperms, Pteridophytes
                     genus, family, majorGroup))
file.remove("classification.txt", "WFO_Backbone.zip")
pfo2$genus[which(duplicated(pfo2$genus))] # genus with multiple family ...
filter(pfo2, genus == "Athyrium")
# use the family with higher frequency??
pfo3 = filter(pfo, genus %in% pfo2$genus[which(duplicated(pfo2$genus))],
              taxonomicStatus == "ACCEPTED", taxonRank == "SPECIES",
              majorGroup %in% c("A", "G", "P")) %>% # Angiosperms, Gymnosperms, Pteridophytes
  group_by(genus, family, majorGroup) %>% 
  tally()
# use the family with higher frequency!
pfo4 = arrange(pfo3, genus, desc(n)) %>% 
  group_by(genus) %>% 
  slice_max(order_by = n) %>% 
  select(-n)

pfo5 = filter(pfo2, !genus %in% pfo2$genus[which(duplicated(pfo2$genus))])

pfo_final = bind_rows(pfo5, pfo4) %>% 
  arrange(genus)
table(pfo_final$majorGroup)
any(duplicated(pfo_final$genus))

# check with existing data
x = filter(classifications, taxon == "plant")
setdiff(pfo_final$genus, x$genus) # 80
filter(pfo_final, genus %in% x$genus) %>% 
  left_join(x, by = "genus") %>% 
  filter(family.x != family.y) %>% View()
# keep the WFO version
setdiff(x$genus, pfo_final$genus)

pc1 = filter(x, !genus %in% pfo_final$genus) %>% 
  bind_rows(select(pfo_final, -majorGroup) %>% 
  mutate(taxon = "plant"))

classifications = filter(classifications, taxon != "plant") %>% 
  bind_rows(pc1)

classifications = add_row(classifications,
                          genus = c("Psalidodon", "Curculionichthys"),
                          family = c("Characidae", "Loricariidae"),
                          taxon = "fish")

# V.PhyloMaker2
xv = mutate(V.PhyloMaker2::tips.info.TPL, genus2 = str_extract(species, "^[^_]+"),
            same = genus == genus2) %>% filter(same) %>% 
  as_tibble() %>% 
  select(group, genus, family) %>% 
  distinct()

xc = filter(classifications, taxon == "plant")

# v phylomaker only
xv2 = filter(xv, genus %in% setdiff(xv$genus, xc$genus)) %>% 
  select(-group) %>% 
  mutate(taxon = "plant")

# some genus in V.PhyloMaker2 have diff family as the World Flora online,
# I keep the World Flora Online version here

classifications = bind_rows(classifications, xv2) 

group_by(classifications, taxon) %>% 
  summarise(dup = any(duplicated(genus)))

xa = filter(classifications, taxon == "amphibian")
filter(xa, duplicated(genus))
filter(xa, genus == "Ingerana")
classifications = filter(classifications, !(genus == "Ingerana" & family == "Ceratobatrachidae"))
                          
classifications = arrange(classifications, taxon, genus) %>% 
  distinct()
usethis::use_data(classifications, overwrite = T, compress = "xz")
