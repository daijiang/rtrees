# plants
test_plant_list = dplyr::bind_rows(
  # out of tree
  tibble::tibble(species = c("Meliosma_laui", "Acer_cordatum", "Fraxinus_mandshurica",
                             "Ormosia_pinnata", "Aglaia_dasyclada", "Claoxylon_indicum"),
                 genus = c("Meliosma", "Acer", "Fraxinus", "Ormosia", "Aglaia", "Claoxylon"),
                 family = c("Sabiaceae", "Sapindaceae", "Oleaceae", "Fabaceae", "Meliaceae", "Euphorbiaceae")),
  # in tree
  tibble::tibble(species = c("Carya_alba", "Carya_cordiformis", "Stewartia_sinensis", "Pterocarya_hupehensis",
                             "Ulmus_davidiana", "Diplospora_dubia"),
                 genus = c("Carya", "Carya", "Stewartia", "Pterocarya", "Ulmus", "Diplospora"),
                 family = c("Juglandaceae", "Juglandaceae", "Theaceae", "Juglandaceae", "Ulmaceae", "Rubiaceae"))
)

test_tree = ape::read.tree(text = "(((((((((Potentilla_intermedia:1.156097,Potentilla_gracilis:1.156097):9.169741,Potentilla_bipinnatifida:10.325839):10.466967,Potentilla_sterilis:20.792806):11.333216,Fragaria_virginiana:32.126022):6.026567,(((((Rosa_setigera:3.434279,Rosa_arkansana:3.434279):1.991106,Rosa_spinosissima:5.425386):0.993924,Rosa_acicularis:6.419309):6.730804,(Rosa_micrantha:0.188519,Rosa_canina:0.188519):12.961594):23.677485,(Agrimonia_gryposepala:4.730863,Agrimonia_parviflora:4.730863):32.096735):1.324992):6.47604,(Geum_urbanum:11.153303,Geum_rivale:11.153303):33.475326):1.449852,((((((((Rubus_semisetosus:3.10529,Rubus_glandicaulis:3.10529):0.957584,Rubus_steelei:4.062873):0.398394,Rubus_cuneifolius:4.461267):2.271628,Rubus_vagus:6.732895):0.023439,Rubus_superioris:6.756333):0.020104,(Rubus_multifer:4.287114,Rubus_elegantulus:4.287115):2.489323):0.025475,((Rubus_laciniatus:0.125186,Rubus_bifrons:0.125186):0.11903,Rubus_praecox:0.244216):6.557697):11.571849,(Rubus_parviflorus:14.85915,Rubus_odoratus:14.85915):3.514612):27.704718):16.915234,Filipendula_rubra:62.993714):13.354116,((((((((((Crataegus_dissona:0.081863,Crataegus_florifera:0.081863):0.49838,Crataegus_fulleriana:0.580243):1.434261,Crataegus_crus-galli:2.014503):0.177737,Crataegus_chrysocarpa:2.192239):0.051159,Crataegus_uniflora:2.243399):0.438351,Crataegus_persimilis:2.68175):4.697259,((Malus_toringo:6.472694,Malus_ioensis:6.472693):0.772012,Chaenomeles_speciosa:7.244705):0.134305):1.72489,Pyrus_communis:9.1039):0.313393,Amelanchier_spicata:9.417293):40.039557,((((((Prunus_pumila:6.189944,Prunus_nigra:6.189945):0.37487,Prunus_pensylvanica:6.564815):4.856241,(Prunus_padus:4.816999,Prunus_serotina:4.817):6.604057):29.162527,Sorbaria_sorbifolia:40.583584):2.924475,Rhodotypos_scandens:43.50806):2.718224,(Spiraea_X_bumalda:25.921582,Aruncus_dioicus:25.921583):20.304701):3.230567):26.89098);")

test_tree_sp = c("Rosa_sp", "Rubus_sp", "Amelanchier_sp", "Prunus_sp", "Sorbus_sp", "Potentilla_sp",
  "Potentilla_intermedia", "Potentilla_gracilis", "Potentilla_bipinnatifida", "Potentilla_sterilis", "Fragaria_virginiana", "Rosa_setigera", "Rosa_arkansana", "Rosa_spinosissima", "Rosa_acicularis", "Rosa_micrantha", "Rosa_canina", "Agrimonia_gryposepala", "Agrimonia_parviflora", "Geum_urbanum", "Geum_rivale", "Rubus_semisetosus", "Rubus_glandicaulis", "Rubus_steelei", "Rubus_cuneifolius", "Rubus_vagus", "Rubus_superioris", "Rubus_multifer", "Rubus_elegantulus", "Rubus_laciniatus", "Rubus_bifrons", "Rubus_praecox", "Rubus_parviflorus", "Rubus_odoratus", "Filipendula_rubra", "Crataegus_dissona", "Crataegus_florifera", "Crataegus_fulleriana", "Crataegus_crus-galli", "Crataegus_chrysocarpa", "Crataegus_uniflora", "Crataegus_persimilis", "Malus_toringo", "Malus_ioensis", "Chaenomeles_speciosa", "Pyrus_communis", "Amelanchier_spicata", "Prunus_pumila", "Prunus_nigra", "Prunus_pensylvanica", "Prunus_padus", "Prunus_serotina", "Sorbaria_sorbifolia", "Rhodotypos_scandens", "Spiraea_X_bumalda", "Aruncus_dioicus")

test_tree_sp_df = sp_list_df(test_tree_sp, "plant")
test_tree_sp_df$close_sp = NA
test_tree_sp_df$close_genus = NA
test_tree_sp_df$close_sp[1] = "Rosa_acicularis"
test_tree_sp_df$close_genus[4] = "Rosa"

setdiff(unique(sp_list_df(test_tree_sp)$genus), unique(sp_list_df(test_tree$tip.label)$genus))

sp_list_phylomatic = c("asteraceae/Achillea/Achillea_sp",
                       "berberidaceae/Achlys/Achlys_sp",
                       "ranunculaceae/Aconitum/Aconitum_sp",
                       "ranunculaceae/Actaea/Actaea_rubra",
                       "asteraceae/Adenocaulon/Adenocaulon_sp")

# fish
test_fish_list = tibble::tibble(
  species = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii", 
              "Periophthalmus_barbarus", "Prognichthys_glaphyrae", "Barathronus_bicolor", 
              "Knipowitschia_croatica", "Rhamphochromis_lucius", "Neolissochilus_tweediei", 
              "Haplochromis_nyanzae", "Astronesthes_micropogon", "Sanopus_reticulatus"),
  genus = c("Serrasalmus", "Careproctus", "Gobiomorphus", "Periophthalmus",
            "Prognichthys", "Barathronus", "Knipowitschia", "Rhamphochromis", 
            "Neolissochilus", "Haplochromis", "Astronesthes", "Sanopus"),
  family = c("Serrasalmidae", "Liparidae", "Eleotridae", "Gobiidae", 
             "Exocoetidae", "Aphyonidae", "Gobiidae", "Cichlidae", 
             "Cyprinidae", "Cichlidae", "Stomiidae", "Batrachoididae")
)

# bird
test_bird_list = c("Brachypteryx_major", "Asthenes_perijana", "Ciridops_anna", 
                   "Leiothlypis_ruficapilla", "Reinwardtoena_reinwardti",
                   "Garrulax_caerulatus", "Buteo_rufofuscus", "Sylvia_mystacea",
                   "Telophorus_viridis", "Trachyphonus_darnaudii") %>% 
  sp_list_df(taxon = "bird")

test_mammal_list = c("Necromys_temchuki", "Hipposideros_beatus", "Ateles_marginatus",
                     "Protemnodon_anak", "Mus_musculus", "Echymipera_sp", "Echymipera_davidi") %>% 
  sp_list_df(taxon = "mammal")

