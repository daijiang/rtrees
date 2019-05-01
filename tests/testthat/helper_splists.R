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

