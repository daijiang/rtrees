library(testthat)
library(rtrees)

# plants
test_plant_list = bind_rows(
  # out of tree
  tibble(species = c("Meliosma_laui", "Acer_cordatum", "Fraxinus_mandshurica",
                     "Ormosia_pinnata", "Aglaia_dasyclada", "Claoxylon_indicum"),
         genus = c("Meliosma", "Acer", "Fraxinus", "Ormosia", "Aglaia", "Claoxylon"),
         family = c("Sabiaceae", "Sapindaceae", "Oleaceae", "Fabaceae", "Meliaceae", "Euphorbiaceae")),
  # in tree
  tibble(species = c("Carya_alba", "Carya_cordiformis", "Stewartia_sinensis", "Pterocarya_hupehensis",
                     "Ulmus_davidiana", "Diplospora_dubia"),
         genus = c("Carya", "Carya", "Stewartia", "Pterocarya", "Ulmus", "Diplospora"),
         family = c("Juglandaceae", "Juglandaceae", "Theaceae", "Juglandaceae", "Ulmaceae", "Rubiaceae"))
)

# fish
test_fish_list = tibble(
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




test_check("rtrees")
