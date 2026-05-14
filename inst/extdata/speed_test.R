# need to also install the following packages
if(!require("xfun")) install.packages("xfun")
options(repos = c(
  rtrees = 'https://daijiang.r-universe.dev',
  CRAN = 'https://cloud.r-project.org')
)
xfun::pkg_attach2(c("phylocomr", "microbenchmark", "tidyverse"))
# I cannot use V.PhyloMaker::phylo.maker() and was forced to library it, 
#     which is a little bit annoying.
library(V.PhyloMaker2) 

d = read_delim("https://raw.githubusercontent.com/daijiang/rtrees_ms/refs/heads/main/Data/plants_phylomatic_names.txt", "/", col_names = F) %>%
  set_names(c("family", "genus", "species")) %>%
  mutate(genus = str_extract(genus, "^[^_]+"),
         species = rtrees:::cap_first_letter(species),
         genus = rtrees:::cap_first_letter(genus),
         family = rtrees:::cap_first_letter(family)) %>%
  select(species) %>% distinct()
n_distinct(d$species)

sp_missing = setdiff(d$species, megatrees::tree_plant_otl$tip.label)
sp_in = intersect(d$species, megatrees::tree_plant_otl$tip.label)

#' Function to test speed
#' 
#' @param n_sp_missing The number of missing species to be binded
#' @param n_sp_in The number of species that were already in the megatree
#' @param n_times How many times to compare? Default of 5 should be enough.
#' @return A data frame with the test methods, and their time used.
#' 
speed_test = function(n_sp_missing, n_sp_in = 500, n_times = 5, ...){
  test_sp = c(sample(sp_in, n_sp_in), sample(sp_missing, n_sp_missing))
  test_sp_df = rtrees::sp_list_df(sp_list = test_sp, taxon = "plant")
  test_sp_df = filter(test_sp_df, !is.na(family))
  test_sp_df2 = mutate(test_sp_df, phylom_sp = paste(family, genus, species, sep = "/"),
                       phylom_sp = casefold(phylom_sp))
  # compare speed
  xx2 = microbenchmark::microbenchmark(
    # Phylomatic = phylocomr::ph_phylomatic(taxa = test_sp_df2$phylom_sp, phylo = megatrees::tree_plant_otl),
    rtrees = get_tree(sp_list = test_sp_df, taxon = "plant"),
    # V.PhyloMaker = V.PhyloMaker::phylo.maker(sp.list = test_sp_df, scenarios = "S1"),
    V.PhyloMaker2 = V.PhyloMaker2::phylo.maker(sp.list = test_sp_df, scenarios = "S1"),
    times = n_times, ...
  )
  
  xx2 = as.data.frame(xx2) %>% 
    mutate(n_sp_in = n_sp_in, n_sp_missing = n_sp_missing)
  
  xx2
}

x = get_tree(sp_list = test_sp_df, taxon = "plant")
y = V.PhyloMaker2::phylo.maker(sp.list = test_sp_df, scenarios = "S1")
 par(mfrow = c(1,2))
 plot(x, type = "fan", show.tip.label = F, main = "rtrees")
 plot(ape::ladderize(y[[1]]), type = "fan", show.tip.label = F, main = "v.p")

# all tests have 500 species already in the megatree, then
# start with 50 missing species, and 100, 200, 500, 1000, 3000, 5000
speed_out2 = purrr::map_dfr(c(50, 100, 200, 500, 1000, 3000, 5000), speed_test)
speed_out3 = purrr::map_dfr(c(3000, 5000), speed_test, n_times = 3)
speed_out = bind_rows(speed_out2, speed_out3)
# for some reason, phylocomr::ph_phylomatic does not work (no mssin

speed_out = dplyr::mutate(speed_out, n_sp_missing_k = n_sp_missing / 1000, time_s = time/1e9)
saveRDS(speed_out, "inst/extdata/rtrees_speed_out.rds")

speed_lm = dplyr::group_by(speed_out, expr) |>
  dplyr::do(broom::tidy(lm(time_s ~ n_sp_missing_k, data = .))) |>
  dplyr::ungroup()

ggplot(speed_out, aes(x = n_sp_missing_k, y = time_s, color = expr, group = expr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Number of binding species (Thousand)",
       y = "Time (Second)", color = NULL, group = NULL) +
  scale_x_continuous(breaks = c(0.05, seq(0.5, 5, 0.5))) +
  scale_y_continuous(breaks = c(0, 10, seq(25, 200, 25))) +
  geom_text(x = 4, y = 30, label = "rtrees: y = 0.35+ 0.95x", inherit.aes = FALSE, color = "#F8766D") +
  geom_text(x = 1.8, y = 150, label = "V.PhyloMaker2: y = 12.3 + 36.8x", inherit.aes = FALSE, color = "#00BFC4") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none")  
