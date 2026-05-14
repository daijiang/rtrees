# profile.R — identify hot spots in get_one_tree / bind_tip
# Run from repo root: Rscript profile.R
suppressPackageStartupMessages({
  library(ape)
  library(devtools)
})
devtools::load_all(".", quiet = TRUE)

load("debug_data.RData")
sp_debug <- rtrees::sp_list_df(true_tree$tip.label, taxon = "plant")
plant_tree <- megatrees::tree_plant_otl
fish_tree  <- megatrees::tree_fish_12k

sp_fish <- tibble::tibble(
  species = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii",
               "Periophthalmus_barbarus", "Prognichthys_glaphyrae", "Barathronus_bicolor",
               "Knipowitschia_croatica", "Rhamphochromis_lucius", "Neolissochilus_tweediei",
               "Haplochromis_nyanzae", "Astronesthes_micropogon", "Sanopus_reticulatus"),
  genus  = c("Serrasalmus", "Careproctus", "Gobiomorphus", "Periophthalmus",
              "Prognichthys", "Barathronus", "Knipowitschia", "Rhamphochromis",
              "Neolissochilus", "Haplochromis", "Astronesthes", "Sanopus"),
  family = c("Serrasalmidae", "Liparidae", "Eleotridae", "Gobiidae",
              "Exocoetidae", "Aphyonidae", "Gobiidae", "Cichlidae",
              "Cyprinidae", "Cichlidae", "Stomiidae", "Batrachoididae")
)

# ---------------------------------------------------------------------------
# Profile helper
# ---------------------------------------------------------------------------
run_prof <- function(label, expr, interval = 0.005) {
  f <- tempfile(fileext = ".out")
  Rprof(f, interval = interval, memory.profiling = TRUE)
  suppressMessages(force(expr))
  Rprof(NULL)
  sm <- summaryRprof(f, memory = "both")
  cat("\n===", label, "=== (by.total, top 20)\n")
  top <- head(sm$by.total, 20)
  print(top[, c("total.time", "total.pct", "mem.total")])
  cat("\n--- by.self, top 15 ---\n")
  top2 <- head(sm$by.self, 15)
  print(top2[, c("self.time", "self.pct")])
  invisible(sm)
}

# ---------------------------------------------------------------------------
# 1. Debug data: 130 spp grafted onto 304-tip user tree  (at_basal_node)
# ---------------------------------------------------------------------------
p1 <- run_prof("debug/plant 130sp at_basal_node",
  get_one_tree(sp_debug, tree = reduced_tree, taxon = "plant",
               scenario = "at_basal_node", tree_by_user = TRUE))

# ---------------------------------------------------------------------------
# 2. Fish 12sp on 12k-tip megatree  (at_basal_node)
# ---------------------------------------------------------------------------
p2 <- run_prof("fish 12sp at_basal_node",
  get_one_tree(sp_fish, tree = fish_tree, taxon = "fish",
               scenario = "at_basal_node"))

# ---------------------------------------------------------------------------
# 3. Fish 12sp on 12k-tip megatree  (random_below_basal)
# ---------------------------------------------------------------------------
p3 <- run_prof("fish 12sp random_below_basal", {
  set.seed(1)
  get_one_tree(sp_fish, tree = fish_tree, taxon = "fish",
               scenario = "random_below_basal")
})

# ---------------------------------------------------------------------------
# 4. Plant 12sp on full plant megatree  (at_basal_node) — megatree is ~75k tips
# ---------------------------------------------------------------------------
sp_plant <- rtrees::sp_list_df(
  c("Meliosma_laui", "Acer_cordatum", "Fraxinus_mandshurica", "Ormosia_pinnata",
    "Aglaia_dasyclada", "Claoxylon_indicum",
    "Carya_alba", "Carya_cordiformis", "Stewartia_sinensis",
    "Pterocarya_hupehensis", "Ulmus_davidiana", "Diplospora_dubia"),
  taxon = "plant"
)
p4 <- run_prof("plant 12sp on 75k-tip megatree at_basal_node",
  get_one_tree(sp_plant, tree = plant_tree, taxon = "plant",
               scenario = "at_basal_node"))

cat("\n=== DONE ===\n")
