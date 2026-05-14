context("Algorithmic invariants")

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

# ultrametric base: Rosa (3 spp) + Rubus (2 spp), all Rosaceae
inv_base <- ape::read.tree(text = paste0(
  "(((Rosa_setigera:3.4,Rosa_arkansana:3.4):2.0,",
  "Rosa_acicularis:5.4):5.0,",
  "(Rubus_odoratus:4.0,Rubus_parviflorus:4.0):6.4);"
))
inv_base$node.label <- paste0("N", seq_len(ape::Nnode(inv_base)))
inv_base_coph <- ape::cophenetic.phylo(inv_base)

# tiny base: Rosa (2 spp) + Meliosma (1 sp), Rosaceae + Sabiaceae
inv_tiny <- ape::read.tree(text = paste0(
  "((Rosa_setigera:3.4,Rosa_arkansana:3.4):5.0,",
  "Meliosma_oldenburgii:8.4);"
))
inv_tiny$node.label <- paste0("N", seq_len(ape::Nnode(inv_tiny)))

# Check that `tips` form a pure clade: their MRCA subtree contains no other tips.
is_pure_clade <- function(tree, tips) {
  if (length(tips) < 2L) return(NA)
  mrca <- ape::getMRCA(tree, tips)
  if (is.null(mrca)) return(FALSE)
  if (mrca == ape::Ntip(tree) + 1L) return(setequal(tree$tip.label, tips))
  setequal(ape::extract.clade(tree, node = mrca)$tip.label, tips)
}

# -----------------------------------------------------------------------
# 1. Non-disturbance: grafting new species must not alter pairwise distances
#    of species already present in the base tree.
# -----------------------------------------------------------------------

test_that("at_basal_node: existing species distances unchanged after grafting", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus", "Rubus_parviflorus",
                "Novus_sp1", "Novus_sp2"),
    genus   = c("Rosa", "Rosa", "Rubus", "Rubus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  D <- ape::cophenetic.phylo(tt)
  expect_equal(D["Rosa_setigera",  "Rosa_arkansana"],
               inv_base_coph["Rosa_setigera", "Rosa_arkansana"],    tolerance = 1e-8)
  expect_equal(D["Rubus_odoratus", "Rubus_parviflorus"],
               inv_base_coph["Rubus_odoratus", "Rubus_parviflorus"], tolerance = 1e-8)
})

test_that("random_below_basal: existing species distances unchanged after grafting", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus", "Rubus_parviflorus",
                "Novus_sp1", "Novus_sp2"),
    genus   = c("Rosa", "Rosa", "Rubus", "Rubus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
  set.seed(42)
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  ))
  D <- ape::cophenetic.phylo(tt)
  expect_equal(D["Rosa_setigera",  "Rosa_arkansana"],
               inv_base_coph["Rosa_setigera", "Rosa_arkansana"],    tolerance = 1e-8)
  expect_equal(D["Rubus_odoratus", "Rubus_parviflorus"],
               inv_base_coph["Rubus_odoratus", "Rubus_parviflorus"], tolerance = 1e-8)
})

# -----------------------------------------------------------------------
# 2. Determinism: at_basal_node must produce bit-identical output for the
#    same input (it is a deterministic algorithm with no random choices).
# -----------------------------------------------------------------------

test_that("at_basal_node: repeated calls with same input produce identical trees", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus",
                "Novus_sp1", "Novus_sp2", "Novus_sp3"),
    genus   = c("Rosa", "Rosa", "Rubus", "Novus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
  tt1 <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  tt2 <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  sps <- sort(tt1$tip.label)
  D1  <- ape::cophenetic.phylo(tt1)[sps, sps]
  D2  <- ape::cophenetic.phylo(tt2)[sps, sps]
  expect_equal(max(abs(D1 - D2)), 0, tolerance = 1e-8)
})

# -----------------------------------------------------------------------
# 3. Stranded genus clade purity: multiple species from a genus absent from
#    the base tree should form a pure clade with no interlopers.
# -----------------------------------------------------------------------

test_that("at_basal_node: stranded genus species form a pure clade", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus",
                "Novus_sp1", "Novus_sp2", "Novus_sp3"),
    genus   = c("Rosa", "Rosa", "Rubus", "Novus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  novus_tips <- grep("^Novus_", tt$tip.label, value = TRUE)
  expect_length(novus_tips, 3)
  expect_true(is_pure_clade(tt, novus_tips))
})

test_that("random_below_basal: stranded genus species form a pure clade", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus",
                "Novus_sp1", "Novus_sp2", "Novus_sp3"),
    genus   = c("Rosa", "Rosa", "Rubus", "Novus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
  set.seed(1)
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  ))
  novus_tips <- grep("^Novus_", tt$tip.label, value = TRUE)
  expect_length(novus_tips, 3)
  expect_true(is_pure_clade(tt, novus_tips))
})

# -----------------------------------------------------------------------
# 4. Star topology: stranded genus species (at_basal_node) all attach to
#    the same internal node, so their pairwise distances must all be equal.
# -----------------------------------------------------------------------

test_that("at_basal_node: stranded genus forms a star (all pairwise distances equal)", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Rubus_odoratus",
                "Novus_sp1", "Novus_sp2", "Novus_sp3"),
    genus   = c("Rosa", "Rosa", "Rubus", "Novus", "Novus", "Novus"),
    family  = rep("Rosaceae", 6)
  )
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  novus_tips <- grep("^Novus_", tt$tip.label, value = TRUE)
  D <- ape::cophenetic.phylo(ape::keep.tip(tt, novus_tips))
  dvals <- D[lower.tri(D)]
  expect_lt(diff(range(dvals)), 1e-6)
})

# -----------------------------------------------------------------------
# 5. n_spp == 1 family case: when the family has exactly one species in the
#    base tree, multiple new-genus species should still form a pure clade.
# -----------------------------------------------------------------------

test_that("at_basal_node: new genus in 1-species family forms a pure clade", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Meliosma_oldenburgii",
                "Sabia_sp1", "Sabia_sp2", "Sabia_sp3"),
    genus   = c("Rosa", "Rosa", "Meliosma", "Sabia", "Sabia", "Sabia"),
    family  = c("Rosaceae", "Rosaceae", "Sabiaceae",
                "Sabiaceae", "Sabiaceae", "Sabiaceae")
  )
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_tiny, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  sabia_tips <- grep("^Sabia_", tt$tip.label, value = TRUE)
  expect_length(sabia_tips, 3)
  expect_true(is_pure_clade(tt, sabia_tips))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})

test_that("random_below_basal: new genus in 1-species family forms a pure clade", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana", "Meliosma_oldenburgii",
                "Sabia_sp1", "Sabia_sp2", "Sabia_sp3"),
    genus   = c("Rosa", "Rosa", "Meliosma", "Sabia", "Sabia", "Sabia"),
    family  = c("Rosaceae", "Rosaceae", "Sabiaceae",
                "Sabiaceae", "Sabiaceae", "Sabiaceae")
  )
  set.seed(3)
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_tiny, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  ))
  sabia_tips <- grep("^Sabia_", tt$tip.label, value = TRUE)
  expect_length(sabia_tips, 3)
  expect_true(is_pure_clade(tt, sabia_tips))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})

# -----------------------------------------------------------------------
# 6. Two stranded genera from the same family must each form their own
#    separate pure clade (no cross-contamination between genera).
# -----------------------------------------------------------------------

test_that("at_basal_node: two stranded genera form independent pure clades", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana",
                "Novus_sp1", "Novus_sp2", "Novus_sp3",
                "Alter_sp1", "Alter_sp2"),
    genus   = c("Rosa", "Rosa", "Novus", "Novus", "Novus", "Alter", "Alter"),
    family  = rep("Rosaceae", 7)
  )
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "at_basal_node", tree_by_user = TRUE)
  ))
  novus_tips <- grep("^Novus_", tt$tip.label, value = TRUE)
  alter_tips <- grep("^Alter_", tt$tip.label, value = TRUE)
  expect_true(is_pure_clade(tt, novus_tips))
  expect_true(is_pure_clade(tt, alter_tips))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})

test_that("random_below_basal: two stranded genera form independent pure clades", {
  sp <- tibble::tibble(
    species = c("Rosa_setigera", "Rosa_arkansana",
                "Novus_sp1", "Novus_sp2", "Novus_sp3",
                "Alter_sp1", "Alter_sp2"),
    genus   = c("Rosa", "Rosa", "Novus", "Novus", "Novus", "Alter", "Alter"),
    family  = rep("Rosaceae", 7)
  )
  set.seed(7)
  tt <- suppressMessages(suppressWarnings(
    get_tree(sp, tree = inv_base, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  ))
  novus_tips <- grep("^Novus_", tt$tip.label, value = TRUE)
  alter_tips <- grep("^Alter_", tt$tip.label, value = TRUE)
  expect_true(is_pure_clade(tt, novus_tips))
  expect_true(is_pure_clade(tt, alter_tips))
  expect_true(ape::is.ultrametric(tt, tol = 1e-4))
})
