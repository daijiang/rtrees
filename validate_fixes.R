## Validation script for rtrees bug fixes (branch rtrees_opt)
## Run from the package root: source("validate_fixes.R")
## or: Rscript validate_fixes.R

library(rtrees)
library(ape)

cat("=== rtrees bug-fix validation ===\n\n")

pass = function(msg) cat("[PASS]", msg, "\n")
fail = function(msg) cat("[FAIL]", msg, "\n")

check = function(label, expr) {
  result = tryCatch(expr, error = function(e) {
    fail(paste(label, "-- ERROR:", conditionMessage(e))); return(invisible(FALSE))
  })
  if (isTRUE(result)) pass(label) else fail(label)
}

# -----------------------------------------------------------------------
# 1. Baseline: existing test suite
# -----------------------------------------------------------------------
cat("--- 1. Existing test suite (devtools::test) ---\n")
test_result = tryCatch(
  devtools::test(reporter = "minimal"),
  error = function(e) { fail("devtools::test() threw an error"); NULL }
)
if (!is.null(test_result)) {
  n_fail = sum(as.data.frame(test_result)$failed, na.rm = TRUE)
  if (n_fail == 0) pass("All existing tests pass") else fail(paste(n_fail, "existing tests failed"))
}

# -----------------------------------------------------------------------
# 2. at_basal_node: smoke tests for each major taxon
# -----------------------------------------------------------------------
cat("\n--- 2. at_basal_node smoke tests ---\n")

plant_sp = tibble::tibble(
  species = c("Carya_alba", "Carya_cordiformis", "Meliosma_laui",
              "Acer_cordatum", "Fraxinus_mandshurica", "Ormosia_pinnata"),
  genus   = c("Carya", "Carya", "Meliosma", "Acer", "Fraxinus", "Ormosia"),
  family  = c("Juglandaceae", "Juglandaceae", "Sabiaceae",
               "Sapindaceae", "Oleaceae", "Fabaceae")
)
tr_plant = suppressMessages(get_tree(plant_sp, taxon = "plant", scenario = "at_basal_node"))
check("plant tree is phylo",      inherits(tr_plant, "phylo"))
check("plant tip count correct",  ape::Ntip(tr_plant) == nrow(plant_sp))
check("plant no negative edges",  !any(tr_plant$edge.length < -1e-8))
check("graft_status returns tibble", inherits(get_graft_status(tr_plant), "data.frame"))

# -----------------------------------------------------------------------
# 3. random_below_basal scenario — exercises root_time / basal_time diff
# -----------------------------------------------------------------------
cat("\n--- 3. random_below_basal scenario ---\n")

set.seed(42)
tr_rbb = suppressMessages(
  get_tree(plant_sp, taxon = "plant", scenario = "random_below_basal")
)
check("random_below_basal returns phylo",     inherits(tr_rbb, "phylo"))
check("random_below_basal tip count correct", ape::Ntip(tr_rbb) == nrow(plant_sp))
check("random_below_basal no negative edges", !any(tr_rbb$edge.length < -1e-8))
plot(tr_rbb)

# -----------------------------------------------------------------------
# 4. Bug 2 regression: close_sp introduces a new genus (not in tree), then
#    a second congeneric is grafted under random_below_basal.
#    Pre-conditions fixed:
#    (a) line 186: isTRUE() guards against logical(0) crash when idx_row empty
#    (b) line 206: root_time uses node age not branch.length
#    (c) line 208: idx_row updated after add_row so n_spp increments correctly
# -----------------------------------------------------------------------
cat("\n--- 4. Bug 2 regression: close_sp new genus + random_below_basal ---\n")

rosaceae_tree = ape::read.tree(text = paste0(
  "(((Rosa_setigera:3.4,Rosa_arkansana:3.4):2.0,",
  "Rosa_acicularis:5.4):5.0,",
  "(Rubus_odoratus:4.0,Rubus_parviflorus:4.0):6.4);"
))
rosaceae_tree$node.label = paste0("N", seq_len(ape::Nnode(rosaceae_tree)))

# Novus genus is NOT in the tree; sp1 grafted via close_sp, then sp2 and sp3
# under random_below_basal. Before the fix, line 186 would crash (logical(0)
# crash), then line 206 would use branch.length instead of node age.
sp_bug2 = tibble::tibble(
  species    = c("Rosa_setigera", "Rosa_arkansana", "Rosa_acicularis",
                 "Rubus_odoratus", "Rubus_parviflorus",
                 "Novus_sp1", "Novus_sp2", "Novus_sp3"),
  genus      = c("Rosa", "Rosa", "Rosa", "Rubus", "Rubus",
                 "Novus", "Novus", "Novus"),
  family     = rep("Rosaceae", 8),
  close_sp   = c(NA, NA, NA, NA, NA, "Rosa_acicularis", NA, NA),
  close_genus = rep(NA_character_, 8)
)

set.seed(1)
tr_bug2 = tryCatch(
  suppressMessages(suppressWarnings(
    get_tree(sp_bug2, tree = rosaceae_tree, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  )),
  error = function(e) { fail(paste("Bug 2 test errored:", conditionMessage(e))); NULL }
)
if (!is.null(tr_bug2)) {
  check("Bug 2: returns phylo",           inherits(tr_bug2, "phylo"))
  check("Bug 2: tip count correct",       ape::Ntip(tr_bug2) == nrow(sp_bug2))
  check("Bug 2: no negative edges",       !any(tr_bug2$edge.length < -1e-8))
  check("Bug 2: no NaN/NA edge lengths",  !any(is.nan(tr_bug2$edge.length) | is.na(tr_bug2$edge.length)))
}
plot(tr_bug2)

# -----------------------------------------------------------------------
# 5. Bug 3 regression: family with 1 sp; graft new genus; 3rd sp of that
#    genus under random_below_basal hits root_node == basal_node (zero prob).
# -----------------------------------------------------------------------
cat("\n--- 5. Bug 3 regression: single-family-member + new genus + random_below_basal ---\n")

# Tiny tree: one family (Sabiaceae) has only 1 tip; Rosaceae has several.
tiny_tree = ape::read.tree(text = paste0(
  "((Rosa_setigera:3.4,Rosa_arkansana:3.4):5.0,",
  "Meliosma_oldenburgii:8.4);"
))
tiny_tree$node.label = paste0("N", seq_len(ape::Nnode(tiny_tree)))

# Graft two Sabiaceae species of a new genus (Sabia is not in tiny_tree)
sp_bug3 = tibble::tibble(
  species = c("Rosa_setigera", "Rosa_arkansana",
              "Meliosma_oldenburgii",
              "Sabia_sp1", "Sabia_sp2", "Sabia_sp3"),
  genus   = c("Rosa", "Rosa", "Meliosma", "Sabia", "Sabia", "Sabia"),
  family  = c("Rosaceae", "Rosaceae", "Sabiaceae",
               "Sabiaceae", "Sabiaceae", "Sabiaceae")
)

set.seed(2)
tr_bug3 = tryCatch(
  suppressMessages(suppressWarnings(
    get_tree(sp_bug3, tree = tiny_tree, taxon = "plant",
             scenario = "random_below_basal", tree_by_user = TRUE)
  )),
  error = function(e) { fail(paste("Bug 3 test errored:", conditionMessage(e))); NULL }
)
if (!is.null(tr_bug3)) {
  check("Bug 3: returns phylo",          inherits(tr_bug3, "phylo"))
  check("Bug 3: tip count correct",      ape::Ntip(tr_bug3) == nrow(sp_bug3))
  check("Bug 3: no negative edges",      !any(tr_bug3$edge.length < -1e-8))
  check("Bug 3: no NaN/NA edge lengths", !any(is.nan(tr_bug3$edge.length) | is.na(tr_bug3$edge.length)))
}
plot(tr_bug3)

# -----------------------------------------------------------------------
# 6. Bug 5 regression: ultrametric output check for a grafted tree
# -----------------------------------------------------------------------
cat("\n--- 6. Bug 5: ultrametric preservation ---\n")

check("plant at_basal_node is ultrametric",   ape::is.ultrametric(tr_plant, tol = 1e-4))
check("plant random_below_basal is ultrametric", ape::is.ultrametric(tr_rbb, tol = 1e-4))
if (!is.null(tr_bug2)) {
  check("Bug 2 tree is ultrametric",    ape::is.ultrametric(tr_bug2, tol = 1e-4))
  check("Bug 2 no negative edges",      !any(tr_bug2$edge.length < -1e-8))
}
if (!is.null(tr_bug3))
  check("Bug 3 tree is ultrametric", ape::is.ultrametric(tr_bug3, tol = 1e-4))

# -----------------------------------------------------------------------
# 7. graft_status completeness
# -----------------------------------------------------------------------
cat("\n--- 7. graft_status output ---\n")

gs = get_graft_status(tr_plant)
check("graft_status has tip_label column",  "tip_label" %in% names(gs))
check("graft_status has species column",    "species"   %in% names(gs))
check("graft_status has status column",     "status"    %in% names(gs))
check("graft_status row count matches tips", nrow(gs) >= ape::Ntip(tr_plant))
check("all species accounted for",
      all(plant_sp$species %in% gs$species))

# -----------------------------------------------------------------------
# 8. bind_tip directly
# -----------------------------------------------------------------------
cat("\n--- 8. bind_tip direct tests ---\n")

tr2 = bind_tip(tree = tr_plant, where = "Acer_cordatum", tip_label = "Test_sp")
check("bind_tip to tip: tip count +1",  ape::Ntip(tr2) == ape::Ntip(tr_plant) + 1)
check("bind_tip to tip: no neg edges",  !any(tr2$edge.length < -1e-8))
plot(tr2)

tr3 = bind_tip(tr_plant, where = tr_plant$node.label[1], tip_label = "Test_sp2")
check("bind_tip to node: tip count +1", ape::Ntip(tr3) == ape::Ntip(tr_plant) + 1)

tr4 = bind_tip(tr_plant, where = tr_plant$node.label[2],
               tip_label = "Test_sp3", new_node_above = TRUE)
check("bind_tip new_node_above: extra internal node",
      ape::Nnode(tr4) == ape::Nnode(tr3) + 1)

cat("\n=== Validation complete ===\n")

plot(tr3)
plot(ape::ladderize(tr3))
plot(tr4)
plot(ape::ladderize(tr4))
