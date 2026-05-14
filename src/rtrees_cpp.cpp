// rtrees_cpp.cpp — performance-critical helpers for rtrees
//
// bind_tip_core_cpp():
//   Replaces the multiple [.data.table passes inside bind_tip() with two
//   single C++ loops.  Handles the four grafting cases.
//
// graft_all_cpp():
//   Applies ALL N graft operations in a single C++ call, keeping the growing
//   tree in C++ std::vectors.  Eliminates N tibble-column extractions (the
//   [[.tbl_df overhead that dominated after the first round of optimisation)
//   and N result conversions back to R.  The result is one List of final
//   column vectors, converted to a tbl_tree once at the end.
//
// fast_internal_offspring_cpp():
//   BFS returning row-indices of all INTERNAL descendants of a given node.
//   Replaces the split()/unlist() BFS in fast_internal_offspring() for
//   random_below_basal sampling.

#include <Rcpp.h>
#include <string>
#include <unordered_map>
#include <vector>
#include <queue>
using namespace Rcpp;

// ---------------------------------------------------------------------------
// bind_tip_core_cpp
// ---------------------------------------------------------------------------
// Input vectors are the columns of the tree data.table (parent, node,
// branch.length, label, is_tip).  Returns a named list of five new vectors
// (same columns, extended by 1 or 2 rows) ready to be coerced back into a
// data.table/tibble without any R-level looping.
//
// sequential is always FALSE here (max_offspring = Ntip), which is safe
// because get_one_tree() rebuilds the phylo from the tibble at the end.

// [[Rcpp::export]]
List bind_tip_core_cpp(
    IntegerVector   parent,
    IntegerVector   node,
    NumericVector   branch_length,
    CharacterVector label,
    LogicalVector   is_tip,
    std::string     where,
    std::string     tip_label,
    std::string     node_label_str,
    double          frac,
    bool            new_node_above,
    double          node_height
) {
  const int n = parent.size();

  // --- locate target row ---
  int where_idx = -1;
  for (int i = 0; i < n; i++) {
    if ((std::string)label[i] == where) { where_idx = i; break; }
  }
  if (where_idx < 0) stop("'where' label not found in tree");

  const bool   w_is_tip  = (bool)is_tip[where_idx];
  const int    node_orig = node[where_idx];
  const int    par_orig  = parent[where_idx];
  const bool   at_root   = (par_orig == node_orig);
  const double bl_orig   = branch_length[where_idx];

  // Ntip = max_offspring (sequential = FALSE)
  int Ntip = 0;
  for (int i = 0; i < n; i++) if (is_tip[i]) Ntip++;

  // Determine output size
  const int n_extra = (w_is_tip || (new_node_above && !at_root)) ? 2 : 1;
  const int n_new   = n + n_extra;

  IntegerVector   r_parent(n_new);
  IntegerVector   r_node(n_new);
  NumericVector   r_bl(n_new);
  CharacterVector r_label(n_new);
  LogicalVector   r_is_tip(n_new, false);

  // -------------------------------------------------------------------------
  // Case 1: internal node, direct attach (NOT new_node_above, NOT at_root)
  //   all parents += 1
  //   internal nodes (> Ntip) += 1
  //   append: parent = node_orig+1, node = Ntip+1
  // -------------------------------------------------------------------------
  if (!w_is_tip && !at_root && !new_node_above) {
    for (int i = 0; i < n; i++) {
      r_parent[i] = parent[i] + 1;
      r_node[i]   = (node[i] > Ntip) ? node[i] + 1 : node[i];
      r_bl[i]     = branch_length[i];
      r_label[i]  = label[i];
      r_is_tip[i] = is_tip[i];
    }
    r_parent[n] = node_orig + 1;
    r_node[n]   = Ntip + 1;
    r_bl[n]     = node_height;
    r_label[n]  = tip_label;
    r_is_tip[n] = true;
  }

  // -------------------------------------------------------------------------
  // Case 2: root node
  //   all parents += 1, all nodes += 1
  //   new tip: node = 1 (lowest), parent = root_node+1
  // -------------------------------------------------------------------------
  else if (!w_is_tip && at_root) {
    for (int i = 0; i < n; i++) {
      r_parent[i] = parent[i] + 1;
      r_node[i]   = node[i] + 1;
      r_bl[i]     = branch_length[i];
      r_label[i]  = label[i];
      r_is_tip[i] = is_tip[i];
    }
    r_parent[n] = node_orig + 1;
    r_node[n]   = 1;
    r_bl[n]     = node_height;
    r_label[n]  = tip_label;
    r_is_tip[n] = true;
  }

  // -------------------------------------------------------------------------
  // Case 3: internal node, new_node_above = TRUE
  //   Pass 1: parent >= node_orig += 1; node >= node_orig += 1
  //   Insert new internal node at slot n (node = node_orig)
  //   Update where's parent = node_orig, bl = bl*(1-frac)
  //   Pass 2: all parents += 1; nodes > Ntip += 1
  //   Append tip: parent = node_orig+1, node = Ntip+1
  // -------------------------------------------------------------------------
  else if (!w_is_tip && new_node_above && !at_root) {
    // Pass 1
    for (int i = 0; i < n; i++) {
      r_parent[i] = (parent[i] >= node_orig) ? parent[i] + 1 : parent[i];
      r_node[i]   = (node[i] >= node_orig)   ? node[i] + 1   : node[i];
      r_bl[i]     = branch_length[i];
      r_label[i]  = label[i];
      r_is_tip[i] = is_tip[i];
    }
    // Override where's parent and bl
    r_parent[where_idx] = node_orig;
    r_bl[where_idx]     = bl_orig * (1.0 - frac);

    // New internal node
    r_parent[n] = par_orig;      // par_orig < node_orig, unchanged after pass 1
    r_node[n]   = node_orig;
    r_bl[n]     = bl_orig * frac;
    r_label[n]  = node_label_str;
    r_is_tip[n] = false;

    // Pass 2 over n+1 rows
    for (int i = 0; i < n + 1; i++) {
      r_parent[i]++;
      if (r_node[i] > Ntip) r_node[i]++;
    }

    // New tip
    r_parent[n + 1] = node_orig + 1;   // node_orig is internal (> Ntip), got +1 in pass2
    r_node[n + 1]   = Ntip + 1;
    r_bl[n + 1]     = bl_orig * (1.0 - frac) + node_height;
    r_label[n + 1]  = tip_label;
    r_is_tip[n + 1] = true;
  }

  // -------------------------------------------------------------------------
  // Case 4: target is a tip
  //   Pass 1: parent > par_orig += 1; node > par_orig += 1
  //   Override where's parent = par_orig+1, bl = bl*(1-frac)
  //   New internal node at slot n: parent=par_orig, node=par_orig+1
  //   Pass 2: all parents += 1; nodes > node_orig += 1
  //   Append tip: parent=par_orig+2, node=node_orig+1
  // -------------------------------------------------------------------------
  else {
    // Pass 1
    for (int i = 0; i < n; i++) {
      r_parent[i] = (parent[i] > par_orig) ? parent[i] + 1 : parent[i];
      r_node[i]   = (node[i] > par_orig)   ? node[i] + 1   : node[i];
      r_bl[i]     = branch_length[i];
      r_label[i]  = label[i];
      r_is_tip[i] = is_tip[i];
    }
    // Override where (tip): new parent = par_orig+1 (the new internal node)
    r_parent[where_idx] = par_orig + 1;
    r_bl[where_idx]     = bl_orig * (1.0 - frac);

    // New internal node
    r_parent[n] = par_orig;
    r_node[n]   = par_orig + 1;
    r_bl[n]     = bl_orig * frac;
    r_label[n]  = node_label_str;
    r_is_tip[n] = false;

    // Pass 2 over n+1 rows
    for (int i = 0; i < n + 1; i++) {
      r_parent[i]++;
      if (r_node[i] > node_orig) r_node[i]++;
    }

    // New tip
    r_parent[n + 1] = par_orig + 2;
    r_node[n + 1]   = node_orig + 1;
    r_bl[n + 1]     = bl_orig * (1.0 - frac);
    r_label[n + 1]  = tip_label;
    r_is_tip[n + 1] = true;
  }

  return List::create(
    Named("parent")        = r_parent,
    Named("node")          = r_node,
    Named("branch.length") = r_bl,
    Named("label")         = r_label,
    Named("is_tip")        = r_is_tip
  );
}

// ---------------------------------------------------------------------------
// graft_all_cpp
// ---------------------------------------------------------------------------
// Applies N graft operations (supplied as parallel vectors of length N) onto
// an initial tree (supplied as 5 column vectors).  All tree arithmetic stays
// in C++ std::vectors — no R allocations during the loop.
//
// new_node_labels[g] == "" signals that no new internal node is created
// (Cases 1 and 2 — the most common paths).
//
// Returns a named List of 5 column vectors for the final tree, ready to be
// wrapped in a tbl_tree once.

// [[Rcpp::export]]
List graft_all_cpp(
    IntegerVector   parent0,
    IntegerVector   node0,
    NumericVector   bl0,
    CharacterVector label0,
    LogicalVector   is_tip0,
    CharacterVector where_labels,
    CharacterVector new_tip_labels,
    CharacterVector new_node_labels,   // "" = no new internal node
    NumericVector   fracs,
    LogicalVector   above_flags,
    NumericVector   node_heights_vec
) {
  const int n0 = parent0.size();
  const int N  = where_labels.size();

  // --- initialise C++ containers from R input ---
  std::vector<int>    r_parent(parent0.begin(), parent0.end());
  std::vector<int>    r_node(node0.begin(), node0.end());
  std::vector<double> r_bl(bl0.begin(), bl0.end());
  std::vector<bool>   r_is_tip(is_tip0.begin(), is_tip0.end());
  std::vector<std::string> r_label(n0);
  for (int i = 0; i < n0; i++) r_label[i] = (std::string)label0[i];

  // label → row-index hash map (updated after each graft)
  std::unordered_map<std::string, int> label_idx;
  label_idx.reserve((n0 + 2 * N) * 2);
  for (int i = 0; i < n0; i++) label_idx[r_label[i]] = i;

  // Ntip tracked incrementally (starts at n_tips, +1 per graft)
  int Ntip = 0;
  for (int i = 0; i < n0; i++) if (r_is_tip[i]) Ntip++;

  r_parent.reserve(n0 + 2 * N);
  r_node  .reserve(n0 + 2 * N);
  r_bl    .reserve(n0 + 2 * N);
  r_label .reserve(n0 + 2 * N);
  r_is_tip.reserve(n0 + 2 * N);

  for (int g = 0; g < N; g++) {
    const std::string where    = (std::string)where_labels[g];
    const std::string tip_lbl  = (std::string)new_tip_labels[g];
    const std::string node_lbl = (std::string)new_node_labels[g];
    const double frac          = fracs[g];
    const bool   above         = (bool)above_flags[g];
    const double nh            = node_heights_vec[g];

    auto it = label_idx.find(where);
    if (it == label_idx.end()) stop("'where' label not found");
    const int    wi       = it->second;
    const bool   w_tip    = r_is_tip[wi];
    const int    n_orig   = r_node[wi];
    const int    p_orig   = r_parent[wi];
    const bool   at_root  = (p_orig == n_orig);
    const double bl_orig  = r_bl[wi];
    const int    cur_n    = (int)r_parent.size();

    // ------------------------------------------------------------------
    // Case 1: attach directly to existing internal node (most common)
    // ------------------------------------------------------------------
    if (!w_tip && !at_root && !above) {
      for (int i = 0; i < cur_n; i++) {
        r_parent[i]++;
        if (r_node[i] > Ntip) r_node[i]++;
      }
      r_parent.push_back(n_orig + 1);
      r_node  .push_back(Ntip + 1);
      r_bl    .push_back(nh);
      r_label .push_back(tip_lbl);
      r_is_tip.push_back(true);
      label_idx[tip_lbl] = cur_n;
    }
    // ------------------------------------------------------------------
    // Case 2: attach to root
    // ------------------------------------------------------------------
    else if (!w_tip && at_root) {
      for (int i = 0; i < cur_n; i++) {
        r_parent[i]++;
        r_node[i]++;
      }
      r_parent.push_back(n_orig + 1);
      r_node  .push_back(1);
      r_bl    .push_back(nh);
      r_label .push_back(tip_lbl);
      r_is_tip.push_back(true);
      label_idx[tip_lbl] = cur_n;
    }
    // ------------------------------------------------------------------
    // Case 3: insert new node above an internal node (new_node_above=TRUE)
    // ------------------------------------------------------------------
    else if (!w_tip && above && !at_root) {
      // Pass 1: shift parent/node >= n_orig
      for (int i = 0; i < cur_n; i++) {
        if (r_parent[i] >= n_orig) r_parent[i]++;
        if (r_node[i]   >= n_orig) r_node[i]++;
      }
      r_parent[wi] = n_orig;
      r_bl[wi]     = bl_orig * (1.0 - frac);

      r_parent.push_back(p_orig);
      r_node  .push_back(n_orig);
      r_bl    .push_back(bl_orig * frac);
      r_label .push_back(node_lbl);
      r_is_tip.push_back(false);
      label_idx[node_lbl] = cur_n;

      // Pass 2: all parents +1, internal nodes +1
      for (int i = 0; i < cur_n + 1; i++) {
        r_parent[i]++;
        if (r_node[i] > Ntip) r_node[i]++;
      }
      r_parent.push_back(n_orig + 1);
      r_node  .push_back(Ntip + 1);
      r_bl    .push_back(bl_orig * (1.0 - frac) + nh);
      r_label .push_back(tip_lbl);
      r_is_tip.push_back(true);
      label_idx[tip_lbl] = (int)r_parent.size() - 1;
    }
    // ------------------------------------------------------------------
    // Case 4: target is a tip
    // ------------------------------------------------------------------
    else {
      // Pass 1: shift parent/node > p_orig
      for (int i = 0; i < cur_n; i++) {
        if (r_parent[i] > p_orig) r_parent[i]++;
        if (r_node[i]   > p_orig) r_node[i]++;
      }
      r_parent[wi] = p_orig + 1;
      r_bl[wi]     = bl_orig * (1.0 - frac);

      r_parent.push_back(p_orig);
      r_node  .push_back(p_orig + 1);
      r_bl    .push_back(bl_orig * frac);
      r_label .push_back(node_lbl);
      r_is_tip.push_back(false);
      label_idx[node_lbl] = cur_n;

      // Pass 2: all parents +1, nodes > n_orig +1
      for (int i = 0; i < cur_n + 1; i++) {
        r_parent[i]++;
        if (r_node[i] > n_orig) r_node[i]++;
      }
      r_parent.push_back(p_orig + 2);
      r_node  .push_back(n_orig + 1);
      r_bl    .push_back(bl_orig * (1.0 - frac));
      r_label .push_back(tip_lbl);
      r_is_tip.push_back(true);
      label_idx[tip_lbl] = (int)r_parent.size() - 1;
    }

    Ntip++;
  }

  // --- convert back to Rcpp types (one allocation) ---
  const int nf = (int)r_parent.size();
  IntegerVector   op(r_parent.begin(), r_parent.end());
  IntegerVector   on(r_node.begin(),   r_node.end());
  NumericVector   ob(r_bl.begin(),     r_bl.end());
  LogicalVector   ot(r_is_tip.begin(), r_is_tip.end());
  CharacterVector ol(nf);
  for (int i = 0; i < nf; i++) ol[i] = r_label[i];

  return List::create(
    Named("parent")        = op,
    Named("node")          = on,
    Named("branch.length") = ob,
    Named("label")         = ol,
    Named("is_tip")        = ot
  );
}

// ---------------------------------------------------------------------------
// fast_internal_offspring_cpp
// ---------------------------------------------------------------------------
// BFS from root_node returning 1-based row indices of all INTERNAL descendants.
// Replaces the R split()/unlist() BFS used in random_below_basal.

// [[Rcpp::export]]
IntegerVector fast_internal_offspring_cpp(
    IntegerVector parent,
    IntegerVector node,
    LogicalVector is_tip,
    int           root_node
) {
  const int n = parent.size();

  // Build children table: node_number -> vector of row indices (0-based)
  std::unordered_map<int, std::vector<int>> children;
  children.reserve(n * 2);
  for (int i = 0; i < n; i++) {
    if (parent[i] != node[i]) {   // skip root self-reference
      children[parent[i]].push_back(i);
    }
  }

  std::vector<int> result;
  result.reserve(n / 2);

  std::queue<int> frontier;
  frontier.push(root_node);

  while (!frontier.empty()) {
    int cur = frontier.front();
    frontier.pop();

    auto it = children.find(cur);
    if (it == children.end()) continue;

    for (int idx : it->second) {
      if (!(bool)is_tip[idx]) {
        result.push_back(idx + 1);   // convert to 1-based for R
        frontier.push(node[idx]);
      }
    }
  }

  return wrap(result);
}
