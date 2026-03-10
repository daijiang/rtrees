# Add genus and family basal/root node information to a phylogeny

Based on the classification of tips, find where is the basal and root
node for each genus and each family. Such information can be later used
to graft new tips onto the phylogeny. This function can be used to
process a user provided tree.

## Usage

``` r
add_root_info(
  tree,
  classification,
  process_all_tips = TRUE,
  genus_list = NULL,
  family_list = NULL,
  show_warning = FALSE
)
```

## Arguments

- tree:

  A phylogeny with class "phylo".

- classification:

  A data frame of 2 columns: genus, family. It should include all genus
  the tips of the tree belong to.

- process_all_tips:

  Whether to find basal nodes for all tips? Default is `TRUE`.

- genus_list:

  An optinoal subset list of genus to find root information.

- family_list:

  An optinoal subset list of family to find root information. This
  should be for species that do not have co-genus in the tree.

- show_warning:

  Whether to print warning information about non-monophyletic clades or
  not.

## Value

A phylogeny with basal nodes information attached.
