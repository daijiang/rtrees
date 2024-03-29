% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sp_list_df}
\alias{sp_list_df}
\title{Convert a vector of species names to a data frame}
\usage{
sp_list_df(sp_list, taxon)
}
\arguments{
\item{sp_list}{A string vector or a data frame with at least one column named "species".}

\item{taxon}{The taxon group of this species list. If not specified, only species and
genus will be returned.}
}
\value{
A data frame with columns: species, genus, and family (if \code{taxon} is specified).
}
\description{
Convert a vector of species names to a data frame
}
\examples{
sp_list_df(sp_list = c("Serrasalmus_geryi", "Careproctus_reinhardti", "Gobiomorphus_coxii"),
           taxon = "fish")
}
