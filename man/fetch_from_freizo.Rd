% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rawdata_import.R
\name{fetch_from_freizo}
\alias{fetch_from_freizo}
\title{Fetch Raw Data from FREIZO}
\usage{
fetch_from_freizo(
  AVIS_YEARS = 1729:1844,
  dest_path = "../avis-databuffer/raw_data_uncorrected",
  gt_years = c(1734, 1754, 1774, 1834)
)
}
\arguments{
\item{AVIS_YEARS}{numeric vector indicating selected years.}

\item{dest_path}{character path to write the data files to.}

\item{gt_years}{numeric vector indicating ground truth years, i.e., that were
manually inspected.}
}
\description{
The FREIZO archive contains raw, scanned data. This information does not account
for OCR errors. Most users will rather want to work with cleaned, collection data
which can be pulled from the avisdata GitHub repository.
}
