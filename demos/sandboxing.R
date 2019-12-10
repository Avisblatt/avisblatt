# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tagfilters_utils.R")
source("R/tagfilters_household.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")


labor <- tagfilter_labor()
labor_ids <- labor$filtrate(corpus_1834)

# Validate the quality of filters
# by checking filters vs. manual classification
id_hc <- corpus_1834$documents$id[grepl("arbeit",
                                        corpus_1834$documents$adcontent)]

# labor filter & human classification
lab_in_hc <- labor_ids[labor_ids %in% id_hc]
# labor filter not in human classification
lab_notin_hc <- labor_ids[!labor_ids %in% id_hc]

tt <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      lab_notin_hc)

# These are interesting as these might
# be some human misclassification,
# the other out of these 15 seem problems of
# the filter.
tt$documents$texts[1:5]

## Added a more general solution
o <- validate_filter(corpus_1834, labor_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")

o






