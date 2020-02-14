# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
#added encoding parameter, as otherwise Umlaute get scrambled when loading external dictionaries
source("R/avis_stop.R", encoding = "UTF-8")
source("R/ocr_corrections.R", encoding = "UTF-8")
source("R/tagfilters_utils.R", encoding = "UTF-8")
source("R/tagfilters_household.R", encoding = "UTF-8")
source("R/tagfilters_main.R", encoding = "UTF-8")
source("R/cleaners.R")
source("R/validate_filters.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text", encoding = "UTF-8")
avis_1834$text <- correct_ocr(avis_1834$text)

#just ads in German
ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                      docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                                (docvars(corpus_1834_all,"id") %in%
                                   ids_by_lang$de))

print <- tagfilter_print()

print_ids <- print$filtrate(corpus_1834,ignore.case = F)
# Validation of Filters ----
## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither filter nor hc
o <- validate_filter(corpus_1834, print_ids,
                     search_col = "adcontent",
                     pattern = "drucksachen")
o




#---
#show difference between result of merged filters and merged results of (unmerged) filters
# approach here: differentiate labor and apprenticeships,
# use different dict$negs in both, join results;
# gives different results than using the merged filters
# (as filter merging merges both dict$neg into a single dict$neg)
tagfilter_labor_apprentice <- function(){
  dict <- list()
  dict$pos <- list(
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld"
  )
  #no dict$neg necessary, no (real) false positives
  create_filter_output(dict)
}



#TRUE positives
b_t <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       o$filter_T_hc_T)
b_t$documents$texts[21:30]

#- FALSE positives
b_f <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       o$filter_T_hc_F)
b_f$documents$texts[1:15]
