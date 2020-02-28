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
source("R/tagfilters_textiles.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")
source("R/tagfilters_things.R")

# corpus 1734

avis_1734 <- readtext("data/groundtruth1734.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1734$text <- correct_ocr(avis_1734$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1734_all <- corpus(avis_1734,
                          docid_field = "doc_id")
corpus_1734 <- corpus_subset(corpus_1734_all,
                             (docvars(corpus_1734_all,"id") %in%
                                ids_by_lang$de))

# corpus 1834

avis_1834 <- readtext("data/groundtruth1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                          docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                             (docvars(corpus_1834_all,"id") %in%
                                ids_by_lang$de))

corpus_all <- c(corpus_1734, corpus_1834)

# negating %in% operator
`%notin%` <- Negate(`%in%`)

### checking and cleaning different tagfilters for misc things

## Carriages
carriage <- tagfilter_carriage()

carriage_ids_1734 <- carriage$filtrate(corpus_1734, ignore.case = T)
carriage_ids_1834 <- carriage$filtrate(corpus_1834, ignore.case = T)

carriage_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   carriage_ids)

carriage_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1734, "id") %in%
                                        carriage_ids)

carriage_subset_all <- c(carriage_subset_1734, carriage_subset_1834)
carriage_ids_all <- c(carriage_ids_1734, carriage_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carriage_kwic <- kwic(carriage_subset_all,
                      pattern = "Wagen",
                      valuetype = "regex",
                      ignore.case = T)
carriage_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
carriage_subset_all_clean <- carriage_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(carriage_subset_all_clean),
                   max_words = 200)


## Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_carriage_1734 <- validate_filter(corpus_1734, carriage_ids_1734,
                                       search_col = "adcontent",
                                       pattern = "06ding")
validation_carriage_1734

# Filters for TRUE and FALSE positives
doc_ids <- corpus_1734$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% carriage_ids_1734)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% carriage_ids_1734)]

#TRUE positives
b_t <- corpus_subset(corpus_1734,
                     docvars(corpus_1734,"id") %in%
                       validation_carriage_1734$filter_T_hc_T)
b_t$documents$texts[1:29]

#FALSE positives
b_f <- corpus_subset(corpus_1734,
                     docvars(corpus_1734,"id") %in%
                       validation_carriage_1734$filter_T_hc_F)
b_f$documents$texts[1:2]

# No FALSE positives and TRUE positives all correct!!!





## TEMPLATE
XXX <- tagfilter_XXX()

XXX_ids_1734 <- XXX$filtrate(corpus_1734, ignore.case = T)
XXX_ids_1834 <- XXX$filtrate(corpus_1834, ignore.case = T)

XXX_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        XXX_ids_1734)

XXX_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        XXX_ids_1834)

XXX_subset_all <- c(XXX_subset_1734, XXX_subset_1834)
XXX_ids_all <- c(XXX_ids_1734, XXX_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
XXX_kwic <- kwic(XXX_subset_all,
                      pattern = "Wagen",
                      valuetype = "regex",
                      ignore.case = T)
XXX_kwic

## Validation
validation_XXX_all <- validate_filter(corpus_all, XXX_ids_all,
                                            search_col = "adcontent",
                                            pattern = "06ding")
validation_XXX_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% XXX_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% XXX_ids_all)]

#TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_XXX_all$filter_T_hc_T)
b_t$documents$texts[1:10]

#FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_XXX_all$filter_T_hc_F)
b_f$documents$texts[1:10]
