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
                                   carriage_ids_1734)

carriage_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        carriage_ids_1834)

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
validation_carriage <- validate_filter(corpus_all, carriage_ids_all,
                                       search_col = "adcontent",
                                       pattern = "06ding")
validation_carriage

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

## Pushchairs
pushchair <- tagfilter_pushchair()

pushchair_ids_1734 <- pushchair$filtrate(corpus_1734, ignore.case = T)
pushchair_ids_1834 <- pushchair$filtrate(corpus_1834, ignore.case = T)

pushchair_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                         pushchair_ids_1734)

pushchair_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                         pushchair_ids_1834)

pushchair_subset_all <- c(pushchair_subset_1734, pushchair_subset_1834)
pushchair_ids_all <- c(pushchair_ids_1734, pushchair_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
pushchair_kwic <- kwic(pushchair_subset_all,
                       pattern = "Korbw(a|ä|ae)g|Kinderw(a|ä|ae)g|Kinder(chais|schäs)|Korb-W(ag|äg|ae)|Kinder-W(a|ä|ae)g|Kinder-(Chais|Schäs)",
                       valuetype = "regex",
                       ignore.case = T)
pushchair_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
pushchair_subset_clean <- pushchair_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(pushchair_subset_clean),
                   max_words = 100)

## Validation
validation_pushchair_all <- validate_filter(corpus_all, pushchair_ids_all,
                                            search_col = "adcontent",
                                            pattern = "02hausrat")
validation_pushchair_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% pushchair_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% pushchair_ids_all)]

#TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_pushchair_all$filter_T_hc_T)
b_t$documents$texts[1:4]

#FALSE positives
b_f <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       validation_pushchair_all$filter_T_hc_F)
b_f$documents$texts[1:54]


##  Storage
storage <- tagfilter_storage()

storage_ids_1734 <- storage$filtrate(corpus_1734, ignore.case = T)
storage_ids_1834 <- storage$filtrate(corpus_1834, ignore.case = T)

storage_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       storage_ids_1734)

storage_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       storage_ids_1834)

storage_subset_all <- c(storage_subset_1734, storage_subset_1834)
storage_ids_all <- c(storage_ids_1734, storage_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
storage_kwic <- kwic(storage_subset_all,
                     pattern = "Trog",
                     valuetype = "regex",
                     ignore.case = T)
storage_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
storage_subset_clean <- storage_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(storage_subset_clean),
                   max_words = 100)

## Validation
validation_storage_all <- validate_filter(corpus_all, storage_ids_all,
                                          search_col = "adcontent",
                                          pattern = "ding")
validation_storage_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% storage_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% storage_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_storage_all$filter_T_hc_T)
b_t$documents$texts[1:152]

# FALSE positives: most are correct, but dictionary not suitable for lost and found (because often description how other object lost from a basket)
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_storage_all$filter_T_hc_F)
b_f$documents$texts[1:34]

## Building Components
building <- tagfilter_building()

building_ids_1734 <- building$filtrate(corpus_1734, ignore.case = T)
building_ids_1834 <- building$filtrate(corpus_1834, ignore.case = T)

building_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        building_ids_1734)

building_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        building_ids_1834)

building_subset_all <- c(building_subset_1734, building_subset_1834)
building_ids_all <- c(building_ids_1734, building_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
building_kwic <- kwic(building_subset_all,
                      pattern = "Fenster-",
                      valuetype = "regex",
                      ignore.case = T)
building_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
building_subset_clean <- building_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(building_subset_clean),
                   max_words = 100)

## Validation
validation_building_all <- validate_filter(corpus_all, building_ids_all,
                                           search_col = "adcontent",
                                           pattern = "hausrat")
validation_building_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% building_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% building_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_building_all$filter_T_hc_T)
b_t$documents$texts[1:76]

# FALSE positives: almost all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_building_all$filter_T_hc_F)
b_f$documents$texts[1:104]

## Suitcases and Travel Bags
suitcase <- tagfilter_suitcase()

suitcase_ids_1734 <- suitcase$filtrate(corpus_1734, ignore.case = T)
suitcase_ids_1834 <- suitcase$filtrate(corpus_1834, ignore.case = T)

suitcase_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        suitcase_ids_1734)

suitcase_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        suitcase_ids_1834)

suitcase_subset_all <- c(suitcase_subset_1734, suitcase_subset_1834)
suitcase_ids_all <- c(suitcase_ids_1734, suitcase_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
suitcase_kwic <- kwic(suitcase_subset_all,
                      pattern = "Wagen",
                      valuetype = "regex",
                      ignore.case = T)
suitcase_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
suitcase_subset_clean <- suitcase_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(suitcase_subset_clean),
                   max_words = 100)

## Validation
validation_suitcase_all <- validate_filter(corpus_all, suitcase_ids_all,
                                           search_col = "adcontent",
                                           pattern = "ding")
validation_suitcase_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% suitcase_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% suitcase_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_suitcase_all$filter_T_hc_T)
b_t$documents$texts[1:76]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_suitcase_all$filter_T_hc_F)
b_f$documents$texts[1:9]

## Measuring instruments
measure <- tagfilter_measure()

measure_ids_1734 <- measure$filtrate(corpus_1734, ignore.case = T)
measure_ids_1834 <- measure$filtrate(corpus_1834, ignore.case = T)

measure_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       measure_ids_1734)

measure_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       measure_ids_1834)

measure_subset_all <- c(measure_subset_1734, measure_subset_1834)
measure_ids_all <- c(measure_ids_1734, measure_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
measure_kwic <- kwic(measure_subset_all,
                     pattern = "Wagen",
                     valuetype = "regex",
                     ignore.case = T)
measure_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
measure_subset_clean <- measure_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(measure_subset_clean),
                   max_words = 100)

## Validation
validation_measure_all <- validate_filter(corpus_all, measure_ids_all,
                                          search_col = "adcontent",
                                          pattern = "02hausrat")
validation_measure_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% measure_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% measure_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_measure_all$filter_T_hc_T)
b_t$documents$texts[1:6]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_measure_all$filter_T_hc_F)
b_f$documents$texts[1:8]





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
                      pattern = "YYY",
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
