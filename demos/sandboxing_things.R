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

## Mercery and Non Textile Accessoires
mercery <- tagfilter_mercery()

mercery_ids_1734 <- mercery$filtrate(corpus_1734,ignore.case = T)
mercery_ids_1834 <- mercery$filtrate(corpus_1834,ignore.case = T)

mercery_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       mercery_ids_1734)
mercery_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       mercery_ids_1834)

mercery_subset_all <- c(mercery_subset_1734, mercery_subset_1834)
mercery_ids_all <- c(mercery_ids_1734, mercery_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mercery_kwic <- kwic(mercery_subset_all,
                     pattern = "Kn[o|ö]pf",
                     valuetype = "regex",
                     ignore.case = T)

mercery_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
mercery_subset_clean <- mercery_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mercery_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_mercery_all <- validate_filter(corpus_all, mercery_ids_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_mercery_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% mercery_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% mercery_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_mercery_all$filter_T_hc_T)
b_t$documents$texts[1:93]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_mercery_all$filter_T_hc_F)
b_f$documents$texts[1:49]



## Bags and Purses
bag <- tagfilter_bag()

bag_ids_1734 <- bag$filtrate(corpus_1734,ignore.case = T)
bag_ids_1834 <- bag$filtrate(corpus_1834,ignore.case = T)

bag_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   bag_ids_1734)
bag_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   bag_ids_1834)

bag_subset_all <- c(bag_subset_1734, bag_subset_1834)
bag_ids_all <- c(bag_ids_1734, bag_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bag_kwic <- kwic(bag_subset_all,
                 pattern = "Tasche|Seckel|Beutel|Säck|Ridicule",
                 valuetype = "regex",
                 ignore.case = T)

bag_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
bag_subset_clean <- bag_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bag_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_bag_all <- validate_filter(corpus_all, bag_ids_all,
                                      search_col = "adcontent",
                                      pattern = "01textilien")
validation_bag_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bag_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bag_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bag_all$filter_T_hc_T)
b_t$documents$texts[1:117]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bag_all$filter_T_hc_F)
b_f$documents$texts[1:113]


## Animal Raw Materials
animalraw <- tagfilter_animalraw()

animalraw_ids_1734 <- animalraw$filtrate(corpus_1734,ignore.case = T)
animalraw_ids_1834 <- animalraw$filtrate(corpus_1834,ignore.case = T)

animalraw_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                         animalraw_ids_1734)
animalraw_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                         animalraw_ids_1834)

animalraw_subset_all <- c(animalraw_subset_1734, animalraw_subset_1834)
animalraw_ids_all <- c(animalraw_ids_1734, animalraw_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
animalraw_kwic <- kwic(animalraw_subset_all,
                       pattern = "Pferdhaar",
                       valuetype = "regex",
                       ignore.case = T)

animalraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
animalraw_subset_clean <- animalraw_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(animalraw_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_animalraw_all <- validate_filter(corpus_all, animalraw_ids_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_animalraw_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% animalraw_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% animalraw_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_animalraw_all$filter_T_hc_T)
b_t$documents$texts[1:51]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_animalraw_all$filter_T_hc_F)
b_f$documents$texts[1:5]


## Plant Raw Materials
plantraw <- tagfilter_plantraw()

plantraw_ids_1734 <- plantraw$filtrate(corpus_1734,ignore.case = T)
plantraw_ids_1834 <- plantraw$filtrate(corpus_1834,ignore.case = T)

plantraw_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        plantraw_ids_1734)
plantraw_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        plantraw_ids_1834)

plantraw_subset_all <- c(plantraw_subset_1734, plantraw_subset_1834)
plantraw_ids_all <- c(plantraw_ids_1734, plantraw_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
plantraw_kwic <- kwic(plantraw_subset_all,
                      pattern = "Waldhaar",
                      valuetype = "regex",
                      ignore.case = T)

plantraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
plantraw_subset_clean <- plantraw_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(plantraw_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_plantraw_all <- validate_filter(corpus_all, plantraw_ids_all,
                                           search_col = "adcontent",
                                           pattern = "01textilien")
validation_plantraw_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% plantraw_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% plantraw_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_plantraw_all$filter_T_hc_T)
b_t$documents$texts[1:12]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_plantraw_all$filter_T_hc_F)
b_f$documents$texts[1:4]


## Umbrellas
umbrella <- tagfilter_umbrella()

umbrella_ids_1734 <- umbrella$filtrate(corpus_1734,ignore.case = T)
umbrella_ids_1834 <- umbrella$filtrate(corpus_1834,ignore.case = T)

umbrella_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        umbrella_ids_1734)
umbrella_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        umbrella_ids_1834)

umbrella_subset_all <- c(umbrella_subset_1734, umbrella_subset_1834)
umbrella_ids_all <- c(umbrella_ids_1734, umbrella_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
umbrella_kwic <- kwic(umbrella_subset_all,
                      pattern = "\\bSchirme",
                      valuetype = "regex",
                      ignore.case = T)
umbrella_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
umbrella_subset_clean <- umbrella_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(umbrella_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
# most FALSE positives are actually correct if counting umbrellas as textiles
validation_umbrella_all <- validate_filter(corpus_all, umbrella_ids_all,
                                           search_col = "adcontent",
                                           pattern = "01textilien")
validation_umbrella_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% umbrella_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% umbrella_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_umbrella_all$filter_T_hc_T)
b_t$documents$texts[1:43]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_umbrella_all$filter_T_hc_F)
b_f$documents$texts[1:121]


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
                      pattern = "(1|2|3|4)spännig|Schlitten|(T|D)ro(sch|tsch)ke|Chai(se|s)|Trosque|Wägen|Kutsch//b|Schwanenhäls|
                      Haußschlitten|Geschell|Berline//b|Kutschen-Kasich|Kutschenkasich",
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


## Trolleys
trolley <- tagfilter_trolley()

trolley_ids_1734 <- trolley$filtrate(corpus_1734, ignore.case = T)
trolley_ids_1834 <- trolley$filtrate(corpus_1834, ignore.case = T)

trolley_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   trolley_ids_1734)

trolley_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   trolley_ids_1834)

trolley_subset_all <- c(trolley_subset_1734, trolley_subset_1834)
trolley_ids_all <- c(trolley_ids_1734, trolley_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
trolley_kwic <- kwic(trolley_subset_all,
                 pattern = "Wagenkette|(Leit|Zug)seil|Fuhrwerk",
                 valuetype = "regex",
                 ignore.case = T)
trolley_kwic

## Validation
validation_trolley_all <- validate_filter(corpus_all, trolley_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_trolley_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% trolley_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% trolley_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_trolley_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_trolley_all$filter_T_hc_F)
b_f$documents$texts[1:10]



##  Health, Cosmetics and Drugstore Products
health <- tagfilter_health()

health_ids_1734 <- health$filtrate(corpus_1734, ignore.case = T)
health_ids_1834 <- health$filtrate(corpus_1834, ignore.case = T)

health_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   health_ids_1734)

health_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   health_ids_1834)

health_subset_all <- c(health_subset_1734, health_subset_1834)
health_ids_all <- c(health_ids_1734, health_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
health_kwic <- kwic(health_subset_all,
                 pattern = phrase("(K|C)(o|ö)(l|ll)ni(sch|sches) Wasser"),
                 valuetype = "regex",
                 ignore.case = T)
health_kwic

## Validation
validation_health_all <- validate_filter(corpus_all, health_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_health_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% health_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% health_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_health_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_health_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Weapons
weapon <- tagfilter_weapon()

weapon_ids_1734 <- weapon$filtrate(corpus_1734, ignore.case = T)
weapon_ids_1834 <- weapon$filtrate(corpus_1834, ignore.case = T)

weapon_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   weapon_ids_1734)

weapon_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   weapon_ids_1834)

weapon_subset_all <- c(weapon_subset_1734, weapon_subset_1834)
weapon_ids_all <- c(weapon_ids_1734, weapon_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
weapon_kwic <- kwic(weapon_subset_all,
                 pattern = "Scheide",
                 valuetype = "regex",
                 ignore.case = T)
weapon_kwic

## Validation
validation_weapon_all <- validate_filter(corpus_all, weapon_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_weapon_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% weapon_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% weapon_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_weapon_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_weapon_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Shop Equipment
shopequip <- tagfilter_shopequip()

shopequip_ids_1734 <- shopequip$filtrate(corpus_1734, ignore.case = T)
shopequip_ids_1834 <- shopequip$filtrate(corpus_1834, ignore.case = T)

shopequip_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   shopequip_ids_1734)

shopequip_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   shopequip_ids_1834)

shopequip_subset_all <- c(shopequip_subset_1734, shopequip_subset_1834)
shopequip_ids_all <- c(shopequip_ids_1734, shopequip_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shopequip_kwic <- kwic(shopequip_subset_all,
                 pattern = "Ladengerä(th|t)schaft",
                 valuetype = "regex",
                 ignore.case = T)
shopequip_kwic

## Validation
validation_shopequip_all <- validate_filter(corpus_all, shopequip_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_shopequip_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% shopequip_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% shopequip_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_shopequip_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_shopequip_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Tools and Instruments
tool <- tagfilter_tool()

tool_ids_1734 <- tool$filtrate(corpus_1734, ignore.case = T)
tool_ids_1834 <- tool$filtrate(corpus_1834, ignore.case = T)

tool_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   tool_ids_1734)

tool_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   tool_ids_1834)

tool_subset_all <- c(tool_subset_1734, tool_subset_1834)
tool_ids_all <- c(tool_ids_1734, tool_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tool_kwic <- kwic(tool_subset_all,
                 pattern = "Kupferdruckerpre(ss|ß)|Schraubst(o|ö)ck|M(ü|a)hlstein|Wendelbaum|F(u|ü)gbl(o|ö)ch|Drucktisch|
                 Brennhäu(s|ß)lein|Reibstein",
                 valuetype = "regex",
                 ignore.case = T)
tool_kwic

## Validation
validation_tool_all <- validate_filter(corpus_all, tool_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_tool_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tool_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tool_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tool_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tool_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Stationary and Paperware
stationary <- tagfilter_stationary()

stationary_ids_1734 <- stationary$filtrate(corpus_1734, ignore.case = T)
stationary_ids_1834 <- stationary$filtrate(corpus_1834, ignore.case = T)

stationary_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                    stationary_ids_1734)

stationary_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    stationary_ids_1834)

stationary_subset_all <- c(stationary_subset_1734, stationary_subset_1834)
stationary_ids_all <- c(stationary_ids_1734, stationary_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
stationary_kwic <- kwic(stationary_subset_all,
                  pattern = "Schreibzeug",
                  valuetype = "regex",
                  ignore.case = T)
stationary_kwic

## Validation
validation_stationary_all <- validate_filter(corpus_all, stationary_ids_all,
                                       search_col = "adcontent",
                                       pattern = "06ding")
validation_stationary_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% stationary_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% stationary_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_stationary_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_stationary_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Jewellery
jewellery <- tagfilter_jewellery()

jewellery_ids_1734 <- jewellery$filtrate(corpus_1734, ignore.case = T)
jewellery_ids_1834 <- jewellery$filtrate(corpus_1834, ignore.case = T)

jewellery_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   jewellery_ids_1734)

jewellery_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   jewellery_ids_1834)

jewellery_subset_all <- c(jewellery_subset_1734, jewellery_subset_1834)
jewellery_ids_all <- c(jewellery_ids_1734, jewellery_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
jewellery_kwic <- kwic(jewellery_subset_all,
                 pattern = "Bijouteri",
                 valuetype = "regex",
                 ignore.case = T)
jewellery_kwic

## Validation
validation_jewellery_all <- validate_filter(corpus_all, jewellery_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_jewellery_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% jewellery_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% jewellery_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_jewellery_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_jewellery_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Wood
wood <- tagfilter_wood()

wood_ids_1734 <- wood$filtrate(corpus_1734, ignore.case = T)
wood_ids_1834 <- wood$filtrate(corpus_1834, ignore.case = T)

wood_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   wood_ids_1734)

wood_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   wood_ids_1834)

wood_subset_all <- c(wood_subset_1734, wood_subset_1834)
wood_ids_all <- c(wood_ids_1734, wood_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wood_kwic <- kwic(wood_subset_all,
                 pattern = "S(a|ä)gsp(ä|a|äh|ah)n|Lohst(o|ö)ck|Drehsp(äh|ä)n",
                 valuetype = "regex",
                 ignore.case = T)
wood_kwic

## Validation
validation_wood_all <- validate_filter(corpus_all, wood_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_wood_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% wood_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% wood_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_wood_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_wood_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Barrels and Bottles
barrel <- tagfilter_barrel()

barrel_ids_1734 <- barrel$filtrate(corpus_1734, ignore.case = T)
barrel_ids_1834 <- barrel$filtrate(corpus_1834, ignore.case = T)

barrel_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   barrel_ids_1734)

barrel_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   barrel_ids_1834)

barrel_subset_all <- c(barrel_subset_1734, barrel_subset_1834)
barrel_ids_all <- c(barrel_ids_1734, barrel_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
barrel_kwic <- kwic(barrel_subset_all,
                 pattern = phrase("Stücklein Fa(s|ß|ss)"),
                 valuetype = "regex",
                 ignore.case = T)
barrel_kwic

## Validation
validation_barrel_all <- validate_filter(corpus_all, barrel_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_barrel_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% barrel_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% barrel_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_barrel_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_barrel_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Tobacco and Related Objects
tobacco <- tagfilter_tobacco()

tobacco_ids_1734 <- tobacco$filtrate(corpus_1734, ignore.case = T)
tobacco_ids_1834 <- tobacco$filtrate(corpus_1834, ignore.case = T)

tobacco_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   tobacco_ids_1734)

tobacco_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   tobacco_ids_1834)

tobacco_subset_all <- c(tobacco_subset_1734, tobacco_subset_1834)
tobacco_ids_all <- c(tobacco_ids_1734, tobacco_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tobacco_kwic <- kwic(tobacco_subset_all,
                 pattern = "Pfeife|Taba(k|ck|ks|cks)pfeife|Pfeifenkopf|Pfeifenraumer|Pfeifenrohr",
                 valuetype = "regex",
                 ignore.case = T)
tobacco_kwic

## Validation
validation_tobacco_all <- validate_filter(corpus_all, tobacco_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_tobacco_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tobacco_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tobacco_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tobacco_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tobacco_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Hay and Straw
hay <- tagfilter_hay()

hay_ids_1734 <- hay$filtrate(corpus_1734, ignore.case = T)
hay_ids_1834 <- hay$filtrate(corpus_1834, ignore.case = T)

hay_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   hay_ids_1734)

hay_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   hay_ids_1834)

hay_subset_all <- c(hay_subset_1734, hay_subset_1834)
hay_ids_all <- c(hay_ids_1734, hay_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
hay_kwic <- kwic(hay_subset_all,
                 pattern = "Stroh\\b",
                 valuetype = "regex",
                 ignore.case = T)
hay_kwic

## Validation
validation_hay_all <- validate_filter(corpus_all, hay_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_hay_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% hay_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% hay_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_hay_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_hay_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Unspecified Wooden Objects
woodobject <- tagfilter_woodobject()

woodobject_ids_1734 <- woodobject$filtrate(corpus_1734, ignore.case = T)
woodobject_ids_1834 <- woodobject$filtrate(corpus_1834, ignore.case = T)

woodobject_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   woodobject_ids_1734)

woodobject_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   woodobject_ids_1834)

woodobject_subset_all <- c(woodobject_subset_1734, woodobject_subset_1834)
woodobject_ids_all <- c(woodobject_ids_1734, woodobject_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
woodobject_kwic <- kwic(woodobject_subset_all,
                 pattern = "Holzdecke|H(ö|o)lzenwerk|H(ö|o)lzwerk",
                 valuetype = "regex",
                 ignore.case = T)
woodobject_kwic

## Validation
validation_woodobject_all <- validate_filter(corpus_all, woodobject_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_woodobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% woodobject_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% woodobject_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_woodobject_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_woodobject_all$filter_T_hc_F)
b_f$documents$texts[1:10]


## Dung
dung <- tagfilter_dung()

dung_ids_1734 <- dung$filtrate(corpus_1734, ignore.case = T)
dung_ids_1834 <- dung$filtrate(corpus_1834, ignore.case = T)

dung_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   dung_ids_1734)

dung_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   dung_ids_1834)

dung_subset_all <- c(dung_subset_1734, dung_subset_1834)
dung_ids_all <- c(dung_ids_1734, dung_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
dung_kwic <- kwic(dung_subset_all,
                 pattern = "Bau\\b|K(ü|u)hbau|Pfer(d|de)bau|Schwei(n|ne)bau|Taubenmist|Ziegenbau",
                 valuetype = "regex",
                 ignore.case = T)
dung_kwic

## Validation
validation_dung_all <- validate_filter(corpus_all, dung_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_dung_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% dung_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% dung_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_dung_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_dung_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Plants
plant <- tagfilter_plant()

plant_ids_1734 <- plant$filtrate(corpus_1734, ignore.case = T)
plant_ids_1834 <- plant$filtrate(corpus_1834, ignore.case = T)

plant_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   plant_ids_1734)

plant_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   plant_ids_1834)

plant_subset_all <- c(plant_subset_1734, plant_subset_1834)
plant_ids_all <- c(plant_ids_1734, plant_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
plant_kwic <- kwic(plant_subset_all,
                 pattern = "Pflanze|Gewächs",
                 valuetype = "regex",
                 ignore.case = T)
plant_kwic

## Validation
validation_plant_all <- validate_filter(corpus_all, plant_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_plant_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% plant_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% plant_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_plant_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_plant_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Glasses and Optical Instruments
glasses <- tagfilter_glasses()

glasses_ids_1734 <- glasses$filtrate(corpus_1734, ignore.case = T)
glasses_ids_1834 <- glasses$filtrate(corpus_1834, ignore.case = T)

glasses_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   glasses_ids_1734)

glasses_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   glasses_ids_1834)

glasses_subset_all <- c(glasses_subset_1734, glasses_subset_1834)
glasses_ids_all <- c(glasses_ids_1734, glasses_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
glasses_kwic <- kwic(glasses_subset_all,
                 pattern = phrase("optische Instrumente"),
                 valuetype = "regex",
                 ignore.case = T)
glasses_kwic

## Validation
validation_glasses_all <- validate_filter(corpus_all, glasses_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_glasses_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% glasses_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% glasses_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_glasses_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_glasses_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Soil, Gravel, Lime and Related Goods
soil <- tagfilter_soil()

soil_ids_1734 <- soil$filtrate(corpus_1734, ignore.case = T)
soil_ids_1834 <- soil$filtrate(corpus_1834, ignore.case = T)

soil_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   soil_ids_1734)

soil_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   soil_ids_1834)

soil_subset_all <- c(soil_subset_1734, soil_subset_1834)
soil_ids_all <- c(soil_ids_1734, soil_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
soil_kwic <- kwic(soil_subset_all,
                 pattern = phrase("G(y|i)ps"),
                 valuetype = "regex",
                 ignore.case = T)
soil_kwic

## Validation
validation_soil_all <- validate_filter(corpus_all, soil_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_soil_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% soil_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% soil_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_soil_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_soil_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Agriculture
agriculture <- tagfilter_agriculture()

agriculture_ids_1734 <- agriculture$filtrate(corpus_1734, ignore.case = T)
agriculture_ids_1834 <- agriculture$filtrate(corpus_1834, ignore.case = T)

agriculture_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   agriculture_ids_1734)

agriculture_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   agriculture_ids_1834)

agriculture_subset_all <- c(agriculture_subset_1734, agriculture_subset_1834)
agriculture_ids_all <- c(agriculture_ids_1734, agriculture_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
agriculture_kwic <- kwic(agriculture_subset_all,
                 pattern = "Dreschflegel|(Heu|Lad|Lade)gabel|Pfl(u|ü)g|Sense|Rechen|Mattenmesser|Matten-Messer|
                 Güllenkarren|Heuwagen|Obsthurte|Obstk(o|ö)rb|Rebsteck|Mastbütte|Bienenst(o|ö)ck",
                 valuetype = "regex",
                 ignore.case = T)
agriculture_kwic

## Validation
validation_agriculture_all <- validate_filter(corpus_all, agriculture_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_agriculture_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% agriculture_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% agriculture_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_agriculture_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_agriculture_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Riding Objects
riding <- tagfilter_riding()

riding_ids_1734 <- riding$filtrate(corpus_1734, ignore.case = T)
riding_ids_1834 <- riding$filtrate(corpus_1834, ignore.case = T)

riding_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   riding_ids_1734)

riding_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   riding_ids_1834)

riding_subset_all <- c(riding_subset_1734, riding_subset_1834)
riding_ids_all <- c(riding_ids_1734, riding_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
riding_kwic <- kwic(riding_subset_all,
                 pattern = "Pfer(ded|dd)ecke",
                 valuetype = "regex",
                 ignore.case = T)
riding_kwic

## Validation
validation_riding_all <- validate_filter(corpus_all, riding_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_riding_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% riding_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% riding_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_riding_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_riding_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Objects Related to Wells and Fountains
well <- tagfilter_well()

well_ids_1734 <- well$filtrate(corpus_1734, ignore.case = T)
well_ids_1834 <- well$filtrate(corpus_1834, ignore.case = T)

well_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   well_ids_1734)

well_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   well_ids_1834)

well_subset_all <- c(well_subset_1734, well_subset_1834)
well_ids_all <- c(well_ids_1734, well_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
well_kwic <- kwic(well_subset_all,
                 pattern = "Wasserstein|Wasser-Stein",
                 valuetype = "regex",
                 ignore.case = T)
well_kwic

## Validation
validation_well_all <- validate_filter(corpus_all, well_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_well_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% well_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% well_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_well_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_well_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Naturalia
naturalia <- tagfilter_naturalia()

naturalia_ids_1734 <- naturalia$filtrate(corpus_1734, ignore.case = T)
naturalia_ids_1834 <- naturalia$filtrate(corpus_1834, ignore.case = T)

naturalia_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   naturalia_ids_1734)

naturalia_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   naturalia_ids_1834)

naturalia_subset_all <- c(naturalia_subset_1734, naturalia_subset_1834)
naturalia_ids_all <- c(naturalia_ids_1734, naturalia_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
naturalia_kwic <- kwic(naturalia_subset_all,
                 pattern = "Muscheln|Schmetterling|Versteinerung|Mineralien",
                 valuetype = "regex",
                 ignore.case = T)
naturalia_kwic

## Validation
validation_naturalia_all <- validate_filter(corpus_all, naturalia_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_naturalia_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% naturalia_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% naturalia_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_naturalia_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_naturalia_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Containers
container <- tagfilter_container()

container_ids_1734 <- container$filtrate(corpus_1734, ignore.case = T)
container_ids_1834 <- container$filtrate(corpus_1834, ignore.case = T)

container_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   container_ids_1734)

container_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   container_ids_1834)

container_subset_all <- c(container_subset_1734, container_subset_1834)
container_ids_all <- c(container_ids_1734, container_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
container_kwic <- kwic(container_subset_all,
                 pattern = "Z(u|ü)ber|Eimer|Trog|Bö(ck|k)lin|Bo(ck|k)te|Bu(ck|k)te",
                 valuetype = "regex",
                 ignore.case = T)
container_kwic

## Validation
validation_container_all <- validate_filter(corpus_all, container_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_container_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% container_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% container_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_container_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_container_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Firestarters
firestart <- tagfilter_firestart()

firestart_ids_1734 <- firestart$filtrate(corpus_1734, ignore.case = T)
firestart_ids_1834 <- firestart$filtrate(corpus_1834, ignore.case = T)

firestart_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   firestart_ids_1734)

firestart_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   firestart_ids_1834)

firestart_subset_all <- c(firestart_subset_1734, firestart_subset_1834)
firestart_ids_all <- c(firestart_ids_1734, firestart_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
firestart_kwic <- kwic(firestart_subset_all,
                 pattern = "Schwefelh(o|ö)lz|Zündh(o|ö)lz",
                 valuetype = "regex",
                 ignore.case = T)
firestart_kwic

## Validation
validation_firestart_all <- validate_filter(corpus_all, firestart_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_firestart_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% firestart_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% firestart_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_firestart_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_firestart_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Fire Extinguisher
extinguisher <- tagfilter_extinguisher()

extinguisher_ids_1734 <- extinguisher$filtrate(corpus_1734, ignore.case = T)
extinguisher_ids_1834 <- extinguisher$filtrate(corpus_1834, ignore.case = T)

extinguisher_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   extinguisher_ids_1734)

extinguisher_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   extinguisher_ids_1834)

extinguisher_subset_all <- c(extinguisher_subset_1734, extinguisher_subset_1834)
extinguisher_ids_all <- c(extinguisher_ids_1734, extinguisher_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
extinguisher_kwic <- kwic(extinguisher_subset_all,
                 pattern = phrase("Feuer Eimer"),
                 valuetype = "regex",
                 ignore.case = T)
extinguisher_kwic

## Validation
validation_extinguisher_all <- validate_filter(corpus_all, extinguisher_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_extinguisher_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% extinguisher_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% extinguisher_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_extinguisher_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_extinguisher_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Fireworks
firework <- tagfilter_firework()

firework_ids_1734 <- firework$filtrate(corpus_1734, ignore.case = T)
firework_ids_1834 <- firework$filtrate(corpus_1834, ignore.case = T)

firework_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   firework_ids_1734)

firework_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   firework_ids_1834)

firework_subset_all <- c(firework_subset_1734, firework_subset_1834)
firework_ids_all <- c(firework_ids_1734, firework_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
firework_kwic <- kwic(firework_subset_all,
                 pattern = "Feuerwerk",
                 valuetype = "regex",
                 ignore.case = T)
firework_kwic

## Validation
validation_firework_all <- validate_filter(corpus_all, firework_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_firework_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% firework_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% firework_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_firework_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_firework_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Antiques
antique <- tagfilter_antique()

antique_ids_1734 <- antique$filtrate(corpus_1734, ignore.case = T)
antique_ids_1834 <- antique$filtrate(corpus_1834, ignore.case = T)

antique_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   antique_ids_1734)

antique_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   antique_ids_1834)

antique_subset_all <- c(antique_subset_1734, antique_subset_1834)
antique_ids_all <- c(antique_ids_1734, antique_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
antique_kwic <- kwic(antique_subset_all,
                 pattern = phrase("römische Figur"),
                 valuetype = "regex",
                 ignore.case = T)
antique_kwic

## Validation
validation_antique_all <- validate_filter(corpus_all, antique_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_antique_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% antique_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% antique_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_antique_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_antique_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Keys
key <- tagfilter_key()

key_ids_1734 <- key$filtrate(corpus_1734, ignore.case = T)
key_ids_1834 <- key$filtrate(corpus_1834, ignore.case = T)

key_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   key_ids_1734)

key_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   key_ids_1834)

key_subset_all <- c(key_subset_1734, key_subset_1834)
key_ids_all <- c(key_ids_1734, key_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
key_kwic <- kwic(key_subset_all,
                 pattern = "Hausschlüssel|Schrankschlüssel|Uhrenschlüssel",
                 valuetype = "regex",
                 ignore.case = T)
key_kwic

## Validation
validation_key_all <- validate_filter(corpus_all, key_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_key_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% key_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% key_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_key_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_key_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Walking canes
cane <- tagfilter_cane()

cane_ids_1734 <- cane$filtrate(corpus_1734, ignore.case = T)
cane_ids_1834 <- cane$filtrate(corpus_1834, ignore.case = T)

cane_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   cane_ids_1734)

cane_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   cane_ids_1834)

cane_subset_all <- c(cane_subset_1734, cane_subset_1834)
cane_ids_all <- c(cane_ids_1734, cane_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cane_kwic <- kwic(cane_subset_all,
                 pattern = "Spazierst(o|ö)ck",
                 valuetype = "regex",
                 ignore.case = T)
cane_kwic

## Validation
validation_cane_all <- validate_filter(corpus_all, cane_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_cane_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% cane_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% cane_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cane_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cane_all$filter_T_hc_F)
b_f$documents$texts[1:10]



## Objects Related to Wine
wineobject <- tagfilter_wineobject()

wineobject_ids_1734 <- wineobject$filtrate(corpus_1734, ignore.case = T)
wineobject_ids_1834 <- wineobject$filtrate(corpus_1834, ignore.case = T)

wineobject_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   wineobject_ids_1734)

wineobject_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   wineobject_ids_1834)

wineobject_subset_all <- c(wineobject_subset_1734, wineobject_subset_1834)
wineobject_ids_all <- c(wineobject_ids_1734, wineobject_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wineobject_kwic <- kwic(wineobject_subset_all,
                 pattern = "Wei(nhah|nhäh|nha|nhä)nen|Wein-H(ah|a)nen|Wein-H(äh|ä)nen|Weinkrause|Weinschild",
                 valuetype = "regex",
                 ignore.case = T)
wineobject_kwic

## Validation
validation_wineobject_all <- validate_filter(corpus_all, wineobject_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_wineobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% wineobject_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% wineobject_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_wineobject_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_wineobject_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Ropes
rope <- tagfilter_rope()

rope_ids_1734 <- rope$filtrate(corpus_1734, ignore.case = T)
rope_ids_1834 <- rope$filtrate(corpus_1834, ignore.case = T)

rope_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   rope_ids_1734)

rope_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   rope_ids_1834)

rope_subset_all <- c(rope_subset_1734, rope_subset_1834)
rope_ids_all <- c(rope_ids_1734, rope_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
rope_kwic <- kwic(rope_subset_all,
                 pattern = "Strick|Seil",
                 valuetype = "regex",
                 ignore.case = T)
rope_kwic

## Validation
validation_rope_all <- validate_filter(corpus_all, rope_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_rope_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% rope_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% rope_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_rope_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_rope_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Objects for Taverns and Inns
tavernobject <- tagfilter_tavernobject()

tavernobject_ids_1734 <- tavernobject$filtrate(corpus_1734, ignore.case = T)
tavernobject_ids_1834 <- tavernobject$filtrate(corpus_1834, ignore.case = T)

tavernobject_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   tavernobject_ids_1734)

tavernobject_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   tavernobject_ids_1834)

tavernobject_subset_all <- c(tavernobject_subset_1734, tavernobject_subset_1834)
tavernobject_ids_all <- c(tavernobject_ids_1734, tavernobject_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tavernobject_kwic <- kwic(tavernobject_subset_all,
                 pattern = "Wir(th|t)sschild|Wir(t|th)schaftsgerät",
                 valuetype = "regex",
                 ignore.case = T)
tavernobject_kwic

## Validation
validation_tavernobject_all <- validate_filter(corpus_all, tavernobject_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_tavernobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tavernobject_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tavernobject_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tavernobject_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tavernobject_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Animal Feed
feed <- tagfilter_feed()

feed_ids_1734 <- feed$filtrate(corpus_1734, ignore.case = T)
feed_ids_1834 <- feed$filtrate(corpus_1834, ignore.case = T)

feed_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   feed_ids_1734)

feed_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   feed_ids_1834)

feed_subset_all <- c(feed_subset_1734, feed_subset_1834)
feed_ids_all <- c(feed_ids_1734, feed_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
feed_kwic <- kwic(feed_subset_all,
                 pattern = "Schwei(n|ns)erdäpfel|Schwei(n|ns)-Erdäpfel",
                 valuetype = "regex",
                 ignore.case = T)
feed_kwic

## Validation
validation_feed_all <- validate_filter(corpus_all, feed_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_feed_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% feed_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% feed_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_feed_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_feed_all$filter_T_hc_F)
b_f$documents$texts[1:10]

## Misc Objects
miscobject <- tagfilter_miscobject()

miscobject_ids_1734 <- miscobject$filtrate(corpus_1734, ignore.case = T)
miscobject_ids_1834 <- miscobject$filtrate(corpus_1834, ignore.case = T)

miscobject_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   miscobject_ids_1734)

miscobject_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   miscobject_ids_1834)

miscobject_subset_all <- c(miscobject_subset_1734, miscobject_subset_1834)
miscobject_ids_all <- c(miscobject_ids_1734, miscobject_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
miscobject_kwic <- kwic(miscobject_subset_all,
                 pattern = "Gegenstände",
                 valuetype = "regex",
                 ignore.case = T)
miscobject_kwic

## Validation
validation_miscobject_all <- validate_filter(corpus_all, miscobject_ids_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_miscobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% miscobject_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% miscobject_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_miscobject_all$filter_T_hc_T)
b_t$documents$texts[1:10]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_miscobject_all$filter_T_hc_F)
b_f$documents$texts[1:10]



### showing ids for documents (ads) NOT classified as things (ding) by automatic classification, but classified manually as "ding"


# creating a corpus of all the ads (ids) not found by automatic classification of ads for sale/ to buy

things_manual_1734 <- corpus_subset(corpus_1734, grepl("ding", adcontent))

things_manual_1834 <- corpus_subset(corpus_1834, grepl("ding", adcontent))

things_missed_1734 <- corpus_subset(things_manual_1734, docvars(things_manual_1734, "id") %notin%
                                   c(mercery_ids_all, bag_ids_all, animalraw_ids_all, plantraw_ids_all, umbrella_ids_all, carriage_ids_all,
                                     pushchair_ids_all, storage_ids_all, building_ids_all, suitcase_ids_all, measure_ids_all, trolley_ids_all,
                                     health_ids_all, weapon_ids_all, shopequip_ids_all, tool_ids_all, stationary_ids_all, jewellery_ids_all,
                                     wood_ids_all, barrel_ids_all, tobacco_ids_all, hay_ids_all, woodobject_ids_all, dung_ids_all, plant_ids_all,
                                     glasses_ids_all, soil_ids_all, agriculture_ids_all, riding_ids_all, well_ids_all, naturalia_ids_all, container_ids_all,
                                     firestart_ids_all, extinguisher_ids_all, firework_ids_all, antique_ids_all, key_ids_all, cane_ids_all, wineobject_ids_all,
                                     tavernobject_ids_all, rope_ids_all, feed_ids_all, miscobject_ids_all))

things_missed_1834 <- corpus_subset(things_manual_1834, docvars(things_manual_1834, "id") %notin%
                                      c(mercery_ids_all, bag_ids_all, animalraw_ids_all, plantraw_ids_all, umbrella_ids_all, carriage_ids_all,
                                        pushchair_ids_all, storage_ids_all, building_ids_all, suitcase_ids_all, measure_ids_all, trolley_ids_all,
                                        health_ids_all, weapon_ids_all, shopequip_ids_all, tool_ids_all, stationary_ids_all, jewellery_ids_all,
                                        wood_ids_all, barrel_ids_all, tobacco_ids_all, hay_ids_all, woodobject_ids_all, dung_ids_all, plant_ids_all,
                                        glasses_ids_all, soil_ids_all, agriculture_ids_all, riding_ids_all, well_ids_all, naturalia_ids_all, container_ids_all,
                                        firestart_ids_all, extinguisher_ids_all, firework_ids_all, antique_ids_all, key_ids_all, cane_ids_all, wineobject_ids_all,
                                        tavernobject_ids_all, rope_ids_all, feed_ids_all, miscobject_ids_all))

things_aut_1834 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                     c(mercery_ids_all, bag_ids_all, animalraw_ids_all, plantraw_ids_all, umbrella_ids_all, carriage_ids_all,
                                       pushchair_ids_all, storage_ids_all, building_ids_all, suitcase_ids_all, measure_ids_all, trolley_ids_all,
                                       health_ids_all, weapon_ids_all, shopequip_ids_all, tool_ids_all, stationary_ids_all, jewellery_ids_all,
                                       wood_ids_all, barrel_ids_all, tobacco_ids_all, hay_ids_all, woodobject_ids_all, dung_ids_all, plant_ids_all,
                                       glasses_ids_all, soil_ids_all, agriculture_ids_all, riding_ids_all, well_ids_all, naturalia_ids_all, container_ids_all,
                                       firestart_ids_all, extinguisher_ids_all, firework_ids_all, antique_ids_all, key_ids_all, cane_ids_all, wineobject_ids_all,
                                       tavernobject_ids_all, rope_ids_all, feed_ids_all, miscobject_ids_all))

things_aut_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     c(mercery_ids_all, bag_ids_all, animalraw_ids_all, plantraw_ids_all, umbrella_ids_all, carriage_ids_all,
                                       pushchair_ids_all, storage_ids_all, building_ids_all, suitcase_ids_all, measure_ids_all, trolley_ids_all,
                                       health_ids_all, weapon_ids_all, shopequip_ids_all, tool_ids_all, stationary_ids_all, jewellery_ids_all,
                                       wood_ids_all, barrel_ids_all, tobacco_ids_all, hay_ids_all, woodobject_ids_all, dung_ids_all, plant_ids_all,
                                       glasses_ids_all, soil_ids_all, agriculture_ids_all, riding_ids_all, well_ids_all, naturalia_ids_all, container_ids_all,
                                       firestart_ids_all, extinguisher_ids_all, firework_ids_all, antique_ids_all, key_ids_all, cane_ids_all, wineobject_ids_all,
                                       tavernobject_ids_all, rope_ids_all, feed_ids_all, miscobject_ids_all))

write.csv2(things_missed_1734, file = "data/things_missed_1734.csv", fileEncoding = "UTF-8")

