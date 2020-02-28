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
source("R/tagfilters_quality.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")

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


### checking and cleaning different tagfilters for household objects and descriptions of quality
## validations for single dictionaries are not very significant, because a lot of objects in these dictionaries are manually also classified as things...


## Bed
bed <- tagfilter_bed()

bed_ids_1734 <- bed$filtrate(corpus_1734,ignore.case = T)
bed_ids_1834 <- bed$filtrate(corpus_1834,ignore.case = T)

bed_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        bed_ids_1734)
bed_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        bed_ids_1834)

bed_subset_all <- c(bed_subset_1734, bed_subset_1834)
bed_ids_all <- c(bed_ids_1734, bed_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bed_kwic <- kwic(bed_subset_all,
                  pattern = "Wiege",
                  valuetype = "regex")

bed_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bed_subset_clean <- bed_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bed_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_bed_all <- validate_filter(corpus_all, bed_ids_all,
                                           search_col = "adcontent",
                                           pattern = "02hausrat")
validation_bed_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bed_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bed_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bed_all$filter_T_hc_T)
b_t$documents$texts[1:171]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bed_all$filter_T_hc_F)
b_f$documents$texts[1:39]


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


## Tablelinen
tablelinen <- tagfilter_tablelinen()

tablelinen_ids_1734 <- tablelinen$filtrate(corpus_1734,ignore.case = T)
tablelinen_ids_1834 <- tablelinen$filtrate(corpus_1834,ignore.case = T)

tablelinen_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                          tablelinen_ids_1734)
tablelinen_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                          tablelinen_ids_1834)

tablelinen_subset_all <- c(tablelinen_subset_1734, tablelinen_subset_1834)
tablelinen_ids_all <- c(tablelinen_ids_1734, tablelinen_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tablelinen_kwic <- kwic(tablelinen_subset_all,
                        pattern = "Tisch(zeug|deck)|(Tisch|Tafel)t(ü|u)ch|Serviette|Handt(ü|u)ch",
                        valuetype = "regex")

tablelinen_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
tablelinen_subset_clean <- tablelinen_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tablelinen_subset_clean),
                   max_words = 100)

## Validation
validation_tablelinen_all <- validate_filter(corpus_all, tablelinen_ids_all,
                                             search_col = "adcontent",
                                             pattern = "01textilien")
validation_tablelinen_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% tablelinen_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% tablelinen_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tablelinen_all$filter_T_hc_T)
b_t$documents$texts[1:43]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tablelinen_all$filter_T_hc_F)
b_f$documents$texts[1:5]

## Carpets and Curtains
carpet <- tagfilter_carpet()

carpet_ids_1734 <- carpet$filtrate(corpus_1734,ignore.case = T)
carpet_ids_1834 <- carpet$filtrate(corpus_1834,ignore.case = T)

carpet_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                      carpet_ids_1734)
carpet_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                      carpet_ids_1834)

carpet_subset_all <- c(carpet_subset_1734, carpet_subset_1834)
carpet_ids_all <- c(carpet_ids_1734, carpet_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carpet_kwic <- kwic(carpet_subset_all,
                    pattern = "T(e|a)(pp|p)i|Bodent(u|ü)ch|Vorh(a|ä)ng",
                    valuetype = "regex")

carpet_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
carpet_subset_clean <- carpet_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(carpet_subset_clean),
                   max_words = 100)

## Validation
validation_carpet_all <- validate_filter(corpus_all, carpet_ids_all,
                                         search_col = "adcontent",
                                         pattern = "01textilien")
validation_carpet_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% carpet_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% carpet_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_carpet_all$filter_T_hc_T)
b_t$documents$texts[1:57]

# FALSE positives: both correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_carpet_all$filter_T_hc_F)
b_f$documents$texts[1:2]


## Bedding
bedding <- tagfilter_bedding()

bedding_ids_1734 <- bedding$filtrate(corpus_1734,ignore.case = T)
bedding_ids_1834 <- bedding$filtrate(corpus_1834,ignore.case = T)

bedding_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       bedding_ids_1734)
bedding_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       bedding_ids_1834)

bedding_subset_all <- c(bedding_subset_1734, bedding_subset_1834)
bedding_ids_all <- c(bedding_ids_1734, bedding_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bedding_kwic <- kwic(bedding_subset_all,
                     pattern = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|
    Bethzeug|Bettsack|Bethsack|Decke|Strochsack|Strohsäck|Kissen|Unterbe(tt|th)|Nachtsack|Betteingu(ß|ss)",
                     valuetype = "regex")

bedding_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bedding_subset_clean <- bedding_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bedding_subset_clean),
                   max_words = 100)

## Validation
validation_bedding_all <- validate_filter(corpus_all, bedding_ids_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_bedding_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bedding_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bedding_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bedding_all$filter_T_hc_T)
b_t$documents$texts[1:130]

# FALSE positives: almost all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bedding_all$filter_T_hc_F)
b_f$documents$texts[1:35]


## Chairs
chair <- tagfilter_chair()

chair_ids_1734 <- chair$filtrate(corpus_1734, ignore.case = T)
chair_ids_1834 <- chair$filtrate(corpus_1834, ignore.case = T)

chair_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   chair_ids_1734)

chair_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   chair_ids_1834)

chair_subset_all <- c(chair_subset_1734, chair_subset_1834)
chair_ids_all <- c(chair_ids_1734, chair_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
chair_kwic <- kwic(chair_subset_all,
                 pattern = "St(u|ü)hl|Sitz|B(a|ä)nk",
                 valuetype = "regex")

chair_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
chair_subset_clean <- chair_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(chair_subset_clean),
                   max_words = 100)

## Validation
validation_chair_all <- validate_filter(corpus_all, chair_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_chair_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% chair_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% chair_ids_all)]

#TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_chair_all$filter_T_hc_T)
b_t$documents$texts[1:58]

#FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_chair_all$filter_T_hc_F)
b_f$documents$texts[1:8]



## Cupboards, Cabinets and Storage
cabinet <- tagfilter_cabinet()

cabinet_ids_1734 <- cabinet$filtrate(corpus_1734,ignore.case = T)
cabinet_ids_1834 <- cabinet$filtrate(corpus_1834,ignore.case = T)

cabinet_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       cabinet_ids_1734)
cabinet_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       cabinet_ids_1834)

cabinet_subset_all <- c(cabinet_subset_1734, cabinet_subset_1834)
cabinet_ids_all <- c(cabinet_ids_1734, cabinet_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cabinet_kwic <- kwic(cabinet_subset_all,
                  pattern = "Büchersch|Bücherk",
                  valuetype = "regex")

cabinet_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cabinet_subset_clean <- cabinet_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cabinet_subset_clean),
                   max_words = 100)

# Validation
validation_cabinet_all <- validate_filter(corpus_all, cabinet_ids_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_cabinet_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% cabinet_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% cabinet_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cabinet_all$filter_T_hc_T)
b_t$documents$texts[1:289]

# FALSE positives: almost all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cabinet_all$filter_T_hc_F)
b_f$documents$texts[1:33]


## Stoves and Related Objects
stove <- tagfilter_stove()

stove_ids_1734 <- stove$filtrate(corpus_1734, ignore.case = T)
stove_ids_1834 <- stove$filtrate(corpus_1834, ignore.case = T)

stove_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   stove_ids_1734)

stove_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   stove_ids_1834)

stove_subset_all <- c(stove_subset_1734, stove_subset_1834)
stove_ids_all <- c(stove_ids_1734, stove_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
stove_kwic <- kwic(stove_subset_all,
                 pattern = "Be(th|tt)pfanne",
                 valuetype = "regex",
                 ignore.case = T)
stove_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
stove_subset_clean <- stove_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(stove_subset_clean),
                   max_words = 100)

## Validation
validation_stove_all <- validate_filter(corpus_all, stove_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_stove_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% stove_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% stove_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_stove_all$filter_T_hc_T)
b_t$documents$texts[1:202]

# FALSE positives: almost all are correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_stove_all$filter_T_hc_F)
b_f$documents$texts[1:32]



## Mirrors
mirror <- tagfilter_mirror()

mirror_ids_1734 <- mirror$filtrate(corpus_1734, ignore.case = T)
mirror_ids_1834 <- mirror$filtrate(corpus_1834, ignore.case = T)

mirror_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   mirror_ids_1734)

mirror_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   mirror_ids_1834)

mirror_subset_all <- c(mirror_subset_1734, mirror_subset_1834)
mirror_ids_all <- c(mirror_ids_1734, mirror_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mirror_kwic <- kwic(mirror_subset_all,
                 pattern = "Spiegel",
                 valuetype = "regex",
                 ignore.case = T)
mirror_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
mirror_subset_clean <- mirror_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mirror_subset_clean),
                   max_words = 100)

## Validation
validation_mirror_all <- validate_filter(corpus_all, mirror_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_mirror_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% mirror_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% mirror_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_mirror_all$filter_T_hc_T)
b_t$documents$texts[1:57]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_mirror_all$filter_T_hc_F)
b_f$documents$texts[1:15]



## Timepieces
timepiece <- tagfilter_timepiece()

timepiece_ids_1734 <- timepiece$filtrate(corpus_1734, ignore.case = T)
timepiece_ids_1834 <- timepiece$filtrate(corpus_1834, ignore.case = T)

timepiece_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   timepiece_ids_1734)

timepiece_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   timepiece_ids_1834)

timepiece_subset_all <- c(timepiece_subset_1734, timepiece_subset_1834)
timepiece_ids_all <- c(timepiece_ids_1734, timepiece_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
timepiece_kwic <- kwic(timepiece_subset_1734,
                 pattern = "(U|Ü|Ue)hr|Pendul",
                 valuetype = "regex",
                 ignore.case = T)
timepiece_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
timepiece_subset_clean <- timepiece_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(timepiece_subset_clean),
                   max_words = 100)

## Validation
validation_timepiece_all <- validate_filter(corpus_all, timepiece_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_timepiece_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% timepiece_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% timepiece_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_timepiece_all$filter_T_hc_T)
b_t$documents$texts[1:63]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_timepiece_all$filter_T_hc_F)
b_f$documents$texts[1:13]


## Tables
table <- tagfilter_table()

table_ids_1734 <- table$filtrate(corpus_1734, ignore.case = T)
table_ids_1834 <- table$filtrate(corpus_1834, ignore.case = T)

table_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   table_ids_1734)

table_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   table_ids_1834)

table_subset_all <- c(table_subset_1734, table_subset_1834)
table_ids_all <- c(table_ids_1734, table_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
table_kwic <- kwic(table_subset_1834,
                 pattern = "(T|t)isch",
                 valuetype = "regex",
                 ignore.case = T)
table_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
table_subset_clean <- table_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(table_subset_clean),
                   max_words = 100)

## Validation
validation_table_all <- validate_filter(corpus_all, table_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_table_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% table_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% table_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_table_all$filter_T_hc_T)
b_t$documents$texts[1:170]

# FALSE positives
b_f <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       validation_table_all$filter_T_hc_F)
b_f$documents$texts[1:20]


## Tableware
tableware <- tagfilter_tableware()

tableware_ids_1734 <- tableware$filtrate(corpus_1734, ignore.case = T)
tableware_ids_1834 <- tableware$filtrate(corpus_1834, ignore.case = T)

tableware_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   tableware_ids_1734)

tableware_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   tableware_ids_1834)

tableware_subset_all <- c(tableware_subset_1734, tableware_subset_1834)
tableware_ids_all <- c(tableware_ids_1734, tableware_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tableware_kwic <- kwic(tableware_subset_1734,
                 pattern = "Teller|Geschir|Biergläs|Bierglas|Tasse|Humpe|Becher|Porzel|Porcel|Steingut|Fayence|Krystallwaa|Krystall-Waa|
                 Zin(n|nen)geschirr|Zin(n|nen)-Geschirr|Zin(n|nen)waare|Zin(n|nen)-Waare|Blechwaare|Blech-Waare|(K|C)anne|(K|C)arafe|
                 Caffeti(e|è)r|Kaffeti(e|è)r|Kaffeeti(e|è)r|Cr(e|ê)mier",
                 valuetype = "regex",
                 ignore.case = T)
tableware_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
tableware_subset_clean <- tableware_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tableware_subset_clean),
                   max_words = 100)

## Validation
validation_tableware_all <- validate_filter(corpus_all, tableware_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_tableware_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tableware_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tableware_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tableware_all$filter_T_hc_T)
b_t$documents$texts[1:82]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tableware_all$filter_T_hc_F)
b_f$documents$texts[1:16]


## Bureau
bureau <- tagfilter_bureau()

bureau_ids_1734 <- bureau$filtrate(corpus_1734, ignore.case = T)
bureau_ids_1834 <- bureau$filtrate(corpus_1834, ignore.case = T)

bureau_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   bureau_ids_1734)

bureau_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   bureau_ids_1834)

bureau_subset_all <- c(bureau_subset_1734, bureau_subset_1834)
bureau_ids_all <- c(bureau_ids_1734, bureau_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bureau_kwic <- kwic(bureau_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
bureau_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bureau_subset_clean <- bureau_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bureau_subset_clean),
                   max_words = 100)

# Validation
validation_bureau_all <- validate_filter(corpus_all, bureau_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_bureau_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% bureau_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% bureau_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bureau_all$filter_T_hc_T)
b_t$documents$texts[1:84]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bureau_all$filter_T_hc_F)
b_f$documents$texts[1:2]


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

## Toys
toy <- tagfilter_toy()

toy_ids_1734 <- toy$filtrate(corpus_1734, ignore.case = T)
toy_ids_1834 <- toy$filtrate(corpus_1834, ignore.case = T)

toy_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   toy_ids_1734)

toy_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   toy_ids_1834)

toy_subset_all <- c(toy_subset_1734, toy_subset_1834)
toy_ids_all <- c(toy_ids_1734, toy_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
toy_kwic <- kwic(toy_subset_all,
                 pattern = "Puppe|Bauhölz(chen|lein)|Baukasten|Felsenburg|Spiel-Boit|Spielboit|
                 Aufstellschachtel|(Schwung|Steck|Stecken)pferd|Spiel(waa|a)re|Kinderspiel",
                 valuetype = "regex",
                 ignore.case = T)
toy_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
toy_subset_clean <- toy_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(toy_subset_clean),
                   max_words = 100)

## Validation
validation_toy_all <- validate_filter(corpus_all, toy_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_toy_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% toy_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% toy_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_toy_all$filter_T_hc_T)
b_t$documents$texts[1:32]

# FALSE positives - All are correct!
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_toy_all$filter_T_hc_F)
b_f$documents$texts[1:12]


## Games
game <- tagfilter_game()

game_ids_1734 <- game$filtrate(corpus_1734, ignore.case = T)
game_ids_1834 <- game$filtrate(corpus_1834, ignore.case = T)

game_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   game_ids_1734)

game_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   game_ids_1834)

game_subset_all <- c(game_subset_1734, game_subset_1834)
game_ids_all <- c(game_ids_1734, game_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
game_kwic <- kwic(game_subset_all,
                 pattern = "Billard|Billiard|Bilard|Biliard|Schachbret|Schachspiel|Taschenspiel|
                 Domino|Lottospiel|Spielkarten|Whist|Würfelspiel|Kegelspiel|Kegelries|Spiele",
                 valuetype = "regex",
                 ignore.case = T)
game_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
game_subset_clean <- game_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(game_subset_clean),
                   max_words = 100)

## Validation
validation_game_all <- validate_filter(corpus_all, game_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_game_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% game_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% game_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_game_all$filter_T_hc_T)
b_t$documents$texts[1:21]

# FALSE positives: Most are correctly identified
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_game_all$filter_T_hc_F)
b_f$documents$texts[1:13]


## Kitchen Utensils
kitchen <- tagfilter_kitchen()

kitchen_ids_1734 <- kitchen$filtrate(corpus_1734, ignore.case = T)
kitchen_ids_1834 <- kitchen$filtrate(corpus_1834, ignore.case = T)

kitchen_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   kitchen_ids_1734)

kitchen_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   kitchen_ids_1834)

kitchen_subset_all <- c(kitchen_subset_1734, kitchen_subset_1834)
kitchen_ids_all <- c(kitchen_ids_1734, kitchen_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
kitchen_kwic <- kwic(kitchen_subset_1734,
                 pattern = "Fischbeck(i|e)n",
                 valuetype = "regex",
                 ignore.case = T)
kitchen_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
kitchen_subset_clean <- kitchen_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(kitchen_subset_clean),
                   max_words = 100)

## Validation
validation_kitchen_all <- validate_filter(corpus_all, kitchen_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_kitchen_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% kitchen_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% kitchen_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_kitchen_all$filter_T_hc_T)
b_t$documents$texts[1:72]

# FALSE positives: almost all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_kitchen_all$filter_T_hc_F)
b_f$documents$texts[1:55]


## Lighting
lighting <- tagfilter_lighting()

lighting_ids_1734 <- lighting$filtrate(corpus_1734, ignore.case = T)
lighting_ids_1834 <- lighting$filtrate(corpus_1834, ignore.case = T)

lighting_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   lighting_ids_1734)

lighting_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   lighting_ids_1834)

lighting_subset_all <- c(lighting_subset_1734, lighting_subset_1834)
lighting_ids_all <- c(lighting_ids_1734, lighting_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
lighting_kwic <- kwic(lighting_subset_1734,
                 pattern = "Leuchter|Chandelier|Lampe|Latern|Nachtlicht|Lichtstock|Lichtstöck|Gingette|Lamepngl(a|ä)s|
                 Lampe(nö|noe|n-Ö|n-Oe)hl|Wachskerzen|Wachslichter|Kerze|Lamendocht|Lichtscheer",
                 valuetype = "regex",
                 ignore.case = T)
lighting_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
lighting_subset_clean <- lighting_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(lighting_subset_clean),
                   max_words = 100)

## Validation
validation_lighting_all <- validate_filter(corpus_all, lighting_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_lighting_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% lighting_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% lighting_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_lighting_all$filter_T_hc_T)
b_t$documents$texts[1:44]

# FALSE positives: almost all correct, some description of carriages
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_lighting_all$filter_T_hc_F)
b_f$documents$texts[1:53]



## Musical Instruments
instrument <- tagfilter_instrument()

instrument_ids_1734 <- instrument$filtrate(corpus_1734, ignore.case = T)
instrument_ids_1834 <- instrument$filtrate(corpus_1834, ignore.case = T)

instrument_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   instrument_ids_1734)

instrument_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   instrument_ids_1834)

instrument_subset_all <- c(instrument_subset_1734, instrument_subset_1834)
instrument_ids_all <- c(instrument_ids_1734, instrument_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
instrument_kwic <- kwic(instrument_subset_all,
                 pattern = "Spi(nn|n)et",
                 valuetype = "regex",
                 ignore.case = T)
instrument_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
instrument_subset_clean <- instrument_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(instrument_subset_clean),
                   max_words = 100)

## Validation
validation_instrument_all <- validate_filter(corpus_all, instrument_ids_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_instrument_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% instrument_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% instrument_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_instrument_all$filter_T_hc_T)
b_t$documents$texts[1:62]

# FALSE positives: all are correct!
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_instrument_all$filter_T_hc_F)
b_f$documents$texts[1:107]


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



## Wallpaper
wallpaper <- tagfilter_wallpaper()

wallpaper_ids_1734 <- wallpaper$filtrate(corpus_1734, ignore.case = T)
wallpaper_ids_1834 <- wallpaper$filtrate(corpus_1834, ignore.case = T)

wallpaper_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   wallpaper_ids_1734)

wallpaper_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   wallpaper_ids_1834)

wallpaper_subset_all <- c(wallpaper_subset_1734, wallpaper_subset_1834)
wallpaper_ids_all <- c(wallpaper_ids_1734, wallpaper_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wallpaper_kwic <- kwic(wallpaper_subset_all,
                 pattern = "Tapez",
                 valuetype = "regex",
                 ignore.case = T)
wallpaper_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
wallpaper_subset_clean <- wallpaper_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(wallpaper_subset_clean),
                   max_words = 100)

## Validation
validation_wallpaper_all <- validate_filter(corpus_all, wallpaper_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_wallpaper_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% wallpaper_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% wallpaper_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_wallpaper_all$filter_T_hc_T)
b_t$documents$texts[1:14]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_wallpaper_all$filter_T_hc_F)
b_f$documents$texts[1:10]


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


## Cutlery
cutlery <- tagfilter_cutlery()

cutlery_ids_1734 <- cutlery$filtrate(corpus_1734, ignore.case = T)
cutlery_ids_1834 <- cutlery$filtrate(corpus_1834, ignore.case = T)

cutlery_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   cutlery_ids_1734)

cutlery_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   cutlery_ids_1834)

cutlery_subset_all <- c(cutlery_subset_1734, cutlery_subset_1834)
cutlery_ids_all <- c(cutlery_ids_1734, cutlery_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cutlery_kwic <- kwic(cutlery_subset_all,
                 pattern = "Messerschmied|Messerschmid",
                 valuetype = "regex",
                 ignore.case = T)
cutlery_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cutlery_subset_clean <- cutlery_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cutlery_subset_clean),
                   max_words = 100)

## Validation
validation_cutlery_all <- validate_filter(corpus_all, cutlery_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_cutlery_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% cutlery_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% cutlery_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cutlery_all$filter_T_hc_T)
b_t$documents$texts[1:30]

# FALSE positives: most are correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cutlery_all$filter_T_hc_F)
b_f$documents$texts[1:25]


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


## Room Dividers
divider <- tagfilter_divider()

divider_ids_1734 <- divider$filtrate(corpus_1734, ignore.case = T)
divider_ids_1834 <- divider$filtrate(corpus_1834, ignore.case = T)

divider_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   divider_ids_1734)

divider_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   divider_ids_1834)

divider_subset_all <- c(divider_subset_1734, divider_subset_1834)
divider_ids_all <- c(divider_ids_1734, divider_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
divider_kwic <- kwic(divider_subset_all,
                 pattern = "spannisch",
                 valuetype = "regex",
                 ignore.case = T)
divider_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
divider_subset_clean <- divider_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(divider_subset_clean),
                   max_words = 100)

## Validation
validation_divider_all <- validate_filter(corpus_all, divider_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_divider_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% divider_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% divider_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_divider_all$filter_T_hc_T)
b_t$documents$texts[1:21]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_divider_all$filter_T_hc_F)
b_f$documents$texts[1:6]


## Object for Pets
petobject <- tagfilter_petobject()

petobject_ids_1734 <- petobject$filtrate(corpus_1734, ignore.case = T)
petobject_ids_1834 <- petobject$filtrate(corpus_1834, ignore.case = T)

petobject_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   petobject_ids_1734)

petobject_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   petobject_ids_1834)

petobject_subset_all <- c(petobject_subset_1734, petobject_subset_1834)
petobject_ids_all <- c(petobject_ids_1734, petobject_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
petobject_kwic <- kwic(petobject_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
petobject_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
petobject_subset_clean <- petobject_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(petobject_subset_clean),
                   max_words = 100)

## Validation
validation_petobject_all <- validate_filter(corpus_all, petobject_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_petobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% petobject_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% petobject_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_petobject_all$filter_T_hc_T)
b_t$documents$texts[1:14]

# FALSE positives: most are correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_petobject_all$filter_T_hc_F)
b_f$documents$texts[1:37]


## Upholstery
upholstery <- tagfilter_upholstery()

upholstery_ids_1734 <- upholstery$filtrate(corpus_1734, ignore.case = T)
upholstery_ids_1834 <- upholstery$filtrate(corpus_1834, ignore.case = T)

upholstery_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   upholstery_ids_1734)

upholstery_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   upholstery_ids_1834)

upholstery_subset_all <- c(upholstery_subset_1734, upholstery_subset_1834)
upholstery_ids_all <- c(upholstery_ids_1734, upholstery_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
upholstery_kwic <- kwic(upholstery_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
upholstery_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
upholstery_subset_clean <- upholstery_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(upholstery_subset_clean),
                   max_words = 100)

## Validation
validation_upholstery_all <- validate_filter(corpus_all, upholstery_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_upholstery_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% upholstery_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% upholstery_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_upholstery_all$filter_T_hc_T)
b_t$documents$texts[1:128]

# FALSE positives: all are correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_upholstery_all$filter_T_hc_F)
b_f$documents$texts[1:3]


## Domestic Appliances
domestic <- tagfilter_domestic()

domestic_ids_1734 <- domestic$filtrate(corpus_1734, ignore.case = T)
domestic_ids_1834 <- domestic$filtrate(corpus_1834, ignore.case = T)

domestic_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   domestic_ids_1734)

domestic_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   domestic_ids_1834)

domestic_subset_all <- c(domestic_subset_1734, domestic_subset_1834)
domestic_ids_all <- c(domestic_ids_1734, domestic_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
domestic_kwic <- kwic(domestic_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
domestic_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
domestic_subset_clean <- domestic_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(domestic_subset_clean),
                   max_words = 100)

## Validation
validation_domestic_all <- validate_filter(corpus_all, domestic_ids_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_domestic_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% domestic_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% domestic_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_domestic_all$filter_T_hc_T)
b_t$documents$texts[1:76]

# FALSE positives: almost all correctly recongised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_domestic_all$filter_T_hc_F)
b_f$documents$texts[1:93]


## Garden Furniture and Objects
garden <- tagfilter_garden()

garden_ids_1734 <- garden$filtrate(corpus_1734, ignore.case = T)
garden_ids_1834 <- garden$filtrate(corpus_1834, ignore.case = T)

garden_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   garden_ids_1734)

garden_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   garden_ids_1834)

garden_subset_all <- c(garden_subset_1734, garden_subset_1834)
garden_ids_all <- c(garden_ids_1734, garden_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
garden_kwic <- kwic(garden_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
garden_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
garden_subset_clean <- garden_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(garden_subset_clean),
                   max_words = 100)

## Validation
validation_garden_all <- validate_filter(corpus_all, garden_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_garden_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% garden_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% garden_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_garden_all$filter_T_hc_T)
b_t$documents$texts[1:19]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_garden_all$filter_T_hc_F)
b_f$documents$texts[1:13]


## Home Decoration
homedeco <- tagfilter_homedeco()

homedeco_ids_1734 <- homedeco$filtrate(corpus_1734, ignore.case = T)
homedeco_ids_1834 <- homedeco$filtrate(corpus_1834, ignore.case = T)

homedeco_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   homedeco_ids_1734)

homedeco_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   homedeco_ids_1834)

homedeco_subset_all <- c(homedeco_subset_1734, homedeco_subset_1834)
homedeco_ids_all <- c(homedeco_ids_1734, homedeco_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
homedeco_kwic <- kwic(homedeco_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
homedeco_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
homedeco_subset_clean <- homedeco_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(homedeco_subset_clean),
                   max_words = 100)

## Validation
validation_homedeco_all <- validate_filter(corpus_all, homedeco_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_homedeco_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% homedeco_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% homedeco_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_homedeco_all$filter_T_hc_T)
b_t$documents$texts[1:6]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_homedeco_all$filter_T_hc_F)
b_f$documents$texts[1:5]

## Art
art <- tagfilter_art()

art_ids_1734 <- art$filtrate(corpus_1734, ignore.case = T)
art_ids_1834 <- art$filtrate(corpus_1834, ignore.case = T)

art_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   art_ids_1734)

art_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   art_ids_1834)

art_subset_all <- c(art_subset_1734, art_subset_1834)
art_ids_all <- c(art_ids_1734, art_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
art_kwic <- kwic(art_subset_all,
                 pattern = "Bilder|Gem(ä|äh)lde|M(a|ah)lere(y|i)en",
                 valuetype = "regex",
                 ignore.case = T)
art_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
art_subset_clean <- art_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(art_subset_clean),
                   max_words = 100)

## Validation
validation_art_all <- validate_filter(corpus_all, art_ids_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_art_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% art_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% art_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_art_all$filter_T_hc_T)
b_t$documents$texts[1:25]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_art_all$filter_T_hc_F)
b_f$documents$texts[1:23]

## Bathing Objects
bathobject <- tagfilter_bathobject()

bathobject_ids_1734 <- bathobject$filtrate(corpus_1734, ignore.case = T)
bathobject_ids_1834 <- bathobject$filtrate(corpus_1834, ignore.case = T)

bathobject_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   bathobject_ids_1734)

bathobject_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   bathobject_ids_1834)

bathobject_subset_all <- c(bathobject_subset_1734, bathobject_subset_1834)
bathobject_ids_all <- c(bathobject_ids_1734, bathobject_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bathobject_kwic <- kwic(bathobject_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
bathobject_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bathobject_subset_clean <- bathobject_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bathobject_subset_clean),
                   max_words = 100)

## Validation
validation_bathobject_all <- validate_filter(corpus_all, bathobject_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_bathobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% bathobject_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% bathobject_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bathobject_all$filter_T_hc_T)
b_t$documents$texts[1:15]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bathobject_all$filter_T_hc_F)
b_f$documents$texts[1:3]

## Misc Household Goods (Unspecified)
mischousehold <- tagfilter_mischousehold()

mischousehold_ids_1734 <- mischousehold$filtrate(corpus_1734, ignore.case = T)
mischousehold_ids_1834 <- mischousehold$filtrate(corpus_1834, ignore.case = T)

mischousehold_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   mischousehold_ids_1734)

mischousehold_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   mischousehold_ids_1834)

mischousehold_subset_all <- c(mischousehold_subset_1734, mischousehold_subset_1834)
mischousehold_ids_all <- c(mischousehold_ids_1734, mischousehold_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mischousehold_kwic <- kwic(mischousehold_subset_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
mischousehold_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
mischousehold_subset_clean <- mischousehold_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mischousehold_subset_clean),
                   max_words = 100)

## Validation
validation_mischousehold_all <- validate_filter(corpus_all, mischousehold_ids_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_mischousehold_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% mischousehold_ids_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% mischousehold_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_mischousehold_all$filter_T_hc_T)
b_t$documents$texts[1:36]

# FALSE positives: all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_mischousehold_all$filter_T_hc_F)
b_f$documents$texts[1:3]



### using existing categories for wordclouds and dfm to find missing objects in dictionaries
# creating subset of houshold objects for sale and for buying
household_1734 <- corpus_subset(corpus_1734, grepl("02hausrat", adcontent) & grepl("01kauf", finance))
household_1834 <- corpus_subset(corpus_1834, grepl("02hausrat", adcontent) & grepl("01kauf", finance))

# cleaning subset
household_1834_clean <- household_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

# textplot of most frequent words
textplot_wordcloud(dfm(household_1834_clean),
                   max_words = 400)

# tokenize ads of subcorpus
household_tok <- household_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

# creating document feature matrix
household_dfm <- household_tok %>%
  dfm()

household_df <- docfreq(household_dfm)

household_s_df <- sort(household_df,decreasing = T)

topfeatures(household_dfm)

# exporting csv with sorted document feature matrix to Desktop
write.csv2(household_s_df, file = "data/household_s_df_1834.csv", fileEncoding = "UTF-8")

### showing ids for documents (ads) NOT classified as any houshold good by automatic classification, but classified manually as "02haushalt"
# looking at household_1834 corpus
household_1834

# negating %in% operator
`%notin%` <- Negate(`%in%`)

# creating a corpus of all the ads (ids) not found by automatic classification
household_missed <- corpus_subset(household_1734, docvars(household_1734, "id") %notin%
                                    c(bed_ids_all, tablelinen_ids_all, carpet_ids_all, bedding_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
                                      timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                      chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                      instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                      petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                      wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))
# 17/01/20: only 52 missed ads (if only looking at "for sale/to buy", 168 if including others)
# most of them French or wrongly classified as "Hausrat" in manual classification
# some missed ads don't make any sense, should be included through dictionaries, words in question are: "Vorfenster",
# "Trumeau", "Messer", "Fensterflügel", "Fenster", "WaarenKorpus" and "Bücherkästchen"
# rest of missed ads are very special objects, makes no sense to make a dictionary for them (re-evaluate later)

household_missed_texts <- household_missed$documents$texts

# write csv for texts of missed ads
write.csv2(household_missed_texts, file = "data/household_missed.csv", fileEncoding = "UTF-8")

# creating a corpus of all ads (ids) found automatically, but not by manual classification
# new household_1834 corpus with all ads (not only "01kauf")

household_1834 <- corpus_subset(corpus_1834, grepl("02hausrat", adcontent))

household_ids_all <- household_1834$documents$id

not_household <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %notin%
                                  household_ids_all)

household_automatic <- corpus(c(bed_ids_all, tablelinen_ids_all, carpet_ids_all, bedding_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
         timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
         chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
         instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
         petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
         wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))

household_aut_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                     c(tablelinen_ids_all, carpet_ids_all, bedding_ids_all,bed_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
                                       timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                       chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                       instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                       petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                       wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))

household_aut_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                      c(tablelinen_ids_all, carpet_ids_all, bedding_ids_all,bed_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
                                        timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                        chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                        instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                        petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                        wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))


household_oops <- corpus_subset(not_household, docvars(not_household, "id") %in%
                                  c(tablelinen_ids_all, carpet_ids_all, bedding_ids_all,bed_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
                                    timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                    chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                    instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                    petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                    wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))

household_oops_texts <- household_oops$documents$texts

write.csv2(household_oops_texts, file = "data/household_oops.csv", fileEncoding = "UTF-8")

# 20/01/20: a lot of "oops" cases (936!), but most of them seem to be work or immo related and could be excluded by detection of work/immo ads
# some of them are adverts for shops or services, were houshold goods are advertised, so actually correctly recognized via automatic recognition
# and wrongly not tagged manually, in general ads frequently not classified as houshold by manual clasification
# sometimes also negative dictionaries don't seem to work consistently,  e.g. "Sitzung" or "Langmesser"

# creating a corpus of all ads recognised by automated household filters
household_filters <- corpus_subset(corpus_1834, docvars(household_1834, "id") %in%
                                    c(tablelinen_ids_all, carpet_ids_all, bedding_ids_all,bed_ids_all, seat_ids_all, table_ids_all, tableware_ids_all,
                                      timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                      chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                      instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                      petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                      wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))
