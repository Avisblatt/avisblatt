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
source("R/tagfilters_household.R")
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



### checking and cleaning different tagfilters for textiles


## Clothing
clothing <- tagfilter_clothing()

clothing_ids_1734 <- clothing$filtrate(corpus_1734,ignore.case = T)
clothing_ids_1834 <- clothing$filtrate(corpus_1834,ignore.case = T)

clothing_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                   clothing_ids_1734)
clothing_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                             clothing_ids_1834)

clothing_subset_all <- c(clothing_subset_1734, clothing_subset_1834)
clothing_ids_all <- c(clothing_ids_1734, clothing_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
clothing_kwic <- kwic(clothing_subset_all,
                  pattern = "Tschako",
                  valuetype = "regex",
                  ignore.case = T)
clothing_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
clothing_subset_clean_all <- clothing_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(clothing_subset_clean_all),
                   max_words = 200)


# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_clothing_all <- validate_filter(corpus_all, clothing_ids_all,
                                  search_col = "adcontent",
                                  pattern = "01textilien")
validation_clothing_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% clothing_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% clothing_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_clothing_all$filter_T_hc_T)
b_t$documents$texts[1:277]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_clothing_all$filter_T_hc_F)
b_f$documents$texts[1:18]


## Sleepwear
sleepwear <- tagfilter_sleepwear()

sleepwear_ids_1734 <- sleepwear$filtrate(corpus_1734,ignore.case = T)
sleepwear_ids_1834 <- sleepwear$filtrate(corpus_1834,ignore.case = T)

sleepwear_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                         sleepwear_ids_1734)
sleepwear_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                         sleepwear_ids_1834)

sleepwear_subset_all <- c(sleepwear_subset_1734, sleepwear_subset_1834)
sleepwear_ids_all <- c(sleepwear_ids_1734, sleepwear_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
sleepwear_kwic <- kwic(sleepwear_subset_all,
                      pattern = "Schlafrock|Schlafröck|Nachtärmel",
                      valuetype = "regex",
                      ignore.case = T)
sleepwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
sleepwear_subset_clean <- sleepwear_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(sleepwear_subset_clean),
                   max_words = 200)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_sleepwear_all <- validate_filter(corpus_all, sleepwear_ids_all,
                                           search_col = "adcontent",
                                           pattern = "01textilien")
validation_sleepwear_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% sleepwear_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% sleepwear_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_sleepwear_all$filter_T_hc_T)
b_t$documents$texts[1:16]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_sleepwear_all$filter_T_hc_F)
b_f$documents$texts[0]



## Military/ Uniforms
uniform <- tagfilter_uniform()

uniform_ids_1734 <- uniform$filtrate(corpus_1734,ignore.case = T)
uniform_ids_1834 <- uniform$filtrate(corpus_1834,ignore.case = T)

uniform_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       uniform_ids_1734)
uniform_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       uniform_ids_1834)

uniform_subset_all <- c(uniform_subset_1734, uniform_subset_1834)
uniform_ids_all <- c(uniform_ids_1734, uniform_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
uniform_kwic <- kwic(uniform_subset_all,
                       pattern = "Militär-Effekt|Militäreffekt",
                       valuetype = "regex",
                       ignore.case = T)
uniform_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
uniform_subset_clean <- uniform_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(uniform_subset_clean),
                   max_words = 200)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_uniform_all <- validate_filter(corpus_all, uniform_ids_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_uniform_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% uniform_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% uniform_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_uniform_all$filter_T_hc_T)
b_t$documents$texts[1:28]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_uniform_all$filter_T_hc_F)
b_f$documents$texts[1:6]



## Underwear
underwear <- tagfilter_underwear()

underwear_ids_1734 <- underwear$filtrate(corpus_1734,ignore.case = T)
underwear_ids_1834 <- underwear$filtrate(corpus_1834,ignore.case = T)

underwear_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                         underwear_ids_1734)
underwear_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                         underwear_ids_1834)

underwear_subset_all <- c(underwear_subset_1734, underwear_subset_1834)
underwear_ids_all <- c(underwear_ids_1734, underwear_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
underwear_kwic <- kwic(underwear_subset_all,
                     pattern = "Socke|Strumpf|Strümpf",
                     valuetype = "regex",
                     ignore.case = T)
underwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
underwear_subset_clean <- underwear_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(underwear_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_underwear_all <- validate_filter(corpus_1734, underwear_ids_1734,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_underwear_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% underwear_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% underwear_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_underwear_all$filter_T_hc_T)
b_t$documents$texts[1:19]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_underwear_all$filter_T_hc_F)
b_f$documents$texts[1:5]


## Outerwear
outerwear <- tagfilter_outerwear()

outerwear_ids_1734 <- outerwear$filtrate(corpus_1734,ignore.case = T)
outerwear_ids_1834 <- outerwear$filtrate(corpus_1834,ignore.case = T)

outerwear_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                         outerwear_ids_1734)
outerwear_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                         outerwear_ids_1834)

outerwear_subset_all <- c(outerwear_subset_1734, outerwear_subset_1834)
outerwear_ids_all <- c(outerwear_ids_1734, outerwear_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
outerwear_kwic <- kwic(outerwear_subset_all,
                       pattern = "Mantel|Mäntel|Coat|Cotte|Cols|Schbrack|Mantille|Kittelein|Pellerine",
                       valuetype = "regex",
                       ignore.case = T)
outerwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
outerwear_subset_clean <- outerwear_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(outerwear_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_outerwear_all <- validate_filter(corpus_all, outerwear_ids_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_outerwear_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% outerwear_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% outerwear_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_outerwear_all$filter_T_hc_T)
b_t$documents$texts[1:150]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_outerwear_all$filter_T_hc_F)
b_f$documents$texts[1:2]


## Costumes/ Special Occasion Garments
costume <- tagfilter_costume()

costume_ids_1734 <- costume$filtrate(corpus_1734,ignore.case = T)
costume_ids_1834 <- costume$filtrate(corpus_1834,ignore.case = T)

costume_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                       costume_ids_1734)
costume_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       costume_ids_1834)

costume_subset_all <- c(costume_subset_1734, costume_subset_1834)
costume_ids_all <- c(costume_ids_1734, costume_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
costume_kwic <- kwic(costume_subset_all,
                       pattern = "Taufe",
                       valuetype = "regex",
                       ignore.case = T)
costume_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
costume_subset_clean <- costume_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(costume_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_costume_all <- validate_filter(corpus_all, costume_ids_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_costume_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% costume_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% costume_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_costume_all$filter_T_hc_T)
b_t$documents$texts[1:12]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_costume_all$filter_T_hc_F)
b_f$documents$texts[0]


## Shoes
shoes <- tagfilter_shoes()

shoes_ids_1734 <- shoes$filtrate(corpus_1734,ignore.case = T)
shoes_ids_1834 <- shoes$filtrate(corpus_1834,ignore.case = T)

shoes_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                     shoes_ids_1734)
shoes_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     shoes_ids_1834)

shoes_subset_all <- c(shoes_subset_1734, shoes_subset_1834)
shoes_ids_all <- c(shoes_ids_1734, shoes_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shoes_kwic <- kwic(shoes_subset_all,
                      pattern = "[S|s]chuh|[S|s]chüh|[S|s]tiefel[S|s]ohle|[S|s]öhle|[P|p]antoffel",
                      valuetype = "regex")
shoes_kwic

# some "schuh" as measurement remain, e.g. "Länge 3 1/ 2 Schuh"
# excluding these through regex of "Länge, Breite etc." in proximity of 3 words to "Schuh"?


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
shoes_subset_clean <- shoes_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(shoes_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_shoes_all <- validate_filter(corpus_all, shoes_ids_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_shoes_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% shoes_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% shoes_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_shoes_all$filter_T_hc_T)
b_t$documents$texts[1:61]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_shoes_all$filter_T_hc_F)
b_f$documents$texts[1:17]


## Handkerchiefs
handkerchief <- tagfilter_handkerchief()

handkerchief_ids_1734 <- handkerchief$filtrate(corpus_1734,ignore.case = T)
handkerchief_ids_1834 <- handkerchief$filtrate(corpus_1834,ignore.case = T)

handkerchief_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                            handkerchief_ids_1734)
handkerchief_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                            handkerchief_ids_1834)

handkerchief_subset_all <- c(handkerchief_subset_1734, handkerchief_subset_1834)
handkerchief_ids_all <- c(handkerchief_ids_1734, handkerchief_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
handkerchief_kwic <- kwic(handkerchief_subset_all,
                     pattern = "Schnupftuch|Mouchoir|Sacktuch|Sacktüch",
                     valuetype = "regex",
                     ignore.case = T)
handkerchief_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
handkerchief_subset_clean <- handkerchief_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(handkerchief_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_handkerchief_all <- validate_filter(corpus_all, handkerchief_ids_all,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_handkerchief_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% handkerchief_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% handkerchief_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_handkerchief_all$filter_T_hc_T)
b_t$documents$texts[1:67]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_handkerchief_all$filter_T_hc_F)
b_f$documents$texts[1:2]


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
                          pattern = "Ombrelle|Parasol|Parresol",
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
b_t$documents$texts[1:42]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_umbrella_all$filter_T_hc_F)
b_f$documents$texts[1:121]


## Gloves and Muffs
hand <- tagfilter_hand()

hand_ids_1734 <- hand$filtrate(corpus_1734,ignore.case = T)
hand_ids_1834 <- hand$filtrate(corpus_1834,ignore.case = T)

hand_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                    hand_ids_1734)
hand_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    hand_ids_1834)

hand_subset_all <- c(hand_subset_1734, hand_subset_1834)
hand_ids_all <- c(hand_ids_1734, hand_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
hand_kwic <- kwic(hand_subset_all,
                      pattern = "Schlupfer|Schlüpfer|Handschuh",
                      valuetype = "regex",
                      ignore.case = T)
hand_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
hand_subset_clean <- hand_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(hand_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_hand_all <- validate_filter(corpus_all, hand_ids_all,
                                           search_col = "adcontent",
                                           pattern = "01textilien")
validation_hand_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% hand_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% hand_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_hand_all$filter_T_hc_T)
b_t$documents$texts[1:138]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_hand_all$filter_T_hc_F)
b_f$documents$texts[1:13]


## Scarves, Colars, and Neckties
neck <- tagfilter_neck()

neck_ids_1734 <- neck$filtrate(corpus_1734,ignore.case = T)
neck_ids_1834 <- neck$filtrate(corpus_1834,ignore.case = T)

neck_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                    neck_ids_1734)
neck_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    neck_ids_1834)

neck_subset_all <- c(neck_subset_1734, neck_subset_1834)
neck_ids_all <- c(neck_ids_1734, neck_ids_1834)


# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
neck_kwic <- kwic(neck_subset_all,
                  pattern = "Halstuch|Foulard|Schal|Schawl|Shaul|Chal|Fichu",
                  valuetype = "regex",
                  ignore.case = T)
neck_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
neck_subset_clean <- neck_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(neck_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_neck_all <- validate_filter(corpus_all, neck_ids_all,
                                       search_col = "adcontent",
                                       pattern = "01textilien")
validation_neck_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% neck_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% neck_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_neck_all$filter_T_hc_T)
b_t$documents$texts[1:205]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_neck_all$filter_T_hc_F)
b_f$documents$texts[1:4]



## Headdress and Wigs
headdress <- tagfilter_headdress()

headdress_ids_1734 <- headdress$filtrate(corpus_1734,ignore.case = T)
headdress_ids_1834 <- headdress$filtrate(corpus_1834,ignore.case = T)

headdress_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                         headdress_ids_1734)
headdress_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    headdress_ids_1834)

headdress_subset_all <- c(headdress_subset_1734, headdress_subset_1834)
headdress_ids_all <- c(headdress_ids_1734, headdress_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
headdress_kwic <- kwic(headdress_subset_all,
                  pattern = "Haube",
                  valuetype = "regex",
                  ignore.case = T)
headdress_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
headdress_subset_clean <- headdress_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(headdress_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_headdress_all <- validate_filter(corpus_all, headdress_ids_all,
                                       search_col = "adcontent",
                                       pattern = "01textilien")
validation_headdress_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% headdress_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% headdress_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_headdress_all$filter_T_hc_T)
b_t$documents$texts[1:210]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_headdress_all$filter_T_hc_F)
b_f$documents$texts[1:54]



## Certain Types of Fabric/ Textile Material
# includes some "Pfeifenspitzen", where "Pfeifen" is not right before "Spitzen" - how to exclude those?
texmaterial <- tagfilter_texmaterial()

texmaterial_ids_1734 <- texmaterial$filtrate(corpus_1734,ignore.case = T)
texmaterial_ids_1834 <- texmaterial$filtrate(corpus_1834,ignore.case = T)

texmaterial_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                           texmaterial_ids_1734)
texmaterial_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                           texmaterial_ids_1834)

texmaterial_subset_all <- c(texmaterial_subset_1734, texmaterial_subset_1834)
texmaterial_ids_all <- c(texmaterial_ids_1734, texmaterial_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
texmaterial_kwic <- kwic(texmaterial_subset_all,
                       pattern = "Wachslappen|Alépin",
                       valuetype = "regex",
                       ignore.case = T)
texmaterial_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
texmaterial_subset_clean <- texmaterial_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(texmaterial_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_texmaterial_1834 <- validate_filter(corpus_1834, texmaterial_ids_1834,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_texmaterial_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% texmaterial_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% texmaterial_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_texmaterial_all$filter_T_hc_T)
b_t$documents$texts[1:560]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_texmaterial_1834$filter_T_hc_F)
b_f$documents$texts[1:62]


## Unspecified Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_ids_1734 <- cloth$filtrate(corpus_1734,ignore.case = T)
cloth_ids_1834 <- cloth$filtrate(corpus_1834,ignore.case = T)

cloth_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                     cloth_ids_1734)
cloth_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     cloth_ids_1834)

cloth_subset_all <- c(cloth_subset_1734, cloth_subset_1834)
cloth_ids_all <- c(cloth_ids_1734, cloth_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cloth_kwic <- kwic(cloth_subset_all,
                       pattern = "Reste|Zeug|Tuch|Tüch|Geflecht|Etoffe|Gewebe",
                       valuetype = "regex",
                   ignore.case = T)

cloth_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cloth_subset_clean <- cloth_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cloth_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_cloth_all <- validate_filter(corpus_all, cloth_ids_all,
                                              search_col = "adcontent",
                                              pattern = "01textilien")
validation_cloth_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% cloth_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% cloth_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cloth_all$filter_T_hc_T)
b_t$documents$texts[1:342]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cloth_all$filter_T_hc_F)
b_f$documents$texts[1:9]


## Yarn
yarn <- tagfilter_yarn()

yarn_ids_1734 <- yarn$filtrate(corpus_1734,ignore.case = T)
yarn_ids_1834 <- yarn$filtrate(corpus_1834,ignore.case = T)

yarn_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                    yarn_ids_1734)
yarn_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    yarn_ids_1834)

yarn_subset_all <- c(yarn_subset_1734, yarn_subset_1834)
yarn_ids_all <- c(yarn_ids_1734, yarn_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
yarn_kwic <- kwic(yarn_subset_all,
                 pattern = "Strickseide|Strickwolle|Strickbaum",
                 valuetype = "regex",
                 ignore.case = T)

yarn_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
yarn_subset_clean <- yarn_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(yarn_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_yarn_all <- validate_filter(corpus_all, yarn_ids_all,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_yarn_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% yarn_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% yarn_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_yarn_all$filter_T_hc_T)
b_t$documents$texts[1:116]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_yarn_all$filter_T_hc_F)
b_f$documents$texts[1:5]


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



## Household Textiles
household_textile <- tagfilter_household_textile()

household_textile_ids_1734 <- household_textile$filtrate(corpus_1734,ignore.case = T)
household_textile_ids_1834 <- household_textile$filtrate(corpus_1834,ignore.case = T)

household_textile_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                                 household_textile_ids_1734)
household_textile_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                                 household_textile_ids_1834)

household_textile_subset_all <- c(household_textile_subset_1734, household_textile_subset_1834)
household_textile_ids_all <- c(household_textile_ids_1734, household_textile_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
household_textile_kwic <- kwic(household_textile_subset_all,
                               pattern = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|
    Bethzeug|Bettsack|Bethsack|Decke|Strochsack|Strohsäck|Kissen|Unterbett",
                               valuetype = "regex")

household_textile_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
household_textile_subset_clean <- household_textile_subset_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(household_textile_subset_clean),
                   max_words = 100)


# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_household_textile_all <- validate_filter(corpus_all, household_textile_ids_all,
                                      search_col = "adcontent",
                                      pattern = "01textilien")
validation_household_textile_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% household_textile_ids_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% household_textile_ids_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_household_textile_all$filter_T_hc_T)
b_t$documents$texts[1:181]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_household_textile_all$filter_T_hc_F)
b_f$documents$texts[1:35]


### using existing categories for wordclouds and dfm to find missing objects in dictionaries
# creating subset of textile objects for sale
textiles_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent) & grepl("01kauf", finance))
buy_1834 <- corpus_subset(corpus_1834, grepl("01kauf", finance) & grepl("01biete", adtype))
textiles_1734 <- corpus_subset(corpus_1734, grepl("01textilien", adcontent) & grepl("01kauf", finance))
buy_1734 <- corpus_subset(corpus_1734, grepl("01kauf", finance) & grepl("01biete", adtype))
textiles_all <- c(textiles_1734, textiles_1834)

# cleaning subset
textiles_all_clean <- textiles_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

# textplot of most frequent words overall
textplot_wordcloud(dfm(textiles_all_clean),
                   max_words = 200)


# tokenize ads of subcorpus
textiles_tok <- textiles_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

# creating document feature matrix
textiles_dfm <- textiles_tok %>%
  dfm()

textiles_df <- docfreq(textiles_dfm)

textiles_s_df <- sort(textiles_df,decreasing = T)

# exporting csv with sorted document feature matrix to Desktop
write.csv2(textiles_s_df, file = "data/textiles_s_df_1834.csv", fileEncoding = "UTF-8")


### showing ids for documents (ads) NOT classified as textiles by automatic classification, but classified manually as "01textilien"
# looking at textiles_1834 corpus
textiles_all

# negating %in% operator
`%notin%` <- Negate(`%in%`)

# creating a corpus of all the ads (ids) not found by automatic classification of ads for sale/ to buy
textiles_missed <- corpus_subset(textiles_all, docvars(textiles_all, "id") %notin%
                                    c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all, costume_ids_all, shoes_ids_all,
                                      handkerchief_ids_all, umbrella_ids_all, hand_ids_all, neck_ids_all, headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                                      animalraw_ids_all, plantraw_ids_all, mercery_ids_all, bag_ids_all, household_textile_ids_all))

textiles_aut_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                 c(clothing_ids_1734, sleepwear_ids_1734, uniform_ids_1734, underwear_ids_1734, outerwear_ids_1734, costume_ids_1734, shoes_ids_1734,
                                   handkerchief_ids_1734, umbrella_ids_1734, hand_ids_1734, neck_ids_1734, headdress_ids_1734, texmaterial_ids_1734, cloth_ids_1734, yarn_ids_1734,
                                   animalraw_ids_1734, plantraw_ids_1734, mercery_ids_1734, bag_ids_1734, household_textile_ids_1734))

textiles_aut_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     c(clothing_ids_1834, sleepwear_ids_1834, uniform_ids_1834, underwear_ids_1834, outerwear_ids_1834, costume_ids_1834, shoes_ids_1834,
                                       handkerchief_ids_1834, umbrella_ids_1834, hand_ids_1834, neck_ids_1834, headdress_ids_1834, texmaterial_ids_1834, cloth_ids_1834, yarn_ids_1834,
                                       animalraw_ids_1834, plantraw_ids_1834, mercery_ids_1834, bag_ids_1834, household_textile_ids_1834))


tex_ids_automatic <- c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all, costume_ids_all, shoes_ids_all,
                       handkerchief_ids_all, umbrealla_ids_all, hand_ids_all, neck_ids_all, headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                       animalraw_ids_all, plantraw_ids_all, mercery_ids_all, bag_ids_all, household_textile_ids_all)

textiles_missed_texts <- textiles_missed$documents$texts

# write csv for texts of missed ads
write.csv2(textiles_missed_texts, file = "data/textiles_missed_1834.csv", fileEncoding = "UTF-8")

# 21/01/2020: 88 missed ads (by automated classification)
# most often wrongly classified manually ("Bett" and "spanische Wand" and upholstery is very often classified as textile)
# some French ads
# not recognized due to ocr mistakes (Gane/Gaze)
# few with "Wolle" or "Atlas" not recognizes (difficult to sort out negatives)
# artificial flowers are not included (Blumenarbeit as another dictionary?)

# 20/02/2020: only 125 missed tex ads in 1734 and 1834 combined, most of them actually wrongly classified as textile manually (mistakes see above)

# creating a corpus of all for sale/ to buy ads (ids) found automatically, but not by manual classification
textiles_ids <- textiles_all$documents$id

not_textiles <- corpus_subset(corpus_all, docvars(corpus_all, "id") %notin%
                                textiles_ids)

#### In the following code something doesn't seem to work... When checking,
# "not_textiles" most ads that are indeed tagged manually as "textile"!!! Why?
# textile_oops gets 838 hits, while manually adding up the false negatives for each dictionary only results in 521 hits (and most of them are actually correct!)

textiles_oops <- corpus_subset(not_textiles, docvars(not_textiles, "id") %in%
                                  tex_ids_automatic)

textiles_oops_texts <- textiles_oops$documents$texts

write.csv2(textiles_oops_texts, file = "data/textiles_oops_texts.csv", fileEncoding = "UTF-8")

textiles_oops_texts[100:150]

# creating a corpus of all ads recognised by automated textiles filters
textiles_filters <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all, costume_ids_all, shoes_ids_all,
                                       handkerchief_ids_all, umbrella_ids_all, hand_ids_all, neck_ids_all, headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                                       animalraw_ids_all, plantraw_ids_all, mercery_ids_all, bag_ids_all, household_textile_ids_all))

