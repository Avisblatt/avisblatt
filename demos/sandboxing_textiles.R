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
                  pattern = "Kleid|Kleyd|Rock|Röck|Ärmel|Weste",
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
b_t$documents$texts[1:91]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_clothing_all$filter_T_hc_F)
b_f$documents$texts[1:26]


## Sleepwear
sleepwear <- tagfilter_sleepwear()

sleepwear_ids <- sleepwear$filtrate(corpus_1834,ignore.case = T)

sleepwear_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    sleepwear_ids)

sleepwear_texts <- sleepwear_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
sleepwear_kwic <- kwic(sleepwear_subset,
                      pattern = "Schlafrock|Schlafröck|Nachtärmel",
                      valuetype = "regex",
                      ignore.case = T)
sleepwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
sleepwear_subset_clean <- sleepwear_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(sleepwear_subset_clean),
                   max_words = 200)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_sleepwear <- validate_filter(corpus_1834, sleepwear_ids,
                                       search_col = "adcontent",
                                       pattern = "01textilien")
validation_sleepwear



## Military/ Uniforms
uniform <- tagfilter_uniform()

uniform_ids <- uniform$filtrate(corpus_1834,ignore.case = T)

uniform_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  uniform_ids)

uniform_texts <- uniform_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
uniform_kwic <- kwic(uniform_subset,
                       pattern = "Epaulett",
                       valuetype = "regex",
                       ignore.case = T)
uniform_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
uniform_subset_clean <- uniform_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(uniform_subset_clean),
                   max_words = 200)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_uniform <- validate_filter(corpus_1834, uniform_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_uniform


## Underwear
underwear <- tagfilter_underwear()

underwear_ids <- underwear$filtrate(corpus_1834,ignore.case = T)

underwear_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    underwear_ids)

underwear_texts <- underwear_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
underwear_kwic <- kwic(underwear_subset,
                     pattern = "Socke|Strumpf|Strümpf",
                     valuetype = "regex",
                     ignore.case = T)
underwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
underwear_subset_clean <- underwear_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(underwear_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_underwear <- validate_filter(corpus_1834, underwear_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_underwear


## Outerwear
outerwear <- tagfilter_outerwear()

outerwear_ids <- outerwear$filtrate(corpus_1834,ignore.case = T)

outerwear_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    outerwear_ids)

outerwear_texts <- outerwear_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
outerwear_kwic <- kwic(outerwear_subset,
                       pattern = "Mantel|Mäntel|Coat|Cotte|Cols|Schbrack|Mantille|Kittelein|Pellerine",
                       valuetype = "regex",
                       ignore.case = T)
outerwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
outerwear_subset_clean <- outerwear_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(outerwear_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_outerwear <- validate_filter(corpus_1834, outerwear_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_outerwear


## Costumes/ Special Occasion Garments
costume <- tagfilter_costume()

costume_ids <- costume$filtrate(corpus_1834,ignore.case = T)

costume_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  costume_ids)

costume_texts <- costume_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
costume_kwic <- kwic(costume_subset,
                       pattern = "Taufe",
                       valuetype = "regex",
                       ignore.case = T)
costume_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
costume_subset_clean <- costume_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(costume_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_costume <- validate_filter(corpus_1834, costume_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_costume


## Shoes
shoes <- tagfilter_shoes()

shoes_ids <- shoes$filtrate(corpus_1834,ignore.case = F)

shoes_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                shoes_ids)

shoes_texts <- shoes_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shoes_kwic <- kwic(shoes_subset,
                      pattern = "[S|s]chuh|[S|s]chüh|[S|s]tiefel[S|s]ohle|[S|s]öhle|[P|p]antoffel",
                      valuetype = "regex")
shoes_kwic

# some "schuh" as measurement remain, e.g. "Länge 3 1/ 2 Schuh"
# excluding these through regex of "Länge, Breite etc." in proximity of 3 words to "Schuh"?


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
shoes_subset_clean <- shoes_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(shoes_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_shoes <- validate_filter(corpus_1834, shoes_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_shoes


## Handkerchiefs
handkerchief <- tagfilter_handkerchief()

handkerchief_ids <- handkerchief$filtrate(corpus_1834,ignore.case = T)

handkerchief_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       handkerchief_ids)

handkerchief_texts <- handkerchief_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
handkerchief_kwic <- kwic(handkerchief_subset,
                     pattern = "Schnupftuch|Mouchoir|Sacktuch|Sacktüch",
                     valuetype = "regex",
                     ignore.case = T)
handkerchief_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
handkerchief_subset_clean <- handkerchief_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(handkerchief_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_handkerchief <- validate_filter(corpus_1834, handkerchief_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_handkerchief


## Umbrellas
umbrella <- tagfilter_umbrella()

umbrella_ids <- umbrella$filtrate(corpus_1834,ignore.case = T)

umbrella_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   umbrella_ids)

umbrella_texts <- umbrella_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
umbrella_kwic <- kwic(umbrella_subset,
                          pattern = "Ombrelle|Parasol|Parresol",
                          valuetype = "regex",
                          ignore.case = T)
umbrella_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
umbrella_subset_clean <- umbrella_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(umbrella_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_umbrella <- validate_filter(corpus_1834, umbrella_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_umbrella



## Gloves and Muffs
hand <- tagfilter_hand()

hand_ids <- hand$filtrate(corpus_1834,ignore.case = T)

hand_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               hand_ids)

hand_texts <- hand_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
hand_kwic <- kwic(hand_subset,
                      pattern = "Schlupfer|Schlüpfer|Handschuh",
                      valuetype = "regex",
                      ignore.case = T)
hand_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
hand_subset_clean <- hand_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(hand_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_hand <- validate_filter(corpus_1834, hand_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_hand



## Scarves, Colars, and Neckties
neck <- tagfilter_neck()

neck_ids <- neck$filtrate(corpus_1834,ignore.case = T)

neck_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               neck_ids)

neck_texts <- neck_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
neck_kwic <- kwic(neck_subset,
                  pattern = "Halstuch|Foulard|Schal|Schawl|Shaul|Chal|Fichu",
                  valuetype = "regex",
                  ignore.case = T)
neck_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
neck_subset_clean <- neck_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(neck_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_neck <- validate_filter(corpus_1834, neck_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_neck


## Headdress and Wigs
headdress <- tagfilter_headdress()

headdress_ids <- headdress$filtrate(corpus_1834,ignore.case = T)

headdress_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    headdress_ids)

headdress_texts <- headdress_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
headdress_kwic <- kwic(headdress_subset,
                  pattern = "Haube",
                  valuetype = "regex",
                  ignore.case = T)
headdress_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
headdress_subset_clean <- headdress_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(headdress_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_headdress <- validate_filter(corpus_1834, headdress_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_headdress



## Certain Types of Fabric/ Textile Material
texmaterial <- tagfilter_texmaterial()

texmaterial_ids <- texmaterial$filtrate(corpus_1834,ignore.case = T)

texmaterial_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                      texmaterial_ids)

texmaterial_texts <- texmaterial_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
texmaterial_kwic <- kwic(texmaterial_subset,
                       pattern = "Wachslappen|Alépin",
                       valuetype = "regex",
                       ignore.case = T)
texmaterial_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
texmaterial_subset_clean <- texmaterial_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(texmaterial_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_texmaterial <- validate_filter(corpus_1834, texmaterial_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_texmaterial


## Unspecified Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_ids <- cloth$filtrate(corpus_1834,ignore.case = T)

cloth_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                cloth_ids)

cloth_texts <- cloth_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cloth_kwic <- kwic(cloth_subset,
                       pattern = "Reste|Zeug|Tuch|Tüch|Geflecht|Etoffe|Gewebe",
                       valuetype = "regex",
                   ignore.case = T)

cloth_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cloth_subset_clean <- cloth_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cloth_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_cloth <- validate_filter(corpus_1834, cloth_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_cloth


## Yarn
yarn <- tagfilter_yarn()

yarn_ids <- yarn$filtrate(corpus_1834,ignore.case = T)

yarn_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               yarn_ids)

yarn_texts <- yarn_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
yarn_kwic <- kwic(yarn_subset,
                 pattern = "Strickseide|Strickwolle|Strickbaum",
                 valuetype = "regex",
                 ignore.case = T)

yarn_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
yarn_subset_clean <- yarn_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(yarn_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_yarn <- validate_filter(corpus_1834, yarn_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_yarn


## Animal Raw Materials
animalraw <- tagfilter_animalraw()

animalraw_ids <- animalraw$filtrate(corpus_1834,ignore.case = T)

animalraw_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                 animalraw_ids)

animalraw_texts <- animalraw_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
animalraw_kwic <- kwic(animalraw_subset,
                   pattern = "Pferdhaar",
                   valuetype = "regex",
                   ignore.case = T)

animalraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
animalraw_subset_clean <- animalraw_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(animalraw_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_animalraw <- validate_filter(corpus_1834, animalraw_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_animalraw


## Plant Raw Materials
plantraw <- tagfilter_plantraw()

plantraw_ids <- plantraw$filtrate(corpus_1834,ignore.case = T)

plantraw_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   plantraw_ids)

plantraw_texts <- plantraw_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
plantraw_kwic <- kwic(plantraw_subset,
                       pattern = "Waldhaar",
                       valuetype = "regex",
                       ignore.case = T)

plantraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
plantraw_subset_clean <- plantraw_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(plantraw_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_plantraw <- validate_filter(corpus_1834, plantraw_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_plantraw


## Mercery and Non Textile Accessoires
mercery <- tagfilter_mercery()

mercery_ids <- mercery$filtrate(corpus_1834,ignore.case = T)

mercery_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  mercery_ids)

mercery_texts <- mercery_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mercery_kwic <- kwic(mercery_subset,
                    pattern = "Kn[o|ö]pf",
                    valuetype = "regex",
                    ignore.case = T)

mercery_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
mercery_subset_clean <- mercery_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mercery_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_mercery <- validate_filter(corpus_1834, mercery_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_mercery


## Bags and Purses
bag <- tagfilter_bag()

bag_ids <- bag$filtrate(corpus_1834,ignore.case = T)

bag_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                              bag_ids)

bag_texts <- bag_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bag_kwic <- kwic(bag_subset,
                       pattern = "Tasche|Seckel|Beutel|Säck|Ridicule",
                       valuetype = "regex",
                       ignore.case = T)

bag_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
bag_subset_clean <- bag_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bag_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_bag <- validate_filter(corpus_1834, bag_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_bag


## Household Textiles
household_textile <- tagfilter_household_textile()

household_textile_ids <- household_textile$filtrate(corpus_1834,ignore.case = T)

household_textile_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                            household_textile_ids)

household_textile_texts <- household_textile_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
household_textile_kwic <- kwic(household_textile_subset,
                               pattern = "Bodentüch",
                               valuetype = "regex")

household_textile_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
household_textile_subset_clean <- household_textile_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(household_textile_subset_clean),
                   max_words = 100)


# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_household_textile <- validate_filter(corpus_1834, household_textile_ids,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_household_textile


### using existing categories for wordclouds and dfm to find missing objects in dictionaries
# creating subset of textile objects for sale
textiles_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent) & grepl("01kauf", finance))

# cleaning subset
textiles_1834_clean <- textiles_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

# textplot of most frequent words
textplot_wordcloud(dfm(textiles_1834_clean),
                   max_words = 400)

# tokenize ads of subcorpus
textiles_tok <- textiles_1834 %>%
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
textiles_1834

# negating %in% operator
`%notin%` <- Negate(`%in%`)

# creating a corpus of all the ads (ids) not found by automatic classification of ads for sale/ to buy
textiles_missed <- corpus_subset(textiles_1834, docvars(textiles_1834, "id") %notin%
                                    c(clothing_ids, sleapwear_ids, uniform_ids, underwear_ids, outerwear_ids, costume_ids, shoes_ids,
                                      handkerchief_ids, hand_ids, neck_ids, headdress_ids, texmaterial_ids, cloth_ids, yarn_ids,
                                      animalraw_ids, plantraw_ids, mercery_ids, bag_ids, household_textile_ids))

tex_id_automatic <- c(clothing_ids, sleapwear_ids, uniform_ids, underwear_ids, outerwear_ids, costume_ids, shoes_ids, handkerchief_ids,
                      hand_ids, neck_ids, headdress_ids, texmaterial_ids, cloth_ids, yarn_ids, animalraw_ids, plantraw_ids, mercery_ids,
                      bag_ids, household_textile_ids)

textiles_missed_texts <- textiles_missed$documents$texts

# write csv for texts of missed ads
write.csv2(textiles_missed_texts, file = "data/textiles_missed.csv", fileEncoding = "UTF-8")

# 21/01/2020: 88 missed ads (by automated classification)
# most often wrongly classified manually ("Bett" and "spanische Wand" and upholstery is very often classified as textile)
# some French ads
# not recognized due to ocr mistakes (Gane/Gaze)
# few with "Wolle" or "Atlas" not recognizes (difficult to sort out negatives)
# some should be included through dictionaries, no idea why they are not (e.g. Regenschirm)
# artificial flowers are not included (Blumenarbeit as another dictionary?)



# creating a corpus of all for sale/ to buy ads (ids) found automatically, but not by manual classification
textiles_ids <- textiles_1834$documents$id

not_textiles <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %notin%
                                textiles_ids)

#### In the following code something doesn't seem to work... When checking,
# "not_textiles" includes a lot of ads that are indeed tagged manually as "textile"!!! Why?


textiles_oops <- corpus_subset(not_textiles, docvars(not_textiles, "id") %in%
                                  tex_id_automatic)

textiles_oops_texts <- textiles_oops$documents$texts

write.csv2(textiles_oops_texts, file = "data/textiles_oops_texts.csv", fileEncoding = "UTF-8")

textiles_oops_texts[100:150]

# creating a corpus of all ads recognised by automated textiles filters
textiles_filters <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     c(clothing_ids, sleapwear_ids, uniform_ids, underwear_ids, outerwear_ids, costume_ids, shoes_ids,
                                       handkerchief_ids, hand_ids, neck_ids, headdress_ids, texmaterial_ids, cloth_ids, yarn_ids,
                                       animalraw_ids, plantraw_ids, mercery_ids, bag_ids, household_textile_ids))

