# Load libraries and source functions
library(avisblatt)

# During Development rather run
devtools::load_all()

# corpus 1734

corpus_1734 <- avis_create_corpus("data/groundtruth1734.csv")

## is the following code still necessary, or is avis_create_corpus enough?
# avis_1734 <- readtext("data/groundtruth1734.csv",
                      # text_field = "text",
                      # encoding = "UTF-8")

# avis_1734$text <- correct_ocr(avis_1734$text)

# ids_by_lang <- fromJSON("data/ids_by_lang.json")
#corpus_1734_all <- corpus(avis_1734,
                          # docid_field = "doc_id")
# corpus_1734 <- corpus_subset(corpus_1734_all,
                             # (docvars(corpus_1734_all,"id") %in%
                                # ids_by_lang$de))

# corpus 1834

corpus_1834 <- avis_create_corpus("data/groundtruth1834.csv")

corpus_all <- c(corpus_1734, corpus_1834)

# negating %in% operator
`%notin%` <- Negate(`%in%`)

### checking and cleaning different tagfilters for misc things

## Mercery and Non Textile Accessoires
mercery <- tagfilter_mercery()

mercery_1734 <- mercery$filtrate(corpus_1734,ignore.case = T)
mercery_1834 <- mercery$filtrate(corpus_1834,ignore.case = T)

mercery_all <- c(mercery_1734, mercery_1834)

texts(mercery_1734)
texts(mercery_1834)

# attaching result of filter "mercery" as new docvars (no idea, if this is the "right" way to do it, but it works)
# how to get those docvars back to the main corpus? How to store multiple new "keywords/categories" for a single document?
mercery_1734_new$categories <- "mercery"
docvars(mercery_1734_new)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mercery_kwic <- kwic(mercery_all,
                     pattern = "Kn[o|ö]pf",
                     valuetype = "regex",
                     ignore.case = T)

mercery_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
mercery_subset_clean <- mercery_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mercery_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_mercery_all <- validate_filter(corpus_all, mercery_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_mercery_all

# Filters for TRUE and FALSE positives
doc_ids_mercery <- names(mercery_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_mercery
filter_T_hc_F <- doc_ids_all %notin% doc_ids_mercery

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_mercery_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_mercery_all$filter_T_hc_F)

texts(b_f)



## Bags and Purses
bag <- tagfilter_bag()

bag_1734 <- bag$filtrate(corpus_1734,ignore.case = T)
bag_1834 <- bag$filtrate(corpus_1834,ignore.case = T)

bag_all <- c(bag_1734, bag_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bag_kwic <- kwic(bag_all,
                     pattern = "...",
                     valuetype = "regex",
                     ignore.case = T)

bag_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
bag_subset_clean <- bag_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bag_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_bag_all <- validate_filter(corpus_all, bag_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_bag_all

# Filters for TRUE and FALSE positives
doc_ids_bag <- names(bag_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_bag
filter_T_hc_F <- doc_ids_all %notin% doc_ids_bag

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_bag_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_bag_all$filter_T_hc_F)

texts(b_f)

## Animal Raw Materials
animalraw <- tagfilter_animalraw()

animalraw_1734 <- animalraw$filtrate(corpus_1734,ignore.case = T)
animalraw_1834 <- animalraw$filtrate(corpus_1834,ignore.case = T)

animalraw_all <- c(animalraw_1734, animalraw_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
animalraw_kwic <- kwic(animalraw_all,
                       pattern = "Federen|Pflaum|Flaum|Sohlleder|Sohl-Leder|Zeugleder",
                       valuetype = "regex",
                       ignore.case = T)

animalraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
animalraw_subset_clean <- animalraw_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(animalraw_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_animalraw_all <- validate_filter(corpus_all, animalraw_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_animalraw_all

# Filters for TRUE and FALSE positives
doc_ids_animalraw <- names(animalraw_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_animalraw
filter_T_hc_F <- doc_ids_all %notin% doc_ids_animalraw

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_animalraw_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_animalraw_all$filter_T_hc_F)

texts(b_f)


## Plant Raw Materials
plantraw <- tagfilter_plantraw()

plantraw_1734 <- plantraw$filtrate(corpus_1734,ignore.case = T)
plantraw_1834 <- plantraw$filtrate(corpus_1834,ignore.case = T)

plantraw_all <- c(plantraw_1734, plantraw_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
plantraw_kwic <- kwic(plantraw_all,
                      pattern = "Waldhaar",
                      valuetype = "regex",
                      ignore.case = T)

plantraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
plantraw_subset_clean <- plantraw_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(plantraw_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_plantraw_all <- validate_filter(corpus_all, plantraw_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_plantraw_all

# Filters for TRUE and FALSE positives
doc_ids_plantraw <- names(plantraw_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_plantraw
filter_T_hc_F <- doc_ids_all %notin% doc_ids_plantraw

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_plantraw_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_plantraw_all$filter_T_hc_F)

texts(b_f)


## Umbrellas
umbrella <- tagfilter_umbrella()

umbrella_1734 <- umbrella$filtrate(corpus_1734,ignore.case = T)
umbrella_1834 <- umbrella$filtrate(corpus_1834,ignore.case = T)

umbrella_all <- c(umbrella_1734, umbrella_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
umbrella_kwic <- kwic(umbrella_all,
                      pattern = "\\bSchirme",
                      valuetype = "regex",
                      ignore.case = T)
umbrella_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
umbrella_subset_clean <- umbrella_all %>%
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
validation_umbrella_all <- validate_filter(corpus_all, umbrella_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_umbrella_all

# Filters for TRUE and FALSE positives
doc_ids_umbrella <- names(umbrella_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_umbrella
filter_T_hc_F <- doc_ids_all %notin% doc_ids_umbrella

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_umbrella_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_umbrella_all$filter_T_hc_F)

texts(b_f)


## Carriages
carriage <- tagfilter_carriage()

carriage_1734 <- carriage$filtrate(corpus_1734, ignore.case = T)
carriage_1834 <- carriage$filtrate(corpus_1834, ignore.case = T)

carriage_all <- c(carriage_1734, carriage_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carriage_kwic <- kwic(carriage_all,
                      pattern = "W(a|aa)gen-Winde|W(a|aa)genwinde",
                      valuetype = "regex",
                      ignore.case = T)
carriage_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
carriage_all_clean <- carriage_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(carriage_all_clean),
                   max_words = 200)


## Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_carriage_all <- validate_filter(corpus_all, carriage_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_carriage_all

# Filters for TRUE and FALSE positives
doc_ids_carriage <- names(carriage_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_carriage
filter_T_hc_F <- doc_ids_all %notin% doc_ids_carriage

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_carriage_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_carriage_all$filter_T_hc_F)

texts(b_f)

# No FALSE positives and TRUE positives all correct!!!

## Pushchairs
pushchair <- tagfilter_pushchair()

pushchair_1734 <- pushchair$filtrate(corpus_1734, ignore.case = T)
pushchair_1834 <- pushchair$filtrate(corpus_1834, ignore.case = T)

pushchair_all <- c(pushchair_1734, pushchair_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
pushchair_kwic <- kwic(pushchair_all,
                       pattern = "Korbw(a|ä|ae)g|Kinderw(a|ä|ae)g|Kinder(chais|schäs)|Korb-W(ag|äg|ae)|Kinder-W(a|ä|ae)g|Kinder-(Chais|Schäs)",
                       valuetype = "regex",
                       ignore.case = T)
pushchair_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
pushchair_subset_clean <- pushchair_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(pushchair_subset_clean),
                   max_words = 100)

## Validation
validation_pushchair_all <- validate_filter(corpus_all, pushchair_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_pushchair_all

# Filters for TRUE and FALSE positives
doc_ids_pushchair <- names(pushchair_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_pushchair
filter_T_hc_F <- doc_ids_all %notin% doc_ids_pushchair

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_pushchair_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_pushchair_all$filter_T_hc_F)

texts(b_f)


##  Storage
storage <- tagfilter_storage()

storage_1734 <- storage$filtrate(corpus_1734, ignore.case = T)
storage_1834 <- storage$filtrate(corpus_1834, ignore.case = T)

storage_all <- c(storage_1734, storage_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
storage_kwic <- kwic(storage_all,
                     pattern = "Trog",
                     valuetype = "regex",
                     ignore.case = T)
storage_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
storage_subset_clean <- storage_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(storage_subset_clean),
                   max_words = 100)

## Validation
validation_storage_all <- validate_filter(corpus_all, storage_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_storage_all

# Filters for TRUE and FALSE positives
doc_ids_storage <- names(storage_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_storage
filter_T_hc_F <- doc_ids_all %notin% doc_ids_storage

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_storage_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_storage_all$filter_T_hc_F)

texts(b_f)

## Building Components
building <- tagfilter_building()

building_1734 <- building$filtrate(corpus_1734, ignore.case = T)
building_1834 <- building$filtrate(corpus_1834, ignore.case = T)

building_all <- c(building_1734, building_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
building_kwic <- kwic(building_all,
                      pattern = "Gegitter|Gatter|Stiege|Ziegel|Deichel|Steinplatte|Jalousie-Läden|Jalousieläden|Kellerb(ö|o)gen", # steinerner Bogen
                      valuetype = "regex",
                      ignore.case = T)
building_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
building_subset_clean <- building_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(building_subset_clean),
                   max_words = 100)

## Validation
validation_building_all <- validate_filter(corpus_all, building_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_building_all

# Filters for TRUE and FALSE positives
doc_ids_building <- names(building_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_building
filter_T_hc_F <- doc_ids_all %notin% doc_ids_building

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_building_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_building_all$filter_T_hc_F)

texts(b_f)

## Suitcases and Travel Bags
suitcase <- tagfilter_suitcase()

suitcase_1734 <- suitcase$filtrate(corpus_1734, ignore.case = T)
suitcase_1834 <- suitcase$filtrate(corpus_1834, ignore.case = T)

suitcase_all <- c(suitcase_1734, suitcase_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
suitcase_kwic <- kwic(suitcase_all,
                      pattern = "Wagen",
                      valuetype = "regex",
                      ignore.case = T)
suitcase_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
suitcase_subset_clean <- suitcase_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(suitcase_subset_clean),
                   max_words = 100)

## Validation
validation_suitcase_all <- validate_filter(corpus_all, suitcase_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_suitcase_all

# Filters for TRUE and FALSE positives
doc_ids_suitcase <- names(suitcase_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_suitcase
filter_T_hc_F <- doc_ids_all %notin% doc_ids_suitcase

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_suitcase_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_suitcase_all$filter_T_hc_F)

texts(b_f)

## Measuring instruments
measure <- tagfilter_measure()

measure_1734 <- measure$filtrate(corpus_1734, ignore.case = T)
measure_1834 <- measure$filtrate(corpus_1834, ignore.case = T)

measure_all <- c(measure_1734, measure_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
measure_kwic <- kwic(measure_all,
                     pattern = "Wagen",
                     valuetype = "regex",
                     ignore.case = T)
measure_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
measure_subset_clean <- measure_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(measure_subset_clean),
                   max_words = 100)

## Validation
validation_measure_all <- validate_filter(corpus_all, measure_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_measure_all

# Filters for TRUE and FALSE positives
doc_ids_measure <- names(measure_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_measure
filter_T_hc_F <- doc_ids_all %notin% doc_ids_measure

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_measure_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_measure_all$filter_T_hc_F)

texts(b_f)

## Trolleys
trolley <- tagfilter_trolley()

trolley_1734 <- trolley$filtrate(corpus_1734, ignore.case = T)
trolley_1834 <- trolley$filtrate(corpus_1834, ignore.case = T)

trolley_all <- c(trolley_1734, trolley_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
trolley_kwic <- kwic(trolley_all,
                 pattern = "Wagenkette|(Leit|Zug)seil|Fuhrwerk",
                 valuetype = "regex",
                 ignore.case = T)
trolley_kwic

## Validation
validation_trolley_all <- validate_filter(corpus_all, trolley_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_trolley_all

# Filters for TRUE and FALSE positives
doc_ids_trolley <- names(trolley_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_trolley
filter_T_hc_F <- doc_ids_all %notin% doc_ids_trolley

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_trolley_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_trolley_all$filter_T_hc_F)

texts(b_f)


##  Health, Cosmetics and Drugstore Products
health <- tagfilter_health()

health_1734 <- health$filtrate(corpus_1734, ignore.case = T)
health_1834 <- health$filtrate(corpus_1834, ignore.case = T)

health_all <- c(health_1734, health_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
health_kwic <- kwic(health_all,
                 pattern = "Brocke(l|li)-A(mm|m|ml|mml)ung|Brocke(l|li)a(mm|m|ml|mml)ung",
                 valuetype = "regex",
                 ignore.case = T)
health_kwic

## Validation
validation_health_all <- validate_filter(corpus_all, health_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_health_all

# Filters for TRUE and FALSE positives
doc_ids_health <- names(health_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_health
filter_T_hc_F <- doc_ids_all %notin% doc_ids_health

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_health_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_health_all$filter_T_hc_F)

texts(b_f)


## Weapons
weapon <- tagfilter_weapon()

weapon_1734 <- weapon$filtrate(corpus_1734, ignore.case = T)
weapon_1834 <- weapon$filtrate(corpus_1834, ignore.case = T)

weapon_all <- c(weapon_1734, weapon_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
weapon_kwic <- kwic(weapon_all,
                 pattern = "Kuge(l|ln)model|Kuge(l|ln)-Model|Stutzer|Pistohl|Ordonnanz-G",
                 valuetype = "regex",
                 ignore.case = T)
weapon_kwic

## Validation
validation_weapon_all <- validate_filter(corpus_all, weapon_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_weapon_all

# Filters for TRUE and FALSE positives
doc_ids_weapon <- names(weapon_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_weapon
filter_T_hc_F <- doc_ids_all %notin% doc_ids_weapon

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_weapon_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_weapon_all$filter_T_hc_F)

texts(b_f)


## Shop Equipment
shopequip <- tagfilter_shopequip()

shopequip_1734 <- shopequip$filtrate(corpus_1734, ignore.case = T)
shopequip_1834 <- shopequip$filtrate(corpus_1834, ignore.case = T)

shopequip_all <- c(shopequip_1734, shopequip_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shopequip_kwic <- kwic(shopequip_all,
                 pattern = phrase("Einsatz Gewicht"),
                 valuetype = "regex",
                 ignore.case = T)
shopequip_kwic

## Validation
validation_shopequip_all <- validate_filter(corpus_all, shopequip_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_shopequip_all

# Filters for TRUE and FALSE positives
doc_ids_shopequip <- names(shopequip_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_shopequip
filter_T_hc_F <- doc_ids_all %notin% doc_ids_shopequip

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_shopequip_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_shopequip_all$filter_T_hc_F)

texts(b_f)


## Tools and Instruments
tool <- tagfilter_tool()

tool_1734 <- tool$filtrate(corpus_1734, ignore.case = T)
tool_1834 <- tool$filtrate(corpus_1834, ignore.case = T)

tool_all <- c(tool_1734, tool_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tool_kwic <- kwic(tool_all,
                 pattern = phrase("eisernes Rädlein"),
                 valuetype = "regex",
                 ignore.case = T)
tool_kwic

## Validation
validation_tool_all <- validate_filter(corpus_all, tool_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_tool_all

# Filters for TRUE and FALSE positives
doc_ids_tool <- names(tool_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_tool
filter_T_hc_F <- doc_ids_all %notin% doc_ids_tool

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_tool_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_tool_all$filter_T_hc_F)

texts(b_f)


## Stationary and Paperware
stationary <- tagfilter_stationary()

stationary_1734 <- stationary$filtrate(corpus_1734, ignore.case = T)
stationary_1834 <- stationary$filtrate(corpus_1834, ignore.case = T)

stationary_all <- c(stationary_1734, stationary_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
stationary_kwic <- kwic(stationary_all,
                  pattern = "Papier",
                  valuetype = "regex",
                  ignore.case = T)
stationary_kwic

## Validation
validation_stationary_all <- validate_filter(corpus_all, stationary_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_stationary_all

# Filters for TRUE and FALSE positives
doc_ids_stationary <- names(stationary_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_stationary
filter_T_hc_F <- doc_ids_all %notin% doc_ids_stationary

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_stationary_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_stationary_all$filter_T_hc_F)

texts(b_f)

## Jewellery
jewellery <- tagfilter_jewellery()

jewellery_1734 <- jewellery$filtrate(corpus_1734, ignore.case = T)
jewellery_1834 <- jewellery$filtrate(corpus_1834, ignore.case = T)

jewellery_all <- c(jewellery_1734, jewellery_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
jewellery_kwic <- kwic(jewellery_all,
                 pattern = "Kette",
                 valuetype = "regex",
                 ignore.case = T)
jewellery_kwic

## Validation
validation_jewellery_all <- validate_filter(corpus_all, jewellery_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_jewellery_all

# Filters for TRUE and FALSE positives
doc_ids_jewellery <- names(jewellery_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_jewellery
filter_T_hc_F <- doc_ids_all %notin% doc_ids_jewellery

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_jewellery_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_jewellery_all$filter_T_hc_F)

texts(b_f)


## Wood
wood <- tagfilter_wood()

wood_1734 <- wood$filtrate(corpus_1734, ignore.case = T)
wood_1834 <- wood$filtrate(corpus_1834, ignore.case = T)

wood_all <- c(wood_1734, wood_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wood_kwic <- kwic(wood_all,
                 pattern = "S(a|ä)gsp(ä|a|äh|ah)n|Lohst(o|ö)ck|Drehsp(äh|ä)n",
                 valuetype = "regex",
                 ignore.case = T)
wood_kwic

## Validation
validation_wood_all <- validate_filter(corpus_all, wood_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_wood_all

# Filters for TRUE and FALSE positives
doc_ids_wood <- names(wood_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_wood
filter_T_hc_F <- doc_ids_all %notin% doc_ids_wood

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_wood_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_wood_all$filter_T_hc_F)

texts(b_f)

## Barrels and Bottles
barrel <- tagfilter_barrel()

barrel_1734 <- barrel$filtrate(corpus_1734, ignore.case = T)
barrel_1834 <- barrel$filtrate(corpus_1834, ignore.case = T)

barrel_all <- c(barrel_1734, barrel_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
barrel_kwic <- kwic(barrel_all,
                 pattern = "(Ö|Oe)hlst(u|ü)cklein|Z(a|ä)pfen",
                 valuetype = "regex",
                 ignore.case = T)
barrel_kwic

## Validation
validation_barrel_all <- validate_filter(corpus_all, barrel_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_barrel_all

# Filters for TRUE and FALSE positives
doc_ids_barrel <- names(barrel_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_barrel
filter_T_hc_F <- doc_ids_all %notin% doc_ids_barrel

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_barrel_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_barrel_all$filter_T_hc_F)

texts(b_f)


## Tobacco and Related Objects
tobacco <- tagfilter_tobacco()

tobacco_1734 <- tobacco$filtrate(corpus_1734, ignore.case = T)
tobacco_1834 <- tobacco$filtrate(corpus_1834, ignore.case = T)

tobacco_all <- c(tobacco_1734, tobacco_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tobacco_kwic <- kwic(tobacco_all,
                 pattern = "Tabattier",
                 valuetype = "regex",
                 ignore.case = T)
tobacco_kwic

## Validation
validation_tobacco_all <- validate_filter(corpus_all, tobacco_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_tobacco_all

# Filters for TRUE and FALSE positives
doc_ids_tobacco <- names(tobacco_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_tobacco
filter_T_hc_F <- doc_ids_all %notin% doc_ids_tobacco

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_tobacco_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_tobacco_all$filter_T_hc_F)

texts(b_f)

## Hay and Straw
hay <- tagfilter_hay()

hay_1734 <- hay$filtrate(corpus_1734, ignore.case = T)
hay_1834 <- hay$filtrate(corpus_1834, ignore.case = T)

hay_all <- c(hay_1734, hay_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
hay_kwic <- kwic(hay_all,
                 pattern = phrase("Gra(s|ss|ß) auf"),
                 valuetype = "regex",
                 ignore.case = T)
hay_kwic

## Validation
validation_hay_all <- validate_filter(corpus_all, hay_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_hay_all

# Filters for TRUE and FALSE positives
doc_ids_hay <- names(hay_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_hay
filter_T_hc_F <- doc_ids_all %notin% doc_ids_hay

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_hay_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_hay_all$filter_T_hc_F)

texts(b_f)

## Unspecified Wooden Objects
woodobject <- tagfilter_woodobject()

woodobject_1734 <- woodobject$filtrate(corpus_1734, ignore.case = T)
woodobject_1834 <- woodobject$filtrate(corpus_1834, ignore.case = T)

woodobject_all <- c(woodobject_1734, woodobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
woodobject_kwic <- kwic(woodobject_all,
                 pattern = "Holzw(a|aa)re",
                 valuetype = "regex",
                 ignore.case = T)
woodobject_kwic

## Validation
validation_woodobject_all <- validate_filter(corpus_all, woodobject_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_woodobject_all

# Filters for TRUE and FALSE positives
doc_ids_woodobject <- names(woodobject_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_woodobject
filter_T_hc_F <- doc_ids_all %notin% doc_ids_woodobject

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_woodobject_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_woodobject_all$filter_T_hc_F)

texts(b_f)


## Dung
dung <- tagfilter_dung()

dung_1734 <- dung$filtrate(corpus_1734, ignore.case = T)
dung_1834 <- dung$filtrate(corpus_1834, ignore.case = T)

dung_all <- c(dung_1734, dung_1834)
dung_ids_all <- c(dung_1734, dung_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
dung_kwic <- kwic(dung_all,
                 pattern = "Bau\\b|K(ü|u)hbau|Pfer(d|de)bau|Schwei(n|ne)bau|Taubenmist|Ziegenbau",
                 valuetype = "regex",
                 ignore.case = T)
dung_kwic

## Validation
validation_dung_all <- validate_filter(corpus_all, dung_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_dung_all

# Filters for TRUE and FALSE positives
doc_ids_dung <- names(dung_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_dung
filter_T_hc_F <- doc_ids_all %notin% doc_ids_dung

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_dung_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_dung_all$filter_T_hc_F)

texts(b_f)

## Plants
plant <- tagfilter_plant()

plant_1734 <- plant$filtrate(corpus_1734, ignore.case = T)
plant_1834 <- plant$filtrate(corpus_1834, ignore.case = T)

plant_all <- c(plant_1734, plant_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
plant_kwic <- kwic(plant_all,
                 pattern = "Bu(chs|x)",
                 valuetype = "regex",
                 ignore.case = T)
plant_kwic

## Validation
validation_plant_all <- validate_filter(corpus_all, plant_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_plant_all

# Filters for TRUE and FALSE positives
doc_ids_plant <- names(plant_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_plant
filter_T_hc_F <- doc_ids_all %notin% doc_ids_plant

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_plant_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_plant_all$filter_T_hc_F)

texts(b_f)


## Glasses and Optical Instruments
glasses <- tagfilter_glasses()

glasses_1734 <- glasses$filtrate(corpus_1734, ignore.case = T)
glasses_1834 <- glasses$filtrate(corpus_1834, ignore.case = T)

glasses_all <- c(glasses_1734, glasses_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
glasses_kwic <- kwic(glasses_all,
                 pattern = "Telescop|Optik",
                 valuetype = "regex",
                 ignore.case = T)
glasses_kwic

## Validation
validation_glasses_all <- validate_filter(corpus_all, glasses_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_glasses_all

# Filters for TRUE and FALSE positives
doc_ids_glasses <- names(glasses_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_glasses
filter_T_hc_F <- doc_ids_all %notin% doc_ids_glasses

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_glasses_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_glasses_all$filter_T_hc_F)

texts(b_f)

## Soil, Gravel, Lime and Related Goods
soil <- tagfilter_soil()

soil_1734 <- soil$filtrate(corpus_1734, ignore.case = T)
soil_1834 <- soil$filtrate(corpus_1834, ignore.case = T)

soil_all <- c(soil_1734, soil_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
soil_kwic <- kwic(soil_all,
                 pattern = "Asphalt|Asche|Mattengrund|Matten-Grund",
                 valuetype = "regex",
                 ignore.case = T)
soil_kwic

## Validation
validation_soil_all <- validate_filter(corpus_all, soil_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_soil_all

# Filters for TRUE and FALSE positives
doc_ids_soil <- names(soil_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_soil
filter_T_hc_F <- doc_ids_all %notin% doc_ids_soil

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_soil_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_soil_all$filter_T_hc_F)

texts(b_f)

## Agriculture
agriculture <- tagfilter_agriculture()

agriculture_1734 <- agriculture$filtrate(corpus_1734, ignore.case = T)
agriculture_1834 <- agriculture$filtrate(corpus_1834, ignore.case = T)

agriculture_all <- c(agriculture_1734, agriculture_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
agriculture_kwic <- kwic(agriculture_all,
                 pattern = "Hacken",
                 valuetype = "regex",
                 ignore.case = T)
agriculture_kwic

## Validation
validation_agriculture_all <- validate_filter(corpus_all, agriculture_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_agriculture_all

# Filters for TRUE and FALSE positives
doc_ids_agriculture <- names(agriculture_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_agriculture
filter_T_hc_F <- doc_ids_all %notin% doc_ids_agriculture

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_agriculture_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_agriculture_all$filter_T_hc_F)

texts(b_f)

## Riding Objects
riding <- tagfilter_riding()

riding_1734 <- riding$filtrate(corpus_1734, ignore.case = T)
riding_1834 <- riding$filtrate(corpus_1834, ignore.case = T)

riding_all <- c(riding_1734, riding_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
riding_kwic <- kwic(riding_all,
                 pattern = "Pfer(ded|dd)ecke",
                 valuetype = "regex",
                 ignore.case = T)
riding_kwic

## Validation
validation_riding_all <- validate_filter(corpus_all, riding_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_riding_all

# Filters for TRUE and FALSE positives
doc_ids_riding <- names(riding_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_riding
filter_T_hc_F <- doc_ids_all %notin% doc_ids_riding

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_riding_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_riding_all$filter_T_hc_F)

texts(b_f)

## Objects Related to Wells and Fountains
well <- tagfilter_well()

well_1734 <- well$filtrate(corpus_1734, ignore.case = T)
well_1834 <- well$filtrate(corpus_1834, ignore.case = T)

well_all <- c(well_1734, well_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
well_kwic <- kwic(well_all,
                 pattern = "Brunnstiefel|Brunn-Stiefel",
                 valuetype = "regex",
                 ignore.case = T)
well_kwic

## Validation
validation_well_all <- validate_filter(corpus_all, well_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_well_all

# Filters for TRUE and FALSE positives
doc_ids_well <- names(well_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_well
filter_T_hc_F <- doc_ids_all %notin% doc_ids_well

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_well_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_well_all$filter_T_hc_F)

texts(b_f)

## Naturalia
naturalia <- tagfilter_naturalia()

naturalia_1734 <- naturalia$filtrate(corpus_1734, ignore.case = T)
naturalia_1834 <- naturalia$filtrate(corpus_1834, ignore.case = T)

naturalia_all <- c(naturalia_1734, naturalia_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
naturalia_kwic <- kwic(naturalia_all,
                 pattern = "Muscheln|Schmetterling|Versteinerung|Mineralien",
                 valuetype = "regex",
                 ignore.case = T)
naturalia_kwic

## Validation
validation_naturalia_all <- validate_filter(corpus_all, naturalia_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_naturalia_all

# Filters for TRUE and FALSE positives
doc_ids_naturalia <- names(naturalia_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_naturalia
filter_T_hc_F <- doc_ids_all %notin% doc_ids_naturalia

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_naturalia_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_naturalia_all$filter_T_hc_F)

texts(b_f)

## Containers
container <- tagfilter_container()

container_1734 <- container$filtrate(corpus_1734, ignore.case = T)
container_1834 <- container$filtrate(corpus_1834, ignore.case = T)

container_all <- c(container_1734, container_1834)
container_ids_all <- c(container_1734, container_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
container_kwic <- kwic(container_all,
                 pattern = "Z(u|ü)ber|Eimer|Trog|Bö(ck|k)lin|Bo(ck|k)te|Bu(ck|k)te",
                 valuetype = "regex",
                 ignore.case = T)
container_kwic

## Validation
validation_container_all <- validate_filter(corpus_all, container_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_container_all

# Filters for TRUE and FALSE positives
doc_ids_container <- names(container_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_container
filter_T_hc_F <- doc_ids_all %notin% doc_ids_container

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_container_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_container_all$filter_T_hc_F)

texts(b_f)

## Firestarters
firestart <- tagfilter_firestart()

firestart_1734 <- firestart$filtrate(corpus_1734, ignore.case = T)
firestart_1834 <- firestart$filtrate(corpus_1834, ignore.case = T)

firestart_all <- c(firestart_1734, firestart_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
firestart_kwic <- kwic(firestart_all,
                 pattern = "Schwefelh(o|ö)lz|Zündh(o|ö)lz",
                 valuetype = "regex",
                 ignore.case = T)
firestart_kwic

## Validation
validation_firestart_all <- validate_filter(corpus_all, firestart_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_firestart_all

# Filters for TRUE and FALSE positives
doc_ids_firestart <- names(firestart_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_firestart
filter_T_hc_F <- doc_ids_all %notin% doc_ids_firestart

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_firestart_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_firestart_all$filter_T_hc_F)

texts(b_f)

## Fire Extinguisher
extinguisher <- tagfilter_extinguisher()

extinguisher_1734 <- extinguisher$filtrate(corpus_1734, ignore.case = T)
extinguisher_1834 <- extinguisher$filtrate(corpus_1834, ignore.case = T)

extinguisher_all <- c(extinguisher_1734, extinguisher_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
extinguisher_kwic <- kwic(extinguisher_all,
                 pattern = phrase("Feuer Eimer"),
                 valuetype = "regex",
                 ignore.case = T)
extinguisher_kwic

## Validation
validation_extinguisher_all <- validate_filter(corpus_all, extinguisher_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_extinguisher_all

# Filters for TRUE and FALSE positives
doc_ids_extinguisher <- names(extinguisher_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_extinguisher
filter_T_hc_F <- doc_ids_all %notin% doc_ids_extinguisher

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_extinguisher_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_extinguisher_all$filter_T_hc_F)

texts(b_f)

## Fireworks
firework <- tagfilter_firework()

firework_1734 <- firework$filtrate(corpus_1734, ignore.case = T)
firework_1834 <- firework$filtrate(corpus_1834, ignore.case = T)

firework_all <- c(firework_1734, firework_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
firework_kwic <- kwic(firework_all,
                 pattern = "Feuerwerk",
                 valuetype = "regex",
                 ignore.case = T)
firework_kwic

## Validation
validation_firework_all <- validate_filter(corpus_all, firework_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_firework_all

# Filters for TRUE and FALSE positives
doc_ids_firework <- names(firework_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_firework
filter_T_hc_F <- doc_ids_all %notin% doc_ids_firework

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_firework_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_firework_all$filter_T_hc_F)

texts(b_f)

## Antiques
antique <- tagfilter_antique()

antique_1734 <- antique$filtrate(corpus_1734, ignore.case = T)
antique_1834 <- antique$filtrate(corpus_1834, ignore.case = T)

antique_all <- c(antique_1734, antique_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
antique_kwic <- kwic(antique_all,
                 pattern = phrase("römischen Figur"),
                 valuetype = "regex",
                 ignore.case = T)
antique_kwic

## Validation
validation_antique_all <- validate_filter(corpus_all, antique_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_antique_all

# Filters for TRUE and FALSE positives
doc_ids_antique <- names(antique_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_antique
filter_T_hc_F <- doc_ids_all %notin% doc_ids_antique

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_antique_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_antique_all$filter_T_hc_F)

texts(b_f)

## Keys
key <- tagfilter_key()

key_1734 <- key$filtrate(corpus_1734, ignore.case = T)
key_1834 <- key$filtrate(corpus_1834, ignore.case = T)

key_all <- c(key_1734, key_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
key_kwic <- kwic(key_all,
                 pattern = "Hausschlüssel|Schrankschlüssel|Uhrenschlüssel",
                 valuetype = "regex",
                 ignore.case = T)
key_kwic

## Validation
validation_key_all <- validate_filter(corpus_all, key_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_key_all

# Filters for TRUE and FALSE positives
doc_ids_key <- names(key_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_key
filter_T_hc_F <- doc_ids_all %notin% doc_ids_key

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_key_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_key_all$filter_T_hc_F)

texts(b_f)

## Walking canes
cane <- tagfilter_cane()

cane_1734 <- cane$filtrate(corpus_1734, ignore.case = T)
cane_1834 <- cane$filtrate(corpus_1834, ignore.case = T)

cane_all <- c(cane_1734, cane_1834)
cane_ids_all <- c(cane_1734, cane_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cane_kwic <- kwic(cane_all,
                 pattern = "Spazierst(o|ö)ck",
                 valuetype = "regex",
                 ignore.case = T)
cane_kwic

## Validation
validation_cane_all <- validate_filter(corpus_all, cane_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_cane_all

# Filters for TRUE and FALSE positives
doc_ids_cane <- names(cane_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_cane
filter_T_hc_F <- doc_ids_all %notin% doc_ids_cane

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_cane_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_cane_all$filter_T_hc_F)

texts(b_f)



## Objects Related to Wine
wineobject <- tagfilter_wineobject()

wineobject_1734 <- wineobject$filtrate(corpus_1734, ignore.case = T)
wineobject_1834 <- wineobject$filtrate(corpus_1834, ignore.case = T)

wineobject_all <- c(wineobject_1734, wineobject_1834)
wineobject_ids_all <- c(wineobject_1734, wineobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wineobject_kwic <- kwic(wineobject_all,
                 pattern = "Trotte",
                 valuetype = "regex",
                 ignore.case = T)
wineobject_kwic

## Validation
validation_wineobject_all <- validate_filter(corpus_all, wineobject_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_wineobject_all

# Filters for TRUE and FALSE positives
doc_ids_wineobject <- names(wineobject_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_wineobject
filter_T_hc_F <- doc_ids_all %notin% doc_ids_wineobject

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_wineobject_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_wineobject_all$filter_T_hc_F)

texts(b_f)

## Ropes
rope <- tagfilter_rope()

rope_1734 <- rope$filtrate(corpus_1734, ignore.case = T)
rope_1834 <- rope$filtrate(corpus_1834, ignore.case = T)

rope_all <- c(rope_1734, rope_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
rope_kwic <- kwic(rope_all,
                 pattern = "Strick|Seil",
                 valuetype = "regex",
                 ignore.case = T)
rope_kwic

## Validation
validation_rope_all <- validate_filter(corpus_all, rope_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_rope_all

# Filters for TRUE and FALSE positives
doc_ids_rope <- names(rope_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_rope
filter_T_hc_F <- doc_ids_all %notin% doc_ids_rope

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_rope_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_rope_all$filter_T_hc_F)

texts(b_f)
## Objects for Taverns and Inns
tavernobject <- tagfilter_tavernobject()

tavernobject_1734 <- tavernobject$filtrate(corpus_1734, ignore.case = T)
tavernobject_1834 <- tavernobject$filtrate(corpus_1834, ignore.case = T)

tavernobject_all <- c(tavernobject_1734, tavernobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tavernobject_kwic <- kwic(tavernobject_all,
                 pattern = "Wir(th|t)sschild|Wir(t|th)schaftsgerät",
                 valuetype = "regex",
                 ignore.case = T)
tavernobject_kwic

## Validation
validation_tavernobject_all <- validate_filter(corpus_all, tavernobject_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_tavernobject_all

# Filters for TRUE and FALSE positives
doc_ids_tavernobject <- names(tavernobject_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_tavernobject
filter_T_hc_F <- doc_ids_all %notin% doc_ids_tavernobject

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_tavernobject_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_tavernobject_all$filter_T_hc_F)

texts(b_f)

## Animal Feed
feed <- tagfilter_feed()

feed_1734 <- feed$filtrate(corpus_1734, ignore.case = T)
feed_1834 <- feed$filtrate(corpus_1834, ignore.case = T)

feed_all <- c(feed_1734, feed_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
feed_kwic <- kwic(feed_all,
                 pattern = phrase("zumverfüttern"),
                 valuetype = "regex",
                 ignore.case = T)
feed_kwic

## Validation
validation_feed_all <- validate_filter(corpus_all, feed_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_feed_all

# Filters for TRUE and FALSE positives
doc_ids_feed <- names(feed_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_feed
filter_T_hc_F <- doc_ids_all %notin% doc_ids_feed

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_feed_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_feed_all$filter_T_hc_F)

texts(b_f)

## Misc Objects
miscobject <- tagfilter_miscobject()

miscobject_1734 <- miscobject$filtrate(corpus_1734, ignore.case = T)
miscobject_1834 <- miscobject$filtrate(corpus_1834, ignore.case = T)

miscobject_all <- c(miscobject_1734, miscobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
miscobject_kwic <- kwic(miscobject_all,
                 pattern = "Magnet|Waarenlager",
                 valuetype = "regex",
                 ignore.case = T)
miscobject_kwic

## Validation
validation_miscobject_all <- validate_filter(corpus_all, miscobject_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_miscobject_all

# Filters for TRUE and FALSE positives
doc_ids_miscobject <- names(miscobject_all)
doc_ids_all <- names(corpus_all)
filter_T_hc_T <- doc_ids_all %in% doc_ids_miscobject
filter_T_hc_F <- doc_ids_all %notin% doc_ids_miscobject

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_miscobject_all$filter_T_hc_T)

texts(b_t)

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     names(corpus_all) %in%
                       validation_miscobject_all$filter_T_hc_F)

texts(b_f)



### showing ids for documents (ads) NOT classified as things (ding) by automatic classification, but classified manually as "ding"


# creating a corpus of all the ads (ids) not found by automatic classification of ads for sale/ to buy

things_manual_1734 <- corpus_subset(corpus_1734, grepl("ding", adcontent))

things_manual_1834 <- corpus_subset(corpus_1834, grepl("ding", adcontent))

things_auto_1734 <- c(docvars(mercery_1734, "id"), docvars(bag_1734, "id"), dovars(animalraw_1734, "id"), dovars(plantraw_1734, "id"), dovars(umbrella_1734, "id"),
                      dovars(carriage_1734, "id"), dovars(pushchair_1734, "id"), dovars(storage_1734, "id"), dovars(building_1734, "id"), dovars(suitcase_1734, "id"),
                      dovars(measure_1734, "id"), dovars(trolley_1734, "id"), dovars(health_1734, "id"), dovars(weapon_1734, "id"), dovars(shopequip_1734, "id"),
                      dovars(tool_1734, "id"), dovars(stationary_1734, "id"), dovars(jewellery_1734, "id"), dovars(woodobject_1734, "id"), dovars(barrel_1734, "id"),
                      dovars(tobacco_1734, "id"), dovars(hay_1734, "id"), dovars(woodobject_1734, "id"), dovars(dung_1734, "id"), dovars(plant_1734, "id"),
                      dovars(glasses_1734, "id"), dovars(soil_1734, "id"), dovars(agriculture_1734, "id"), dovars(riding_1734, "id"), dovars(well_1734, "id"),
                      dovars(naturalia_1734, "id"), dovars(container_1734, "id"), dovars(firestart_1734, "id"), dovars(extinguisher_1734, "id"), dovars(firework_1734, "id"),
                      dovars(antique_1734, "id"), dovars(key_1734, "id"), dovars(cane_1734, "id"), dovars(wineobject_1734, "id"), dovars(tavernobject_1734, "id"),
                      dovars(rope_1734, "id"), dovars(feed_1734, "id"), dovars(miscobject_1734, "id"))

things_auto_1834 <- c(docvars(mercery_1834, "id"), docvars(bag_1834, "id"), dovars(animalraw_1834, "id"), dovars(plantraw_1834, "id"), dovars(umbrella_1834, "id"),
                      dovars(carriage_1834, "id"), dovars(pushchair_1834, "id"), dovars(storage_1834, "id"), dovars(building_1834, "id"), dovars(suitcase_1834, "id"),
                      dovars(measure_1834, "id"), dovars(trolley_1834, "id"), dovars(health_1834, "id"), dovars(weapon_1834, "id"), dovars(shopequip_1834, "id"),
                      dovars(tool_1834, "id"), dovars(stationary_1834, "id"), dovars(jewellery_1834, "id"), dovars(woodobject_1834, "id"), dovars(barrel_1834, "id"),
                      dovars(tobacco_1834, "id"), dovars(hay_1834, "id"), dovars(woodobject_1834, "id"), dovars(dung_1834, "id"), dovars(plant_1834, "id"),
                      dovars(glasses_1834, "id"), dovars(soil_1834, "id"), dovars(agriculture_1834, "id"), dovars(riding_1834, "id"), dovars(well_1834, "id"),
                      dovars(naturalia_1834, "id"), dovars(container_1834, "id"), dovars(firestart_1834, "id"), dovars(extinguisher_1834, "id"), dovars(firework_1834, "id"),
                      dovars(antique_1834, "id"), dovars(key_1834, "id"), dovars(cane_1834, "id"), dovars(wineobject_1834, "id"), dovars(tavernobject_1834, "id"),
                      dovars(rope_1834, "id"), dovars(feed_1834, "id"), dovars(miscobject_1834, "id"))

things_missed_1734 <- corpus_subset(things_manual_1734, docvars(things_manual_1734, "id") %notin% docvars(things_auto_1734, "id"))

write.csv2(things_missed_1734, file = "data/things_missed_1734.csv", fileEncoding = "UTF-8")

things_missed_1834 <- corpus_subset(things_manual_1834, docvars(things_manual_1834, "id") %notin% docvars(things_auto_1834, "id"))

write.csv2(things_missed_1834, file = "data/things_missed_1834.csv", fileEncoding = "UTF-8")

