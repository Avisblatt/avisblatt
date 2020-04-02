# Load libraries and source functions
library(avisblatt)

# During Development rather run
devtools::load_all()

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

mercery_1734 <- mercery$filtrate(corpus_1734,ignore.case = T)
mercery_1834 <- mercery$filtrate(corpus_1834,ignore.case = T)

mercery_all <- c(mercery_1734, mercery_1834)

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
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% mercery_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% mercery_all)]

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
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bag_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bag_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bag_all$filter_T_hc_T)
b_t$documents$texts[1:93]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bag_all$filter_T_hc_F)
b_f$documents$texts[1:49]


## Animal Raw Materials
animalraw <- tagfilter_animalraw()

animalraw_1734 <- animalraw$filtrate(corpus_1734,ignore.case = T)
animalraw_1834 <- animalraw$filtrate(corpus_1834,ignore.case = T)

animalraw_all <- c(animalraw_1734, animalraw_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
animalraw_kwic <- kwic(animalraw_all,
                       pattern = "Pferdhaar",
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
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% animalraw_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% animalraw_all)]

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
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% plantraw_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% plantraw_all)]

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
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% umbrella_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% umbrella_all)]

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

carriage_1734 <- carriage$filtrate(corpus_1734, ignore.case = T)
carriage_1834 <- carriage$filtrate(corpus_1834, ignore.case = T)

carriage_all <- c(carriage_1734, carriage_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carriage_kwic <- kwic(carriage_all,
                      pattern = "(1|2|3|4)spännig|Schlitten|(T|D)ro(sch|tsch)ke|Chai(se|s)|Trosque|Wägen|Kutsch//b|Schwanenhäls|
                      Haußschlitten|Geschell|Berline//b|Kutschen-Kasich|Kutschenkasich",
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
validation_carriage <- validate_filter(corpus_all, carriage_all,
                                       search_col = "adcontent",
                                       pattern = "06ding")
validation_carriage

# Filters for TRUE and FALSE positives
doc_ids <- corpus_1734$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% carriage_1734)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% carriage_1734)]

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
                                            pattern = "02hausrat")
validation_pushchair_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% pushchair_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% pushchair_all)]

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
                                          pattern = "ding")
validation_storage_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% storage_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% storage_all)]

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

building_1734 <- building$filtrate(corpus_1734, ignore.case = T)
building_1834 <- building$filtrate(corpus_1834, ignore.case = T)

building_all <- c(building_1734, building_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
building_kwic <- kwic(building_all,
                      pattern = "Fenster-",
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
                                           pattern = "hausrat")
validation_building_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% building_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% building_all)]

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
                                           pattern = "ding")
validation_suitcase_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% suitcase_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% suitcase_all)]

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
                                          pattern = "02hausrat")
validation_measure_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% measure_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% measure_all)]

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
                                      pattern = "06ding")
validation_trolley_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% trolley_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% trolley_all)]

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

health_1734 <- health$filtrate(corpus_1734, ignore.case = T)
health_1834 <- health$filtrate(corpus_1834, ignore.case = T)

health_all <- c(health_1734, health_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
health_kwic <- kwic(health_all,
                 pattern = phrase("(K|C)(o|ö)(l|ll)ni(sch|sches) Wasser"),
                 valuetype = "regex",
                 ignore.case = T)
health_kwic

## Validation
validation_health_all <- validate_filter(corpus_all, health_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_health_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% health_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% health_all)]

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

weapon_1734 <- weapon$filtrate(corpus_1734, ignore.case = T)
weapon_1834 <- weapon$filtrate(corpus_1834, ignore.case = T)

weapon_all <- c(weapon_1734, weapon_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
weapon_kwic <- kwic(weapon_all,
                 pattern = "Scheide",
                 valuetype = "regex",
                 ignore.case = T)
weapon_kwic

## Validation
validation_weapon_all <- validate_filter(corpus_all, weapon_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_weapon_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% weapon_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% weapon_all)]

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

shopequip_1734 <- shopequip$filtrate(corpus_1734, ignore.case = T)
shopequip_1834 <- shopequip$filtrate(corpus_1834, ignore.case = T)

shopequip_all <- c(shopequip_1734, shopequip_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shopequip_kwic <- kwic(shopequip_all,
                 pattern = "Ladengerä(th|t)schaft",
                 valuetype = "regex",
                 ignore.case = T)
shopequip_kwic

## Validation
validation_shopequip_all <- validate_filter(corpus_all, shopequip_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_shopequip_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% shopequip_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% shopequip_all)]

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

tool_1734 <- tool$filtrate(corpus_1734, ignore.case = T)
tool_1834 <- tool$filtrate(corpus_1834, ignore.case = T)

tool_all <- c(tool_1734, tool_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tool_kwic <- kwic(tool_all,
                 pattern = "Kupferdruckerpre(ss|ß)|Schraubst(o|ö)ck|M(ü|a)hlstein|Wendelbaum|F(u|ü)gbl(o|ö)ch|Drucktisch|
                 Brennhäu(s|ß)lein|Reibstein",
                 valuetype = "regex",
                 ignore.case = T)
tool_kwic

## Validation
validation_tool_all <- validate_filter(corpus_all, tool_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_tool_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tool_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tool_all)]

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

stationary_1734 <- stationary$filtrate(corpus_1734, ignore.case = T)
stationary_1834 <- stationary$filtrate(corpus_1834, ignore.case = T)

stationary_all <- c(stationary_1734, stationary_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
stationary_kwic <- kwic(stationary_all,
                  pattern = "Schreibzeug",
                  valuetype = "regex",
                  ignore.case = T)
stationary_kwic

## Validation
validation_stationary_all <- validate_filter(corpus_all, stationary_all,
                                       search_col = "adcontent",
                                       pattern = "06ding")
validation_stationary_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% stationary_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% stationary_all)]

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

jewellery_1734 <- jewellery$filtrate(corpus_1734, ignore.case = T)
jewellery_1834 <- jewellery$filtrate(corpus_1834, ignore.case = T)

jewellery_all <- c(jewellery_1734, jewellery_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
jewellery_kwic <- kwic(jewellery_all,
                 pattern = "Bijouteri",
                 valuetype = "regex",
                 ignore.case = T)
jewellery_kwic

## Validation
validation_jewellery_all <- validate_filter(corpus_all, jewellery_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_jewellery_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% jewellery_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% jewellery_all)]

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
                                      pattern = "06ding")
validation_wood_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% wood_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% wood_all)]

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

barrel_1734 <- barrel$filtrate(corpus_1734, ignore.case = T)
barrel_1834 <- barrel$filtrate(corpus_1834, ignore.case = T)

barrel_all <- c(barrel_1734, barrel_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
barrel_kwic <- kwic(barrel_all,
                 pattern = phrase("Stücklein Fa(s|ß|ss)"),
                 valuetype = "regex",
                 ignore.case = T)
barrel_kwic

## Validation
validation_barrel_all <- validate_filter(corpus_all, barrel_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_barrel_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% barrel_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% barrel_all)]

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

tobacco_1734 <- tobacco$filtrate(corpus_1734, ignore.case = T)
tobacco_1834 <- tobacco$filtrate(corpus_1834, ignore.case = T)

tobacco_all <- c(tobacco_1734, tobacco_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tobacco_kwic <- kwic(tobacco_all,
                 pattern = "Pfeife|Taba(k|ck|ks|cks)pfeife|Pfeifenkopf|Pfeifenraumer|Pfeifenrohr",
                 valuetype = "regex",
                 ignore.case = T)
tobacco_kwic

## Validation
validation_tobacco_all <- validate_filter(corpus_all, tobacco_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_tobacco_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tobacco_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tobacco_all)]

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

hay_1734 <- hay$filtrate(corpus_1734, ignore.case = T)
hay_1834 <- hay$filtrate(corpus_1834, ignore.case = T)

hay_all <- c(hay_1734, hay_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
hay_kwic <- kwic(hay_all,
                 pattern = "Stroh\\b",
                 valuetype = "regex",
                 ignore.case = T)
hay_kwic

## Validation
validation_hay_all <- validate_filter(corpus_all, hay_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_hay_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% hay_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% hay_all)]

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

woodobject_1734 <- woodobject$filtrate(corpus_1734, ignore.case = T)
woodobject_1834 <- woodobject$filtrate(corpus_1834, ignore.case = T)

woodobject_all <- c(woodobject_1734, woodobject_1834)
woodobject_ids_all <- c(woodobject_1734, woodobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
woodobject_kwic <- kwic(woodobject_all,
                 pattern = "Holzdecke|H(ö|o)lzenwerk|H(ö|o)lzwerk",
                 valuetype = "regex",
                 ignore.case = T)
woodobject_kwic

## Validation
validation_woodobject_all <- validate_filter(corpus_all, woodobject_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_woodobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% woodobject_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% woodobject_all)]

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
                                      pattern = "06ding")
validation_dung_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% dung_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% dung_all)]

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
                                      pattern = "06ding")
validation_plant_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% plant_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% plant_all)]

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

glasses_1734 <- glasses$filtrate(corpus_1734, ignore.case = T)
glasses_1834 <- glasses$filtrate(corpus_1834, ignore.case = T)

glasses_all <- c(glasses_1734, glasses_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
glasses_kwic <- kwic(glasses_all,
                 pattern = phrase("optische Instrumente"),
                 valuetype = "regex",
                 ignore.case = T)
glasses_kwic

## Validation
validation_glasses_all <- validate_filter(corpus_all, glasses_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_glasses_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% glasses_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% glasses_all)]

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

soil_1734 <- soil$filtrate(corpus_1734, ignore.case = T)
soil_1834 <- soil$filtrate(corpus_1834, ignore.case = T)

soil_all <- c(soil_1734, soil_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
soil_kwic <- kwic(soil_all,
                 pattern = phrase("G(y|i)ps"),
                 valuetype = "regex",
                 ignore.case = T)
soil_kwic

## Validation
validation_soil_all <- validate_filter(corpus_all, soil_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_soil_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% soil_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% soil_all)]

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

agriculture_1734 <- agriculture$filtrate(corpus_1734, ignore.case = T)
agriculture_1834 <- agriculture$filtrate(corpus_1834, ignore.case = T)

agriculture_all <- c(agriculture_1734, agriculture_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
agriculture_kwic <- kwic(agriculture_all,
                 pattern = "Dreschflegel|(Heu|Lad|Lade)gabel|Pfl(u|ü)g|Sense|Rechen|Mattenmesser|Matten-Messer|
                 Güllenkarren|Heuwagen|Obsthurte|Obstk(o|ö)rb|Rebsteck|Mastbütte|Bienenst(o|ö)ck",
                 valuetype = "regex",
                 ignore.case = T)
agriculture_kwic

## Validation
validation_agriculture_all <- validate_filter(corpus_all, agriculture_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_agriculture_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% agriculture_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% agriculture_all)]

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
                                      pattern = "06ding")
validation_riding_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% riding_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% riding_all)]

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

well_1734 <- well$filtrate(corpus_1734, ignore.case = T)
well_1834 <- well$filtrate(corpus_1834, ignore.case = T)

well_all <- c(well_1734, well_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
well_kwic <- kwic(well_all,
                 pattern = "Wasserstein|Wasser-Stein",
                 valuetype = "regex",
                 ignore.case = T)
well_kwic

## Validation
validation_well_all <- validate_filter(corpus_all, well_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_well_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% well_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% well_all)]

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
                                      pattern = "06ding")
validation_naturalia_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% naturalia_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% naturalia_all)]

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
                                      pattern = "06ding")
validation_container_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% container_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% container_all)]

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
                                      pattern = "06ding")
validation_firestart_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% firestart_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% firestart_all)]

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
                                      pattern = "06ding")
validation_extinguisher_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% extinguisher_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% extinguisher_all)]

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
                                      pattern = "06ding")
validation_firework_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% firework_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% firework_all)]

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

antique_1734 <- antique$filtrate(corpus_1734, ignore.case = T)
antique_1834 <- antique$filtrate(corpus_1834, ignore.case = T)

antique_all <- c(antique_1734, antique_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
antique_kwic <- kwic(antique_all,
                 pattern = phrase("römische Figur"),
                 valuetype = "regex",
                 ignore.case = T)
antique_kwic

## Validation
validation_antique_all <- validate_filter(corpus_all, antique_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_antique_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% antique_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% antique_all)]

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
                                      pattern = "06ding")
validation_key_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% key_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% key_all)]

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
                                      pattern = "06ding")
validation_cane_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% cane_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% cane_all)]

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

wineobject_1734 <- wineobject$filtrate(corpus_1734, ignore.case = T)
wineobject_1834 <- wineobject$filtrate(corpus_1834, ignore.case = T)

wineobject_all <- c(wineobject_1734, wineobject_1834)
wineobject_ids_all <- c(wineobject_1734, wineobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wineobject_kwic <- kwic(wineobject_all,
                 pattern = "Wei(nhah|nhäh|nha|nhä)nen|Wein-H(ah|a)nen|Wein-H(äh|ä)nen|Weinkrause|Weinschild",
                 valuetype = "regex",
                 ignore.case = T)
wineobject_kwic

## Validation
validation_wineobject_all <- validate_filter(corpus_all, wineobject_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_wineobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% wineobject_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% wineobject_all)]

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
                                      pattern = "06ding")
validation_rope_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% rope_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% rope_all)]

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
                                      pattern = "06ding")
validation_tavernobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tavernobject_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tavernobject_all)]

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

feed_1734 <- feed$filtrate(corpus_1734, ignore.case = T)
feed_1834 <- feed$filtrate(corpus_1834, ignore.case = T)

feed_all <- c(feed_1734, feed_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
feed_kwic <- kwic(feed_all,
                 pattern = "Schwei(n|ns)erdäpfel|Schwei(n|ns)-Erdäpfel",
                 valuetype = "regex",
                 ignore.case = T)
feed_kwic

## Validation
validation_feed_all <- validate_filter(corpus_all, feed_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_feed_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% feed_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% feed_all)]

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

miscobject_1734 <- miscobject$filtrate(corpus_1734, ignore.case = T)
miscobject_1834 <- miscobject$filtrate(corpus_1834, ignore.case = T)

miscobject_all <- c(miscobject_1734, miscobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
miscobject_kwic <- kwic(miscobject_all,
                 pattern = "Gegenstände",
                 valuetype = "regex",
                 ignore.case = T)
miscobject_kwic

## Validation
validation_miscobject_all <- validate_filter(corpus_all, miscobject_all,
                                      search_col = "adcontent",
                                      pattern = "06ding")
validation_miscobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% miscobject_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% miscobject_all)]

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

