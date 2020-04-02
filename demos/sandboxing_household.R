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


### checking and cleaning different tagfilters for household objects and descriptions of quality
## validations for single dictionaries are not very significant, because a lot of objects in these dictionaries are manually also classified as things...


## Bed
bed <- tagfilter_bed()

bed_1734 <- bed$filtrate(corpus_1734,ignore.case = T)
bed_1834 <- bed$filtrate(corpus_1834,ignore.case = T)

bed_all <- c(bed_1734, bed_1834)
bed_ids_all <- c(bed_1734, bed_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bed_kwic <- kwic(bed_all,
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
validation_bed_all <- validate_filter(corpus_all, bed_all,
                                           search_col = "adcontent",
                                           pattern = "02hausrat")
validation_bed_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bed_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bed_all)]

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




## Tablelinen
tablelinen <- tagfilter_tablelinen()

tablelinen_1734 <- tablelinen$filtrate(corpus_1734,ignore.case = T)
tablelinen_1834 <- tablelinen$filtrate(corpus_1834,ignore.case = T)

tablelinen_all <- c(tablelinen_1734, tablelinen_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tablelinen_kwic <- kwic(tablelinen_all,
                        pattern = "Tisch(zeug|deck)|(Tisch|Tafel)t(ü|u)ch|Serviette|Handt(ü|u)ch",
                        valuetype = "regex")

tablelinen_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
tablelinen_subset_clean <- tablelinen_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tablelinen_subset_clean),
                   max_words = 100)

## Validation
validation_tablelinen_all <- validate_filter(corpus_all, tablelinen_all,
                                             search_col = "adcontent",
                                             pattern = "01textilien")
validation_tablelinen_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% tablelinen_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% tablelinen_all)]

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

carpet_1734 <- carpet$filtrate(corpus_1734,ignore.case = T)
carpet_1834 <- carpet$filtrate(corpus_1834,ignore.case = T)

carpet_all <- c(carpet_1734, carpet_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carpet_kwic <- kwic(carpet_all,
                    pattern = "T(e|a)(pp|p)i|Bodent(u|ü)ch|Vorh(a|ä)ng",
                    valuetype = "regex")

carpet_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
carpet_subset_clean <- carpet_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(carpet_subset_clean),
                   max_words = 100)

## Validation
validation_carpet_all <- validate_filter(corpus_all, carpet_all,
                                         search_col = "adcontent",
                                         pattern = "01textilien")
validation_carpet_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% carpet_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% carpet_all)]

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

bedding_1734 <- bedding$filtrate(corpus_1734,ignore.case = T)
bedding_1834 <- bedding$filtrate(corpus_1834,ignore.case = T)

bedding_all <- c(bedding_1734, bedding_1834)
bedding_ids_all <- c(bedding_1734, bedding_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bedding_kwic <- kwic(bedding_all,
                     pattern = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|
    Bethzeug|Bettsack|Bethsack|Decke|Strochsack|Strohsäck|Kissen|Unterbe(tt|th)|Nachtsack|Betteingu(ß|ss)",
                     valuetype = "regex")

bedding_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bedding_subset_clean <- bedding_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bedding_subset_clean),
                   max_words = 100)

## Validation
validation_bedding_all <- validate_filter(corpus_all, bedding_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_bedding_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bedding_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bedding_all)]

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

chair_1734 <- chair$filtrate(corpus_1734, ignore.case = T)
chair_1834 <- chair$filtrate(corpus_1834, ignore.case = T)

chair_all <- c(chair_1734, chair_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
chair_kwic <- kwic(chair_all,
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
validation_chair_all <- validate_filter(corpus_all, chair_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_chair_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% chair_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% chair_all)]

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

cabinet_1734 <- cabinet$filtrate(corpus_1734,ignore.case = T)
cabinet_1834 <- cabinet$filtrate(corpus_1834,ignore.case = T)

cabinet_all <- c(cabinet_1734, cabinet_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cabinet_kwic <- kwic(cabinet_all,
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
validation_cabinet_all <- validate_filter(corpus_all, cabinet_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_cabinet_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% cabinet_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% cabinet_all)]

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

stove_1734 <- stove$filtrate(corpus_1734, ignore.case = T)
stove_1834 <- stove$filtrate(corpus_1834, ignore.case = T)

stove_all <- c(stove_1734, stove_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
stove_kwic <- kwic(stove_all,
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
validation_stove_all <- validate_filter(corpus_all, stove_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_stove_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% stove_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% stove_all)]

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

mirror_1734 <- mirror$filtrate(corpus_1734, ignore.case = T)
mirror_1834 <- mirror$filtrate(corpus_1834, ignore.case = T)

mirror_all <- c(mirror_1734, mirror_1834)
mirror_ids_all <- c(mirror_1734, mirror_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mirror_kwic <- kwic(mirror_all,
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
validation_mirror_all <- validate_filter(corpus_all, mirror_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_mirror_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% mirror_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% mirror_all)]

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

timepiece_1734 <- timepiece$filtrate(corpus_1734, ignore.case = T)
timepiece_1834 <- timepiece$filtrate(corpus_1834, ignore.case = T)

timepiece_all <- c(timepiece_1734, timepiece_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
timepiece_kwic <- kwic(timepiece_1734,
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
validation_timepiece_all <- validate_filter(corpus_all, timepiece_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_timepiece_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% timepiece_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% timepiece_all)]

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

table_1734 <- table$filtrate(corpus_1734, ignore.case = T)
table_1834 <- table$filtrate(corpus_1834, ignore.case = T)

table_all <- c(table_1734, table_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
table_kwic <- kwic(table_1834,
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
validation_table_all <- validate_filter(corpus_all, table_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_table_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% table_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% table_all)]

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

tableware_1734 <- tableware$filtrate(corpus_1734, ignore.case = T)
tableware_1834 <- tableware$filtrate(corpus_1834, ignore.case = T)

tableware_all <- c(tableware_1734, tableware_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tableware_kwic <- kwic(tableware_1734,
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
validation_tableware_all <- validate_filter(corpus_all, tableware_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_tableware_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% tableware_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% tableware_all)]

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

bureau_1734 <- bureau$filtrate(corpus_1734, ignore.case = T)
bureau_1834 <- bureau$filtrate(corpus_1834, ignore.case = T)

bureau_all <- c(bureau_1734, bureau_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bureau_kwic <- kwic(bureau_all,
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
validation_bureau_all <- validate_filter(corpus_all, bureau_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_bureau_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% bureau_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% bureau_all)]

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




## Toys
toy <- tagfilter_toy()

toy_1734 <- toy$filtrate(corpus_1734, ignore.case = T)
toy_1834 <- toy$filtrate(corpus_1834, ignore.case = T)

toy_all <- c(toy_1734, toy_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
toy_kwic <- kwic(toy_all,
                 pattern = "Puppe|Bauhölz(chen|lein)|Baukasten|Felsenburg|Spiel-Boit|Spielboit|
                 Aufstellschachtel|(Schwung|Steck|Stecken)pferd|Spiel(waa|a)re|Kinderspiel",
                 valuetype = "regex",
                 ignore.case = T)
toy_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
toy_subset_clean <- toy_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(toy_subset_clean),
                   max_words = 100)

## Validation
validation_toy_all <- validate_filter(corpus_all, toy_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_toy_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% toy_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% toy_all)]

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

game_1734 <- game$filtrate(corpus_1734, ignore.case = T)
game_1834 <- game$filtrate(corpus_1834, ignore.case = T)

game_all <- c(game_1734, game_1834)
game_ids_all <- c(game_1734, game_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
game_kwic <- kwic(game_all,
                 pattern = "Billard|Billiard|Bilard|Biliard|Schachbret|Schachspiel|Taschenspiel|
                 Domino|Lottospiel|Spielkarten|Whist|Würfelspiel|Kegelspiel|Kegelries|Spiele",
                 valuetype = "regex",
                 ignore.case = T)
game_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
game_subset_clean <- game_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(game_subset_clean),
                   max_words = 100)

## Validation
validation_game_all <- validate_filter(corpus_all, game_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_game_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% game_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% game_all)]

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

kitchen_1734 <- kitchen$filtrate(corpus_1734,ignore.case = T)
kitchen_1834 <- kitchen$filtrate(corpus_1834,ignore.case = T)

kitchen_all <- c(kitchen_1734, kitchen_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
kitchen_kwic <- kwic(kitchen_all,
                     pattern = phrase("Brunn zu (Fisch|Krebs)"),
                     valuetype = "regex",
                     ignore.case = T)

kitchen_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
kitchen_subset_clean <- kitchen_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(kitchen_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_kitchen_all <- validate_filter(corpus_all, kitchen_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_kitchen_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% kitchen_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% kitchen_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_kitchen_all$filter_T_hc_T)
b_t$documents$texts[1:93]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_kitchen_all$filter_T_hc_F)
b_f$documents$texts[1:49]


## Lighting
lighting <- tagfilter_lighting()

lighting_1734 <- lighting$filtrate(corpus_1734, ignore.case = T)
lighting_1834 <- lighting$filtrate(corpus_1834, ignore.case = T)

lighting_all <- c(lighting_1734, lighting_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
lighting_kwic <- kwic(lighting_1734,
                 pattern = "Leuchter|Chandelier|Lampe|Latern|Nachtlicht|Lichtstock|Lichtstöck|Gingette|Lamepngl(a|ä)s|
                 Lampe(nö|noe|n-Ö|n-Oe)hl|Wachskerzen|Wachslichter|Kerze|Lamendocht|Lichtscheer",
                 valuetype = "regex",
                 ignore.case = T)
lighting_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
lighting_subset_clean <- lighting_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(lighting_subset_clean),
                   max_words = 100)

## Validation
validation_lighting_all <- validate_filter(corpus_all, lighting_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_lighting_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% lighting_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% lighting_all)]

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

instrument_1734 <- instrument$filtrate(corpus_1734, ignore.case = T)
instrument_1834 <- instrument$filtrate(corpus_1834, ignore.case = T)

instrument_all <- c(instrument_1734, instrument_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
instrument_kwic <- kwic(instrument_all,
                 pattern = "Spi(nn|n)et",
                 valuetype = "regex",
                 ignore.case = T)
instrument_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
instrument_subset_clean <- instrument_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(instrument_subset_clean),
                   max_words = 100)

## Validation
validation_instrument_all <- validate_filter(corpus_all, instrument_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_instrument_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% instrument_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% instrument_all)]

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






## Wallpaper
wallpaper <- tagfilter_wallpaper()

wallpaper_1734 <- wallpaper$filtrate(corpus_1734, ignore.case = T)
wallpaper_1834 <- wallpaper$filtrate(corpus_1834, ignore.case = T)

wallpaper_all <- c(wallpaper_1734, wallpaper_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wallpaper_kwic <- kwic(wallpaper_all,
                 pattern = "Tapez",
                 valuetype = "regex",
                 ignore.case = T)
wallpaper_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
wallpaper_subset_clean <- wallpaper_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(wallpaper_subset_clean),
                   max_words = 100)

## Validation
validation_wallpaper_all <- validate_filter(corpus_all, wallpaper_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_wallpaper_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% wallpaper_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% wallpaper_all)]

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





## Cutlery
cutlery <- tagfilter_cutlery()

cutlery_1734 <- cutlery$filtrate(corpus_1734, ignore.case = T)
cutlery_1834 <- cutlery$filtrate(corpus_1834, ignore.case = T)

cutlery_all <- c(cutlery_1734, cutlery_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cutlery_kwic <- kwic(cutlery_all,
                 pattern = "Messerschmied|Messerschmid",
                 valuetype = "regex",
                 ignore.case = T)
cutlery_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cutlery_subset_clean <- cutlery_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cutlery_subset_clean),
                   max_words = 100)

## Validation
validation_cutlery_all <- validate_filter(corpus_all, cutlery_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_cutlery_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% cutlery_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% cutlery_all)]

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



## Room Dividers
divider <- tagfilter_divider()

divider_1734 <- divider$filtrate(corpus_1734, ignore.case = T)
divider_1834 <- divider$filtrate(corpus_1834, ignore.case = T)

divider_all <- c(divider_1734, divider_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
divider_kwic <- kwic(divider_all,
                 pattern = "spannisch",
                 valuetype = "regex",
                 ignore.case = T)
divider_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
divider_subset_clean <- divider_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(divider_subset_clean),
                   max_words = 100)

## Validation
validation_divider_all <- validate_filter(corpus_all, divider_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_divider_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% divider_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% divider_all)]

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

petobject_1734 <- petobject$filtrate(corpus_1734, ignore.case = T)
petobject_1834 <- petobject$filtrate(corpus_1834, ignore.case = T)

petobject_all <- c(petobject_1734, petobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
petobject_kwic <- kwic(petobject_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
petobject_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
petobject_subset_clean <- petobject_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(petobject_subset_clean),
                   max_words = 100)

## Validation
validation_petobject_all <- validate_filter(corpus_all, petobject_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_petobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% petobject_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% petobject_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_petobject_all$filter_T_hc_T)
b_t$documents$texts[1:16]

# FALSE positives: most are correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_petobject_all$filter_T_hc_F)
b_f$documents$texts[1:37]


## Upholstery
upholstery <- tagfilter_upholstery()

upholstery_1734 <- upholstery$filtrate(corpus_1734, ignore.case = T)
upholstery_1834 <- upholstery$filtrate(corpus_1834, ignore.case = T)

upholstery_all <- c(upholstery_1734, upholstery_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
upholstery_kwic <- kwic(upholstery_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
upholstery_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
upholstery_subset_clean <- upholstery_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(upholstery_subset_clean),
                   max_words = 100)

## Validation
validation_upholstery_all <- validate_filter(corpus_all, upholstery_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_upholstery_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% upholstery_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% upholstery_all)]

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

domestic_1734 <- domestic$filtrate(corpus_1734, ignore.case = T)
domestic_1834 <- domestic$filtrate(corpus_1834, ignore.case = T)

domestic_all <- c(domestic_1734, domestic_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
domestic_kwic <- kwic(domestic_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
domestic_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
domestic_subset_clean <- domestic_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(domestic_subset_clean),
                   max_words = 100)

## Validation
validation_domestic_all <- validate_filter(corpus_all, domestic_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_domestic_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% domestic_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% domestic_all)]

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

garden_1734 <- garden$filtrate(corpus_1734, ignore.case = T)
garden_1834 <- garden$filtrate(corpus_1834, ignore.case = T)

garden_all <- c(garden_1734, garden_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
garden_kwic <- kwic(garden_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
garden_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
garden_subset_clean <- garden_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(garden_subset_clean),
                   max_words = 100)

## Validation
validation_garden_all <- validate_filter(corpus_all, garden_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_garden_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% garden_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% garden_all)]

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

homedeco_1734 <- homedeco$filtrate(corpus_1734, ignore.case = T)
homedeco_1834 <- homedeco$filtrate(corpus_1834, ignore.case = T)

homedeco_all <- c(homedeco_1734, homedeco_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
homedeco_kwic <- kwic(homedeco_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
homedeco_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
homedeco_subset_clean <- homedeco_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(homedeco_subset_clean),
                   max_words = 100)

## Validation
validation_homedeco_all <- validate_filter(corpus_all, homedeco_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_homedeco_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% homedeco_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% homedeco_all)]

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

art_1734 <- art$filtrate(corpus_1734, ignore.case = T)
art_1834 <- art$filtrate(corpus_1834, ignore.case = T)

art_all <- c(art_1734, art_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
art_kwic <- kwic(art_all,
                 pattern = "Bilder|Gem(ä|äh)lde|M(a|ah)lere(y|i)en",
                 valuetype = "regex",
                 ignore.case = T)
art_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
art_subset_clean <- art_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(art_subset_clean),
                   max_words = 100)

## Validation
validation_art_all <- validate_filter(corpus_all, art_all,
                                      search_col = "adcontent",
                                      pattern = "hausrat")
validation_art_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% art_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% art_all)]

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

bathobject_1734 <- bathobject$filtrate(corpus_1734, ignore.case = T)
bathobject_1834 <- bathobject$filtrate(corpus_1834, ignore.case = T)

bathobject_all <- c(bathobject_1734, bathobject_1834)
bathobject_ids_all <- c(bathobject_1734, bathobject_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bathobject_kwic <- kwic(bathobject_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
bathobject_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bathobject_subset_clean <- bathobject_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bathobject_subset_clean),
                   max_words = 100)

## Validation
validation_bathobject_all <- validate_filter(corpus_all, bathobject_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_bathobject_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% bathobject_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% bathobject_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bathobject_all$filter_T_hc_T)
b_t$documents$texts[1:17]

# FALSE positives: three correct, two bath-tourism ads
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bathobject_all$filter_T_hc_F)
b_f$documents$texts[1:5]

## Misc Household Goods (Unspecified)
mischousehold <- tagfilter_mischousehold()

mischousehold_1734 <- mischousehold$filtrate(corpus_1734, ignore.case = T)
mischousehold_1834 <- mischousehold$filtrate(corpus_1834, ignore.case = T)

mischousehold_all <- c(mischousehold_1734, mischousehold_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mischousehold_kwic <- kwic(mischousehold_all,
                 pattern = "Wagen",
                 valuetype = "regex",
                 ignore.case = T)
mischousehold_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
mischousehold_subset_clean <- mischousehold_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mischousehold_subset_clean),
                   max_words = 100)

## Validation
validation_mischousehold_all <- validate_filter(corpus_all, mischousehold_all,
                                      search_col = "adcontent",
                                      pattern = "02hausrat")
validation_mischousehold_all

# Filters for TRUE and FALSE positives
doc_ids <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% mischousehold_all)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% mischousehold_all)]

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
household_manual_1734 <- corpus_subset(corpus_1734, grepl("02hausrat", adcontent) & grepl("01kauf", finance))
household_manual_1834 <- corpus_subset(corpus_1834, grepl("02hausrat", adcontent) & grepl("01kauf", finance))


# creating a corpus of all the ads (ids) not found by automatic classification

household_auto_1734 <- c(docvars(bed_1734, "id"), docvars(tablelinen_1734, "id"), docvars(carpet_1734, "id"), docvars(bedding_1734, "id"), docvars(chair_1734, "id"),
                         docvars(table_1734, "id"), docvars(tableware_1734, "id"), docvars(timepiece_1734, "id"), docvars(mirror_1734, "id"), docvars(stove_1734, "id"),
                         docvars(cabinet_1734, "id"), docvars(bureau_1734, "id"), docvars(cabinet_1734, "id"), docvars(chair_1734, "id"), docvars(cutlery_1734, "id"),
                         docvars(divider_1734, "id"), docvars(domestic_1734, "id"), docvars(game_1734, "id"), docvars(garden_1734, "id"), docvars(instrument_1734, "id"),
                         docvars(kitchen_1734, "id"), docvars(lighting_1734, "id"), docvars(mischousehold_1734, "id"), docvars(petobject_1734, "id"), docvars(toy_1734, "id"),
                         docvars(upholstery_1734, "id"), docvars(wallpaper_1734, "id"), docvars(art_1734, "id"), docvars(homedeco_1734, "id"), docvars(bathobject_1734, "id"))

household_auto_1834 <- c(docvars(bed_1834, "id"), docvars(tablelinen_1834, "id"), docvars(carpet_1834, "id"), docvars(bedding_1834, "id"), docvars(chair_1834, "id"),
                         docvars(table_1834, "id"), docvars(tableware_1834, "id"), docvars(timepiece_1834, "id"), docvars(mirror_1834, "id"), docvars(stove_1834, "id"),
                         docvars(cabinet_1834, "id"), docvars(bureau_1834, "id"), docvars(cabinet_1834, "id"), docvars(chair_1834, "id"), docvars(cutlery_1834, "id"),
                         docvars(divider_1834, "id"), docvars(domestic_1834, "id"), docvars(game_1834, "id"), docvars(garden_1834, "id"), docvars(instrument_1834, "id"),
                         docvars(kitchen_1834, "id"), docvars(lighting_1834, "id"), docvars(mischousehold_1834, "id"), docvars(petobject_1834, "id"), docvars(toy_1834, "id"),
                         docvars(upholstery_1834, "id"), docvars(wallpaper_1834, "id"), docvars(art_1834, "id"), docvars(homedeco_1834, "id"), docvars(bathobject_1834, "id"))

household_missed_1734 <- corpus_subset(household_manual_1734, docvars(household_manual_1734, "id") %notin% docvars(household_aut_1734, "id"))

household_missed_1834 <- corpus_subset(household_manual_1834, docvars(household_manual_1834, "id") %notin% docvars(household_aut_1834, "id"))

write.csv2(household_missed_1734, file = "data/household_missed_1734.csv", fileEncoding = "UTF-8")

write.csv2(household_missed_1834, file = "data/household_missed_1834.csv", fileEncoding = "UTF-8")

# 17/01/20: only 52 missed ads (if only looking at "for sale/to buy", 168 if including others)
# most of them French or wrongly classified as "Hausrat" in manual classification
# some missed ads don't make any sense, should be included through dictionaries, words in question are: "Vorfenster",
# "Trumeau", "Messer", "Fensterflügel", "Fenster", "WaarenKorpus" and "Bücherkästchen"
# rest of missed ads are very special objects, makes no sense to make a dictionary for them (re-evaluate later)


# 20/01/20: a lot of "oops" cases (936!), but most of them seem to be work or immo related and could be excluded by detection of work/immo ads
# some of them are adverts for shops or services, were houshold goods are advertised, so actually correctly recognized via automatic recognition
# and wrongly not tagged manually, in general ads frequently not classified as houshold by manual clasification
# sometimes also negative dictionaries don't seem to work consistently,  e.g. "Sitzung" or "Langmesser"
