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

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")


### checking and cleaning different tagfilters for textiles


## Clothing
clothing <- tagfilter_clothing()

clothing_ids <- clothing$filtrate(corpus_1834,ignore.case = T)

clothing_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                             clothing_ids)

clothing_texts <- clothing_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
clothing_kwic <- kwic(clothing_subset,
                  pattern = "Kleid|Kleyd|Rock|Röck|Ärmel|Weste",
                  valuetype = "regex",
                  ignore.case = T)
clothing_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
clothing_subset_clean <- clothing_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(clothing_subset_clean),
                   max_words = 200)



## Sleapwear
sleapwear <- tagfilter_sleapwear()

sleapwear_ids <- sleapwear$filtrate(corpus_1834,ignore.case = T)

sleapwear_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    sleapwear_ids)

sleapwear_texts <- sleapwear_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
sleapwear_kwic <- kwic(sleapwear_subset,
                      pattern = "Schlafrock|Schlafröck|Nachtärmel",
                      valuetype = "regex",
                      ignore.case = T)
sleapwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
sleapwear_subset_clean <- sleapwear_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(sleapwear_subset_clean),
                   max_words = 200)



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

### checked up until here ###

## Certain Types of Fabric/ Textile Material
texmaterial <- tagfilter_texmaterial()

texmaterial_ids <- texmaterial$filtrate(corpus_1834,ignore.case = T)

texmaterial_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                      texmaterial_ids)

texmaterial_texts <- texmaterial_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
texmaterial_kwic <- kwic(texmaterial_subset,
                       pattern = "Marceline|Levantine|Seide|Blonde|Floreth|Floret|Taffent|Taffet",
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



## Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_ids <- cloth$filtrate(corpus_1834,ignore.case = F)

cloth_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                cloth_ids)

cloth_texts <- cloth_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cloth_kwic <- kwic(cloth_subset,
                       pattern = "[H|h]austuch|[F|f]uttertuch|[R|r]este|[L|l]einwa[nd|th]|Zeug|[S|s]acktuch|
                   [W|w]ollw[aa|ah]r|[W|w]oll-[W|w][aa|ah]r",
                       valuetype = "regex")

cloth_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cloth_subset_clean <- cloth_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cloth_subset_clean),
                   max_words = 100)


## Yarn
yarn <- tagfilter_yarn()

yarn_ids <- yarn$filtrate(corpus_1834,ignore.case = T)

yarn_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               yarn_ids)

yarn_texts <- yarn_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
yarn_kwic <- kwic(yarn_subset,
                 pattern = "[F|f]lachs|[G|g]arn|[F|f]aden|[S|s]tickseide|[S|s]tickwolle|[S|s]tickbaum|
                 [S|s]trickseide|[S|s]trickwolle|[S|s]trickbaum",
                 valuetype = "regex")

yarn_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
yarn_subset_clean <- yarn_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(yarn_subset_clean),
                   max_words = 100)


## Animal Raw Materials
### This does not work, animalraw_ids = 0 - why? In kwic_textile it works with the exact same dictionary... have to look into it later!
animalraw <- tagfilter_animalraw()

animalraw_ids <- animalraw$filtrate(corpus_1834,ignore.case = T)

animalraw_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                 animalraw_ids)

animalraw_texts <- animalraw_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
animalraw_kwic <- kwic(animalraw_subset,
                   pattern = "Bettfehde|Bethfehde|Bettfede|Bettfehde|Rosshaar|Roßhaar",
                   valuetype = "regex")

animalraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
animalraw_subset_clean <- animalraw_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(animalraw_subset_clean),
                   max_words = 100)


## Non Textile Accessoires
nontexaccess <- tagfilter_nontexaccess()

nontexaccess_ids <- nontexaccess$filtrate(corpus_1834,ignore.case = T)

nontexaccess_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       nontexaccess_ids)

nontexaccess_texts <- nontexaccess_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
nontexaccess_kwic <- kwic(nontexaccess_subset,
                    pattern = "[S|s]chnalle|Hosenträger|[G|g]ürtel|[C|c]eintur|[K|k]n[o|ö]pf",
                    valuetype = "regex")

nontexaccess_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
nontexaccess_subset_clean <- nontexaccess_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(nontexaccess_subset_clean),
                   max_words = 100)


## Bags and Purses
bag <- tagfilter_bag()

bag_ids <- bag$filtrate(corpus_1834,ignore.case = T)

bag_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                              bag_ids)

bag_texts <- bag_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bag_kwic <- kwic(bag_subset,
                       pattern = "[T|t]asche|[S|s]eckel|[B|b]eutel|[S|s]äcklein",
                       valuetype = "regex")

bag_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
bag_subset_clean <- bag_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bag_subset_clean),
                   max_words = 100)


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
textile_subset_clean <- household_textile_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(household_textile_subset_clean),
                   max_words = 100)



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

# creating a corpus of all the ads (ids) not found by automatic classification
textiles_missed <- corpus_subset(textiles_1834, docvars(textiles_1834, "id") %notin%
                                    c(clothing_ids, shoes_ids, texaccess_ids, cloth_ids, yarn_ids,
                                      animalraw_ids, nontexaccess_ids, bag_ids, household_textile_ids))

textiles_missed_texts <- textiles_missed$documents$texts

# write csv for texts of missed ads
write.csv2(textiles_missed_texts, file = "data/textiles_missed.csv", fileEncoding = "UTF-8")

# creating a corpus of all ads (ids) found automatically, but not by manual classification
textiles_ids <- textiles_1834$documents$id

not_textiles <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %notin%
                                textiles_ids)


textiles_oops <- corpus_subset(not_textiles, docvars(not_textiles, "id") %in%
                                  c(bed_ids, household_textile_ids, seat_ids, table_ids, tableware_ids,
                                    timepiece_ids, mirror_ids, stove_ids, cabinet_ids))

textiles_oops_texts <- textiles_oops$documents$texts


textiles_oops_texts[100:150]

# creating a corpus of all ads recognised by automated textiles filters
textiles_filters <- corpus_subset(corpus_1834, docvars(textiles_1834, "id") %in%
                                     c(bed_ids, household_textile_ids, seat_ids, table_ids, tableware_ids,
                                       timepiece_ids, mirror_ids, stove_ids, cabinet_ids, bureau_ids, cabinet_ids,
                                       chair_ids, cutlery_ids, divider_ids, domestic_ids, game_ids, garden_ids,
                                       instrument_ids, kitchen_ids, lighting_ids, measure_ids, mischoushold_ids,
                                       petobject_ids, plantobject_ids, storage_ids, suitcase_ids, toy_ids, upholstery_ids,
                                       wallpaper_ids))

