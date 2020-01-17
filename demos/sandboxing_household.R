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
source("R/tagfilters_things_qualities.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

### checking and cleaning different tagfilters for household objects and descriptions of quality

## Quality: Secondhand

secondhand <- tagfilter_secondhand()

secondhand_ids <- secondhand$filtrate(corpus_1834, ignore.case = FALSE)

secondhand_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in% secondhand_ids)

secondhand_texts <- secondhand_subset$documents$texts

secondhand_subset_clean <- secondhand_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(secondhand_subset_clean),
                   max_words = 200)


## Bed
bed <- tagfilter_bed()

bed_ids <- bed$filtrate(corpus_1834,ignore.case = T)

bed_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                             bed_ids)

bed_texts <- bed_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bed_kwic <- kwic(bed_subset,
                  pattern = "[B|b]ett|[B|b]eth|[K|k]orbwag|[W|w]iege",
                  valuetype = "regex")

bed_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bed_subset_clean <- bed_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bed_subset_clean),
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


## Chairs
chair <- tagfilter_chair()

chair_ids <- chair$filtrate(corpus_1834,ignore.case = T)

chair_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                chair_ids)

chair_texts <- chair_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
chair_kwic <- kwic(chair_subset,
                 pattern = "[S|s]t[u|ü]hl|[S|s]itz",
                 valuetype = "regex")

chair_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
chair_subset_clean <- chair_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(chair_subset_clean),
                   max_words = 100)

## Cupboards, Cabinets and Storage
cabinet <- tagfilter_cabinet()

cabinet_ids <- cabinet$filtrate(corpus_1834,ignore.case = F)

cabinet_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  cabinet_ids)

cabinet_texts <- cabinet_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cabinet_kwic <- kwic(cabinet_subset,
                  pattern = "Kästlein|[K|k]orpus",
                  valuetype = "regex")

cabinet_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cabinet_subset_clean <- cabinet_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cabinet_subset_clean),
                   max_words = 100)

## Stoves and Related Objects
stove <- tagfilter_stove()

stove_ids <- stove$filtrate(corpus_1834,ignore.case = T)

stove_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                stove_ids)

stove_texts <- stove_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
stove_kwic <- kwic(stove_subset,
                 pattern = "öfel",
                 valuetype = "regex")

stove_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
stove_subset_clean <- stove_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(stove_subset_clean),
                   max_words = 100)


## Mirrors
mirror <- tagfilter_mirror()

mirror_ids <- mirror$filtrate(corpus_1834,ignore.case = T)

mirror_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                 mirror_ids)

mirror_texts <- mirror_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mirror_kwic <- kwic(mirror_subset,
                   pattern = "[S|s]piegel",
                   valuetype = "regex")

mirror_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
mirror_subset_clean <- mirror_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mirror_subset_clean),
                   max_words = 100)


## Timepieces
timepiece <- tagfilter_timepiece()

timepiece_ids <- timepiece$filtrate(corpus_1834,ignore.case = T)

timepiece_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    timepiece_ids)

timepiece_texts <- timepiece_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
timepiece_kwic <- kwic(timepiece_subset,
                    pattern = "[P|p]endul",
                    valuetype = "regex")

timepiece_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
timepiece_subset_clean <- timepiece_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(timepiece_subset_clean),
                   max_words = 100)

## Tables
table <- tagfilter_table()

table_ids <- table$filtrate(corpus_1834,ignore.case = T)

table_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                table_ids)

table_texts <- table_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
table_kwic <- kwic(table_subset,
                       pattern = "[T|t]afel",
                       valuetype = "regex")


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
table_subset_clean <- table_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(table_subset_clean),
                   max_words = 100)

## Tableware
tableware <- tagfilter_tableware()

tableware_ids <- tableware$filtrate(corpus_1834,ignore.case = T)

tableware_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    tableware_ids)

tableware_texts <- tableware_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tableware_kwic <- kwic(tableware_subset,
                   pattern = "Tasse|Humpe|Teller|Becher",
                   valuetype = "regex")

tableware_kwic


# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
tableware_subset_clean <- tableware_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tableware_subset_clean),
                   max_words = 100)


## Bureau
bureau <- tagfilter_bureau()

bureau_ids <- bureau$filtrate(corpus_1834,ignore.case = T)

bureau_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                 bureau_ids)

bureau_texts <- bureau_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bureau_kwic <- kwic(bureau_subset,
                       pattern = "[P|p]ult",
                       valuetype = "regex")

bureau_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bureau_subset_clean <- bureau_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bureau_subset_clean),
                   max_words = 100)


## Small Storage
storage <- tagfilter_storage()

storage_ids <- storage$filtrate(corpus_1834,ignore.case = F)

storage_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  storage_ids)

storage_texts <- storage_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
storage_kwic <- kwic(storage_subset,
                    pattern = "[S|s]ack|[S|s]äcke|[S|s]äckch",
                    valuetype = "regex")

storage_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
storage_subset_clean <- storage_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(storage_subset_clean),
                   max_words = 100)


## Toys
toy <- tagfilter_toy()

toy_ids <- toy$filtrate(corpus_1834,ignore.case = F)

toy_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                              toy_ids)

toy_texts <- toy_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
toy_kwic <- kwic(toy_subset,
                     pattern = "[S|s]pielwaare",
                     valuetype = "regex")

toy_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
toy_subset_clean <- toy_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(toy_subset_clean),
                  max_words = 100)


## Games
game <- tagfilter_game()

game_ids <- game$filtrate(corpus_1834,ignore.case = F)

game_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               game_ids)

game_texts <- game_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
game_kwic <- kwic(toy_subset,
                 pattern = "Taschenspiel",
                 valuetype = "regex")

game_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
game_subset_clean <- game_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(game_subset_clean),
                   max_words = 100)


## Kitchen Utensils
kitchen <- tagfilter_kitchen()

kitchen_ids <- kitchen$filtrate(corpus_1834, ignore.case = T)

kitchen_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  kitchen_ids)

kitchen_texts <- kitchen_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
kitchen_kwic <- kwic(kitchen_subset,
                  pattern = "Brennhafen|Milchflasche|Fleischbütte|Sauerkrautstand|Züber",
                  valuetype = "regex",
                  ignore.case = T)

kitchen_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
kitchen_subset_clean <- kitchen_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(kitchen_subset_clean),
                   max_words = 100)


## Lighting
lighting <- tagfilter_lighting()

lighting_ids <- lighting$filtrate(corpus_1834, ignore.case = T)

lighting_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   lighting_ids)

lighting_texts <- lighting_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
lighting_kwic <- kwic(lighting_subset,
                     pattern = "Lichtstock|Lichtstöck",
                     valuetype = "regex",
                     ignore.case = T)

lighting_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
lighting_subset_clean <- lighting_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(lighting_subset_clean),
                   max_words = 100)


## Musical Instruments
instrument <- tagfilter_instrument()

instrument_ids <- instrument$filtrate(corpus_1834, ignore.case = T)

instrument_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     instrument_ids)

instrument_texts <- instrument_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
instrument_kwic <- kwic(instrument_subset,
                      pattern = "Stimmgabel",
                      valuetype = "regex",
                      ignore.case = T)

instrument_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
instrument_subset_clean <- instrument_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(instrument_subset_clean),
                   max_words = 100)


## Building Components
building <- tagfilter_building()

building_ids <- building$filtrate(corpus_1834, ignore.case = F)

building_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   building_ids)

building_texts <- instrument_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
building_kwic <- kwic(building_subset,
                        pattern = "Bodenplättl|Bodenplatt",
                        valuetype = "regex",
                        ignore.case = F)

building_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
building_subset_clean <- building_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(building_subset_clean),
                   max_words = 100)


## Wallpaper
wallpaper <- tagfilter_wallpaper()

wallpaper_ids <- wallpaper$filtrate(corpus_1834, ignore.case = T)

wallpaper_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    wallpaper_ids)

wallpaper_texts <- wallpaper_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
wallpaper_kwic <- kwic(wallpaper_subset,
                      pattern = "Tapet",
                      valuetype = "regex",
                      ignore.case = T)

wallpaper_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
wallpaper_subset_clean <- wallpaper_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(wallpaper_subset_clean),
                   max_words = 100)


## Suitcases
suitcase <- tagfilter_suitcase()

suitcase_ids <- suitcase$filtrate(corpus_1834, ignore.case = T)

suitcase_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   suitcase_ids)

suitcase_texts <- suitcase_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
suitcase_kwic <- kwic(suitcase_subset,
                       pattern = "Koffer|Coffre|Coffer",
                       valuetype = "regex",
                       ignore.case = T)

suitcase_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
suitcase_subset_clean <- suitcase_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(suitcase_subset_clean),
                   max_words = 100)


## Cutlery
cutlery <- tagfilter_cutlery()

cutlery_ids <- cutlery$filtrate(corpus_1834, ignore.case = T)

cutlery_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  cutlery_ids)

cutlery_texts <- cutlery_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cutlery_kwic <- kwic(cutlery_subset,
                     pattern = "Messer",
                     valuetype = "regex",
                     ignore.case = T)

cutlery_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cutlery_subset_clean <- cutlery_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cutlery_subset_clean),
                   max_words = 100)


## Measuring instruments
measure <- tagfilter_measure()

measure_ids <- measure$filtrate(corpus_1834, ignore.case = T)

measure_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  measure_ids)

measure_texts <- measure_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
measure_kwic <- kwic(measure_subset,
                      pattern = "Termometer",
                      valuetype = "regex",
                      ignore.case = T)

measure_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
measure_subset_clean <- measure_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(measure_subset_clean),
                   max_words = 100)


## Room Dividers
divider <- tagfilter_divider()

divider_ids <- divider$filtrate(corpus_1834, ignore.case = T)

divider_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  divider_ids)

divider_texts <- divider_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
divider_kwic <- kwic(divider_subset,
                     pattern = "spanisch",
                     valuetype = "regex",
                     ignore.case = T)

divider_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
divider_subset_clean <- divider_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(divider_subset_clean),
                   max_words = 100)


## Object for Pets
petobject <- tagfilter_petobject()

petobject_ids <- petobject$filtrate(corpus_1834, ignore.case = T)

petobject_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    petobject_ids)

petobject_texts <- petobject_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
petobject_kwic <- kwic(petobject_subset,
                     pattern = "Hundesställ|Hundestall",
                     valuetype = "regex",
                     ignore.case = T)

petobject_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
petobject_subset_clean <- petobject_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(petobject_subset_clean),
                   max_words = 100)


## Upholstery
upholstery <- tagfilter_upholstery()

upholstery_ids <- upholstery$filtrate(corpus_1834, ignore.case = T)

upholstery_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    upholstery_ids)

upholstery_texts <- upholstery_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
upholstery_kwic <- kwic(upholstery_subset,
                       pattern = "Kanefa",
                       valuetype = "regex",
                       ignore.case = T)

upholstery_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
upholstery_subset_clean <- upholstery_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(upholstery_subset_clean),
                   max_words = 100)


## Houseplant and related Objects
plantobject <- tagfilter_plantobject()

plantobject_ids <- plantobject$filtrate(corpus_1834, ignore.case = T)

plantobject_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                      plantobject_ids)

plantobject_texts <- plantobject_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
plantobject_kwic <- kwic(plantobject_subset,
                        pattern = "Blumen",
                        valuetype = "regex",
                        ignore.case = T)

plantobject_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
plantobject_subset_clean <- plantobject_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(plantobject_subset_clean),
                   max_words = 100)


## Domestic Appliances
domestic <- tagfilter_domestic()

domestic_ids <- domestic$filtrate(corpus_1834, ignore.case = T)

domestic_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   domestic_ids)

domestic_texts <- domestic_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
domestic_kwic <- kwic(domestic_subset,
                         pattern = "Spuhlrad|Sticktrommel|Spuhlrad|Plunderstang",
                         valuetype = "regex",
                         ignore.case = T)

domestic_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
domestic_subset_clean <- domestic_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(domestic_subset_clean),
                   max_words = 100)



## Garden Furniture and Objects
garden <- tagfilter_garden()

garden_ids <- garden$filtrate(corpus_1834, ignore.case = T)

garden_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                 garden_ids)

garden_texts <- garden_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
garden_kwic <- kwic(garden_subset,
                      pattern = "Gärtner-Cloches|Baum-Scheere",
                      valuetype = "regex",
                      ignore.case = T)

garden_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
garden_subset_clean <- garden_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(garden_subset_clean),
                   max_words = 100)


## Misc Household Goods (Unspecified)
mischoushold <- tagfilter_mischoushold()

mischoushold_ids <- mischoushold$filtrate(corpus_1834, ignore.case = T)

mischoushold_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       mischoushold_ids)

mischoushold_texts <- mischoushold_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
mischoushold_kwic <- kwic(mischoushold_subset,
                    pattern = "Hausgerät",
                    valuetype = "regex",
                    ignore.case = T)

mischoushold_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
mischoushold_subset_clean <- mischoushold_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(mischoushold_subset_clean),
                   max_words = 100)



### using existing categories for wordclouds and dfm to find missing objects in dictionaries
# creating subset of houshold objects for sale and for buying
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
household_missed <- corpus_subset(household_1834, docvars(household_1834, "id") %notin%
                                    c(bed_ids, household_textile_ids, seat_ids, table_ids, tableware_ids,
                                      timepiece_ids, mirror_ids, stove_ids, cabinet_ids, bureau_ids, cabinet_ids,
                                      chair_ids, cutlery_ids, divider_ids, domestic_ids, game_ids, garden_ids,
                                      instrument_ids, kitchen_ids, lighting_ids, measure_ids, mischoushold_ids,
                                      petobject_ids, plantobject_ids, storage_ids, suitcase_ids, toy_ids, upholstery_ids,
                                      wallpaper_ids))
# 17/01/20: only 52 missed ads - most of them French or wrongly classified as "Hausrat" in manual classification
# some missed ads don't make any sense, should be included through dictionaries, words in question are: "Vorfenster",
# "Trumeau", "Messer", "Fensterflügel", "Fenster", "WaarenKorpus" and "Bücherkästchen"
# rest of missed ads are very special objects, makes no sense to make a dictionary for them (re-evaluate later)

household_missed_texts <- household_missed$documents$texts

# write csv for texts of missed ads
write.csv2(household_missed_texts, file = "data/household_missed.csv", fileEncoding = "UTF-8")

# creating a corpus of all ads (ids) found automatically, but not by manual classification
household_ids <- household_1834$documents$id

not_household <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %notin%
                                  household_ids)


household_oops <- corpus_subset(not_household, docvars(not_household, "id") %in%
                                  c(bed_ids, household_textile_ids, seat_ids, table_ids, tableware_ids,
                                    timepiece_ids, mirror_ids, stove_ids, cabinet_ids))

household_oops_texts <- household_oops$documents$texts

# 17/01/20: 631 oops cases, but most of them seem to be work or immo related and could be excluded by detection of work/immo ads
# some of them are adverts for shops or services, were houshold goods are advertised, so actually correctly recognized
# as such; often also ads wrongly not classified as houshold by manual clasification
# sometimes also negative dictionaries don't seem to work consistently,  e.g. "Sitzung" or "Langmesser"


household_oops_texts[100:150]

# creating a corpus of all ads recognised by automated household filters
household_filters <- corpus_subset(corpus_1834, docvars(household_1834, "id") %in%
                                    c(bed_ids, household_textile_ids, seat_ids, table_ids, tableware_ids,
                                      timepiece_ids, mirror_ids, stove_ids, cabinet_ids, bureau_ids, cabinet_ids,
                                      chair_ids, cutlery_ids, divider_ids, domestic_ids, game_ids, garden_ids,
                                      instrument_ids, kitchen_ids, lighting_ids, measure_ids, mischoushold_ids,
                                      petobject_ids, plantobject_ids, storage_ids, suitcase_ids, toy_ids, upholstery_ids,
                                      wallpaper_ids))




cat(paste("Range (%):\t", household_1834$range, "->", household_filters$range, "| change:", round (household_filters$range-o$range,1),
          "\nPrecision (%):\t", household_1834$precision, "->", household_filters$precision, "| change:", round (household_filters$precision-o$precision,1), "\n"
))
