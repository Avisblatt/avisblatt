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
                 pattern = "Tafeltuch|Tischtuch|Tischzeug|Tischdeck|Deckbet|Hauszeug|Matrat|Madrat|Bettdeck|Bettwer|Bethwer|Bettzeug|
                 Bethzeug|Bettsack|Bethsack|Teppi|Tepi|Tapi|Tappi|Schaubdeck",
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
cabinet_dict <- dictionary(list(
  cabinet = "Schafft"))

cabinet_kwic <- kwic(cabinet_subset,
                  pattern = cabinet_dict,
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
                 pattern = "[O|o]efel",
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
                   pattern = "[B|b]iergläs|[B|b]ierglas",
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

storage_wordcloud(dfm(storage_subset_clean),
                   max_words = 100)


### using existing categories for wordclouds and dfm to find missing objects in dictionaries
# creating subset of houshold objects for sale
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
                                      timepiece_ids, mirror_ids, stove_ids, cabinet_ids))

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

household_oops_texts[1:20]
