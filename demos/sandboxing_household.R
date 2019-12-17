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
textile_dict <- dictionary(list(
  table_linen = "[T|t]afeltuch|[T|t]ischtuch|[T|t]ischzeug|[T|t]ischdeck",
  bedding = "[D|d]eckbet|[H|h]auszeug|[M|m]a[t|d]rat|[B|b]ettdeck|[B|b]e[tt|th]wer|Bettzeug|Bethzeug|Bettsack|Bethsack",
  carpetmis = "[T|t]eppi|[T|t][e|a]pi|[S|s]chaubdeck"))

household_textile_kwic <- kwic(household_textile_subset,
                 pattern = household_textile_dict,
                  valuetype = "regex")

#### Why does this not work? I get the message "Error in x[[length(x)]]" - what is to long here? ####

household_textile_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
textile_subset_clean <- household_textile_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(household_textile_subset_clean),
                   max_words = 100)

#### Here I get the same error message as before... ####


## Seating Furniture
seat <- tagfilter_seat()

seat_ids <- seat$filtrate(corpus_1834,ignore.case = T)

seat_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               seat_ids)

seat_texts <- seat_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
seat_kwic <- kwic(seat_subset,
                 pattern = "[S|s]essel|[F|f][au|ua]teil|[S|s]t[u|ü]hl",
                 valuetype = "regex")

seat_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
seat_subset_clean <- seat_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(seat_subset_clean),
                   max_words = 100)

## Cupboards, Cabinets and Storage
cabinet <- tagfilter_cabinet()

cabinet_ids <- cabinet$filtrate(corpus_1834,ignore.case = F)

cabinet_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                  cabinet_ids)

cabinet_texts <- cabinet_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cabinet_dict <- dictionary(list(
  cabinet = "[K|k]asten|[B|b]uffet|[C|c]orpus|[K|C]ommode"))

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
                 pattern = "[O|o|ö|Ö]fen|[O|o]efen|[K|k|C|c]amin[F|f]euerh[u|ü]nd",
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

#### Still includes "Spiegelgässlein", event though this is on the negative list, probably because of the Umlaut? How to exclude this? ####

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
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
                    pattern = "[U|u]hr",
                    valuetype = "regex")

timepiece_kwic

#### Problem with regex in negative list meant to filter out indications of time ####

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
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
                       pattern = "[T|t]isch",
                       valuetype = "regex")

table_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
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
tableware_kwic <- kwic(table_subset,
                   pattern = "[G|g]eschir|[K|k|C|c]anne",
                   valuetype = "regex")

tableware_kwic

#### still one inclusion of "Pferdgeschirr" even though on negative list - why? ####

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
tableware_subset_clean <- tableware_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tableware_subset_clean),
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

household_s_df <- sort(df,decreasing = T)

# exporting csv with sorted document feature matrix to Desktop
write.csv2(household_s_df, file = "data/household_s_df_1834.csv", fileEncoding = "UTF-8")
