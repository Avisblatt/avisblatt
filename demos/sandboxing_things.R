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
source("R/tagfilters_things.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

#just ads in German
ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                          docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                             (docvars(corpus_1834_all,"id") %in%
                                ids_by_lang$de))

# negating %in% operator
`%notin%` <- Negate(`%in%`)

### checking and cleaning different tagfilters for misc things

## Carriages
carriage <- tagfilter_carriage()

carriage_ids <- carriage$filtrate(corpus_1834,ignore.case = T)

carriage_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   carriage_ids)

carriage_texts <- carriage_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carriage_kwic <- kwic(carriage_subset,
                      pattern = "Trosque",
                      valuetype = "regex",
                      ignore.case = T)
carriage_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
carriage_subset_clean <- carriage_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(carriage_subset_clean),
                   max_words = 200)


# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_carriage <- validate_filter(corpus_1834, carriage_ids,
                                       search_col = "adcontent",
                                       pattern = "06ding")
validation_carriage

# Filters for TRUE and FALSE positives
doc_ids <- corpus_1834$documents[,"id"]
filter_T_hc_T <- doc_ids[(doc_ids %in% carriage_ids)]
filter_T_hc_F <- doc_ids[(doc_ids %notin% carriage_ids)]

#TRUE positives
b_t <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       validation_carriage$filter_T_hc_T)
b_t$documents$texts[21:30]

#FALSE positives
b_f <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       validation_carriage$filter_T_hc_F)
b_f$documents$texts[1:11]

# 9 FALSE positives are actually correctly identified as carriages or related objects!!!; only two (Nr. 9 and 10, repeated ad) are wrongly identified (object lost in a carriage)

