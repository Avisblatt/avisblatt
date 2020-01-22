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
source("R/tagfilters_secondhand.R")
source("R/cleaners.R")
source("R/validate_filters.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

### checking and cleaning different tagfilters for descriptions of quality

## Quality: Secondhand

secondhand <- tagfilter_secondhand()

secondhand_ids <- secondhand$filtrate(corpus_1834, ignore.case = T)

secondhand_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                     secondhand_ids)

secondhand_texts <- secondhand_subset$documents$texts

secondhand_subset_clean <- secondhand_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(secondhand_subset_clean),
                   max_words = 200)
