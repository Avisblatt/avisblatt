# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(ggplot2)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tag_dictionaries.R")

# READ DOCUMENTS ############
avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "ad_content")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

french_doc_ids <- detect_lang(corpus_1834)

