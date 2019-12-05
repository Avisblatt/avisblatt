# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tag_dictionaries.R")
source("R/cleaners.R")


avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

# cats inspiration
# a <- strsplit(avis_1834$adcontent,",")
# unique(unlist(a))

xx <- corpus_id_by_dict(corpus_1834, dict_work())

head(docvars(corpus_1834))


ids_work <- filter_by_dict(corpus_1834, tag_dict_work())

corpus_work <- corpus_subset(corpus_1834,
                             docvars(corpus_1834,"id") %in% ids_work)


debug(filter_by_dict)
ids_bed <- filter_by_dict(corpus_1834, tag_dict_bed())
corpus_bed <- corpus_subset(corpus_1834,
                             docvars(corpus_1834,"id") %in%
                              ids_bed)


corpus_bed$documents$texts[20]



