# load libraries
library(readtext)
library(quanteda)
library(jsonlite)
library(dplyr)

# avisblatt package candidates
# this is not packaged yet.
# hence single files have to be sourced 'manually'
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/cleaners.R")

# READ DOCUMENT ############
# and make use of previously assigned tags
# in order to save time.
# here: select german ads only
avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "adcontent")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

ids_by_lang <- fromJSON("data/ids_by_lang.json")
de_1834 <- corpus_subset(corpus_1834,
                         docvars(corpus_1834, "id") %in% ids_by_lang$de)


de_1834_dfm <- de_1834 %>%
  dfm()

de_1834_dist <- textstat_dist(de_1834_dfm)

dim(de_1834_dist)




by_year <- lapply(1734:1844, avis_get_corpus)

c_1834 <- avis_get_corpus(1834)

by_year[[1]]
