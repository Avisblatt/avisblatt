# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
#added encoding parameter, as otherwise Umlaute get scrambled when loading external dictionaries
source("R/avis_stop.R", encoding = "UTF-8")
source("R/ocr_corrections.R", encoding = "UTF-8")
source("R/tagfilters_utils.R", encoding = "UTF-8")
source("R/tagfilters_household.R", encoding = "UTF-8")
source("R/tagfilters_main.R", encoding = "UTF-8")
source("R/tagfilters_practices.R")
source("R/cleaners.R")
source("R/validate_filters.R")
source("R/utils.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text", encoding = "UTF-8")
avis_1834$text <- correct_ocr(avis_1834$text)

#just ads in German
ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                          docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                             (docvars(corpus_1834_all,
                                      "id") %in%
                                ids_by_lang$de))

weave <- tagfilter_weaving()
weave_ids <- weave$filtrate(corpus_1834,ignore.case = F)

spin <- tagfilter_spinning()
spin_ids <- spin$filtrate(corpus_1834, ignore.case = F)

cook <- tagfilter_cooking()
cook_ids <- cook$filtrate(corpus_1834, ignore.case = F)

knit_f <- tagfilter_knitting()
knit_ids <- knit_f$filtrate(corpus_1834, ignore.case = F)



get_text_by_id(corpus_1834, cook_ids)
get_text_by_id(corpus_1834, knit_ids)
xx <- get_text_by_id(corpus_1834, spin_ids)


