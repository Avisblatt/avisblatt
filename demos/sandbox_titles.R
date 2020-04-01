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
source("R/tagfilters_titles.R")
source("R/cleaners.R")
source("R/avis_create_corpus.R")
source("R/utils.R")
source("R/validate_filters.R")


### corpus
# ... has to be replaced with the year of the current Avisblatt data
avis_... <- readtext("data/groundtruth....csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_...$text <- correct_ocr(avis_...$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_all_... <- corpus(avis_1834,
                          docid_field = "doc_id")

corpus_... <- corpus_subset(corpus_all_...,
                            (docvars(corpus_all_...,"id") %in%
                               ids_by_lang$de))

### subcorpus with ads classified as titles
# forming a subcorpus with only those ads categorised as "isheader"
# if dictionaries work well enough, this step can maybe be skipped
# and thus titles wrongly not categorised as "isheader" could
# also be identified
titles_... <- corpus_subset(corpus_..., grepl("1", isheader))



### Sale offers
# example for code, has to be tested with actual data
saleoffer <- tagfilter_saleoffer()

saleoffer_ids_... <- saleoffer$filtrate(titles_...,ignore.case = T)

saleoffer_... <- corpus_subset(titles_..., docvars(titles_..., "id") %in%
                                        saleoffer_ids_...)

# identifying title-ads that are too long (threshold has to be adapted to each type of title)
# and therefore are either falsely classified as titles or the text region is too large
# and contains text from other ads
saleoffer_toolong_... <- corpus_subset(saleoffer_..., ntoken(saleoffer_...) > 6)

## If this code works, it has to be extended to all other types of titles
## Then there has to be a code identifying those ads  classified as "isheader"
## but not found by dictionaries for either dismissing them as titles
## or expanding and correcting the dictionaries

