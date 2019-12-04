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

# READ DOCUMENT ############
# Creates German and French Corpora
avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "ad_content")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

french_doc_ids <- detect_lang(corpus_1834)

corpus_1834_de <- corpus_subset(corpus_1834,
                                !(docvars(corpus_1834,"id") %in%
                                    french_doc_ids))

corpus_1834_fr <- corpus_subset(corpus_1834,
                                (docvars(corpus_1834,"id") %in%
                                    french_doc_ids))

ids_by_lang <- list(de = docvars(corpus_1834_de,"id"),
                    fr = docvars(corpus_1834_fr,"id"))

write_json(ids_by_lang,
           path =  "data/ids_by_lang.json")


# Document ids by category
dispatch <- list(work = dict_work(),
                 real_estate = dict_real_estate())

ids_by_tag <- lapply(dispatch,
              corpus_id_by_dict,
              corp = corpus_1834_de)


# store mapping to disk persistently
write_json(ids_by_tag,
           path =  "data/ids_by_tag.json")














