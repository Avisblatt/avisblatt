# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/cleaners.R")

# READ DOCUMENT ############
# Creates German and French Corpora
groundtruth <- readtext("data/*.csv",
                        text_field = "text")

groundtruth$text <- correct_ocr(groundtruth$text)

corpus_gt <- corpus(groundtruth,
                      docid_field = "doc_id")

french_doc_ids <- detect_lang(corpus_gt)

corpus_gt_de <- corpus_subset(corpus_gt,
                                !(docvars(corpus_gt,"id") %in%
                                    french_doc_ids))

corpus_gt_fr <- corpus_subset(corpus_gt,
                                (docvars(corpus_gt,"id") %in%
                                   french_doc_ids))

ids_by_lang <- list(de = docvars(corpus_gt_de,"id"),
                    fr = docvars(corpus_gt_fr,"id"))

write_json(ids_by_lang,
           path =  "data/ids_by_lang.json")
