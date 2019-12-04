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


#CHECKING RECOGNITION RATE of a single potential dict entry

#Try different regular expressions as candidates
#Problem: expressions with spaces (like "Platz als") don't work at all :-/

candidate <- "Knecht"

docnames(corpus_1834) <- docvars(corpus_1834, "id")
kwic_res <- kwic(corpus_1834,
                 pattern = candidate,
                 valuetype = "regex")
ids_by_candidate <- kwic_res$docname
ids_by_manualcat <- fromJSON("data/ids_by_manualcat.json")

fn_errors <- corpus_subset(corpus_1834,
                           (docvars(corpus_1834,"id") %in% ids_by_manualcat$work
                            & !(docvars(corpus_1834,"id") %in% ids_by_candidate)))
fp_errors <- corpus_subset(corpus_1834,
                           (docvars(corpus_1834,"id") %in% ids_by_candidate) &
                             !(docvars(corpus_1834,"id") %in% ids_by_manualcat$work))

tagged_correctly <- length(ids_by_manualcat$work)-ndoc(fn_errors)

print( paste("There are", ndoc(fn_errors), "false negatives (undetected ads) out of",
             length(ids_by_manualcat$work) ,"ads, i.e. you correctly identified",
             tagged_correctly, "ads (success rate:",
             round(100*(1-ndoc(fn_errors)/length(ids_by_manualcat$work)),2),
             "%). You also produced", ndoc(fp_errors), "false positives out of",
             ndoc(corpus_1834)-length(ids_by_manualcat$work), "unrelated ads (",
             round(100*ndoc(fp_errors)/(ndoc(corpus_1834)-length(ids_by_manualcat$work)),2),
             "%). Of the", tagged_correctly+ndoc(fp_errors), "ads you tagged,",
             round(100*tagged_correctly/(tagged_correctly+ndoc(fp_errors)),2),
             "% were correct."
        ))
