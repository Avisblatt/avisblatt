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



#CHECKING RECOGNITION RATE of complete ids_by_tag

#Recreate ids_by_tag (List of ads autotagged as work by dictionary)
#to speed up recreation, it is confined to work tag, saved in a different json)
dispatch <- list(work = dict_work())
ids_by_tag_work <- lapply(dispatch,
                     corpus_id_by_dict,
                     corp = corpus_1834)
write_json(ids_by_tag_work,
           path =  "data/ids_by_tag_work.json")
################

ids_by_manualcat <- fromJSON("data/ids_by_manualcat.json")
ids_by_tag <- fromJSON("data/ids_by_tag_work.json")

#ads wrongly not identified as work (false negatives = type II error)

fn_errors <- corpus_subset(corpus_1834,
  (docvars(corpus_1834,"id") %in% ids_by_manualcat$work & !(docvars(corpus_1834,"id") %in% ids_by_tag$work)))

dfm_corpus_fn <- fn_errors %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm_corpus_fn,
                   max_words = 100)

fn_errors
length(ids_by_manualcat$work)
ndoc(fn_errors)/length(ids_by_manualcat$work)
"also Trefferquote"
1-ndoc(fn_errors)/length(ids_by_manualcat$work)


#ads wrongly identified as work (false positives = type I error)

fp_errors <- corpus_subset(corpus_1834,
                           (docvars(corpus_1834,"id") %in% ids_by_tag$work) &
                             !(docvars(corpus_1834,"id") %in% ids_by_manualcat$work))


fp_errors
ndoc(fp_errors)/(ndoc(corpus_1834)-length(ids_by_manualcat$work))

####view some false positives and negatives
fp_errors$documents$texts[1]

fn_errors$documents$texts[3]
