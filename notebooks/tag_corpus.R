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
                      text_field = "text")

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




# Document ids by manual category ("adcontent")

#!!! not sure how to do it - pick ids for different values possible in adcontent, i.e.
#    01textilien,02hausrat,03lebensmittel,04schmuck,05drucksachen,06ding,07tier,08immo,09kirchenstuhl,10arbeitsstelle,11kost,12platzierung,13caritas,14finanz,15lotterie,16transport,17auskunft,18uneindeutig
#   At least here an attempt to pick work ads - works fine except for ads that have adcontent like "06ding,10arbeitsstelle")

corpus_1834_work <- corpus_subset(corpus_1834,
                                  grepl("10arbeitsstelle",adcontent))

grepl("10arbeitsstelle",avis_1834$adcontent)


corpus_1834_work <- corpus_subset(corpus_1834,
                                  adcontent %in% "10arbeitsstelle")

ids_by_manualcat <- list(work = docvars(corpus_1834_work,"id"))

write_json(ids_by_manualcat,
           path =  "data/ids_by_manualcat.json")
