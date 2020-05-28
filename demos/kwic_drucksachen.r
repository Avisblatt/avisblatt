# Interactive exploratory
# data analysis based on
# avisblatt year 1834
library(readtext)
library(quanteda)
library(jsonlite)
library(dplyr)
library(textcat)
library(ggplot2)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
avis_1834 <- readtext("data/groundtruth1834.csv")
avis_1834$doc_id <- avis_1834$text
avis_1834$text <- NULL
getOption("max.print")


# ocr corrections 1834
avis_1834 <- readtext("data/groundtruth1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
ids_by_lang

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

summary(corpus_1834)

#subset
corpus_1834_de <- corpus_subset(corpus_1834,
                                (docvars(corpus_1834,"id")%in%
                                   ids_by_lang$de))
docvars(corpus_1834_de)

# ocr corrections 1734
avis_1734 <- readtext("data/groundtruth1734.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1734$text <- correct_ocr(avis_1734$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")

corpus_1734 <- corpus(avis_1734,
                      docid_field = "doc_id")

summary(corpus_1734)

#subset
corpus_1734_de <- corpus_subset(corpus_1734,
                                (docvars(corpus_1734,"id")%in%
                                   ids_by_lang$de))

#drucksachen
# generate csv-file to get an impression of ads including print-related terms

druckzeug_1834 <- kwic(corpus_1834_de,
                       pattern = "Buch|Bücher[n]|Bucher",
                       valuetype = "regex")
write.table(druckzeug_1834, "temp/druckzeugkwic.csv",sep=",")

# List of entries in adcontent with tag "05" drucksachen
lAdcontents <- unique(docvars(druckzeug_corpus_clean_1834,"adcontent"))
# Indices of lAdcontents with entries pertaining to Drucksachen (encoded by substring "05")
# Note: grepl("05",lAdcontents) is a vector (say v) such that
#       v[i] is TRUE if and only if lAdcontents[i] contains "05"
#       as a substring.
#       which(v) is the vector (say w) of indices of v which is TRUE
#       e.g. w[1] = 3 if and only if v[3] = TRUE
iAdcontentsDruck <- which(grepl("05", lAdcontents))
# Given a vector w with integer entries and a vector u, the vector
# returned by u[w] is the vector containing only the
# entries for indices w
# e.g. let w <- c(1,3,4,8) and u <- c("a","b","c","a","b","c","d","a","x")
# we have
# > u[w]
# [1] "a" "c" "a" "a"

#generate a subset including ads tagged with "05" drucksachen

drucksachenfilter <- lAdcontents[iAdcontentsDruck]

Corpus_druck_set <- corpus_subset(corpus_1834_de,(docvars(corpus_1834_de, "adcontent")%in% drucksachenfilter))



### remove punctuation and numbers
druckzeug_corpus_clean_1834 <- corpus_druck_set %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

### some plots/wordclouds

textplot_wordcloud(dfm(druckzeug_corpus_clean_1834),
                   max_words = 100)

textplot_wordcloud(dfm(Corpus_druck_set),
                   max_words = 50)

dict <- dictionary(list(book = "Buch|Bücher[n]|Bucher",
                        edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
                        material = "gedruckt|Pergament",
                        person = "Buchhändler|Buchdrucker|Buchbinder",
                        place = "Buchhand|Buchdruckere[y|i]|Buchladen|Leihbibl|Leseanstalt",
                        format_1 = "in Fol.",
                        format_2 = "in 4to.",
                        format_3 = "4°|8°|tom.$|[O|o]ctavo|Bogen|Bögen|Halbfranzband|[ein|un]gebunden|brosch[.|iert]",
                        format_4 = "in [1-9] Bänden",
                        format_5 = "gedruckte[n] Fortsetzung",
                        ausstattung = "Kupf[f]er|Holzschnitt|Stahlstich",
                        catalog = "Catalogus|Katalog",
                        participant = "Mithalte*|Pr[ae|ä]numerant[en]|Abonnent",
                        types = "Wörterbuch|Zeitung|Zeitschrift",
                        type = "Neueste Schriften",
                        title_1 = "Rauracher|Rau-racher|Raura-cher",
                        title_2 = "Allgemeine[n] Zeitung",
                        title_3 = "Christliche[r|n] Volksbote*",
                        # title_4 = "Kantonsblatt|Kantons-blatt",
                        title_5 = "Annalen",
                        title_6 = "Missions-Magazin",
                        title_7 = "Basler-Zeitung|Basler Zeitung",
                        title_8 = "Wochenblatt"
))

druckzeug_1734 <- kwic(corpus_1734_de,
                       dictionary(dict),
                       valuetype = "regex")
druckzeug_1734
write.csv(druckzeug_1734, file = "data/druckzeug", fileEncoding = "UTF-8")

druckzeug_corpus_clean_1734 <- druckzeug_1734 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(druckzeug_corpus_clean_1734),
                   max_words = 50)

# KWIC #######################################
# Keywords in context analysis examples ######
# The idea here is to identify
# patterns (defined as a regular expression)
# that would match all ads representing
# a particular category / tag



