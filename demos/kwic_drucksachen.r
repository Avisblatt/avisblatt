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

druckzeug_corpus_clean_1834 <- corpus_1834_de %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

druckzeug_1834 <- kwic(druckzeug_corpus_clean_1834,
                       pattern = "Buch|Bücher[n]|Bucher",
                       valuetype = "regex")
druckzeug_1834

textplot_wordcloud(dfm(druckzeug_corpus_clean_1834),
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

########### Problem with lexicons for things:
### words or part of words do often occur in different contexts
### (e.g. as part of verbs) than description of relevant objects
### is there a possibility to only search nouns
### (uppercase in beginning of word, not necessarily beginning of search term)
### or to exclude certain words from search
### after looking through kwic-analysis?

## Heimtextilien
heimtex_1834 <- kwic(corpus_1834,
                pattern = "[T|t]afeltuch|[T|t]ischtuch|[T|t]ischzeug|
                [S|s]chaubdeck|[T|t]ischdeck|[D|d]eckbet|[H|h]auzeug|
                [T|t]eppi|[T|t][e|a]pi|[M|m]a[t|d]rat",
                valuetype = "regex")

heimtex_1834

heimtex_corpus <- corpus_subset(corpus_1834,
                           docnames(corpus_1834) %in%
                             heimtex_1834$docname)

heimtex_corpus_clean <- heimtex_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(heimtex_corpus_clean),
                   max_words = 50)

## Betten
bett_1834 <- kwic(corpus_1834,
                     pattern = "[B|b]ett|[B|b]eth|[K|k]orbwag|[W|w]iege",
                     valuetype = "regex")
# includes "Elisabeth", "Verschwiegenheit", Babette" and "bettel"
# how do i exclude that without loosing
# "-beth" (e.g. "Himmelbeth"), "bett-" (e.g. "Bettstatt") and "-wiege" (e.g. "Kinderwiege") in other words?

bett_1834

bett_corpus <- corpus_subset(corpus_1834,
                                docnames(corpus_1834) %in%
                                  bett_1834$docname)

bett_corpus_clean <- bett_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bett_corpus_clean),
                   max_words = 50)


## Schränke und Aufbewahrung
schrank_1834 <- kwic(corpus_1834,
                  pattern = "[K|k]asten|[B|b]uffet|[C|c]orpus|[K|k|C|c]ommode",
                  valuetype = "regex")
# "kommode" is a very problematic, since it can also be an adjective for other objects,
# "[S|s]ch[a|ä]fft" would be another possible word for Schrank
# but then it does include a lot of unrelated verbs (z.B. "angeschafft")

schrank_1834

schrank_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                               schrank_1834$docname)

schrank_corpus_clean <- schrank_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(schrank_corpus_clean),
                   max_words = 100)


## Ofen und verwandte Objekte
ofen_1834 <- kwic(corpus_1834,
                     pattern = "[O|o|ö|Ö]fen|[O|o]efen|[F|f]euerh[u|ü]nd|[K|k|C|c]amin",
                     valuetype = "regex")
# includes "Oberhofen", "Bachofen", "Alekoven", "Alekofen" how do I exclude that?

ofen_1834

ofen_corpus <- corpus_subset(corpus_1834,
                                docnames(corpus_1834) %in%
                               ofen_1834$docname)

ofen_corpus_clean <- ofen_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(ofen_corpus_clean),
                   max_words = 100)
# some of these ads are ads for immo with an "ofen", not for the object itself
# how can i exclude ads with immo-words in them?


## Spiegel
spiegel_1834 <- kwic(corpus_1834,
                  pattern = "[S|s]piegel",
                  valuetype = "regex")

spiegel_1834

spiegel_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                               spiegel_1834$docname)

spiegel_corpus_clean <- spiegel_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(spiegel_corpus_clean),
                   max_words = 50)


## Uhren
uhr_1834 <- kwic(corpus_1834,
                  pattern = "[U|u]hr",
                  valuetype = "regex")
# includes indications of time: how to exclude these? regex for "no numbers before uhr"?
# includes "Uhrenmacher", "Fuhrgeschirr", "Fuhrwerk", "Fuhrmann";
# how to exclude these? regex for "no F/f before uhr"?

uhr_1834

uhr_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                              uhr_1834$docname)

uhr_corpus_clean <- uhr_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(uhr_corpus_clean),
                   max_words = 100)


## Sitzmöbel
sitz_1834 <- kwic(corpus_1834,
                  pattern = "[S|s]essel|[S|s]t[u|ü]hl|[F|f][au|ua]teil",
                  valuetype = "regex")
# "[S|s]t[u|ü]hl" includes "Webstuhl", "Bandstuhl", "Frauenstuhl", "Weiberstuhl", "Stuhlschreiner"
# How to exclude these?

sitz_1834

sitz_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                               sitz_1834$docname)

sitz_corpus_clean <- sitz_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(sitz_corpus_clean),
                   max_words = 100)


## Tische
tisch_1834 <- kwic(corpus_1834,
                  pattern = "[T|t]isch",
                  valuetype = "regex")
# includes "praktisch", "schottisch", "optisch", "homeopatisch",
# "Tischzeug" (=Haustextilien), "Bautischler", "brittisch", "politisch", etc.:
# How to exclude those? is it possible to find nouns only?

tisch_1834

tisch_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                               tisch_1834$docname)

tisch_corpus_clean <- tisch_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tisch_corpus_clean),
                   max_words = 100)


## Geschirr
geschirr_1834 <- kwic(corpus_1834,
                   pattern = "[G|g]eschir|[K|k|C|c]anne",
                   valuetype = "regex")
# includes "Pferdegeschirr", "Chaise-Geschirr" "Wirtshaus zur Kanne",
# "Kummetgeschirr", Fuhrgeschirr" etc.: How to exclude these?

geschirr_1834

geschirr_corpus <- corpus_subset(corpus_1834,
                              docnames(corpus_1834) %in%
                                geschirr_1834$docname)

geschirr_corpus_clean <- geschirr_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(geschirr_corpus_clean),
                   max_words = 100)


