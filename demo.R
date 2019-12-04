# Interactive exploratory
# data analysis based on
# avisblatt year 1834
library(readtext)
library(quanteda)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
avis_1834 <- readtext("data/avis_1834.csv")
avis_1834$doc_id <- avis_1834$text
avis_1834$text <- NULL

# ocr corrections
avis_1834$ad_content <- correct_ocr(avis_1834$ad_content)



corpus_1834 <- corpus(avis_1834,
                      text_field = "ad_content",
                      docid_field = "doc_id")

summary(corpus_1834)

# KWIC #######################################
# Keywords in context analysis examples ######
# The idea here is to identify
# patterns (defined as a regular expression)
# that would match all ads representing
# a particular category / tag


# Immobilien
ra_1834 <- kwic(corpus_1834,
                pattern = "losament|^Zimmer|Wohnung|Kammer|Keller",
                valuetype = "regex")

ra_corpus <- corpus_subset(corpus_1834,
                           docnames(corpus_1834) %in%
                             ra_1834$docname)

ra_corpus_clean <- ra_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(ra_corpus_clean),
                   max_words = 200)

# Arbeit
# combine phrase and single words in a dictionary
# the expression phrase() is not needed
work_dict <- dictionary(list(work = "Arbeit$|arbeiten$|Arbeiter$|Lehrling",
                             position ="Platz als"))

work_1834 <- kwic(corpus_1834,
                  pattern = work_dict,
                  valuetype = "regex")

work_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                               work_1834$docname)


work_corpus_clean <- work_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(work_corpus_clean),
                   max_words = 100)

# Heimtextilien
heimtex_1834 <- kwic(corpus_1834,
                pattern = "[T|t]afeltuch|[T|t]ischtuch|[T|t]ischzeug|[S|s]chaubdeck|[T|t]ischdeck|[D|d]eckbet|[H|h]auzeug|[T|t]eppi|[T|t][e|a]pi|[M|m]a[t|d]rat",
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

# Secondhand
secondhand_dict <- dictionary(list(secondhand = "neuverf",
                             position ="so viel als neu"))

secondhand_1834 <- kwic(corpus_1834,
                  pattern = secondhand_dict,
                  valuetype = "regex")

secondhand_corpus <- corpus_subset(corpus_1834,
                             docnames(corpus_1834) %in%
                               secondhand_1834$docname)


secondhand_corpus_clean <- secondhand_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(secondhand_corpus_clean),
                   max_words = 100)

