# Load libraries and source functions
# in pre-package state
library(avisblatt)
# During Development rather run
devtools::load_all()


# corpus 1734

avis_1734 <- readtext("data/groundtruth1734.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1734$text <- correct_ocr(avis_1734$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1734_all <- corpus(avis_1734,
                          docid_field = "doc_id")
corpus_1734 <- corpus_subset(corpus_1734_all,
                             (docvars(corpus_1734_all,"id") %in%
                                ids_by_lang$de))

# corpus 1834

avis_1834 <- readtext("data/groundtruth1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                          docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                             (docvars(corpus_1834_all,"id") %in%
                                ids_by_lang$de))

corpus_all <- c(corpus_1734, corpus_1834)

### checking and cleaning different tagfilters for descriptions of quality


## Secondhand

secondhand <- tagfilter_secondhand()
sh_corpus <- secondhand$filtrate(corpus_1834)


secondhand_ids_1734 <- secondhand$filtrate(corpus_1734, ignore.case = T)
secondhand_ids_1834 <- secondhand$filtrate(corpus_1834, ignore.case = T)

secondhand_subset_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                        secondhand_ids_1734)

secondhand_subset_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                        secondhand_ids_1834)

secondhand_subset_all <- c(secondhand_subset_1734, secondhand_subset_1834)
secondhand_ids_all <- c(secondhand_ids_1734, secondhand_ids_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
secondhand_kwic <- kwic(secondhand_subset_all,
                      pattern = phrase("so viel als neu"),
                      valuetype = "regex",
                      ignore.case = T)
secondhand_kwic

secondhand_subset_clean <- secondhand_subset_1834 %>%

  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(c(avis_stop(),
                  "neu","neuer","neues",
                  "neue","viel"))

textplot_wordcloud(dfm(sh_clean),
                   max_words = 120)

groundtruth <- readtext("data/groundtruth*.csv",
                        text_field = "text", encoding = "UTF-8")
