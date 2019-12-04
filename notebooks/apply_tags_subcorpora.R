library(jsonlite)

ids_by_tag <- fromJSON("data/ids_by_tag.json")

corpus_1834_other <-
  corpus_subset(corpus_1834,
                !(docvars(corpus_1834,"id") %in%
                   c(ids_by_tag$work,ids_by_tag$real_estate)
                ))

dfm_corpus_1834 <- corpus_1834_other %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm_corpus_1834,
                   max_words = 100)




#check "work ads by dict" against ground-truth

ids_by_manualcat <- fromJSON("data/ids_by_manualcat.json")
ids_by_tag <- fromJSON("data/ids_by_tag.json")

#ads wrongly not identified as work (false negatives = type II error)
#PROBLEM: does the following really pick only those ids that are in manualcat$work but not in tag$work?
corpus_1834_error_is_work <-
  corpus_subset(corpus_1834,
                (docvars(corpus_1834,"id") %in% c(ids_by_manualcat$work))
                & (!docvars(corpus_1834,"id") %in% c(ids_by_tag$work)))

dfm_corpus_1834e2 <- corpus_1834_error_is_work %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm_corpus_1834e2,
                   max_words = 100)

ndoc(corpus_1834_error_is_work)
length(ids_by_manualcat$work)
ndoc(corpus_1834_error_is_work)/length(ids_by_manualcat$work)


#ads wrongly identified as work (false positives = type I error)

corpus_1834_error_aint_work <-
  corpus_subset(corpus_1834,
                (!docvars(corpus_1834,"id") %in% c(ids_by_manualcat$work))
                & (docvars(corpus_1834,"id") %in% c(ids_by_tag$work)))

ndoc(corpus_1834_error_aint_work)

summary(corpus_1834_error_aint_work)
