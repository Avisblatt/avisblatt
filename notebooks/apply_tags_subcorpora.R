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
