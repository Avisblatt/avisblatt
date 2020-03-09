# Load libraries and source functions
# in pre-package state
library(avisblatt)
# During Development rather run
devtools::load_all()

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

### checking and cleaning different tagfilters for descriptions of quality

## Quality: Secondhand

secondhand <- tagfilter_secondhand()
sh_corpus <- secondhand$filtrate(corpus_1834)

sh_clean <- sh_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(c(avis_stop(),
                  "neu","neuer","neues",
                  "neue","viel"))

textplot_wordcloud(dfm(sh_clean),
                   max_words = 200)
