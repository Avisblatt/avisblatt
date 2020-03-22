# Load libraries and source functions
# in pre-package state
library(avisblatt)
# During Development rather run
devtools::load_all()

corpus_1834 <- avis_create_corpus("data/avis_1834.csv")

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
                   max_words = 120)

groundtruth <- readtext("data/groundtruth*.csv",
                        text_field = "text", encoding = "UTF-8")
