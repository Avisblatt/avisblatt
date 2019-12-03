# Quanteda based tf-idf example

# load libraries
library(readtext)
library(quanteda)
library(jsonlite)
library(dplyr)

# avisblatt package candidates
# this is not packaged yet.
# hence single files have to be sourced 'manually'
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tag_dictionaries.R")
source("R/cleaners.R")

# READ DOCUMENT ############
# and make use of previously assigned tags
# in order to save time.
# here: select german ads only
avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "ad_content")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

ids_by_lang <- fromJSON("data/ids_by_lang.json")

de_1834 <- corpus_subset(corpus_1834,
              docvars(corpus_1834, "id") %in% ids_by_lang$de)

# tokenize ads
de_tok <- de_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

# create a document feature matrix from
# tokens in order to count term frequency
# and/or apply weighting.
de_dfm <- de_tok %>%
  dfm()

# make sure to check ?docfreq
# no weighting argumens simply
# means count, threshold set to zero
# showing all features that occur at least once.
df <- docfreq(de_dfm)

s_df <- sort(df,decreasing = T)
# top 10 (most frequent terms)
head(s_df,10)

de_tf_idf <- dfm_tfidf(de_dfm)
head(de_tf_idf[1,1:10])

class(de_tf_idf)

tsf <- textstat_frequency(de_dfm)
head(tsf)
tail(tsf)


# this can take quite some time
# hence we only run it for 10 lines of
# the weighting dfm here.
topfeatures(
  de_tf_idf[1:10,],
  n = 3,
  decreasing = TRUE,
  groups = "id"
)





