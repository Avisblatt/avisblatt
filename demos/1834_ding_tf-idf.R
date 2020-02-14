# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tagfilters_utils.R")
source("R/tagfilters_household.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")

# read document
avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

# create subcorpus using existing categories with ads selling things
ding_1834 <- corpus_subset(corpus_1834, grepl("06ding", adcontent) & grepl("01kauf", finance))

ding_1834[1:10]

# tokenize ads, bigrams
de_tok <- ding_1834 %>%
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


# runing 50 lines of
# the weighting dfm
topfeatures(
  de_tf_idf[1:50,],
  n = 2,
  decreasing = TRUE,
  groups = "id"
)
