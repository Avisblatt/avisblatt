# Interactive exploratory
# data analysis based on
# avisblatt year 1834
library(readtext)
library(quanteda)
library(ggplot2)
library(caret) # for the confusion matrix

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

avis_dfm <- corpus_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop()) %>%
  dfm()


work_dict <- dictionary(list(work = "Arbeit$|arbeiten$|Arbeiter$|Lehrling|Anstellung",
                             position ="Platz als"))

work_1834 <- kwic(corpus_1834,
                  pattern = work_dict,
                  valuetype = "regex")

docvars(corpus_1834, "group") <- "default"
docvars(corpus_1834, "group")[docnames(corpus_1834) %in% work_1834$docname] <- "work"


# Naive Bayes Classifier for Avisblatt example
# is this work or not ?
set.seed(300)
id_train <- sample(1:ndoc(corpus_1834), 3000,
                   replace = FALSE)
# get ready to split the corpus into test and train
docvars(corpus_1834, "id_numeric") <- 1:ndoc(corpus_1834)

avis_train <- corpus_subset(corpus_1834,
                            id_numeric %in% id_train) %>%
  dfm()

avis_test <- corpus_subset(corpus_1834,
                           !id_numeric %in% id_train) %>%
  dfm()


avis_nb <- textmodel_nb(avis_train,
                        docvars(avis_train,
                                "group"))

avis_matched <- dfm_match(avis_test,
                           features = featnames(avis_train))

actual_class <- docvars(avis_matched, "group")
predicted_class <- predict(avis_nb,
                           newdata = avis_matched)
tab_class <- table(actual_class, predicted_class)
tab_class


confusionMatrix(tab_class, mode = "everything")


# Unsupervised learning
# Topic models for text classification

require(quanteda)
require(quanteda.corpora)
require(lubridate)
require(topicmodels)

corp_news <- download('data_corpus_guardian')

corp_news_subset <- corpus_subset(corp_news, 'date' >= 2016)
ndoc(corp_news_subset)

dfmat_news <- dfm(corp_news, remove_punct = TRUE, remove = stopwords('en')) %>%
  dfm_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST')) %>%
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

dfmat_news <- dfmat_news[ntoken(dfmat_news) > 0,]

dtm <- convert(dfmat_news,
               to = "topicmodels")
lda <- LDA(dtm, k = 10)












