# Interactive exploratory
# data analysis based on
# avisblatt year 1834
library(readtext)
library(quanteda)
library(ggplot2)
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

# KWIC #######################################
# Keywords in context analysis examples ######
# The idea here is to identify
# patterns (defined as a regular expression)
# that would match all ads representing
# a particular category / tag

# Gesuche
search_1834 <- kwic(corpus_1834,
                pattern = "wünscht",
                valuetype = "regex")

search_corpus <- corpus_subset(corpus_1834,
                           docnames(corpus_1834) %in%
                             search_1834$docname)

search_sample <- corpus_sample(search_corpus,
              size = 5,
              replace = FALSE)

texts(search_sample)

# Immobilien
# Could 'Wohnung' be used in different contexts, e.g.,
# indicate where people /  workforce lives (as opposed)
# to rent / sale
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
work_dict <- dictionary(list(work = "Arbeit$|arbeiten$|Arbeiter$|Lehrling|Anstellung",
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


# Creating new corpuses,
# by flattening them.. but we could also
# stay on a single document level, which makes
# corpora merges more difficult because documents are
# not unique across corpora...
work_m <- corpus(texts(work_corpus,
                         groups = rep(1, ndoc(work_corpus))))
ra_m <- corpus(texts(ra_corpus,
                     groups = rep(1, ndoc(ra_corpus))))

search_m <- corpus(texts(search_corpus,
                     groups = rep(1, ndoc(search_corpus))))
docvars(work_m, "group") <- "work"
docvars(ra_m, "group") <- "ra"
docvars(search_m, "group") <- "search"
m <- c(ra_m, work_m,search_m)

m_g_dfm <- m %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop()) %>%
  dfm() %>%
  dfm_group(groups = "group")



table(docvars(m, "group"),
      useNA="al")

# Lexical Diversity
m_lexdiv <- textstat_lexdiv(m_g_dfm,
                            measure = c("TTR",
                                                   "CTTR"))

# similarity
tstat_dist <- as.dist(textstat_dist(m_g_dfm))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)

# topfeatures by group
topfeatures(m_g_dfm, groups = "group")

slim_dfm <- dfm_trim(avis_dfm,
                     min_termfreq = 10)
topfeatures(slim_dfm)
nfeat(slim_dfm)
nfeat(avis_dfm)
fcm_avis <- fcm(slim_dfm)
feat <- names(topfeatures(fcm_avis, 50))
fcmat_avis_select <- fcm_select(fcm_avis,
                                pattern = feat)

s <- log(colSums(dfm_select(avis_dfm, feat)))
set.seed(144)
textplot_network(fcmat_avis_select,
                 min_freq = .7,
                 vertex_size = s / max(s) * 3)

work_slim <- work_m %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop()) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5)
topfeatures(work_slim)
fcm_work <- fcm(work_slim)
feat_work <- names(topfeatures(fcm_work, 50))
fcmat_work_select <- fcm_select(fcm_work,
                                pattern = feat_work)
s <- log(colSums(dfm_select(work_slim, feat_work)))
set.seed(144)
textplot_network(fcmat_work_select,
                 min_freq = .7,
                 vertex_size = s / max(s) * 3)


work_slim %>%
  textstat_frequency(n = 15) %>%
  ggplot(aes(x = reorder(feature, frequency),
             y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


## Keyness of Terms for Topic ########
# interactive robustness checks for words
# filters
tstat_key <- textstat_keyness(m_g_dfm,
                              target = docvars(m_g_dfm) == "search")

textplot_keyness(tstat_key)


## Collocation analysis
oo <- work_m %>% tokens()
tstat_col2 <- tokens_select(oo,
                            pattern = "^[A-Z]",
                            valuetype = 'regex',
                            selection = "keep",
                            case_insensitive = TRUE,
                            padding = TRUE) %>%
  textstat_collocations(min_count = 30, size = 3)
head(tstat_col2, 20)





