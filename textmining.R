library(tm)
library(textmineR)
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# mining for the immo category...
tm_set <- ads_1834 %>%
  select(id, adcontent, ad_content)

docs <- Corpus(VectorSource(tm_set$ad_content))
docs_l <- tm_map(docs, tolower)

dtm <- DocumentTermMatrix(docs_l)
# remove sparse items to reduce memory usage
# https://stackoverflow.com/questions/47403591/convert-large-document-term-document-matrix-into-matrix

dtm2 <- removeSparseTerms(dtm, sparse = 0.99)


mat <- as.matrix(dtm2)
nrow(mat)


dtm2 <- as.matrix(dtm)
tf_simple <- colSums(dtm2)

# this is certainly cool to
# identify avisblatt stopwords
# might wanna strip commas
tf_sorted <- sort(tf_simple,
                  decreasing=TRUE)

tf_gt_20 <- tf_sorted[tf_sorted > 20]

save("dtm2", file="dtm2.RData")

# looking for alternatives here
# # people seem to like Wards method here
# dist_m <- dist(dtm2, method="Ward")
#
# dtm2[2,1:10]
# s <- dtm2[1, which(colnames(dtm2) %in% names(tf_gt_20))]
# for(i in 2:nrow(dtm2)){
#   s <- cbind(s,dtm2[i,which(colnames(dtm2) %in% names(tf_gt_20))])
# }
#
#
# colnames(s) titles
# h <- hclust(dist(t(s0)), method = "ward")


# textmineR example
# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
# https://rpubs.com/saqib/DocumentClustering




# create a document term matrix
dtm_txtminr <- CreateDtm(doc_vec = ads_1834$ad_content, #
                 doc_names = ads_1834$id,
                 ngram_window = c(1, 2), # minimum and
                 stopword_vec = bsl_stop, # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

tdf <- TermDocFreq(dtm_txtminr)

tfidf <- t(dtm_txtminr[ , tdf$term]) * tdf$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)


# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/


# tidy textmining topic modelling
# https://www.tidytextmining.com/topicmodeling.html



library(tidytext)
library(topicmodels)

to_dtm <- ad_words %>%
  select(id, word, n)

dtm_tt <- to_dtm %>%
  cast_dtm(id, word, n)

# where are my ads at?
chapters_lda <- LDA(dtm_tt, k = 8,
                    control = list(seed = 1234))

chapter_topics <- tidy(chapters_lda, matrix = "beta")

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
tail(chapters_gamma)
head(chapters_gamma)

# LDA: does not rely on distances...
# The intuition behind the LDA topic model is that words
# belonging to a topic appear together in documents.
# softer clusters



# Token distribution analysis maybe interesting (4)  !!
# https://quanteda.io/articles/pkgdown/replication/digital-humanities.html




# these are all ones
show_ad("2fc3ab80-2cf0-5680-a5c4-b9ba1820596e/a0")
show_ad("0d8e807a-3ecf-50a5-a62d-24ff22f96bcd/a0")
show_ad("d65ada34-e92e-540c-b42f-a098dfee0fdf/a0")

# these are all fours
show_ad("ffe69d71-cea6-5b18-bd5c-58c9500c903b/a4")
show_ad("ffe69d71-cea6-5b18-bd5c-58c9500c903b/a5")
show_ad("ffe69d71-cea6-5b18-bd5c-58c9500c903b/a9")



library(quanteda)



qc <- corpus(min_1834,
             docid_field = "id",
             text_field = "ad_content")

avis_dfm <- dfm(qc, remove = bsl_stop,
                remove_punct = TRUE,
                remove_numbers = TRUE,
                verbose = FALSE)
avis_dfm_trim <- dfm_trim(avis_dfm,
                          min_docfreq = 200,
                          min_termfreq = 80)

textplot_wordcloud(avis_dfm_trim,
                   max_words = 200,
                   rotation = .25,
                   random_order = FALSE,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

# might want to exclude terms on tf-idf basis as well.

# keyword in context
sell <- kwic(qc, pattern = "verkaufen")
immo <- kwic(qc, pattern = "losament*")


# How come this trim stuff produces
# all those NAs, what does essentially happen?
dd <- textstat_dist(avis_dfm_trim,
                    margin = "documents")
tstat_dist <- textstat_dist(dfm_weight(avis_dfm_trim,
                                       scheme = "prop"))

hc$

dd@x <- na.omit(dd@x)

ddd <- as.dist(dd)

hc <- hclust(ddd,method = "ward.D")
summary(hc$height)

a <- cutree(hc, k = 30)

names(head(a[a == 2]))

show_ad(names(head(a[a == 10])))

# there might be empty ads !!! check this
show_ad("ca0b9ae-9202-5f1d-be89-4e7f62d9a630/a22")

ads_1834 %>%
  filter(id =="ca0b9ae-9202-5f1d-be89-4e7f62d9a630/a22")


show_ad("edb3f6f6-742c-506d-a604-db78a4d34678/a8")
show_ad("aca0b9ae-9202-5f1d-be89-4e7f62d9a630/a14")

show_ad("71004b8e-e50c-52f1-af9b-973d0b4c02a2/a12")
show_ad("2d8496b7-5ac5-508f-a685-c777bffd1d5d/a37")


# First take away...
# with all those documents,
# distances are probably not the way to go
# unless we look at years... each document
# is a year...
# Question to the quanteda guy:
# What do you do if you have alot of rather short
# documents.. .





save("hc",file = "hc.RData")

plot(hc)

hc$labels



mat <- as.matrix(tstat_dist)


tstat_dist@x

avis_dfm_trim@p

moo <- hclust(as.dist(tstat_dist))

tstat_dist@x

avis_dfm_trim@

length(avis_dfm_trim@)


pres_cluster <- hclust(dd)




ddd <- as.dist(dd)
print(object.size(ddd), units="Mb")

oo <- na.omit(ddd)
table(oo)
library(cluster)
sum(is.infinite(ddd))


ward <- hclust(ddd, method = "ward.D")

save("dd", file="textstat_dist.RData")

head(avis_dfm)
avis_dfm[,1:5]














