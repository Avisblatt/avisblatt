# library(avisblatt)
# During Development rather run
devtools::load_all()

# READ DOCUMENT ############
# and make use of previously assigned tags
# in order to save time.
# here: select german ads only
avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

ids_by_lang <- fromJSON("data/ids_by_lang.json")
de_1834 <- corpus_subset(corpus_1834,
                         docvars(corpus_1834, "id") %in% ids_by_lang$de)


de_1834_dfm <- de_1834 %>%
  dfm()

de_1834_dist <- textstat_dist(de_1834_dfm)
rownames(de_1834_dist) <- 1:nrow(de_1834_dist)
colnames(de_1834_dist) <- 1:ncol(de_1834_dist)

# around  ~ 3.8 seems like a reasonable distance.


dim(de_1834_dist)

de_1834_dist[1:10,1:10]

texts(de_1834)[1:10]


by_year <- lapply(1734:1844, avis_get_corpus)

c_1834 <- avis_get_corpus(1834)

by_year[[1]]

li <- list()
for (i in 1:ncol(de_1834_dist[,1:20])){
  li[[i]] <- head(sort(de_1834_dist[i,],decreasing = F))
}






head(sort(de_1834_dist[2012,],decreasing = F))
head(sort(de_1834_dist[2121,],decreasing = F))

dim(de_1834_dist)
length(texts(de_1834))

de_1834_dist[2121,2012]
de_1834_dist[2121,2045]
de_1834_dist[10,262]
texts(de_1834[10])
texts(de_1834[262])





