# library(avisblatt)
# During Development rather run
devtools::load_all()

corpus_test <- avis_create_corpus("data/1804.csv")

corpus_headers <- corpus_subset(corpus_test, isheader %in% "1")

#using Anna's code here:
corpus_headertoolong <- corpus_subset(corpus_headers, ntoken(corpus_headers) > 6)
corpus_headertoolong
texts(corpus_headertoolong)

#maybe use > 8 because of "Kost, Information und Bedienungen werden angetragen und begehrt."
#hm, say 10 because , and . seem to count as tokens. Or strip them before testing

corpus_headertoolong <- corpus_subset(corpus_headers, ntoken(corpus_headers) > 10)
corpus_headertoolong

#next step (later): comparing this header-groundtruth to header-filter of Anna&Lars
