library(avisblatt)
library(readtext)
library(quanteda)
source("R/tagfilters_titles.R", encoding = "UTF-8")
source("R/tagfilters_utils.R", encoding = "UTF-8")
source("R/ocr_corrections.R", encoding = "UTF-8")

csvfiles <- file.path("..", "avis-data", "raw_data", "orig_*.csv")
txt <- readtext(csvfiles, docid_field = "id", text_field = "text", encoding = "UTF-8")
txt$text <- correct_ocr(txt$text)

corpus_header <- corpus(txt)
corpus_header <- subset(corpus_header, (corpus_header$isheader == 1))


saleoffer <- tagfilter_saleoffer()
saledemand <- tagfilter_saledemand()
lendoffer <- tagfilter_lendoffer()
lenddemand <- tagfilter_lenddemand()
lostandfound <- tagfilter_lostandfound()
death <- tagfilter_death()
marriage <- tagfilter_marriage()
labourinfo <- tagfilter_labourinfo()
auctions <- tagfilter_auctions()
othernews <- tagfilter_othernews()
official <- tagfilter_official()
taxes <- tagfilter_taxes()
bookstore <- tagfilter_bookstore()
travel <- tagfilter_travel()
exchange <- tagfilter_exchange()
alltitle <- merge_filters(saledemand,
                          saleoffer,
                          lendoffer,
                          lenddemand,
                          lostandfound,
                          death,
                          marriage,
                          labourinfo,
                          auctions,
                          othernews,
                          official,
                          taxes,
                          bookstore,
                          travel,
                          exchange)


corpus_saleoffer <- saleoffer$filtrate(corpus_header)
corpus_saledemands <- saledemand$filtrate(corpus_header)
corpus_lendoffer <- lendoffer$filtrate(corpus_header)
corpus_lenddemand <- lenddemand$filtrate(corpus_header)
corpus_lostandfound <- lostandfound$filtrate(corpus_header)
corpus_death <- death$filtrate(corpus_header)
corpus_marriage <- marriage$filtrate(corpus_header)
corpus_labourinfo <- labourinfo$filtrate(corpus_header)
corpus_auctions <- auctions$filtrate(corpus_header)
corpus_othernews <- othernews$filtrate(corpus_header)
corpus_official <- official$filtrate(corpus_header)
corpus_taxes <- taxes$filtrate(corpus_header)
corpus_bookstore <- bookstore$filtrate(corpus_header)
corpus_travel <- travel$filtrate(corpus_header)
corpus_exchange <- exchange$filtrate(corpus_header)

corpus_alltitle <- alltitle$filtrate(corpus_header)
corpus_unrecognized <- subset(corpus_header, !(names(corpus_header) %in% names(corpus_alltitle)))


#' check recognized text versions
#' for false positives
unique(texts(corpus_saleoffer))
unique(texts(corpus_saledemands))
unique(texts(corpus_lendoffer))
unique(texts(corpus_lenddemand))
unique(texts(corpus_lostandfound))
unique(texts(corpus_death))
unique(texts(corpus_marriage))
unique(texts(corpus_labourinfo))
unique(texts(corpus_auctions))
unique(texts(corpus_othernews))
unique(texts(corpus_official))
unique(texts(corpus_taxes))
unique(texts(corpus_bookstore))
unique(texts(corpus_travel))
unique(texts(corpus_exchange))

#' check unrecognized header texts
unique(texts(corpus_unrecognized))


