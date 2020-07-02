library(avisblatt)
library(readtext)
library(quanteda)
source("R/tagfilters_headers.R", encoding = "UTF-8")
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
labourinfo <- tagfilter_labourandinfo()
auctions <- tagfilter_auctions()
othernews <- tagfilter_othernews()
official <- tagfilter_official()
taxes <- tagfilter_taxes()
bookstore <- tagfilter_bookstore()
travel <- tagfilter_travel()
exchange <- tagfilter_exchange()
charity <- tagfilter_charityheader()
foreigners <- tagfilter_foreigners()
merkwuerdig <- tagfilter_merkwuerdig()
registry <- tagfilter_registry()
prices <- tagfilter_prices()
election <- tagfilter_election()
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
                          exchange,
                          charity,
                          foreigners,
                          merkwuerdig,
                          registry,
                          prices,
                          election)


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
corpus_charity <- charity$filtrate(corpus_header)
corpus_foreigners <- foreigners$filtrate(corpus_header)
corpus_merkwuerdig <- merkwuerdig$filtrate(corpus_header)
corpus_registry <- registry$filtrate(corpus_header)
corpus_prices <- prices$filtrate(corpus_header)
corpus_election <- election$filtrate(corpus_header)


corpus_alltitle <- alltitle$filtrate(corpus_header)
corpus_unrecognized <- subset(corpus_header, !(names(corpus_header) %in% names(corpus_alltitle)))


#' check recognized text versions
#' for false positives
corpus_saleoffer <- data.frame(unique(texts(corpus_saleoffer)))
corpus_saledemands <- data.frame(unique(texts(corpus_saledemands)))
corpus_lendoffer <- unique(texts(corpus_lendoffer))
corpus_lenddemand <- unique(texts(corpus_lenddemand))
corpus_lostandfound <- unique(texts(corpus_lostandfound))
corpus_death <- unique(texts(corpus_death))
corpus_marriage <- unique(texts(corpus_marriage))
corpus_labourinfo <- unique(texts(corpus_labourinfo))
corpus_auctions <- unique(texts(corpus_auctions))
corpus_othernews <- unique(texts(corpus_othernews))
corpus_official <- unique(texts(corpus_official))
corpus_taxes <- unique(texts(corpus_taxes))
corpus_bookstore <- unique(texts(corpus_bookstore))
corpus_travel <- unique(texts(corpus_travel))
corpus_exchange <- unique(texts(corpus_exchange))
corpus_charity <- unique(texts(corpus_charity))
corpus_foreigners <- unique(texts(corpus_foreigners))
corpus_merkwuerdig <- unique(texts(corpus_merkwuerdig))
corpus_registry <- unique(texts(corpus_registry))
corpus_prices <- unique(texts(corpus_prices))
corpus_election <- unique(texts(corpus_election))


#' check unrecognized header texts
corpus_unrecognized <- unique(texts(corpus_unrecognized))
