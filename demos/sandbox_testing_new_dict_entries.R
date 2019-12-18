# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
#added encoding parameter, otherwise Umlaute get scrambled when loading dictionaries
source("R/avis_stop.R", encoding = "UTF-8")
source("R/ocr_corrections.R", encoding = "UTF-8")
source("R/tagfilters_utils.R", encoding = "UTF-8")
source("R/tagfilters_household.R", encoding = "UTF-8")
source("R/tagfilters_main.R", encoding = "UTF-8")
source("R/cleaners.R")
source("R/validate_filters.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text", encoding = "UTF-8")
avis_1834$text <- correct_ocr(avis_1834$text)

#for consistency of the filter, consider just ads in German
ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                      docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                                (docvars(corpus_1834_all,"id") %in%
                                   ids_by_lang$de))
labor <- tagfilter_labor()


#' Testing the impact of new entries
#'
#' Insert potenial terms as candidate below
#' execute code up to line 76 for comprehensive evaluation
tagfilter_test <- function(){
  dict <- list()
  dict$pos <- list(
    candidate = "[f|F]abrik"
  )
  #take over dict$neg from original filter to better gauge potential of candidate
  #disable by adding # at beginning of next line
  dict$neg <- labor$tagfilters$neg
  create_filter_output(dict)
}

#' merging original and test filter and prepare validation
tagfilter_new <- merge_filters(tagfilter_labor(),
                               tagfilter_test())
new_ids <- tagfilter_new$filtrate(corpus_1834,ignore.case = F)
n <- validate_filter(corpus_1834, new_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")

labor_ids <- labor$filtrate(corpus_1834,ignore.case = F)
o <- validate_filter(corpus_1834, labor_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")

test <- tagfilter_test()
test_ids <- test$filtrate(corpus_1834,ignore.case = F)
t <- validate_filter(corpus_1834, test_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")

# Validation of Filters ----
## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither filter nor hc

o
t
n
cat(paste("Range (%):\t", o$range, "->", n$range, "| change:", round (n$range-o$range,1),
                "\nPrecision (%):\t", o$precision, "->", n$precision, "| change:", round (n$precision-o$precision,1), "\n"
))





#' -----Check into the ads found/missed by the different filters
#'
#'

#' ---test
#'
#' TRUE positives ("yay!") of test
#' ...useful if one checks for entries who seem more fordict$neg
yay <- corpus_subset(corpus_1834,
                     docvars(corpus_1834,"id") %in%
                       t$filter_T_hc_T)
yay$documents$texts[1:10]

#- FALSE positives ("oops") of test
oops <- corpus_subset(corpus_1834,
                      docvars(corpus_1834,"id") %in%
                        t$filter_T_hc_F)
oops$documents$texts[1:10]



#' ---original
#'
#' FALSE positives ("oops") of original
oops <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      o$filter_T_hc_F)
oops$documents$texts[1:10]

#' FALSE negatives ("What are we still missing?") of original
missing_corpus <- corpus_subset(corpus_1834,
                                docvars(corpus_1834,"id") %in%
                                  o$hc_T_filter_F)
missing_corpus_clean <- missing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm(missing_corpus_clean),
                   max_words = 100)

missing_corpus$documents$texts[1-10]

head(kwic(missing_corpus, pattern = "something"))



#' ---merged/new
#'
#' FALSE positives ("oops") of merged/new
oops <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      n$filter_T_hc_F)
oops$documents$texts[1:10]

#' FALSE negatives ("What are we still missing?") of merged/new
missing_corpus <- corpus_subset(corpus_1834,
                                docvars(corpus_1834,"id") %in%
                                  n$hc_T_filter_F)
missing_corpus_clean <- missing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm(missing_corpus_clean),
                   max_words = 100)

missing_corpus$documents$texts[1-10]

head(kwic(missing_corpus, pattern = "something"))
