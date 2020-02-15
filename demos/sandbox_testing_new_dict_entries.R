# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
source("R/avis_stop.R", encoding = "UTF-8")
source("R/ocr_corrections.R", encoding = "UTF-8")
source("R/tagfilters_utils.R", encoding = "UTF-8")
source("R/tagfilters_household.R", encoding = "UTF-8")
source("R/tagfilters_main.R", encoding = "UTF-8")
source("R/cleaners.R")
source("R/validate_filters.R")


# preparing corpus of German ads
# chose year by modifying csv-file name in line below. Currently possible:
# groundtruth1734.csv
# groundtruth1834.csv
# groundtruth*.csv for all
groundtruth <- readtext("data/groundtruth*.csv",
                      text_field = "text", encoding = "UTF-8")
groundtruth$text <- correct_ocr(groundtruth$text)
ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_groundtruth_all <- corpus(groundtruth,
                          docid_field = "doc_id")
corpus_groundtruth <- corpus_subset(corpus_groundtruth_all,
                             (docvars(corpus_groundtruth_all,"id") %in%
                                ids_by_lang$de))
# PICK category and filter by removing # from the pertinent line below
# reload current version of filters here, so lines ~28-100
# can be executed as a block after changing original and test dictionary
source("R/tagfilters_main.R", encoding = "UTF-8")
#category <- "01textilien"; original <- tagfilter_textiles()
#category <- "02hausrat"; original <- tagfilter_household_goods()
#category <- "03lebensmittel"; original <- tagfilter_grocery()
#category <- "04schmuck"; original <- tagfilter_bling()
#category <- "05drucksachen"; original <- tagfilter_print()
#category <- "06ding"; original <- tagfilter_things()
#category <- "07tier"; original <- tagfilter_animal()
#category <- "08immo"; original <- tagfilter_real_estate()
#category <- "09kirchenstuhl"; original <- tagfilter_churchseat()
category <- "10arbeitsstelle"; original <- tagfilter_labor()
#category <- "11kost"; original <- tagfilter_board()
#category <- "12platzierung"; original <- tagfilter_placement()
#category <- "13caritas"; original <- tagfilter_charity()
#category <- "14finanz"; original <- tagfilter_finance()
#category <- "15lotterie"; original <- tagfilter_lotto()
#category <- "16transport"; original <- tagfilter_transport()
#category <- "17auskunft"; original <- tagfilter_info()
#category <- "18uneindeutig"; original <- tagfilter_other()


#' Examining current filter
#'
original_ids <- original$filtrate(corpus_groundtruth,ignore.case = F)
o <- validate_filter(corpus_groundtruth, original_ids,
                     search_col = "adcontent",
                     pattern <- category)
o


#' Testing the impact of new entries
#'
#' Insert potenial terms as candidate below
#' execute code from line 28 to line ~100 for comprehensive evaluation
tagfilter_test <- function(){
  dict <- list()
  dict$pos <- list(
    candidate = "Omaha"
  )
  #take over dict$neg from original filter to better gauge potential of candidate
  #disable by adding # at beginning of next line
  dict$neg <- original$tagfilters$neg
  create_filter_output(dict)
}

#' merging original and test filter and prepare validation
tagfilter_new <- merge_filters(original,
                               tagfilter_test())
new_ids <- tagfilter_new$filtrate(corpus_groundtruth,ignore.case = F)
n <- validate_filter(corpus_groundtruth, new_ids,
                     search_col = "adcontent",
                     pattern <- category)

test <- tagfilter_test()
test_ids <- test$filtrate(corpus_groundtruth,ignore.case = F)
t <- validate_filter(corpus_groundtruth, test_ids,
                     search_col = "adcontent",
                     pattern <- category)

# Validation of Filters ----
## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither filter nor hc

t
cat(paste("Range (%):\t", o$range, "->", n$range, "| change:", round (n$range-o$range,1),
                "\nPrecision (%):\t", o$precision, "->", n$precision, "| change:", round (n$precision-o$precision,1), "\n"))





#' -----Check into the ads found/missed by the different filters
#'
#'

#' ---test
#'
#' TRUE positives ("yay!") of test
#' ...useful if one checks for entries who seem more fordict$neg
yay <- corpus_subset(corpus_groundtruth,
                     docvars(corpus_groundtruth,"id") %in%
                       t$filter_T_hc_T)
yay$documents$texts[1:10]

#- FALSE positives ("oops") of test
oops <- corpus_subset(corpus_1834,
                      docvars(corpus_groundtruth,"id") %in%
                        t$filter_T_hc_F)
oops$documents$texts[1:10]



#' ---original
#'
#' FALSE positives ("oops") of original
oops <- corpus_subset(corpus_groundtruth,
                    docvars(corpus_groundtruth,"id") %in%
                      o$filter_T_hc_F)
oops$documents$texts[1:10]

#' FALSE negatives ("What are we still missing?") of original
missing_corpus <- corpus_subset(corpus_groundtruth,
                                docvars(corpus_groundtruth,"id") %in%
                                  o$hc_T_filter_F)
missing_corpus_clean <- missing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

missing_corpus$documents$texts[1-10]

textplot_wordcloud(dfm(missing_corpus_clean),
                   max_words = 100)

head(kwic(missing_corpus, pattern = "something"))



#' ---merged/new
#'
#' FALSE positives ("oops") of merged/new
oops <- corpus_subset(corpus_groundtruth,
                    docvars(corpus_groundtruth,"id") %in%
                      n$filter_T_hc_F)
oops$documents$texts[1:10]

#' FALSE negatives ("What are we still missing?") of merged/new
missing_corpus <- corpus_subset(corpus_groundtruth,
                                docvars(corpus_groundtruth,"id") %in%
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
