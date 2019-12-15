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

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")


labor <- tagfilter_labor()


labor_ids <- labor$filtrate(corpus_1834,ignore.case = F)
# Validation of Filters ----
## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither filter nor hc
o <- validate_filter(corpus_1834, labor_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
o



# FALSE positives ("oops"-cases) ----
tt <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      o$filter_T_hc_F)

# Some are interesting as these might
# be some human misclassification,
# the other seem problems of the filter.
# !! HC seems to have trouble with looong ads
# this clearly labor related (see end of the text)
tt$documents$texts[99]





# FALSE negatives ----
# Looking at the false negatives
# ("What are we still missing?")
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

head(kwic(missing_corpus, pattern = "arbeiten"))



# Testing the impact of new entries ---

#test dictionary for potential new entries
tagfilter_test <- function(){
  dict <- list()
  dict$pos <- list(
    candidate = "treue"
  )
  dict$neg <- list(
    #"zum kochen": describes cookware, not people
    misc = "Ornement",
    misc_phrase1 = "zum kochen",
    othercategory = "verloren|gefunden|versteiger|Versteiger|beerdigt|ebendaselbst",
    #othercategory: excluding lost&found, auction, funeral news  - which is never combined with job offers/requests
    #"dito" and "ebendaselbst" is used in funeral ads, but never labor ads (just 1 exception)
    proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    proclamation_phrase_1 = "Publikation in Betreff"
    #proclamation: some of the ads recognized by the filter are public announcements"
  )
  create_filter_output(dict)
}


test <- tagfilter_test()
test_ids <- test$filtrate(corpus_1834,ignore.case = F)


## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | in total ("our universe")
t <- validate_filter(corpus_1834, test_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
t


#- FALSE positives ("oops"-cases) of test-dict
tt <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      t$filter_T_hc_F)

tt$documents$texts[1:10]


# Quality of dictionary if
# candidate(s) become actual entries

tagfilter_new <- merge_filters(tagfilter_labor(),
                               tagfilter_test())

new_ids <- tagfilter_new$filtrate(corpus_1834)

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | in total ("our universe")
n <- validate_filter(corpus_1834, new_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
n

missing_corpus <- corpus_subset(corpus_1834,
                                docvars(corpus_1834,"id") %in%
                                  n$hc_T_filter_F)

# ??? how come this was classified
missing_corpus$documents$texts[120]

# but those seem to fit
missing_corpus$documents$texts[121]
missing_corpus$documents$texts[122]
missing_corpus$documents$texts[123]
missing_corpus$documents$texts[124]

missing_corpus_clean <- missing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm(missing_corpus_clean),
                   max_words = 100)


# How encompassing, and how precise is the filter compared to the old?
rangeold <- round(100/(1+length(o$hc_T_filter_F)/length(o$filter_T_hc_T)),1)
rangenew <- round(100/(1+length(n$hc_T_filter_F)/length(n$filter_T_hc_T)),1)
precisionold <- round(100/(1+length(o$filter_T_hc_F)/length(o$filter_T_hc_T)),1)
precisionnew <- round(100/(1+length(n$filter_T_hc_F)/length(n$filter_T_hc_T)),1)
print( paste("Range: ", rangeold, "% -> ", rangenew, "% (", rangenew-rangeold, "%)."))
print( paste("Precision: ", precisionold, "% -> ", precisionnew, "% (", precisionnew-precisionold, "%)."))
