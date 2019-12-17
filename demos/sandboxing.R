# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
#added encoding parameter, as otherwise Umlaute get scrambles when loading external dictionaries
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
tt$documents$texts[1:10]
tt$documents$texts


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

head(kwic(missing_corpus, pattern = "Platz"))
missing_corpus$documents$texts[1-10]




# Testing the impact of new entries ---

#test dictionary for potential new entries
tagfilter_test <- function(){
  dict <- list()
  dict$pos <- list(
    candidate = "Anzeige|anzeigen|angezeigt"
  )
  dict$neg <- list(
    #should it be necessary to exclude the offering of services
    # (hc-tagging as work), one could use "Anzeige, anzeigen, angezeigt, zeigt an" and also "Zutrauen"
    #
    #"zum kochen": describes cookware, not people
    misc = "Ornement",
    misc_phrase1 = "zum kochen",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "beerdigt|dito|ebendaselbst",
    othercat_realestate = "Losament|Zimmer|Kammer|Stübchen",
    othercat_boarding_phrase1 = "an die Kost",
    #othercategory: excluding lost&found, auction, funeral news,
    # some real estate and boarding  - which is (almost)
    # never combined with job offers/requests
    #"dito" and "ebendaselbst" is used in funeral ads, but never labor ads (just 1 exception)
    other_transactions = "kaufen|Preis|Artikel|versteiger|Versteiger|vergant|//bGant//b",
    #transactions that are not associtaed with the job market
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
## found by filter AND NOT by HC ("oops") | neither hc nor filter
t <- validate_filter(corpus_1834, test_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
t

#Checking into ads found by filter AND hc ("yay!")
#useful if one checks for entries in dict$neg
tt <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      t$filter_T_hc_T)
tt$documents$texts[1:10]



#- FALSE positives ("oops"-cases) of test-dict
oops <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      t$filter_T_hc_F)
oops$documents$texts[1:15]


# Quality of dictionary if
# candidate(s) become actual entries

tagfilter_new <- merge_filters(tagfilter_labor(),
                               tagfilter_test())

new_ids <- tagfilter_new$filtrate(corpus_1834)

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither hc nor filter
n <- validate_filter(corpus_1834, new_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
n

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


# How encompassing, and how precise is the filter compared to the old?
rangeold <- round(100/(1+length(o$hc_T_filter_F)/length(o$filter_T_hc_T)),1)
rangenew <- round(100/(1+length(n$hc_T_filter_F)/length(n$filter_T_hc_T)),1)
precisionold <- round(100/(1+length(o$filter_T_hc_F)/length(o$filter_T_hc_T)),1)
precisionnew <- round(100/(1+length(n$filter_T_hc_F)/length(n$filter_T_hc_T)),1)
print( paste("Range: ", rangeold, "% -> ", rangenew, "% (", rangenew-rangeold, "%)."))
print( paste("Precision: ", precisionold, "% -> ", precisionnew, "% (", precisionnew-precisionold, "%)."))
