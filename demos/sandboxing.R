# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
#added encoding parameter, as otherwise Umlaute get scrambled when loading external dictionaries
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

head(kwic(missing_corpus, pattern = "reco"))
missing_corpus$documents$texts[1-10]




# Testing the impact of new entries ---

#test dictionary for potential new entries
tagfilter_test <- function(){
  dict <- list()
  dict$pos <- list(
    candidate = "x"
  )
  dict$neg <- list(
    #"darin zu / zum kochen": describes cookware, not people
    misc = "Ornement",
    misc_phrase1 = "zum kochen",
    misc_phrase2 = "darin zu kochen",
    misc_phrase3= "Dienst zu erweisen",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "beerdigt|dito|Dito|bendaselbst",
    othercat_realestate = "Losament|Zimmer|Kammer|Stübchen",
    othercat_boarding = "Kosthaus",
    othercat_boarding_phrase1 = "//bdie Kost//b",
    #othercategory: excluding lost&found, auction, funeral news,
    # some real estate and boarding  - which is (almost)
    # never combined with job offers/requests
    #"dito" and "ebendaselbst" is used in funeral ads, but never labor ads (just 1 exception)
    other_transactions = "ubscri|übergeben|vermieten|verlehen|kaufen|Preis|Artikel|versteiger|Versteiger|vergant|//bGant//b",
    #transactions that are not associtaed with the job market (ubscri -> Subscription, subscribieren)
    proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    proclamation_phrase_1 = "Publikation in Betreff",
    proclamation_phrase_2 = "Basel, den"
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
oops$documents$texts[1:10]


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



#---
#show difference between result of merged filters and merged results of (unmerged) filters
# approach here: differentiate labor and apprenticeships,
# use different dict$negs in both, join results;
# gives different results than using the merged filters
# (as filter merging merges both dict$neg into a single dict$neg)
tagfilter_labor_apprentice <- function(){
  dict <- list()
  dict$pos <- list(
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld"
  )
  #no dict$neg necessary, no (real) false positives
  create_filter_output(dict)
}

tagfilter_labor_without_apprentice <- function(){
  dict <- list()
  dict$pos <- list(
    work = "\\bArbeit\\b|Beruf|arbeiten|Beschäfti|beschäfti|Besorgung|kochen|Kochen|nähen|Waschen",
    work_phrase_1 = "zu waschen",
    qualification = "\\bZeugnisse|\\erfahrene|versteht|rechtschaffen",
    position = "magd|Magd|knecht|Knecht|Köchin",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz",
    employment ="Anstellung|angestellt|\\bDienst\\b|\\bDienste\\b|einzutreten|eintreten\\b|unterzukommen|\\bLohn\\b|Verdienst"
  )
    dict$neg <- list(
    #apprenticeship handeld by other filter
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld",
    #excluding boarding ads here - boarding usually (?) only with apprenticeship ads
    #boarding = "//bKost//b|//bKostgeld//b",
    othercat_boarding = "Kosthaus",
    othercat_boarding_phrase1 = "an die Kost",
    misc_phrase1 = "zum kochen",
    misc_phrase2 = "darin zu kochen",
    misc_phrase3= "Dienst zu erweisen",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "beerdigt|dito|Dito|bendaselbst",
    othercat_realestate = "Losament|Zimmer|Kammer|Stübchen",
    #othercategory: excluding lost&found, auction, funeral news,
    # some real estate and boarding  - which is (almost)
    # never combined with job offers/requests
    #"dito" and "ebendaselbst" is used in funeral ads, but never labor ads (just 1 exception)
    other_transactions = "kaufen|Preis|Artikel|versteiger|Versteiger|vergant|//bGant//b",
    #transactions that are not associtaed with the job market
    proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    proclamation_phrase_1 = "Publikation in Betreff",
    proclamation_phrase_2 = "Basel, den"
    #proclamation: some of the ads recognized by the filter are public announcements"
  )
  create_filter_output(dict)
}

labor_apr <- tagfilter_labor_apprentice()
labor_apr_ids <- labor_apr$filtrate(corpus_1834,ignore.case = F)

labor_without_apr <- tagfilter_labor_without_apprentice()
labor_without_apr_ids <- labor_without_apr$filtrate(corpus_1834,ignore.case = F)

tagfilter_new <- merge_filters(tagfilter_labor_without_apprentice(),
                               tagfilter_labor_apprentice())
new_ids <- tagfilter_new$filtrate(corpus_1834)

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither hc nor filter
with_apr <- validate_filter(corpus_1834, labor_apr_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
without_apr <- validate_filter(corpus_1834, labor_without_apr_ids,
                       search_col = "adcontent",
                       pattern = "arbeit")
mergedresults <- validate_filter(corpus_1834, c(labor_apr_ids, labor_without_apr_ids),
                        search_col = "adcontent",
                        pattern = "arbeit")
mergedfilters <- validate_filter(corpus_1834, new_ids,
                                 search_col = "adcontent",
                                 pattern = "arbeit")
with_apr
without_apr
mergedfilters
mergedresults


#-------investigating bizpromo ads
bizpromo <- tagfilter_bizpromo()
bizpromo_ids <- bizpromo$filtrate(corpus_1834,ignore.case = F)
length(bizpromo_ids)

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither hc nor filter
b <- validate_filter(corpus_1834, bizpromo_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
b

#TRUE positives
b_t <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      b$filter_T_hc_T)
b_t$documents$texts[21:30]

#- FALSE positives
b_f <- corpus_subset(corpus_1834,
                      docvars(corpus_1834,"id") %in%
                        b$filter_T_hc_F)
b_f$documents$texts[1:15]



#' How good is the tagfilter_labor
#' when you exclude all bizpromo ads
#' from hc AND filter results?

#excluding bizpromos from labor filter results
labor <- tagfilter_labor()
labor_ids <- labor$filtrate(corpus_1834,ignore.case = F)
labor_ids_without_bizpromo <- setdiff(labor_ids, bizpromo_ids)

#excluding bizpromos from human categorization results
doc_ids <- corpus_1834$documents[,"id"]
human_class_ids <- doc_ids[grepl("arbeit",
                                 corpus_1834$documents[,"adcontent"])]
hc_ids_without_bizpromo <- setdiff(human_class_ids, bizpromo_ids)

filter_T_hc_T <- doc_ids[(doc_ids %in% labor_ids_without_bizpromo) &
                           (doc_ids %in% hc_ids_without_bizpromo)]
filter_T_hc_F <- doc_ids[(doc_ids %in% labor_ids_without_bizpromo) &
                           !(doc_ids %in% hc_ids_without_bizpromo)]
hc_T_filter_F <- doc_ids[!(doc_ids %in% labor_ids_without_bizpromo) &
                           (doc_ids %in% hc_ids_without_bizpromo)]
hc_F_filter_F <- doc_ids[!(doc_ids %in% labor_ids_without_bizpromo) &
                           !(doc_ids %in% hc_ids_without_bizpromo)]

# Confusion Matrix ----
overview <- tibble(
  filter_T = c(length(filter_T_hc_T),
               length(filter_T_hc_F)),
  filter_F = c(length(hc_T_filter_F),
               length(hc_F_filter_F))
)

# Relative Range and Precision
filter_range <- round(100 / (1+ length(hc_T_filter_F)/length(filter_T_hc_T)), 1)

filter_precision <- round(100 / (1 + length(filter_T_hc_F) /
                                   length(filter_T_hc_T)), 1)
filter_range
filter_precision
overview

oops <- corpus_subset(corpus_1834,
                      docvars(corpus_1834,"id") %in%
                        filter_T_hc_F)
oops$documents$texts[41:60]
