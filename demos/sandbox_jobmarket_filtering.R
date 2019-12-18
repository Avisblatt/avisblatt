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

#just ads in German
ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                      docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                                (docvars(corpus_1834_all,"id") %in%
                                   ids_by_lang$de))


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
    work = "Beruf|arbeiten|Beschäfti|beschäfti|Besorgung|kochen|Kochen|nähen|Waschen|glätten|Glätten",
    work_phrase_1 = "zu waschen",
    #removed \\bArbeiter\\b, \\bArbeit\\b and rechtschaffen for now, as it produced too many false positives
    qualification = "\\bZeugnisse|\\erfahrene|versteht|geübt",
    position = "[m|M]agd|[k|K]necht|Köchin|Seidenbandweber|Seidenweber|Seidenwinder|Zettler",
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
    othercat_boarding_phrase1 = "//bdie Kost//b",
    misc_phrase1 = "zum kochen",
    misc_phrase2 = "darin zu kochen",
    misc_phrase3= "Dienst zu erweisen",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "beerdigt|dito|Dito|bendaselbst|unrichtig",
    othercat_info_phrase1 = "meinem Namen",
    othercat_realestate = "Losament|Kammer|Stübchen",
    #othercategory: excluding lost&found, auction, funeral news,
    # some real estate and boarding  - which is (almost)
    # never combined with job offers/requests
    #"dito" and "ebendaselbst" is used in funeral ads, but never labor ads (just 1 exception)
    #"unrichtig" and "in meinem Namen" found in clarification ads
    other_transactions = "//bTausch//b|ubscri|übergeben|abzugeben|überlassen|vermieten|verlehen|usleihe|kaufen|Preis|Artikel|versteiger|Versteiger|vergant|//bGant//b",
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
mergedresults <- validate_filter(corpus_1834, union(labor_apr_ids, labor_without_apr_ids),
                        search_col = "adcontent",
                        pattern = "arbeit")
mergedfilters <- validate_filter(corpus_1834, new_ids,
                                 search_col = "adcontent",
                                 pattern = "arbeit")
with_apr
without_apr
mergedfilters
mergedresults


#-------bizpromo ads
bizpromo <- tagfilter_bizpromo()
bizpromo_ids <- bizpromo$filtrate(corpus_1834,ignore.case = F)

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
oops$documents$texts[1:20]

missing_corpus <- corpus_subset(corpus_1834,
                                docvars(corpus_1834,"id") %in%
                                  hc_T_filter_F)
missing_corpus_clean <- missing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()
textplot_wordcloud(dfm(missing_corpus_clean),
                   max_words = 100)
missing_corpus$documents$texts[11-20]


#investigating bizpromo ads

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
