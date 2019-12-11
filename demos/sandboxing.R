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
labor_ids <- labor$filtrate(corpus_1834)


#-------------------------------------------------------
# Validate the quality of filters
# by checking filters vs. manual classification
id_hc <- corpus_1834$documents$id[grepl("arbeit",
                                        corpus_1834$documents$adcontent)]

# labor filter & human classification
lab_in_hc <- labor_ids[labor_ids %in% id_hc]
# labor filter not in human classification
lab_notin_hc <- labor_ids[!labor_ids %in% id_hc]

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | in total ("our universe")
o <- validate_filter(corpus_1834, labor_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
o

#How encompassing, and how precise is the filter?
print( paste("Range: The filter recognized",
             round(100*length(o$filter_T_hc_T)/length(o$hc_T_filter_F),1),
             "% of the pertinent ads recognized by human classifcation.",
             "Precision: ",
             round(100/(1+length(o$filter_T_hc_F)/length(o$filter_T_hc_T)),1),
             "% of the ads recognized by the filter were also recognized by human classifcation."))


#-------------------------------------------------------
#Looking at the false positives ("oops"-cases)
tt <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      lab_notin_hc)

# Some are interesting as these might
# be some human misclassification,
# the other seem problems of the filter.
tt$documents$texts[1:15]


#-------------------------------------------------------
#Looking at the false negatives ("What are we still missing?")

#word cloud for HC labor ads not found by labor filter
hc_notin_lab <- id_hc[!id_hc %in% labor_ids]

missing_corpus <- corpus_subset(corpus_1834,
                           docvars(corpus_1834,"id") %in%
                             hc_notin_lab)
missing_corpus_clean <- missing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  dfm()

textplot_wordcloud(dfm(missing_corpus_clean),
                   max_words = 100)




#-------------------------------------------------------
#-------------------------------------------------------
#Testing the impact of new entries

#test dictionary for potential new entries
tagfilter_test <- function(){
  dict <- list()
  dict$pos <- list(
    candidate = " Zeugnisse"
  )
  dict$neg <- list(
    misc = "Taufscheine"
  )
  create_filter_output(dict)
}


test <- tagfilter_test()
test_ids <- test$filtrate(corpus_1834)

# now just repeating the pertinent code from above

# test filter & human classification
test_in_hc <- test_ids[test_ids %in% id_hc]
# test filter not in human classification
test_notin_hc <- test_ids[!test_ids %in% id_hc]

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | in total ("our universe")
t <- validate_filter(corpus_1834, test_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
t

#How encompassing, and how precise is the filter?
print( paste("Range: The filter recognized",
             round(100*length(t$filter_T_hc_T)/length(t$hc_T_filter_F),1),
             "% of the pertinent ads recognized by human classifcation.",
             "Precision: ",
             round(100/(1+length(t$filter_T_hc_F)/length(t$filter_T_hc_T)),1),
             "% of the ads recognized by the filter were also recognized by human classifcation."))

#-------------------------------------------------------
#Looking at the false positives ("oops"-cases)
tt <- corpus_subset(corpus_1834,
                    docvars(corpus_1834,"id") %in%
                      test_notin_hc)
tt$documents$texts


#-------------------------------------------------------
#Quality of dictionary if candidate(s) become actual entries
tagfilter_new <- merge_filters(tagfilter_labor(), tagfilter_test())

new <- tagfilter_test()
new_ids <- new$filtrate(corpus_1834)

#and now repeating code again...

new_in_hc <- new_ids[new_ids %in% id_hc]
# test filter not in human classification
new_notin_hc <- new_ids[!new_ids %in% id_hc]

## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | in total ("our universe")
n <- validate_filter(corpus_1834, new_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")
n

#How encompassing, and how precise is the filter compared to the old?
rangeold <- round(100*length(o$filter_T_hc_T)/length(o$hc_T_filter_F),1)
rangenew <- round(100*length(n$filter_T_hc_T)/length(n$hc_T_filter_F),1)
precisionold <- round(100/(1+length(o$filter_T_hc_F)/length(o$filter_T_hc_T)),1)
precisionnew <- round(100/(1+length(n$filter_T_hc_F)/length(n$filter_T_hc_T)),1)
print( paste("Range: ", rangeold, "% -> ", rangenew, "% (", rangenew-rangeold, "%)."))
print( paste("Precision: ", precisionold, "% -> ", precisionnew, "% (", precisionnew-precisionold, "%)."))
