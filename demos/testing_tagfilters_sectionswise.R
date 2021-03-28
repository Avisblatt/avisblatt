# Testing change to tagfilter_util:
# If dict$applicable is given,
# apply tagfilter only to ads
# with specoofed header tag(s).
# If inapplicable is given,
# exclude ads from these sections.
#
# delete this file before merging into experimental!

devtools::load_all()

show_ads <- function(corp){
  p_date <- paste0("[", corp$date, "] ")
  p_text <- as.vector(texts(corp))
  p_id <- paste0(" (", names(corp), ") ")
  cat(paste0(p_date, p_text, p_id, collapse = "\n\n\n"))
}


corpus_1799 <- avis_create_corpus("../avis-data/raw_data/orig_1799.csv")


tagfilter_test_all <- function(){
  dict <- list()
  dict$pos <- list(
    all = "Dienst"
  )
  dict$neg <- list(
    all = "dwfsdfsdf"

  )
  create_filter_output(dict)
}

tagfilter_test_applicable <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo", "death") # I am adding death just to test if it works with more than 1 header tag
  dict$pos <- list(
    all = "Dienst"
  )
  dict$neg <- list(
    all = "dwfsdfsdf"

  )
  create_filter_output(dict)
}

tagfilter_test_inapplicable <- function(){
  dict <- list()
  dict$inapplicable <- list("labourinfo", "death") # same, but this time exclude, not incldude ads in those sections
  dict$pos <- list(
    all = "Dienst"
  )
  dict$neg <- list(
    all = "dwfsdfsdf"

  )
  create_filter_output(dict)
}



#applying to all ads
labinfo1 <- tagfilter_test_all()
test1 <- labinfo1$filtrate(corpus_1799,ignore.case = F)
test1$header_tag

#applying to ads in labourinfo and death sections
labinfo2 <- tagfilter_test_applicable()
test2 <- labinfo2$filtrate(corpus_1799,ignore.case = F)
test2$header_tag

#applying to ads in all sections OTHER than labourinfo and death
labinfo3 <- tagfilter_test_inapplicable()
test3 <- labinfo3$filtrate(corpus_1799,ignore.case = F)
test3$header_tag



# Looking at the ad texts,
# e.g. to confirm "Dienstag"
# should be in dict$neg here ;-)

show_ads(test1[1:20])

show_ads(test2[1:20])

show_ads(test3[1:20])
