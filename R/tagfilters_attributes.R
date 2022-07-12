#' Filter Quanteda Corpus: Transaction type - offers in offer sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124, search terms by Christof Jeggle
#' @note Last changed 2021-11-28
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_attributes_female <- function(){
  dict <- list()
  dict$pos <- list(
    girl = "Madmo|M.dchen|(F|f)i(l|ll)e",
    maid = "(M|m)agd|(S|s)ervante|(D|d)omesti",
    maiden = "Jungfrau|Jungfr|Jungfer|Jgfr",
    widow = "Wi(t|tt|tl)(we|ib|ih|ub)|Witti|Wi(t|tt)frau|Wb.|W(t|ttb)|(V|v)euve",
    woman = "(F|f)rau|Fraub|Frmu|Fra(l|n|nu|ue)|Madam|Mme|Ehefrau|(W|w)eibsperson"
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


