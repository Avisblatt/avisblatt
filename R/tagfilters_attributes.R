#' Tagfilter Attributes
#'
#' #' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters.
#'
#' @seealso tagfilters
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


