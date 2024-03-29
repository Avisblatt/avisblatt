#' Tagfilter Attributes
#'
#' #' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads related to specific attributes or 
#' qualities, like female persons or second-hand goods.
#' 
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_attributes
#' @seealso tagfilters
NULL


#' @name tagfilter_attributes
#' @export
tagfilter_secondhand <- function(){
  # secondhand items can often be identified by specific phrases and wordings
  # but absence of these phrases does not mean absence of secondhand items
  dict <- list()
  dict$pos <- list(
    used_phrase_1 = "halb neu",
    used_phrase_2 = "so viel als neu",
    used_phrase_3 = "noch neu",
    used_phrase_4 = "fast gantz neu",
    used_phrase_5 = "fast neu",
    used_phrase_6 = "nicht gar neu",
    used_phrase_7 = "nicht neu",
    used_phrase_8 = "noch schier neu",
    used_phrase_9 = "schier gantz neu",
    used_phrase_10 = "schier neu",
    used_phrase_11 = "meistens neu",
    used_phrase_12 = "sauber conditioniert",
    used_phrase_13 = "fast durchgehend neu",
    used_phrase_14 = "fast durchgehends neu",
    used_phrase_15 = "beinahe gantz neu",
    used_phrase_16 = "fast ganz neu",
    used_phrase_17 = "schier ganz neu",
    used_phrase_18 = "beinahe ganz neu",
    used_phrase_19 = "so viel wie neu",
    used_phrase_20 = "so viel als",
    used_phrase_21 = "halbalt",
    used_phrase_22 = "in bestem Stand",
    used_phrase_23 = "gut (c|k)ondition(ie|i)rt",
    used_phrase_24 = "(c|k)ondition(ie|i)rt",
    used_phrase_25 = "gut unterhalten",
    used_phrase_26 = "in gutem Stand",
    used_phrase_27 = "schon gebraucht",
    used_phrase_28 = "brauchbar",
    used_phrase_29 = "halbneu",
    used_phrase_30 = "halb alt",
    used_phrase_31 = "noch brauchbar",
    used_phrase_32 = "fast gan(tz|z) neu"
  )
  
  dict$neg <- list(
    # up until now no negatives necessary
    test = "testestest")
  
  create_filter_output(dict)
}


#' @name tagfilter_attributes
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


