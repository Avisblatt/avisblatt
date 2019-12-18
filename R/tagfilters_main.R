#' Filter Quanteda Corpus: Labor
#' @export
tagfilter_labor <- function(){
  # Note how phrases are used inside a dictionary.
  # Dict elements may be converted to quanteda dictionaries
  # and because those are not bare regexp, we need
  # to hold expressions containing whitespace
  # separately, see also ?kwic Note on patterns.
  dict <- list()
  dict$pos <- list(
    work = "\\bArbeit\\b|Beruf|arbeiten|Beschäfti|beschäfti|Besorgung|kochen|Kochen|nähen|Waschen",
    work_phrase_1 = "zu waschen",
    #removed \\bArbeiter\\b for now, as it added only 10 ads while producing 10 false positives
    #'Arbeit not ideal either btw. Removing would cost 35 hits, but spare 14 oops
    qualification = "\\bZeugnisse|\\erfahrene|versteht|rechtschaffen",
    position = "magd|Magd|knecht|Knecht|Köchin",
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz",
    employment ="Anstellung|angestellt|\\bDienst\\b|\\bDienste\\b|einzutreten|eintreten\\b|unterzukommen|\\bLohn\\b|Verdienst"
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


#' Filter Quanteda Corpus: Advertising/promoting business
#' @export
#' as far as services is concerned,
#' this was tagged "labor" (otherwise "things")
#' but might be wise to draw a line between this & job market
tagfilter_bizpromo <- function(){
  dict <- list()
  dict$pos <- list(
    advertising = "Anzeige|anzeigen|angezeigt|benachrichtigen",
    advertising_phrase1 = "zeigt an",
    recommending = "rekommandi",
    recommending_phrase1 = "empfiehlt sich",
    customertrust = "Zutrauen|Zuspruch",
    serving = "bedienen"
  )
    dict$neg <- list(
    # excluding some jobmarket ads and public proclamations
    employment ="Anstellung|angestellt|\\bDienst\\b|einzutreten|eintreten\\b|unterzukommen|\\bLohn\\b|Verdienst",
    proclamation = "Kundmachung|Polizey|Bekanntmachung",
    report_to_registry_office = "Anzeige im Berichthaus",
    report_to_registry_office_phrase1 = "gefälligst Anzeige"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Real Estate
#' @export
tagfilter_real_estate <- function(){
  dict <- list()
  dict$pos <- list(
    apartment = "losament|^Zimmer|Wohnung|Kammer|Keller"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Lottery
#' @export
tagfilter_lotto <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Other
#' @export
tagfilter_other <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Transport
#' @export
tagfilter_transport <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Charity
#' @export
tagfilter_charity <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Placement
#' @export
tagfilter_placement <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Board
#' @export
tagfilter_board <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Animal
#' @export
tagfilter_animal <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}




#' Filter Quanteda Corpus: Information
#' @export
tagfilter_info <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Jewellery
#' @export
tagfilter_bling <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Church Seat
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Textile
#' @export
tagfilter_textile <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Grocery
#' @export
tagfilter_grocery <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Finance
#' @export
tagfilter_finance <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Print
#' @export
tagfilter_print <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Household
#' @export
tagfilter_household_goods <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Miscellaneous Things
#' @export
tagfilter_things <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}









