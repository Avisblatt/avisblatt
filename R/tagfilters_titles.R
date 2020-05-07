#' Dictionary things offered for sale (Title)
#' @export
tagfilter_saleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    saleoffer_1 = "zum Verkauf angetragen",
    saleoffer_2 = "Alte Verkauf-Artikel",
    saleoffer_3 = "Neue Verkauf-Artikel",
    saleoffer_add_2 = "Nachtrag zum Verkauf"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary things demanded for sale (Title)
#' @export
tagfilter_saledemand <- function(){
  dict <- list()
  dict$pos <- list(
    saledemand_1 = "(zu|zum) kaufen begehrt",
    saledemand_add_1 = "Nachtrag (zu|zum) kaufen begehrt" # delete this if saledemand_1 works
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary things offered to lend (Title)
#' @export
tagfilter_lendoffer <- function(){
  dict <- list()
  dict$pos <- list(
    lendoffer_1 = "zum Ausleihen offerirt", # maybe shorten to "zum Ausleihen"
    lendoffer_add_1 = "Nachtrag zum Ausleihen"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary things demanded to lend (Title)
#' @export
tagfilter_lenddemand <- function(){
  dict <- list()
  dict$pos <- list(
    lenddemand_1 = "zu entlehnen begehrt"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary lost and found (Title)
#' @export
tagfilter_lostandfound <- function(){
  dict <- list()
  dict$pos <- list(
    lostandfound_1 = "Verlorne und gefundene Sachen", # maybe shorten to include titles with ocr mistakes
    lostandfound_add_1 = "Nachtrag zum Verlohren"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary death notices (Title)
#' @export
tagfilter_death <- function(){
  dict <- list()
  dict$pos <- list(
    death_1 = "Verstorben und begrabe(n|m)"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary marriage notices (Title)
#' @export
tagfilter_marriage <- function(){
  dict <- list()
  dict$pos <- list(
    marriage_1 = "Getraute Ehen"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary labour and information (Title)
#' @export
tagfilter_labourandinfo <- function(){
  dict <- list()
  dict$pos <- list(
    labourandinfo_1 = "ost, Information und Bedienungen" # maybe shorten/different variations to include those with ocr mistakes
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary auctions (Title)
#' @export
tagfilter_auctions <- function(){
  dict <- list()
  dict$pos <- list(
    auctions_1 = "G(a|ä)nten"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary other news (Title)
#' @export
tagfilter_othernews <- function(){
  dict <- list()
  dict$pos <- list(
    buy_1 = "(Allerhand|Allerha nd) Nachrichten",
    buy_add_1 = "Nachtrag zu den Nachrichten"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary official notices (Title)
#' @export
tagfilter_official <- function(){
  dict <- list()
  dict$pos <- list(
    official_1 = "Kundmachung",
    official_2 = "Bekannt machung",
    official_3 = "K u n d m a ch u n g" # regex for white space between every letter?
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary taxes (Title)
#' @export
tagfilter_taxes <- function(){
  dict <- list()
  dict$pos <- list(
    taxes_1 = "B r o d-T (ä|a)(x|r)", # white space?
    taxes_2 = "B r o d- T (ä|a) (x|r)" # white space?
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Bookstore (Title)
#' @export
tagfilter_bookstore <- function(){
  dict <- list()
  dict$pos <- list(
    bookstore_1 = "Buchhandlung",
    bookstore_2 = "Buchhandlulg", # can be deleted, if ocr correction works
    bookstore_3 = "Buch- handlung",
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}      
