#' Dictionary things offered for sale (Title)
#' @export
tagfilter_saleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    saleoffer_1 = "zum Verkau(f|ff) angetragen",
    saleoffer_2 = "Alte Verkau(f|ff)-Artikel",
    saleoffer_3 = "Neue Verkau(f|ff)-Artikel",
    saleoffer_add_2 = "Nachtrag zum Verkau(f|ff)"
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
    saledemand_1 = "(zu|zum) kau(f|ff)en begehrt",
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
    lendoffer_1 = "zum Ausl(ei|y)hen (offerirt|anerbo(tt|t)en)", # maybe shorten to "zum Ausleihen"
    lendoffer_add_1 = "Nachtrag zum Ausl(ei|y)hen"
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
    lenddemand_1 = "(zu|zum) entlehnen begehrt",
    lenddemand_2 = "(zu|zum) entlehnen wird begehrt"
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
    lostandfound_1 = "(Verl(o|oh)rne|Verl(oh|o)ren) und gefundene Sachen", # maybe shorten to include titles with ocr mistakes
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
    death_1 = "Verstorben und begrabe(n|m)",
    death_2 = "Verstorb(en|ene)"
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
    marriage_1 = "Getraute Ehen",
    marriage_2 = "Copulirte",
    marriage_3 = "ehelich getraut worden"
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
    labourandinfo_1 = "ost, Information und Bedienungen", # maybe shorten/different variations to include those with ocr mistakes
    labourandinfo_2 = "zu verschiedenen Geschäften und Bedienungen angetragen und begehrt" # only occurs in 1734, I think
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
    auctions_1 = "G(a|ä)nten",
    auctions_2 = "G(a|ä)nt"
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
    bookstore_3 = "Buch- handlung"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Travel (Title)
#' @export
tagfilter_travel <- function(){
  dict <- list()
  dict$pos <- list(
    travel_1 = "Gelegenheit für reisende Pers(o|oh)nen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Exchange (Title)
#' @export
tagfilter_exchange <- function(){
  dict <- list()
  dict$pos <- list(
    exchange_1 = "Zum (Vertauschen|Tauschen)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}
