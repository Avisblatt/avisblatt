#' Dictionary things offered for sale (Header)
#' @export
tagfilter_saleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    saleoffer_1 = "Verkau(f|ff)",
    saleoffer_2 = "Zum Verkauf",
    saleoffer_3 = "verkau(ff|f)en"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary things demanded for sale (Header)
#' @export
tagfilter_saledemand <- function(){
  dict <- list()
  dict$pos <- list(
    saledemand = "\\bkau(f|ff)en"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary things offered to lend (Header)
#' @export
tagfilter_lendoffer <- function(){
  dict <- list()
  dict$pos <- list(
    lendoffer = "(au(s|ß)|ver|ent)l(ei|ey)hen"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary things demanded to lend (Header)
#' @export
tagfilter_lenddemand <- function(){
  dict <- list()
  dict$pos <- list(
    lenddemand = "(entl|l)ehnen"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary lost and found (Header)
#' @export
tagfilter_lostandfound <- function(){
  dict <- list()
  dict$pos <- list(
    lost = "verl(o|oh)r(ne|en)",
    found = "gefund(en|ne)",
    stolen = "gesto(hl|l)en"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary death notices (Header)
#' @export
tagfilter_death <- function(){
  dict <- list()
  dict$pos <- list(
    death_1 = "erstorbe|begrabe(n|m)|beerdigt|gestorben|Todes" # "erstorbe" without the "v", because a lot of ocr mistakes
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary marriage notices (Header)
#' @export
tagfilter_marriage <- function(){
  dict <- list()
  dict$pos <- list(
    marriage = "Ehe|copul(i|ie)rt|ehelich|getraut"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary labour and information (Header)
#' @export
tagfilter_labourandinfo <- function(){
  dict <- list()
  dict$pos <- list(
    labourandinfo_1 = "Kost|Information|Bedienung|Bediente|Dienst"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary auctions (Header)
#' @export
tagfilter_auctions <- function(){
  dict <- list()
  dict$pos <- list(
    auctions_1 = "G(a|ä)nt"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary other news (Header)
#' @export
tagfilter_othernews <- function(){
  dict <- list()
  dict$pos <- list(
    news_1= "Allerhand|Nachrichten",
    news_2 = "Allerha nd"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary official notices (Header)
#' @export
tagfilter_official <- function(){
  dict <- list()
  dict$pos <- list(
    official = "machung"
    )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary taxes (Header)
#' @export
tagfilter_taxes <- function(){
  dict <- list()
  dict$pos <- list(
    taxes_1 = "B r o (d|t)",
    taxes_2 = "Ta(x|r)|Bro(d|t)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Bookstore (Header)
#' @export
tagfilter_bookstore <- function(){
  dict <- list()
  dict$pos <- list(
    bookstore = "Buch(handlung|händler|drucker)",
    books = "Bücher|Buch"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Travel (Header)
#' @export
tagfilter_travel <- function(){
  dict <- list()
  dict$pos <- list(
    travel_1 = "reisend"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Exchange (Header)
#' @export
tagfilter_exchange <- function(){
  dict <- list()
  dict$pos <- list(
    exchange_1 = "tauschen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Charity (Header)
#' @export
tagfilter_charityheader <- function(){
  dict <- list()
  dict$pos <- list(
    charityheader_2 = "Arme|Kran(k|ck)e|Mitleiden|gutherzig|Seelsorger|Barmherzigkeit|Liebe|Beysteur"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Foreigners (Header)
#' @export
tagfilter_foreigners <- function(){
  dict <- list()
  dict$pos <- list(
    foreigners_1 = "fr(ö|e)m(bd|d)e"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Merkwürdigkeiten (Header)
#' @export
tagfilter_merkwuerdig <- function(){
  dict <- list()
  dict$pos <- list(
    merkwuerdig_1 = "würdigkeit"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary registry office (Header)
#' @export
tagfilter_registry <- function(){
  dict <- list()
  dict$pos <- list(
    registry_1 = "Contor|Berichthaus"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Prices (Header)
#' @export
tagfilter_prices <- function(){
  dict <- list()
  dict$pos <- list(
    prices_1 = "Prei(s|ß)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Election (Header)
#' @export
tagfilter_election <- function(){
  dict <- list()
  dict$pos <- list(
    election = "Besetzung|(Ä|Ae)mt"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}
