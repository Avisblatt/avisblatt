#' Dictionary things offered for sale (Header)
#' @export
tagfilter_saleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    saleoffer_1 = "zum Verkau(f|ff) (angetragen|offer(i|ie)rt)",
    saleoffer_2 = "Alte Verkau(f|ff)-Artikel",
    saleoffer_3 = "Neue Verkau(f|ff)-Artikel",
    saleoffer_4 = "Zum Kaufen,", # should only find saleoffers, but maybe also some sale demand included (has to be tested)
    saleoffer_5 = "Zum Verkaufen",
    saleoffer_6 = "zum Verkauf(angetragen|offer(i|ie)rt)",
    saleoffer_7 = "P.S. zu (verk|k)au(ff|f)en",
    saleoffer_8 = "verlangt zu kau(ff|f)en",
    saleoffer_add_1 = "Nachtrag zum Verkau(f|ff)"
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
    saledemand_1 = "(zu|zum) kau(f|ff)en begehr",
    saledemand_2 = "kau(ff|f)enbegehr",
    saledemand_3 = "(begehrt|verlangt) annoch zu kau(ff|f)en",
    saledemand_4 = "(begehrt|verlangt) zu kau(ff|f)en"
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
    lendoffer_1 = "(Au(s|ß)|Ver|Ent)l(ei|ey)hen (offer(i|ie)rt|anerbo(tt|t)en)", # maybe shorten to "zum Ausleihen"
    lendoffer_2 = "(Au(s|ß)|Ver|Ent)l(ei|ey)hen(offer(i|ie)rt|anerbo(tt|t)en)",
    lendoffer_3 = "z(um|u) (Au(s|ß)|Ver|Ent)l(ei|ey)hen",
    lendoffer_add_1 = "Nachtrag zum (Au(s|ß)|Ver)l(ei|y)hen"
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
    lenddemand_1 = "(entl|l)ehnen (begehr|gesuch)",
    lenddemand_2 = "(entl|l)lehnen wird (begehrt|gesuch)",
    lenddemand_3 = "lehnen(beger|gesuch)"
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
    death_1 = "erstorbe|begrabe(n|m)|beerdigt|gestorben", # "erstorbe" without the "v", because a lot of ocr mistakes
    death_2 = "Todes-Anzeig"
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
    marriage_1 = "Ehe",
    marriage_2 = "copulirt",
    marriage_3 = "ehelich|getraut"
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
    labourandinfo_1 = "Kost|Information|Bedienung|Bediente"
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
    auctions_1 = "G(a|ä)nten",
    auctions_2 = "G(a|ä)nt"
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
    news_1 = "(Allerhand|Allerha nd) Nachrichten",
    news_2 = "Allerhand-Nachrichten",
    news_add_1 = "Nachtrag zu den Nachrichten"
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
    official_1 = "Kundmachung",
    official_2 = "Bekannt machung",
    official_3 = "K u n d m a ch u n g" # regex for white space between every letter?
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
    taxes_1 = "B r o d-T (ä|a)(x|r)", # white space?
    taxes_2 = "B r o d- T (ä|a) (x|r)" # white space?
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
    bookstore = "Buchhandlung",
    books = "Bücher"
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
    travel_1 = "reisende"
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
    exchange_1 = "Vertauschen|Tauschen"
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
    charityheader_1 = "Arme und Kran(k|ck)e",
    charityheader_2 = "Mitleiden|gutherzig|Seelsorger|Barmherzigkeit|Liebe|Beysteur"
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
    merkwuerdig_1 = "Mer(ck|k)würdigkeit|würdigkeit"
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

