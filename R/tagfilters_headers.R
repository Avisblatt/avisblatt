tf_header <- function(prefix = F){
  l <- c("saledemand",
         "saleoffer",
         "lendoffer",
         "lenddemand",
         "lostandfoundheader",
         "death",
         "marriage",
         "labourinfo",
         "auctions",
         "othernews",
         "bookstore",
         "travelheader",
         "exchange",
         "charityheader",
         "foreigners",
         "curious",
         "registry",
         "prices",
         "election",
         "naturalisation",
         "denaturalisation",
         "propertysaleoffer",
         "insolvency",
         "demanding",
         "offering",
         "ps",
         "merge_to_ad")
  if (prefix) {paste0("tagfilter_", l)}
  else {l}
    }

#' Dictionary Prices (Header) - such headers should not exist (part of other workflow, not ads), filter here just to find them
#' @export
tagfilter_prices <- function(){
  dict <- list()
  dict$pos <- list(
    prices_1 = "Prei(s|ß)|Tax|Bro(d|t)",
    prices_2 = "Mineral"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary postcript (Header)
#' @export
tagfilter_ps <- function(){
  dict <- list()
  dict$pos <- list(
    ps_1= "Na(c|e)htr(ä|a)",
    ps_2= "\\bPS\\b|\\PPSS",
    ps_3= "Ferners",
    other = "Folgendesistnachzuholen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary things offered for sale (Header)
#' @export
tagfilter_saleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    saleoffer_1 = "Verkauf",
    saleoffer_2 = "umVerkauf",
    saleoffer_3 = "verkau(ff|f)en"
    )
  dict$neg <- list(
    double = "zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)" # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary things demanded for sale (Header)
#' @export
tagfilter_saledemand <- function(){
  dict <- list()
  dict$pos <- list(
    saledemand_1 = "\\bkau(f|ff)en|zukau(f|ff)en",
    saledemand_2 = "um(K|k)auf"
    )
  dict$neg <- list(
    double = "zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)" # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary things offered to lend (Header)
#' @export
tagfilter_lendoffer <- function(){
  dict <- list()
  dict$pos <- list(
    lendoffer = "((A|a)u(s|ß)|ver|ent)le(i|y)(b|h|ch)en"
    )
  dict$neg <- list(
    double = "zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)" # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary things demanded to lend (Header)
#' @export
tagfilter_lenddemand <- function(){
  dict <- list()
  dict$pos <- list(
    lenddemand_1 = "(entl|l)ehnen",
    lenddemand_2 = "entehnen" # original misspelled
    )
  dict$neg <- list(
    double = "zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)" # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary lost and found (Header)
#' @export
tagfilter_lostandfoundheader <- function(){
  dict <- list()
  dict$pos <- list(
    lost = "(V|v)erl(o|oh)r(ne|en)",
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
    death_1 = "erstorbe|begrabe(n|m)|(B|b)eerdig|gestorben|Todes|Beerdig|eingesegnet" # "erstorbe" without the "v", because a lot of ocr mistakes
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
    marriage_1 = "teEhe|opul(i|ie)rt|ehelich|(G|g)etraut|Trauungen", #teEhe = getraute Ehen
    marriage_2 = "(C|Co|Copu|Copulir|Cop) (o|op|lir|pu|u|te)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary labour, boarding, and information (Header)
#' @export
tagfilter_labourinfo <- function(){
  dict <- list()
  dict$pos <- list(
    labourinfo_1 = "Ko(s|f)t|Information|Bedienung|Bediente|Di(e|ee)n(s|f)t|Jungen"
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
    auctions_1 = "G(a|ä|ae)nt"
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
    news_1= "Allerhand|achricht",
    university_exams = "Lectiones"
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


#' Dictionary Bookstore (Header)
#' @export
tagfilter_bookstore <- function(){
  dict <- list()
  dict$pos <- list(
    bookstore = "Buch(handlung|händler|drucker)",
    books = "Bücher|Buch",
    printed = "ge(d|t)ru(ck|k)t"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Travel (Header)
#' @export
tagfilter_travelheader <- function(){
  dict <- list()
  dict$pos <- list(
    travel_1 = "reisend",
    travel_2 = "Kutsch",
    travel_3 = "^Reis"
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
    charityheader_2 = "Arme|Kran(k|ck)e|Mitleiden|gutherzig|Seelsorger|Barmherzigkeit|Beysteur"
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


#' Dictionary Curiosities (Header)
#' @export
tagfilter_curious <- function(){
  dict <- list()
  dict$pos <- list(
    curious_1 = "Mer(k|ck)würdig",
    curious_2 = "Chronik",
    curious_3 = "EinigeGedanken|Beilage" # 1844 supplements WITHIN issues...
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Registry office (Header)
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


#' Dictionary Election (Header)
#' @export
tagfilter_election <- function(){
  dict <- list()
  dict$pos <- list(
    election_1 = "Besetzung|(Ä|Ae)mt",
    election_2 = "(W|w)ahl",
    military_promotion = "Milit(a|ä)r"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Demand (unclear if for sale or to lend) (Header)
#' @export
tagfilter_demanding <- function(){
  dict <- list()
  dict$pos <- list(
    demand_1 = "(dergleichen|item|ferne(r|rs)|wird|werden)begehrt",
    demand_2 = "(dergleichen|item|ferne(r|rs)|dann)(.*)begehrt",
    demand_3 = "zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)begehrt"
  )
  dict$neg <- list(
    labourinfo = "Bediente|Jungen" # filters out headers belonging to labourinfo category
  )
  create_filter_output(dict)
}


#' Dictionary Offer (unclear if for sale or to lend) (Header)
#' @export
tagfilter_offering <- function(){
  dict <- list()
  dict$pos <- list(
    offer_1 = "(dergleichen|item|ferne(r|rs)|wird|werden)o(ff|f)er(i|ie)rt",
    offer_2 = "(dergleichen|item|ferne(r|rs)|dann) (.*[^Verleyhen])o(ff|f)er(i|ie)rt",
    offer_3 = "zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)o(ff|f)er(i|ie)rt",
    offer_4 = "(wird|werden)o(ff|f)er(i|ie)rt"
  )
  dict$neg <- list(
    labourinfo = "Bediente|Jungen" # filters out headers belonging to labourinfo category
  )
  create_filter_output(dict)
}


#' Dictionary Naturalisation (gain of citizenship) (Header)
#' @export
tagfilter_naturalisation <- function(){
  dict <- list()
  dict$pos <- list(
    naturalisation_1 = "(A|a)ufnahmen",
    naturalisation_2 = "Einbürg"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Denaturalisation (loss of citizenship) (Header)
#' @export
tagfilter_denaturalisation <- function(){
  dict <- list()
  dict$pos <- list(
    denaturalisationdemand = "Ausk(ü|u)nd"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Property Sale Offer (Header)
#' @export
tagfilter_propertysaleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    propertysaleoffer = "Liegenschaft"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Insolvency (Header)
#' @export
tagfilter_insolvency <- function(){
  dict <- list()
  dict$pos <- list(
    insolvency_1 = "Geldstag",
    insolvency_2 = "(R|r)ehabil"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary of 'false headers' that to do not head a section of ads, but are a heading WITHIN an ad. To be merged to next ad
#' @export
tagfilter_merge_to_ad <- function(){
  dict <- list()
  dict$pos <- list(
    notification = "((AVERTI)|(a|à)verti)|\\bKundmach|\\bAnkün|\\bNachri|(A|a)(nzeig|NZEIG)|Avis\\b|AVIS\\b|\\bMit(th|t)eilung|\\Bekanntmach|\\bN(OTA|ota)",
    listings = "Verzeichn|Subs(c|k)ription",
    shooting_competition = "Freischie" #Freischießen
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}
