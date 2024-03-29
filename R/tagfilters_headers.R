tf_header <- function(prefix = F){
  l <- c('saledemand',
         'saleoffer',
         'lendoffer',
         'lenddemand',
         'lend',
         'demanding',
         'offering',
         'lostandfoundheader',
         'lostheader',
         'foundheader',
         'death',
         'marriage',
         'labourinfo',
         'jobseek',
         'joboffer',
         'boardingheader',
         'auctions',
         'othernews',
         'official',
         'bookstore',
         'travelheader',
         'exchange',
         'charityheader',
         'foreigners',
         'curious',
         'registry',
         'prices',
         'election',
         'naturalisation',
         'denaturalisation',
         'propertysaleoffer',
         'insolvency',
         'ps',
         'merge_to_ad')
  if (prefix) {paste0('tagfilter_', l)}
  else {l}
    }

#' Dictionary Prices (Header) - such headers should not exist (part of other workflow, not ads), filter here just to find them
#' @export
tagfilter_prices <- function(){
  dict <- list()
  dict$pos <- list(
    prices_1 = 'Prei(s|\u00df)|Tax|Bro(d|t)',
    prices_2 = 'Mineral'
  )
  dict$neg <- list(
    placeholder = 'Buch'
  )
  create_filter_output(dict)
}


#' Dictionary postcript (Header)
#' @export
tagfilter_ps <- function(){
  dict <- list()
  dict$pos <- list(
    ps_1= 'Na(c|e)htr(\u00e4|a)',
    ps_2= '\\\\bPS\\\\b|\\\\PPSS',
    ps_3= 'Ferners',
    other = 'Folgendesistnachzuholen'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Demand (unclear if for sale or to lend) (Header)
#' @export
tagfilter_demanding <- function(){
  dict <- list()
  dict$pos <- list(
    demand_1 = '(dergleichen|item|ferne(r|rs)|wird|werden)begehrt',
    demand_2 = '(dergleichen|item|ferne(r|rs)|dann)(.*)begehrt',
    demand_3 = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)begehrt'
  )
  dict$neg <- list(
    labourinfo = 'Bediente|Jungen' # filters out headers belonging to labourinfo category
  )
  create_filter_output(dict)
}


#' Dictionary Offer (unclear if for sale or to lend) (Header)
#' @export
tagfilter_offering <- function(){
  dict <- list()
  dict$pos <- list(
    offer_1 = '(dergleichen|item|ferne(r|rs)|wird|werden)o(ff|f)er(i|ie)rt',
    offer_2 = '(dergleichen|item|ferne(r|rs)|dann) (.*[^Verleyhen])o(ff|f)er(i|ie)rt',
    offer_3 = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)o(ff|f)er(i|ie)rt',
    offer_4 = '(wird|werden)o(ff|f)er(i|ie)rt'
  )
  dict$neg <- list(
    labourinfo = 'Bediente|Jungen' # filters out headers belonging to labourinfo category
  )
  create_filter_output(dict)
}


#' Dictionary things offered for sale (Header)
#' @export
tagfilter_saleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    saleoffer_1 = 'Verkauf',
    saleoffer_2 = 'umVerkauf',
    saleoffer_3 = 'verkau(ff|f)en',
    meta_header1 = 'NeueArt|neuenArt',
    meta_header2 = 'AlteArt' #Alte Artikel and Neue Artikel is used for some years as meta-segments for reprinted and new sales ads. It is always followed by a 'proper' header, almost(?) always sale_offer.
    )
  dict$neg <- list(
    double = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)' # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary things demanded for sale (Header)
#' @export
tagfilter_saledemand <- function(){
  dict <- list()
  dict$pos <- list(
    saledemand_1 = '\\\\b(k|K)au(f|ff)en|zuk(au|u)(f|ff)en',
    saledemand_2 = 'um(K|k)auf'
    )
  dict$neg <- list(
    double = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)' # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary offered to lend (Header)
#' @export
tagfilter_lendoffer <- function(){
  dict <- list()
  dict$pos <- list(
    lendoffer = '((A|a)u(s|\u00df)|ver|ent)le(i|y)(b|h|ch)en'
    )
  dict$neg <- list(
    double = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)' # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary demanded to lend (Header)
#' @export
tagfilter_lenddemand <- function(){
  dict <- list()
  dict$pos <- list(
    lenddemand_1 = '(entl|l)ehnen',
    lenddemand_2 = 'entehnen' # original misspelled
    )
  dict$neg <- list(
    double = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)' # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary demanded or offered to lend (Header)
#' @export
tagfilter_lend <- function(){
  dict <- list()
  dict$pos <- list(
    offer_or_request = 'usleihen(.*)lehnen'
  )
  dict$neg <- list(
    double = 'zu(kau(ff|f)en|entlehnen)oderzu(entlehnen|kau(f|ff)en)' # both sale- and lendoffer
  )
  create_filter_output(dict)
}


#' Dictionary lost and found (Header)
#' @export
tagfilter_lostandfoundheader <- function(){
  dict <- list()
  dict$pos <- list(
    lost = '(V|v)erl(o|oh)r(ne|en)',
    found = 'gefund(en|ne)',
    stolen = 'gesto(hl|l)en',
    flownaway = 'entflogen'
    )
  dict$neg <- list(
    lost = 'Verlorenes',
    found = 'Gefundenes\\\\.'
  )
  create_filter_output(dict)
}

#' Dictionary lost (Header)
#' @export
tagfilter_lostheader <- function(){
  dict <- list()
  dict$pos <- list(
    lost = 'Verlorenes'
    )
  dict$neg <- list(
    found = '(G|g)efund(en|ne)'
  )
  create_filter_output(dict)
}

#' Dictionary found (Header)
#' @export
tagfilter_foundheader <- function(){
  dict <- list()
  dict$pos <- list(
    lost = 'Gefundenes'
  )
  dict$neg <- list(
    lost = '(V|v)erl(o|oh)r(ne|en)'
  )
  create_filter_output(dict)
}


#' Dictionary death notices (Header)
#' @export
tagfilter_death <- function(){
  dict <- list()
  dict$pos <- list(
    death_1 = 'erstorbe|begrabe(n|m)|(B|b)eerdig|gestorben|Todes|Beerdig|eingesegnet' # 'erstorbe' without the 'v', because a lot of ocr mistakes
    )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary marriage notices (Header)
#' @export
tagfilter_marriage <- function(){
  dict <- list()
  dict$pos <- list(
    marriage_1 = 'teEhe|opul(i|ie)rt|ehelich|(G|g)etraut|Trauungen', #teEhe = getraute Ehen
    marriage_2 = '(C|Co|Copu|Copulir|Cop) (o|op|lir|pu|u|te)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary labour, boarding, and information (Header)
#' @export
tagfilter_labourinfo <- function(){
  dict <- list()
  dict$pos <- list(
    labourinfo_1 = '(I|l)nformation|Bedienung|Bediente|Jungen|KostundTausch',
    jobseekandoffer1 = 'offerirenundDienste',
    jobseekandoffer2 = 'gebenseindundbegehr'
  )
  dict$neg <- list(
    jobseek = 'Di(e|ee)n(s|f)tge',
    joboffer = 'Di(e|ee)n(s|f)tan'
  )
  create_filter_output(dict)
}

#' Dictionary job seek (Header)
#' @export
tagfilter_jobseek <- function(){
  dict <- list()
  dict$pos <- list(
    jobseek1 = 'Di(e|ee)n(s|f)tge',
    jobseek2 = 'Diens(t|te)(of|an)',
    jobseek3 = 'PersohnensoihreDienste',
    jobseek4 = 'Dienstesuche'
  )
  dict$neg <- list(
    joboffer = 'vergeben'
  )
  create_filter_output(dict)
}

#' Dictionary job offer (Header)
#' @export
tagfilter_joboffer <- function(){
  dict <- list()
  dict$pos <- list(
    joboffer1 = 'Di(e|ee)n(s|f)tan',
    joboffer2 = 'Dienstesozuvergeben'
  )
  dict$neg <- list(
    jobseek = 'begehrt'
  )
  create_filter_output(dict)
}

#' Dictionary boarding (Header)
#' @export
tagfilter_boardingheader <- function(){
  dict <- list()
  dict$pos <- list(
    boarding_1 = 'Kost'
  )
  dict$neg <- list(
    not_labourinfo = 'Information|Bedienung|Bediente|Di(e|ee)n(s|f)t|Jungen|KostundTausch'
  )
  create_filter_output(dict)
}


#' Dictionary auctions (Header)
#' @export
tagfilter_auctions <- function(){
  dict <- list()
  dict$pos <- list(
    auctions_1 = 'G(a|\u00e4|ae)nt',
    auctions_2 = 'Versteiger'
    )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary other news (Header)
#' @export
tagfilter_othernews <- function(){
  dict <- list()
  dict$pos <- list(
    news_1 = 'Allerhand|achricht',
    university_exams = 'Lectiones',
    special_ad = 'Feuerwerk',
    start_up = 'Gesch\u00e4ftsAntrag'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary official notices (Header)
#' @export
tagfilter_official <- function(){
  dict <- list()
  dict$pos <- list(
    official = 'machung',
    announcement = 'Publikation',
    explanation = 'Erkl\u00e4rung'
    )
  dict$neg <- list(
    bookstore = 'Buch(handlung|h\u00e4ndler|drucker)',
    books = 'B\u00fccher|Buch',
    printed = 'ge(d|t)ru(ck|k)t'
  )
  create_filter_output(dict)
}


#' Dictionary Bookstore (Header)
#' @export
tagfilter_bookstore <- function(){
  dict <- list()
  dict$pos <- list(
    bookstore = 'Buch(handlung|h\u00e4ndler|drucker)',
    books = 'B\u00fccher|Buch',
    printed = 'ge(d|t)ru(ck|k)t',
    Flick = 'SamuelFlick',
    bookad = 'Literar'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Travel (Header)
#' @export
tagfilter_travelheader <- function(){
  dict <- list()
  dict$pos <- list(
    travel_1 = 'reisend',
    travel_2 = 'Kutsch',
    travel_3 = '^Reis'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Exchange (Header)
#' @export
tagfilter_exchange <- function(){
  dict <- list()
  dict$pos <- list(
    exchange_1 = 'tauschen'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Charity (Header)
#' @export
tagfilter_charityheader <- function(){
  dict <- list()
  dict$pos <- list(
    charityheader_2 = 'Arme|Kran(k|ck)e|Mitleiden|gutherzig|Seelsorger|Barmherzigkeit|Beysteur'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Foreigners (Header)
#' @export
tagfilter_foreigners <- function(){
  dict <- list()
  dict$pos <- list(
    foreigners_1 = 'fr(\u00f6|e)m(bd|d)e'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Curiosities (Header)
#' @export
tagfilter_curious <- function(){
  dict <- list()
  dict$pos <- list(
    curious = 'Mer(k|ck)w\u00fcrdig',
    chronicle = 'Chronik',
    weather = 'Witterung',
    appendix = '\\\\bAnhang',
    somethoughts = 'EinigeGedanken|Beilage|Etwas[\u00fc|u]ber' 
    
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Registry office (Header)
#' @export
tagfilter_registry <- function(){
  dict <- list()
  dict$pos <- list(
    registry_1 = 'Contor|Berichthaus'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Election (Header)
#' @export
tagfilter_election <- function(){
  dict <- list()
  dict$pos <- list(
    election_1 = 'Besetzung|(\u00c4|Ae)mt',
    election_2 = '(W|w)ahl',
    military_promotion = 'Milit(a|\u00e4)r'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Naturalisation (gain of citizenship) (Header)
#' @export
tagfilter_naturalisation <- function(){
  dict <- list()
  dict$pos <- list(
    naturalisation_1 = '(A|a)ufnahme',
    naturalisation_2 = 'Einb.rge'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Denaturalisation (loss of citizenship) (Header)
#' @export
tagfilter_denaturalisation <- function(){
  dict <- list()
  dict$pos <- list(
    denaturalisationdemand = 'Ausk(\u00fc|u)nd'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Property Sale Offer (Header)
#' @export
tagfilter_propertysaleoffer <- function(){
  dict <- list()
  dict$pos <- list(
    propertysaleoffer = 'Liegenschaft'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Insolvency (Header)
#' @export
tagfilter_insolvency <- function(){
  dict <- list()
  dict$pos <- list(
    insolvency_1 = 'Geldstag|Dorneck',
    insolvency_2 = '(R|r)ehabil'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary of 'false headers' that to do not head a section of ads, but are a heading WITHIN an ad. To be merged to next ad
#' @export
tagfilter_merge_to_ad <- function(){
  dict <- list()
  dict$pos <- list(
    notification = '((AVERTI)|(a|\u00e0)verti)|\\\\bKundmach|\\\\bAnk\u00fcn|(A|a)(nzeig|NZEIG)|Avis\\\\b|AVIS\\\\b|\\\\bMit(th|t)eilung|\\\\Bekanntmach|\\\\bN(OTA|ota)|Buchbinder',
    listings = 'Verzeichn|Subs(c|k)ription',
    shooting_competition = 'Freischie' #Freischie\u00dfen
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}
