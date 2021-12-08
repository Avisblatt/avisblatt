#' Filter Quanteda Corpus: Transaction type - offers in offer sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_transactiontype_offer1 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "offering", "joboffer", "foundheader", "propertysaleoffer", "auctions", "bookstore")
  dict$pos <- list(
    all = "."
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - requests in request/seek/demand sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_transactiontype_request1 <- function(){
  dict <- list()
  dict$applicable <- list("saledemand", "lenddemand", "demanding", "jobseek", "lostheader")
  dict$pos <- list(
    all = "."
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Transaction type - offers in sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_transactiontype_offer2 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo", "boardingheader", "travelheader", "exchange", "othernews", "ps")
  dict$pos <- list(
    offered = "(O|o)(f|ff)er(i|ie)rt|(A|a)ngeboten",
    available = "zu\\shaben|kau(f|ff)en|bekommen",
    recommendyourself = "(re(k|c)o(m|mm)(e|a)nd(i|ie)r(t|en)|empf(iehlt|ehlen))\\ssich"
  )
  dict$neg <- list(
    requested = "beg(e|eh)r(t|en)|gesucht|\\ssuch(t|en)\\s|\\swünsch(t|en)"
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - requests in other sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_transactiontype_request2 <- function(){
  dict <- list()
  dict$applicable <- list("lostandfoundheader", "labourinfo", "boardingheader", "travelheader", "exchange", "othernews", "ps")
  dict$pos <- list(
    requested = "beg(e|eh)r(t|en)|gesucht|\\ssuch(t|en)\\s|\\swünsch(t|en)"
  )
  dict$neg <- list(
    offered = "(O|o)(f|ff)er(i|ie)rt|(A|a)ngeboten",
    available = "zu\\shaben|kau(f|ff)en|bekommen",
    recommendyourself = "(re(k|c)o(m|mm)(e|a)nd(i|ie)r(t|en)|empf(iehlt|ehlen))\\ssich"
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Transaction type - offers of found items
#' @author Alexander Engel, ORCID 0000-0002-8592-3124; most regex by Lars Dickmann
#' @note Last changed 2021-11-03
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_transactiontype_offer3 <- function(){
  dict <- list()
  dict$applicable <- list("lostandfoundheader")
  dict$pos <- list(
    found_1 = "(gefunden\\sworden|aufbehalten|w(i|ie)der|eingefunden|zustellen|(Z|z)ur(u|ü)ckgabe)(.*)offer(i|ie)rt",
    found_2 = "hat.*gefunden",
    pick_up_1 = "(ab|abzu|abge|ab\\s)ho(len|t)",
    pick_up_2 = "allda(.*)zu\\senth(e|\\s)ben",
    bring = "((B|b)ericht-Hauß|Berichthaus|BerichtHauß|Adresse-Contor)\\sgebracht",
    spotted_bird = "zugeflogen"
  )
  dict$neg <- list(
    loss_1="(E|e)ntwen",
    loss_2="(W|w)egge(nommen|gekommen)",
    loss_3="(V|v)erl(o|oh)ren\\s(geg(a|ä)ngen|worden)",
    loss_4="(H|h)inweg(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)",
    loss_5="(H|h)inweg\\s(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)",
    loss_6="(A|a)b(H|h)anden"
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - requests for lost items
#' @author Alexander Engel, ORCID 0000-0002-8592-3124; most regex by Lars Dickmann
#' @note Last changed 2021-11-03
#' @usage Tagfilters are used internally in the creation of yearly collections of Avisblatt ads, to attribute tags to different ads.
#' @export
tagfilter_transactiontype_request3 <- function(){
  dict <- list()
  dict$applicable <- list("lostandfoundheader")
  dict$pos <- list(
    loss_1="(E|e)ntwen",
    loss_2="(W|w)egge(nommen|gekommen)",
    loss_3="(V|v)erl(o|oh)ren\\s(geg(a|ä)ngen|worden)",
    loss_4="(H|h)inweg(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)",
    loss_5="(H|h)inweg\\s(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)",
    loss_6="(A|a)b(H|h)anden",
    discoverer="(F|f)inder|(W|w)(i|ie)derbringer|Ueberbringer",
    return="(dessen|deren|gütige)\\s(Z|z)ur(ü|u)ckgabe",
    fair_reward="rechtm(ä|a)(ß|ss|t)ig(e|en|er)\\s(Belohnung|Rückgabe)"
  )
  dict$neg <- list(
    found_1 = "(gefunden\\sworden|aufbehalten|w(i|ie)der|eingefunden|zustellen|(Z|z)ur(u|ü)ckgabe)(.*)offer(i|ie)rt",
    found_2 = "hat.*gefunden"
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}