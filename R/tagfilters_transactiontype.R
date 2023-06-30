#' Tagfilter Transactiontype
#'
#' Tagfilters are regular expression based filters designed to tag ads in 
#' order to classify ads based on their content. The avisblatt R package comes 
#' with curated filters to search for ads according to the type of the proposed 
#' transaction: Is it an offer or a request?
#' 
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The precision and sensitivity of some (families of) tagfilters can be 
#' measured by comparison to a manual classification for four sample years 
#' (1734, 1754, 1774 and 1834) from an early stage of the Avisblatt project.
#' Since the manual classification does often only roughly match the scope of 
#' the tagfilters, their true precision and sensitivity are underestimated.
#' 
#' Calculated that way, the family of tagfilters concerning offers 
#' shows a precision >80% and a sensitivity >87%, those concerning requests a 
#' precision >75% and a sensitivity >66%. 
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_transactiontype
#' @seealso tagfilters
NULL


#' Filter Quanteda Corpus: Transaction type - sale in sale sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2023-02-16
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_sale1 <- function(){
  dict <- list()
  dict$applicable <- list('saledemand', 'saleoffer', 'auctions', 'propertysaleoffer')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - lend in lend sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2023-02-16
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_lend1 <- function(){
  dict <- list()
  dict$applicable <- list('lendoffer', 'lenddemand', 'lend', 'auctions', 'exchange')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - auction in auction sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2023-02-16
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_auction1 <- function(){
  dict <- list()
  dict$applicable <- list('auctions')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - auction in other sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2023-02-16
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_auction2 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'offering', 'labourinfo', 'othernews', 'bookstore', 'ps')
  dict$pos <- list(
    auctioning = 'ver(steig|gant)|meistbieten',
    auctionevent = 'Gant[|e|en]\\\\b|Versteiger'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - Swap in exchange sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2023-02-16
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_swap1 <- function(){
  dict <- list()
  dict$applicable <- list('exchange')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - offers in offer sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_offer1 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'offering', 'joboffer', 'foundheader', 'propertysaleoffer', 'auctions', 'bookstore')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - requests in request/seek/demand sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_request1 <- function(){
  dict <- list()
  dict$applicable <- list('saledemand', 'lenddemand', 'demanding', 'jobseek', 'lostheader')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Transaction type - offers in other sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2021-11-03
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_offer2 <- function(){
  dict <- list()
  dict$applicable <- list('lend', 'labourinfo', 'boardingheader', 'travelheader', 'exchange', 'othernews', 'ps')
  dict$pos <- list(
    offered = '(O|o)(f|ff)er(i|ie)rt|(A|a)ngeboten',
    available = 'zu\\\\shaben|kau(f|ff)en|bekommen',
    recommendyourself = '(re(k|c)o(m|mm)(e|a)nd(i|ie)r(t|en)|empf(iehlt|ehlen))\\\\ssich',
    lending = '(z|Z)u(m|) verle(h|i|y)'
  )
  dict$neg <- list(
    requested = 'beg(e|eh)r(t|en)|gesucht|\\\\ssuch(t|en)\\\\s|\\\\sw\u00fcnsch(t|en)'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - requests in other sections
#' @author Alexander Engel, ORCID 0000-0002-8592-3124
#' @note Last changed 2023-02-01
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_request2 <- function(){
  dict <- list()
  dict$applicable <- list('lend', 'labourinfo', 'boardingheader', 'travelheader', 'exchange', 'othernews', 'ps')
  dict$pos <- list(
    requested_Alex = 'beg(e|eh)r(t|en)|gesucht|\\\\ssuch(t|en)\\\\s|\\\\sw\u00fcnsch(t|en)',
    requested_Ina = '(z|Z)u entlehnen begehrt|(verlangt|such(e|)t|w\u00fcnscht).* zu entlehnen'
  )
  dict$neg <- list(
    offered = '(O|o)(f|ff)er(i|ie)rt|(A|a)ngeboten',
    available = 'zu\\\\shaben|kau(f|ff)en|bekommen',
    recommendyourself = '(re(k|c)o(m|mm)(e|a)nd(i|ie)r(t|en)|empf(iehlt|ehlen))\\\\ssich'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Transaction type - offers of found items
#' @author Alexander Engel, ORCID 0000-0002-8592-3124; most regex by Lars Dickmann
#' @note Last changed 2021-11-03
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_offer3 <- function(){
  dict <- list()
  dict$applicable <- list('lostandfoundheader')
  dict$pos <- list(
    found_1 = '(gefunden\\\\sworden|aufbehalten|w(i|ie)der|eingefunden|zustellen|(Z|z)ur(u|\u00fc)ckgabe)(.*)offer(i|ie)rt',
    found_2 = 'hat.*gefunden',
    pick_up_1 = '(ab|abzu|abge|ab\\\\s)ho(len|t)',
    pick_up_2 = 'allda(.*)zu\\\\senth(e|\\\\s)ben',
    bring = '((B|b)ericht-Hau\u00df|Berichthaus|BerichtHau\u00df|Adresse-Contor)\\\\sgebracht',
    spotted_bird = 'zugeflogen'
  )
  dict$neg <- list(
    loss_1='(E|e)ntwen',
    loss_2='(W|w)egge(nommen|gekommen)',
    loss_3='(V|v)erl(o|oh)ren\\\\s(geg(a|\u00e4)ngen|worden)',
    loss_4='(H|h)inweg(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)',
    loss_5='(H|h)inweg\\\\s(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)',
    loss_6='(A|a)b(H|h)anden'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Transaction type - requests for lost items
#' @author Alexander Engel, ORCID 0000-0002-8592-3124; most regex by Lars Dickmann
#' @note Last changed 2021-11-03
#' @name tagfilter_transactiontype
#' @export
tagfilter_transactiontype_request3 <- function(){
  dict <- list()
  dict$applicable <- list('lostandfoundheader')
  dict$pos <- list(
    loss_1='(E|e)ntwen',
    loss_2='(W|w)egge(nommen|gekommen)',
    loss_3='(V|v)erl(o|oh)ren\\\\s(geg(a|\u00e4)ngen|worden)',
    loss_4='(H|h)inweg(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)',
    loss_5='(H|h)inweg\\\\s(gekommen|genommen|verl(o|oh)ren|getragen|practicirt)',
    loss_6='(A|a)b(H|h)anden',
    discoverer='(F|f)inder|(W|w)(i|ie)derbringer|Ueberbringer',
    return='(dessen|deren|g\u00fctige)\\\\s(Z|z)ur(\u00fc|u)ckgabe',
    fair_reward='rechtm(\u00e4|a)(\u00df|ss|t)ig(e|en|er)\\\\s(Belohnung|R\u00fcckgabe)'
  )
  dict$neg <- list(
    found_1 = '(gefunden\\\\sworden|aufbehalten|w(i|ie)der|eingefunden|zustellen|(Z|z)ur(u|\u00fc)ckgabe)(.*)offer(i|ie)rt',
    found_2 = 'hat.*gefunden'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}
