#' Tagfilter Finance
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads related to financial matters such as
#' loan requests and offers, trading securities, or advertising class lotteries.
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
#' Calculated that way, the family of tagfilters concerning loans and securities
#' shows a precision >78% and a sensitivity >64%. The lottery tagfilter shows a
#' precision >73% and a sensitivity >95%.
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_finance
#' @seealso tagfilters
NULL



#' Filter Quanteda Corpus: Financial loans
#' @rdname tagfilter_finance
#' @export
tagfilter_finance_loan1 <- function(){
  dict <- list()
  dict$applicable <- list("lendoffer", "lenddemand", "lend")
  dict$pos <- list(
    capital = "(C|K)apital|Vogtsgeld",
    capitalphrase_1 = "(L|(f|F)(r|l)).? \\\\d{3,6}",
    capitalphrase_2 = "\\\\d{3,6}.? (L|(f|F)(r|l))\\\\.",
    capitalphrase_3 = "\\\\d{3,6}.? (Gulden|Franken|(.eue )?Th?aler)",
    capitalphrase_4 = "Pfund Geld",
    capitalphrase_5 = "(S|s)umm(|a|e) (von|G(e|o)l)",
    interest = "(P|p).{1,5}(c|C|z|Z)(en|on|)t(|o)|\\\\%",
    loan1 = "[H|h]ypoth|[a|A]nle[i|]h[n|e]|Anlage",
    loan2 = "(Geld|Capital|Anlag(e|en)).*((b|B)egehren|(G|g)esuch)",
    security = "[P|p]f[a|\\u00e4]nd|B\\u00fcrgschaft|Versatzung",
    securityphrase_1 = "((A|a)uf|(G|g)egen) ([a-z\\u00e4\\u00f6\\u00fc\\u00df]+ )*(Versicherung|Ueberbesserung|Unterpfand)",
    securityphrase_2 = "(Summ(e|en)|Geld) auf"
  )
  dict$neg <- list(
    othercat_things = "Eisengewicht"
  )
  dict$include <- finance_loan_include()
  create_filter_output(dict)
}


#' @rdname tagfilter_finance
#' @export
tagfilter_finance_loan2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews")
  dict$pos <- list(
    capital = "(C|K)apital|Vogtsgeld",
    capitalphrase_1 = "(L|(f|F)(r|l)).? \\\\d{3,6}",
    capitalphrase_2 = "\\\\d{3,6}.? (L|(f|F)(r|l))\\\\.",
    capitalphrase_3 = "\\\\d{3,6}.? (Gulden|Franken|(.eue )?Th?aler)",
    capitalphrase_4 = "Pfund Geld",
    capitalphrase_5 = "(S|s)umm(|a|e) (von|G(e|o)l)",
    loan1 = "[H|h]ypoth|[a|A]nle[i|]h[n|e]|Anlage",
    loan2 = "(Geld|Capital|Anlag(e|en)).*((b|B)egehren|(G|g)esuch)",
    security = "[P|p]f[a|\\u00e4]nd|B\\u00fcrgschaft|Versatzung",
    securityphrase_1 = "((A|a)uf|(G|g)egen) ([a-z\\u00e4\\u00f6\\u00fc\\u00df]+ )*(Versicherung|Ueberbesserung|Unterpfand)",
    securityphrase_2 = "(Summ(e|en)|Geld) auf"
  )
  dict$neg <- list(
    othercat_things = "Eisengewicht"
  )
  dict$include <- finance_loan_include()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Financial papers for sale
#' @rdname tagfilter_finance
#' @export
tagfilter_finance_sale1 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "saledemand")
  dict$pos <- list(
    interest = "Zinsentrichtung|Dividende|Quartal-Beitrag",
    paper = "[O|b]ligation|Staatsanlehn|Staatsanleih|A[c|k]tie|G\\u00fcltbrief|\\\\bAnleihe",
    insurance = "Lebensversicherung|(Assekuranz|Pensions|Wittwen).?(K|k|C|c)ass(e|a)"
  )
  dict$neg <- list(
    othercat_things = "Geld.?(K|k|C|c)ass(a|e)"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Financial papers for sale
#' @rdname tagfilter_finance
#' @export
tagfilter_finance_sale2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews")
  dict$pos <- list(
    interest = "Zinsentrichtung|Dividende|Quartal-Beitrag",
    paper = "[O|b]ligation|Staatsanlehn|Staatsanleih|A[c|k]tie|G\\u00fcltbrief|\\\\bAnleihe",
    insurance = "Lebensversicherung|(Assekuranz|Pensions|Wittwen).?(K|k|C|c)ass(e|a)"
  )
  dict$neg <- list(
    othercat_things = "Geld.?(K|k|C|c)ass(a|e)"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Lottery
#' @rdname tagfilter_finance
#' @export
tagfilter_lottery <- function(){
  dict <- list()
  dict$pos <- list(
    lottery = "(L|l)otter[i|y]|verlo[o|h]s",
    draw <- "Ziehung",
    lot = "\\bLoos(|e|en)\\b",
    numbers_phrase = "folgende Nummern"
  )


  dict$neg <- list(game = "Spiel",
                   book_titles =
                     paste0("Lotteriesucht|(G|g)l\\u00fcckliche(r|)",
                            "Lottospieler|(F|f)romme(n|) Lotterie|Lotterie der",
                            "(Frommen|Lieben)|(G|g)l\\u00fcckliche Lotterielo(o|)",
                            "(\\u00df|s)|(Vogel|Bilder)-Lotterie|",
                            "Rechtm\\u00e4\\u00dfigkeit der Lotterien|Lottokenntni(\\u00df|s)|",
                            "kosmographische Lotterie|",
                            "Gl\\u00fcck in der Lotterie gemacht|Lotterie f\\u00fcr die Herren|jeder",
                            "(L|l)otto(-|\\\\s|)spielende(r|)Liebhaber|jeder Lotteriefreund|",
                            "Die Sch\\u00e4dlichkeit der Zahlen-Lotterie|Lotteriespiel n\\u00fctzlich",
                            "zu gebrauchen|Lottologie|Gedanken vom gro\\u00dfen Lo(o|)se in der",
                            "Lotterie|Geheimni\\u00df der(I|i)tali(\\u00e4|e)nischen Zahlen",
                            "(-|)(L|l)otterien|Der Lotteriespieler|Anmerkungen \\u00fcber",
                            "die Zahlen(-|)(L|l)otterien|Lotterie der Frommen|Lotterien anwendbare",
                            "Auslegung aller Tr\\u00e4ume|Einer Reichs(-|)(L|l)otterie",
                            "(E|e)in (P|p)ro|Wasserfa(rth|hrt)(-|)(L|l)otterie|Rechtm\\u00e4ssigkeit",
                            "der Lotterien|Die Losende Welt|(G|g)eistliche(n|) Lotterie"))

  create_filter_output(dict)
}
