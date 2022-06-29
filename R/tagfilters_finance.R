#' Filter Quanteda Corpus: Financial loans
#' @export
tagfilter_finance_loan <- function(){
  dict <- list()
  dict$applicable <- list("lendoffer", "lenddemand", "othernews")
  dict$pos <- list(
    capital = "(C|K)apital|Vogtsgeld",
    capitalphrase_1 = "(L|(f|F)(r|l)).? \\d{3,6}",
    capitalphrase_2 = "\\d{3,6}.? (L|(f|F)(r|l))\\.",
    capitalphrase_3 = "\\d{3,6}.? (Gulden|Franken|(.eue )?Th?aler)",
    capitalphrase_4 = "Pfund Geld",
    capitalphrase_5 = "(S|s)umm(|a|e) (von|G(e|o)l)",
    interest = "Pro(z|c)ent|Cento\\b|\\%",
    loan = "[H|h]ypoth|[a|A]nle[i|]h[n|e]|Anlage|Geld(.B|b)egehr|Geld(.G|g)esuch",
    security = "[P|p]f[a|ä]nd|Bürgschaft|Versatzung",
    securityphrase_1 = "((A|a)uf|(G|g)egen) ([a-zäöüß]+ )*(Versicherung|Ueberbesserung|Unterpfand)",
    securityphrase_2 = "(Summ(e|en)|Geld) auf"
  )
  dict$neg <- list(
    othercat_things = "Eisengewicht"
  )
  dict$include <- finance_loan_include()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Financial papers for sale
#' @export
tagfilter_finance_sale <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "saledemand", "othernews")
  dict$pos <- list(
    interest = "Zinsentrichtung|Dividende|Quartal-Beitrag|Pro(z|c)ent|Cento\\b|\\%",
    loan = "[H|h]ypoth|[a|A]nle[i|]h[n|e]|Anlage|Geld(.B|b)egehr|Geld(.G|g)esuch",
    paper = "[O|b]ligation|Staatsanlehn|Staatsanleih|A[c|k]tie|Gültbrief|\\bAnleihe",
    insurance= "Lebensversicherung|(Assekuranz|Pensions|Wittwen).?(K|k|C|c)ass(e|a)"
  )
  dict$neg <- list(
    othercat_things = "Geld.?(K|k|C|c)ass(a|e)"
  )
  create_filter_output(dict)
}
<<<<<<< Updated upstream
=======

#' Filter Quanteda Corpus: Financial papers for sale
#' @export
tagfilter_finance_sale2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews")
  dict$pos <- list(
    interest = "Zinsentrichtung|Dividende|Quartal-Beitrag",
    paper = "[O|b]ligation|Staatsanlehn|Staatsanleih|A[c|k]tie|Gültbrief|\\bAnleihe",
    insurance= "Lebensversicherung|(Assekuranz|Pensions|Wittwen).?(K|k|C|c)ass(e|a)"
  )
  dict$neg <- list(
    othercat_things = "Geld.?(K|k|C|c)ass(a|e)"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Lottery
#' @export
tagfilter_lottery <- function(){
  dict <- list()
  dict$pos <- list(
    lottery = "(L|l)otter[i|y]|verlo[o|h]s",
    draw <- "Ziehung",
    lot = "\\bLoos(|e|en)\\b",
    numbers_phrase = "folgende Nummern"
  )
  dict$neg <- list(
    game = "Spiel"
  )
  create_filter_output(dict)
}
>>>>>>> Stashed changes
