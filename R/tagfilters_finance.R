#' Tagfilter Finance
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for finance related ads and finds ads that contain different
#' loans, sales and lotteries.
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
  dict$applicable <- list("lendoffer", "lenddemand")
  dict$pos <- list(
    capital = "(C|K)apital|Vogtsgeld",
    capitalphrase_1 = "(L|(f|F)(r|l)).? \\d{3,6}",
    capitalphrase_2 = "\\d{3,6}.? (L|(f|F)(r|l))\\.",
    capitalphrase_3 = "\\d{3,6}.? (Gulden|Franken|(.eue )?Th?aler)",
    capitalphrase_4 = "Pfund Geld",
    capitalphrase_5 = "(S|s)umm(|a|e) (von|G(e|o)l)",
    interest = "(P|p).{1,5}(c|C|z|Z)(en|on|)t(|o)|\\%",
    loan1 = "[H|h]ypoth|[a|A]nle[i|]h[n|e]|Anlage",
    loan2 = "(Geld|Capital|Anlag(e|en)).*((b|B)egehren|(G|g)esuch)",
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


#' @rdname tagfilter_finance
#' @export
tagfilter_finance_loan2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews")
  dict$pos <- list(
    capital = "(C|K)apital|Vogtsgeld",
    capitalphrase_1 = "(L|(f|F)(r|l)).? \\d{3,6}",
    capitalphrase_2 = "\\d{3,6}.? (L|(f|F)(r|l))\\.",
    capitalphrase_3 = "\\d{3,6}.? (Gulden|Franken|(.eue )?Th?aler)",
    capitalphrase_4 = "Pfund Geld",
    capitalphrase_5 = "(S|s)umm(|a|e) (von|G(e|o)l)",
    loan1 = "[H|h]ypoth|[a|A]nle[i|]h[n|e]|Anlage",
    loan2 = "(Geld|Capital|Anlag(e|en)).*((b|B)egehren|(G|g)esuch)",
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
#' @rdname tagfilter_finance
#' @export
tagfilter_finance_sale1 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "saledemand")
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

#' Filter Quanteda Corpus: Financial papers for sale
#' @rdname tagfilter_finance
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
  dict$neg <- list(
    game = "Spiel",
    book_titles = "Lotteriesucht|(G|g)lückliche(r|) Lottospieler|(F|f)romme(n|) Lotterie|Lotterie der
                  (Frommen|Lieben)|(G|g)lückliche Lotterielo(o|)(ß|s)|(Vogel|Bilder)-Lotterie|
                  Rechtmäßigkeit der Lotterien|Lottokenntni(ß|s)|kosmographische Lotterie|
                  Glück in der Lotterie gemacht|Lotterie für die Herren|jeder (L|l)otto(-|\\s|)spielende(r|)
                  Liebhaber|jeder Lotteriefreund|Die Schädlichkeit der Zahlen-Lotterie|Lotteriespiel nützlich zu gebrauchen|
                  Lottologie|Gedanken vom großen Lo(o|)se in der Lotterie|Geheimniß der
                  (I|i)tali(ä|e)nischen Zahlen(-|)(L|l)otterien|Der Lotteriespieler|Anmerkungen über
                  die Zahlen(-|)(L|l)otterien|Lotterie der Frommen|Lotterien anwendbare Auslegung aller Träume|
                  Einer Reichs(-|)(L|l)otterie (E|e)in (P|p)ro|Wasserfa(rth|hrt)(-|)(L|l)otterie|Rechtmässigkeit der
                  Lotterien|Die Losende Welt|(G|g)eistliche(n|) Lotterie"
  )
  create_filter_output(dict)
}

