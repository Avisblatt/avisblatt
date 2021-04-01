#' Provide list of umbrella terms
#' @export
umbrella_terms <- function(){
  umbrella_stand <- list(
    drink = "alcohol|broth|chocolate|coffee|milk|mineralwater|tea",
    food = "butter|cheese|driedfruit|eggs|fish|fruit|grain|honey|legumes|meat|mushrooms|nuts|pasta|pastry|poultry|preserves|spices|sugar|syrup|tropicalfruit|vegetable"
  )
  umbrella_stand
}



#' Filter Quanteda Corpus: Finance
#' @export
tagfilter_finance <- function(){
  dict <- list()
  dict$pos <- list(
    capital = "[C|K]apital|Vogtsgeld",
    capitalphrase_1 = "[f|F][r|l]. [0-9]+ auf",
    capitalphrase_2 = "[0-9]+ [f|F][r|l]. auf",
    capitalphrase_1 = "[f|F][r|l]. [0-9]+ nach",
    capitalphrase_2 = "[0-9]+ [f|F][r|l]. nach",
    interest = "Zinsentrichtung|Dividende|Quartal-Beitrag",
    loan = "[H|h]ypothek|[a|A]nle[i|]h[n|e]",
    security = "[P|p]f[a|ä]nd",
    securityphrase_1 = "auf erste Versicherung",
    securityphrase_2 = "auf zwe[i|y]te Versicherung",
    securityphrase_3 = "auf gute Versicherung",
    securityphrase_4 = "auf hinlängliche Versicherung",
    paper = "[O|b]ligation|Staatsanlehn|Staatsanleih|A[c|k]tien|Gültbrief|\\bAnleihe",
    action = "Anforderung|Zahlung",
    insurance= "Lebensversicherung|Assekuranz|-[K|C]ass[e|a]|Pensions[c|k]ass|Wittwen[k|c]ass"
  )
  dict$neg <- list(

    meta = "Hypothekenwesen",
    othercat_things = "Geld-[K|C]ass[a|e]|Geld[k|c]ass[a|e]",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "beerdigt|verstorben|bendaselbst|unrichtig|genöthigt",
    othercat_infophrase1 = "ungültig ansehet",
    othercat_realestate = "Losament|Stübchen|Zimmer|Remise",
    other_transactions = "//bTausch//b|ubscri|übergeben|abzugeben|überlassen|Artikel|versteiger|Versteiger|vergant|//bGant//b"  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Lottery
#' @export
tagfilter_lottery <- function(){
  dict <- list()
  dict$pos <- list(
    lottery = "Lotter[i|y]|verlo[o|h]s",
    lot = "\\bLoos\\b",
    numbers_phrase = "folgende Nummern"
  )
  dict$neg <- list(
    childrensgame = "Lotterie-Spiel"
  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Charity
#' @export
tagfilter_charity <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Placement
#' @export
tagfilter_placement <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Travel opportunities
#' @export
tagfilter_travel <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Print
#' @export
tagfilter_print <- function(){
  dict <- list()
  dict$pos <- list(
    book = "^Buch$|^Bücher[n]$|^Bucher$",
    edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
    material = "^gedruckt$|^Pergament$",
    person = "Buchhändler|^Buchdrucker$|^Buchbinder$",
    place = "Buchhand|Buchdruckere[y|i]|Buchladen|Leihbibl|Leseanstalt",
    format_1 = "\\bin Fol.$",
    format_2 = "in 4to.",
    format_3 = "4°|8°|tom.$|[O|o]ctavo|^Bogen$|^[1-9] Bögen$|Halbfranzband|^[ein|un]gebunden$|^brosch[.|iert]$",
    format_4 = "in [1-9] Bänden",
    format_5 = "^gedruckte[n] Fortsetzung$",
    ausstattung = "^Kupf[f]er$|Holzschnitt|Stahlstich",
    catalog = "Catalogus|Katalog",
    participant = "Mithalte*|Pr[ae|ä]numerant[en]|Abonnent",
    types = "Wörterbuch|Zeitung|Zeitschrift",
    type = "Neueste Schriften",
    title_1 = "Rauracher|Rau-racher|Raura-cher",
    title_2 = "Allgemeine[n] Zeitung",
    title_3 = "Christliche[r|n] Volksbote*",
    title_5 = "Annalen",
    title_6 = "Missions-Magazin",
    title_7 = "Basler-Zeitung|Basler Zeitung",
    title_8 = "Wochenblatt"
  )
  dict$neg <- list(

    lost_prayerbooks = "Psalmbuch|[g|G]esangbuch",
    region = "Entlibuch|Schönenbuch",
    bible = "Buch Mose",
    work = "Platz als",
    other = "Haushaltungsbuch|[Z|z]uber|Fischbeckin|Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe[n]|Buchstabe[n]|Buchführung|Buchhaltung|Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit"
  )
  create_filter_output(dict)

}
