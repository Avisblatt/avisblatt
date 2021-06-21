#' Filter Quanteda Corpus: Print inside bookstore section
#' @export
tagfilter_print1 <- function(){
  dict <- list()
  dict$applicable <- list("bookstore")
  dict$pos <- list(
    all = "." #
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Print outside bookstore section
#' @export
tagfilter_print2 <- function(){
  dict <- list()
  dict$applicable <- list("saledemand", "saleoffer")
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
