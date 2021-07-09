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

#' Filter Quanteda Corpus: Print outside bookstore section, commercial sellers
#' @export
tagfilter_print2 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "offering", "ps")
  dict$pos <- list(
    person = "Buchhändler|\bBuchdrucker|\bBuchbinder",
    names = "Joh(\\.|an|ann) Jak(\\.|ob) Flick|Schweighauser.|
    Bey Hrn. Jakob Bernoulli im Engelh(o|oo)f",
    place = "Buchhand.|Buchdruckere(y|i)|Buchladen/Druckere(i|y)"
  )
  dict$neg <- list(
    lost_prayerbooks = "Psalmbuch|(g|G)esangbuch",
    region = "Entlibuch|Schönenbuch",
    bible = "Buch Mose",
    work = "Platz als",
    other = "Muster(b|-B)(u|ü)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin|
    Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe(n)|
    Buchstabe(n)|Buchführung|Buchhaltung|Sand(b|-B)uchse(|n)|Handels(b|-B)ücher(|n)|
    Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit|Buchführer(|n)|Buchhalter|
    englisch(|e|en) gedruckt(|e|en)|französisch(|e|en) gedruckt(|e|en)|Behausung"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Print outside bookstore section, non-commercial sellers
tagfilter_print3 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "ps", "offering")
  dict$pos <- list(
    book = "Buch\\b|Bücher(n)|Bucher",
    edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
    material = "\\bPergament",
    language = "teutsch und latein(|isch)",
    format_1 = "\\bin Fol(\\.|io)|\\bin Median-Folio|\\fol\\.",
    format_2 = "4to|4tò|8vò|8vo|Quarto|Quart-B(a|ä)nd",
    format_3 = "4°|8°|Tom\\.\\b|tom\\.\\b|Tomis|Tomes|Tome|Tomi|(O|o)ctavo|\\bBogen|^(1-9) Bögen|Halbfranzband|^(ein|un)gebunden.",
    format_4 = "in (1-9) B(än|)den|Fran(|t)zösische(|n) Bände(|n)",
    format_5 = "^gedruckte(n) Fortsetzung|vermehrt(e|) Edition",
    format_6 = "Band gebunden|(Pergament|Leder|Carton) gebunden|ill.* und gebunden|sauber gebunden",
    #format_7 = "\\bbrosch(.|iert)",
    ausstattung = "^Kupf(f)er(|n)|Holzschnitt|Stahlstich",
    #catalog = "Catalogus|Katalog",
    types = "Wörterbuch|Dictionarium|Lexikon|Lexicon",
    types = "Bibel|Biblen",
    prayerbooks = "Psalmb(u|ü)ch(|lein|s|e)|Psalm-B(u|ü)ch(|lein|s|e)|(g|G)esangb(u|ü)ch(|lein|s|e)"
  )
  dict$neg <- list(
    region = "Entlibuch|Schönenbuch",
    bible = "Buch Mose",
    work = "Platz als",
    auction = "vergant(|h)e(n|t)",
    subscription = "Pr(ä|ae)numeration",
    person = "Buchhändl|Buch-Händl|Buchdruck|Buchb(e|i)nd|Buch bind|Buchhänd ler",
    names_1 = "J(\\.|oh\\.|ohan|ohann) J(\\.|a(c|k)\\.|a(c|k)ob) Flick|Schweighauser.",
    names_2 = "(B|b)e(y|i)(|m) (B\\.|Hrn\\.|Herrn|) (Peter Scherb|Heinrich Haag|Eucharius Haag|Wilhelm Haas|
    J(\\.|oh\\.) J(\\.|a(c|k)\\.|a(c|k)ob) Stupanus|Peter Scherb|J(\\.|oh\\.) J(\\.|ak\\.|a(c|k)ob) Freyler|
    Scholer(,|) auf der Rheinbruck|Daniel Haag|Joh\\. Rudolf Pistorius|C\\. A\\. Serini|
    Ja(c|k)ob Bernoulli im Engelh(o|oo)f|J\\. Decker am Spithalsprung|(|B\\.) Bolli|den Bürgern Bolli|
    Bürger Haas, Sohn auf dem Leonhardsgraben|Emanuel Thurneysen|Johann Rudolf(|f) Im H(off|oof)|
    J(\\.|oh\\.) J(\\.|a(c|k)\\.|a(c|k)ob) Schorndorff|J\\. Decker und Wilhelm Haas)|
    Bey Frau Wittib Schorndorf",
    place_1 = "Buchhand.|Buchdruckere(y|i)|Buchladen|Druckere(i|y)",
    place_2 = "Weiber(-Si|si)|Mannen(-Si|si)|(Weiber|Mannen)-Anhen(|c)ker", #to exclude "Bogen" as specification for a place in a church
    other_1 = "Muster(b|-B)(u|ü)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin",
    other_2 = "Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Mousselin",
    other_3 = "Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe(n)|Buchstabe(n)",
    other_4 = "Buchführung|Buchhaltung|Sand(b|-B)uchse(|n)|Handels(b|-B)ücher(|n)",
    other_5 = "Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit|Buchführer(|n)",
    other_6 = "Buchhalter|buchen(|e|es|er|en|em)|reiner Buchs|Musicpapier|Büchsen|Pfundbuchsen",
    other_7 = "englisch(|e|en) gedruckt(|e|en)|französisch(|e|en) gedruckt(|e|en)",
    other_8 = "buchen Holz|Buchenholz|gener Buchs|Buchsen|Buchs-|Gartenbuchs",
    other_9 = "mit Beschläg*. zu einem Bogen|Kanzel-Bogen|steinerner Bogen|samt Bogen|
    Armbrust-Bogen|mit einem Bogen|Schwi bogen|Bogen-Liecht|Fenster(|-) Bogen|aller Auflagen *. frey|
    generalAuflagen|Al(|c)kofen"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: shared newspaper subscriptions
#' @export
tagfilter_print4 <- function(){
  dict <- list()
  dict$applicable <- list("saledemand", "saleoffer", "offering", "ps", "demanding", "exchange")
  dict$pos <- list(
    participant = "Mithalte*|(a|A)bonnent",
    types = "Zeitung|Zeitschrift",
    title_1 = "Rauracher|Rau-racher|Raura-cher",
    title_2 = "Allgemeine(n) Zeitung",
    title_3 = "Christliche(r|n) Volksbote*",
    title_5 = "Annalen",
    title_6 = "Missions-Magazin",
    title_7 = "Basler-Zeitung|Basler Zeitung",
    title_8 = "Wochenblatt"
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: lost books
#' @export
tagfilter_print5 <- function(){
  dict <- list()
  dict$applicable <- list("lostandfoundheader")
  dict$pos <- list(
    book = "\\bBuch|\\bBüchlein|\\bBücher(|n)",
    prayerbooks = "Psalmb(u|ü)ch(|lein|s|e)|Psalm-B(u|ü)ch(|lein|s|e)|(g|G)esangb(u|ü)ch(|lein|s|e)",
    material = "\\bPergament|mit Kup(f|ff)er(|n)"
  )
  dict$neg <- list(
    to_buy_ps = "zu kauf(f)en|zu haben",
    job = "Buchhalter",
    other_objects = "Muster(b|-B)(u|ü)ch(er|ern)|Tabacks-Buchs|Tabacksbuchs|Sand(b|-B)uchse(|n)|Handels(b|-B)ücher(|n)|Buchs Samuelis",
    other = "Lotterie(|n)|Buchstabe(|n)|Buchsbaum|Buchenholz|(b|B)uchene|Buchsener|Pergamenter"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: books/journals in libraries
#' @export
tagfilter_print6 <- function(){
  dict <- list()
  dict$applicable <- list("lostandfoundheader", "ps", "lendoffer", "demanding", "othernews")
  dict$pos <- list(
    institutions = "Leih(b|-B)ibl.|Leseanstalt"
    #book = "\\bBuch|\\bBüchlein|\\bBücher(|n)",
    #material = "\\bPergament|mit Kup(f|ff)er(|n)"

  )
  dict$neg <- list(
    prayerbooks = "Psalm(en)(b|-B)(u|ü)ch(|lein|s|e)|(g|G)esangb(u|ü)ch(|lein|s|e)",
    participation = "Subscription(|es)|Pr(ae|ä)numerant(|en)|Subs(c|k)ribent(|en)|Mithalter(|n)",
    job = "Buchhaltung",
    selling = "Prei(s|ß)|zu (ver|)kauf(f)en|vergant(|h)e(t|n)|Francken|Buchbinder(|ei)|Buch(-H|h)andlung|Buch(-H|h)ändler|Buch(-D|d)rucker(ei)",
    other_objects = "Muster(b|-B)(u|ü)ch(|er|ern)|Tabacks-Buchs|Tabacksbuchs|Sand(b|-B)uchse(|n)|Handels(b|-B)ücher(|n)|Buchs Samuelis",
    other = "mithalten|Lotterie(|n)|Buchstabe(|n)|Buchsbaum|Buchenholz|(b|B)uchene|Buchsener|Pergamenter|Buchführer(|n)|Buchhalter"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: subscription
#' @export
tagfilter_print7 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "ps", "demanding", "offering", "othernews")
  dict$pos <- list(
    participant = "Subscription(|es)|Pr(ae|ä)numerant(|en)|Subs(c|k)ribent(|en)"
  )
  dict$neg <- list(
    xxx = "yyyy"
  )
  create_filter_output(dict)
}


