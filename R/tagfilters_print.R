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
  dict$applicable <- list("saleoffer", "offering", "ps", "othernews")
  dict$pos <- list(
    person = "Buchhändler|\\bBuchdrucker|\\bBuchbinder",
    names = "J(\\.|oh\\.|ohan|ohann) J(\\.|a(c|k)\\.|a(c|k)ob) Flick|Schweighauser|
    (B|b)e(y|i)(|m) (B\\.|Hrn\\.|Herrn|) (Peter Scherb|Heinrich Haag|Eucharius Haag|
    Wilhelm Haas|J(\\.|oh\\.) J(\\.|a(c|k)\\.|a(c|k)ob) Stupanus|Peter Scherb|
    J(\\.|oh\\.) J(\\.|ak\\.|a(c|k)ob) Freyler|Scholer(,|) auf der Rheinbruck|
    Daniel Haag|Joh\\. Rudolf Pistorius|C\\. A\\. Serini|Ja(c|k)ob Bernoulli im
    Engelh(o|oo)f|J\\. Decker am Spithalsprung|(|B\\.) Bolli|den Bürgern Bolli|
    Bürger Haas, Sohn auf dem Leonhardsgraben|Emanuel Thurneysen|Johann Rudolf(|f)
    Im H(off|oof)|J(\\.|oh\\.) J(\\.|a(c|k)\\.|a(c|k)ob) Schorndorff|J\\. Decker
    und Wilhelm Haas|Emanuel Hoffmann)|Bey Frau Wittib Schorndorf",
    place = "Buchhand*.|Buchdruckere(y|i)|Buchladen|Druckere(i|y)"
  )
  dict$neg <- list(
    bible = "Buch Mose",
    work = "Platz als",
    lottery = "(Lo(|o)(s|ß)|Lotterie)",
    other = "Muster(b|-B)(u|ü)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin|
    Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Tabacks-Buchs|Tabacksbuchs|
    Anfangsbuchstabe(n)|Buchstabe(n)|Buchführung|Buchhaltung|Sand(b|-B)uchse(|n)|
    Handels(b|-B)ücher(|n)|Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit|
    Buchführer(|n)|Buchhalter|englisch(|e|en) gedruckt(|e|en)|französisch(|e|en)
    gedruckt(|e|en)|Behausung|Buchbinder-Werckz"
    )
  dict$include <- list(
    "cf9c106c-ec5f-5e14-a74e-1f6228fa248b/t34", "db99a10c-a6b0-5689-9ef0-74336dbd2da3/t30", # book titles that contain "Lotterie" etc. See complete list at bottom
    "c8ef15ad-a52d-5396-b8e2-30c10e3557b0/t20", "73827605-442a-5c56-b23b-a9f13a885e9f/t6",
    "cfd1358e-ef18-5bc9-90f5-d204528f668c/t2", "fae377f7-7756-5317-acea-cc0c95792b0c/t10",
    "1e1cd3eb-eb5b-501a-bb75-a8eee1a12cb2/a1", "29c7d8ab-3e6c-5070-814b-af8d99aedaa2/a10",
    "011550b7-0e34-5b42-9245-ff3a6a4c0df4/t18", "8cf75f5d-ba0b-5e68-a280-59eeb5e2a49b/t6",
    "93dccc0b-3541-54df-8776-c0c852afbede/t3", "e3510f92-2f6d-5192-9815-5850ee07e631/t8",
    "578b8710-d104-5409-ae4d-b11d04a4ba65/t7", "cdb7d1fd-5b56-5834-b849-b3fb17cc08bd/t8",
    "8ca55bbf-4adc-5cb4-bed7-034d3ef312bf/t12", "d1424e64-d176-531d-90fe-7d247e471cc9/t14",
    "264925f3-f7e1-530b-aed2-92a75c213b97/t6", "dbe46241-0df8-5092-b6d9-19f8f78096a4/t9"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Print outside bookstore section, non-commercial sellers
tagfilter_print3 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "ps", "offering")
  dict$pos <- list(
    book = "Buch\\b|Bücher(n|)|Bucher",
    edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
    material = "\\bPergament",
    language = "teutsch und latein(|isch)",
    format_1 = "\\bin Fol(\\.|io)|\\binFol|\\bin Median-Folio|\\sfol\\.|großOctav",
    format_2 = "4to|4tò|8vò|8vo|8v0|Quarto|Quart-B(a|ä)nd",
    format_3 = "4°|8°|Tom\\.\\b|tom\\.\\b|Tomis|Tomes|Tome|Tomi|(O|o)ctavo|\\bBogen|^(1-9) Bögen|Halbfranzband|^(ein|un)gebunden.",
    format_4 = "in (1-9) B(än|)den|Fran(|t)zösische(|n|m) B(ä|a)nd(|en|n)",
    format_5 = "^gedruckte(n) Fortsetzung|vermehrt(e|) Edition",
    format_6 = "Band gebunden|(Pergament|Leder|Carton) gebunden|ill.* und gebunden|sauber gebunden|Schweinsleder gebunden",
    #format_7 = "\\bbrosch(.|iert)",
    feautures = "^Kupf(f)er(|n)|Holzschnitt|Stahlstich",
    #catalog = "Catalogus|Katalog",
    types_1 = "Wörterbuch|Dictionarium|Lexikon|Lexicon",
    types_2 = "Bibel|Biblen|Biblia",
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
    place_2 = "Weiber(-Si|si)|Mannen(-Si|si)|(Weiber|Mannen)-Anhen(|c)ker", #to exclude "Bogen" as specification for the location of a church seat
    other_1 = "Muster(b|-B)(u|ü)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin",
    other_2 = "Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Mousselin",
    other_3 = "Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe(n)|Buchstabe(n)",
    other_4 = "Buchführung|Buchhaltung|Sand(b|-B)uchse(|n)|Handels(b|-B)ücher(|n)",
    other_5 = "Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit|Buchführer(|n)",
    other_6 = "Buchhalter|buchen(|e|es|er|en|em)|reiner Buchs|Musicpapier|Büchsen|Pfundbuchsen",
    other_7 = "englisch(|e|en) gedruckt(|e|en)|französisch(|e|en) gedruckt(|e|en)",
    other_8 = "buchen Holz|Buchenholz|gener Buchs|Buchsen|Buchs-|Gartenbuchs",
    other_9 = "mit Beschläg*. zu einem Bogen|Kanzel-Bogen|steinerner Bogen|samt Bogen|
    Armbrust-Bogen|mit einem Bogen|Schwi bogen|Bogen-Liecht|Fenster(|-) Bogen|Violin-Bogen|
    Pfeil und Bogen|aller Auflagen *. frey|generalAuflagen|Al(|c)kofen"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Print outside bookstore section, non-commercial buyers
tagfilter_print4 <- function(){
  dict <- list()
  dict$applicable <- list("saledemand", "ps", "demanding", "othernews")
  dict$pos <- list(
    book = "Buch\\b|Bücher(n|)|Bucher",
    edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
    material = "\\bPergament",
    language = "teutsch und latein(|isch)",
    format_1 = "\\bin Fol(\\.|io)|\\binFol|\\bin Median-Folio|\\sfol\\.|großOctav",
    format_2 = "4to|4tò|8vò|8vo|8v0|Quarto|Quart-B(a|ä)nd",
    format_3 = "4°|8°|Tom\\.\\b|tom\\.\\b|Tomis|Tomes|Tome|Tomi|(O|o)ctavo|\\bBogen|^(1-9) Bögen|Halbfranzband|^(ein|un)gebunden.",
    format_4 = "in (1-9) B(än|)den|Fran(|t)zösische(|n|m) B(ä|a)nd(|en|n)",
    format_5 = "^gedruckte(n) Fortsetzung|vermehrt(e|) Edition",
    format_6 = "Band gebunden|(Pergament|Leder|Carton) gebunden|ill.* und gebunden|sauber gebunden|Schweinsleder gebunden",
    #format_7 = "\\bbrosch(.|iert)",
    feautures = "^Kupf(f)er(|n)|Holzschnitt|Stahlstich",
    #catalog = "Catalogus|Katalog",
    types_1 = "Wörterbuch|Dictionarium|Lexikon|Lexicon",
    types_2 = "Hand-Bibel|Bibel|Biblen|Biblia",
    prayerbooks = "Psalmb(u|ü)ch(|lein|s|e)|Psalm-B(u|ü)ch(|lein|s|e)|(g|G)esangb(u|ü)ch(|lein|s|e)"
  )
  dict$neg <- list(
    region = "Entlibuch|Schönenbuch",
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
    place_2 = "Weiber(-Si|si)|Mannen(-Si|si)|(Weiber|Mannen)-Anhen(|c)ker", #to exclude "Bogen" as specification for the location of a church seat
    other_1 = "Muster(b|-B)(u|ü)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin",
    other_2 = "Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Mousselin",
    other_3 = "Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe(n)|Buchstabe(n)",
    other_4 = "Buchführung|Buchhaltung|Sand(b|-B)uchse(|n)|Handels(b|-B)ücher(|n)",
    other_5 = "Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit|Buchführer(|n)",
    other_6 = "Buchhalter|buchen(|e|es|er|en|em)|reiner Buchs|Musicpapier|Büchsen|Pfundbuchsen",
    other_7 = "englisch(|e|en) gedruckt(|e|en)|französisch(|e|en) gedruckt(|e|en)",
    other_8 = "buchen Holz|Buchenholz|gener Buchs|Buchsen|Buchs-|Gartenbuchs",
    other_9 = "mit Beschläg*. zu einem Bogen|Kanzel-Bogen|steinerner Bogen|samt Bogen|
    Armbrust-Bogen|mit einem Bogen|Schwi bogen|Bogen-Liecht|Fenster(|-) Bogen|Violin-Bogen|
    Pfeil und Bogen|aller Auflagen *. frey|generalAuflagen|Al(|c)kofen"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: shared newspaper subscriptions
#' @export
tagfilter_print5 <- function(){
  dict <- list()
  dict$applicable <- list("saledemand", "saleoffer", "offering", "ps", "demanding", "exchange")
  dict$pos <- list(
    participant = "Mithalte.*|(a|A)bonnent.*",
    types = "Zeitung|Zeitschrift",
    title_1 = "Rauracher|Rau-racher|Raura-cher",
    title_2 = "Allgemeine(n) Zeitung",
    title_3 = "Christliche(r|n) Volksbote.*",
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
tagfilter_print6 <- function(){
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
tagfilter_print7 <- function(){
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
tagfilter_print8 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "ps", "demanding", "offering", "othernews")
  dict$pos <- list(
    participant = "S(o|)u(b|p|)s(c|k)ription|Pr(ae|ä)num(m|)erant|Pr(ae|ä)num(m|)eration|
    S(o|)u(b|p|)s(c|k)ribent",
    participate = "s(o|)u(b|p|)s(c|k)ribi(e|)r(t|)|pränumeri(|e)r(t|)"
  )
  dict$neg <- list(
    lottery = "Lotterie",
    mine = " Berg(-W|w)erck"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: co-subscription
#' @export
tagfilter_print9 <- function(){
  dict <- list()
  dict$applicable <- list("saledemand", "saleoffer", "offering", "ps", "demanding", "exchange")
  dict$pos <- list(
    participant = "Mi(t|tt)halte.*|mi(t|tt)halten|Gemeinder.{0,5}|Liebhaber.*(J|j)ournal",
    with = "mit einem oder mehr Freunden in Compagnie|Zeitung.*mittheilen|
    (mit andern|gemeinschaftlich)(| zu) halten|mitzuhalten|\\bmit ander(n|en).*zu halten|
    zum behalten.*zu haben|Theil an dem.*zu haben",
    money = "Gebühr zum Durchlesen"
  )
  dict$neg <- list(
    publication = "AVERTISSEMENT|Avertissement",
    milk = "Eselsmilch|Schotte",
    lotterie = "Lo(|o)(s|ß)|Lotterie",
    council = "Gemeinderat.*"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: book lottery
#' @export
tagfilter_print10 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "offering", "ps", "exchange", "othernews")
  dict$pos <- list(
    lotterie = "(Buch\\b|Bücher(n|)|Bucher|Bibel)(|\\s|-)(|\\s|-)(Lo(|o)(s|ß)|Lotterie)",
    several_lotteries = "(Buch\b|Bücher(n|)|Bucher)(-| |)(?= und .*-Lotterie)"
  )
  dict$neg <- list(
    publication = "AVERTISSEMENT|Avertissement",
    book_titles = "Lotteriesucht|glückliche(r|) Lottospieler|Fromme(n|) Lotterie|Lotterie der
                  (Frommen|Lieben)| glückliche Lotterielo(o|)(ß|s)|(Vogel|Bilder)-Lotterie|
                  Rechtmäßigkeit der Lotterien| Lottokenntniß|kosmographische Lotterie|
                  Glück in der Lotterie gemacht|Lotterie für die Herren|Traum(-B|b)üchlein|
                  Die Schädlichkeit der Zahlen-Lotterie|(C|K)abbalistische Tabellen|
                  Lottologie|Gedanken vom großen Lo(o|)se in der Lotterie|Geheimniß der
                  Itali(ä|e)nischen Zahlen-Lotterien|Der Lotteriespieler|Anmerkungen über
                  die Zahlen-Lotterien|Lotterie der Frommen|Auslegung aller Träume|
                  Privat(-G|g)edan(c|)ken|Wasserfart(h|)-Lotterie|Rechtmässigkeit der
                  Lotterien|Die Losende Welt"
  )
  create_filter_output(dict)
}

### book titles containing "lotterie" etc.:
### "Lotteriesucht|(G|g)lückliche(r|) Lottospieler|(F|f)romme(n|) Lotterie|Lotterie der (Frommen|Lieben)|
### (G|g)lückliche Lotterielo(o|)(ß|s)|(Vogel|Bilder)-Lotterie|Rechtmäßigkeit der Lotterien|Lottokenntniß|
### kosmographische Lotterie|Glück in der Lotterie gemacht|Lotterie für die Herren|jeder (L|l)otto(-|\\s|)spielende(r|)
### Liebhaber|jeder Lotteriefreund|Die Schädlichkeit der Zahlen-Lotterie|Lotteriespiel nützlich zu gebrauchen|Lottologie|
### Gedanken vom großen Lo(o|)se in der Lotterie|Geheimniß der (I|i)tali(ä|e)nischen Zahlen(-|)(L|l)otterien|
### Der Lotteriespieler|Anmerkungen über die Zahlen(-|)(L|l)otterien|Lotterie der Frommen|
### Lotterien anwendbare Auslegung aller Träume|Einer Reichs(-|)(L|l)otterie (E|e)in (P|p)ro|
### Wasserfa(rth|hrt)(-|)(L|l)otterie|Rechtmässigkeit der Lotterien|Die Losende Welt|(G|g)eistliche(n|) Lotterie"
