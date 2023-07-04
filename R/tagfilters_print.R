#' Tagfilter Print
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads concerning books and other printed items,
#' including promotion by bookstores and publishers, (shared) subscriptions to 
#' books and journals, and books in specific contexts like books in libraries, 
#' lost books, or  book lotteries.
#'
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_print
#' @seealso tagfilters
NULL


#' Filter Quanteda Corpus: Print inside bookstore section
#' @rdname tagfilter_print
#' @export
tagfilter_print1 <- function(){
  dict <- list()
  dict$applicable <- list('bookstore')
  dict$pos <- list(
    all = '.' #
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Print outside bookstore section, commercial sellers
#' @rdname tagfilter_print
#' @export
tagfilter_print2 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'offering', 'ps', 'othernews')
  dict$pos <- list(
    person = 'Buchh\u00e4ndler|\\\\bBuchdrucker|\\\\bBuchbinder',
    names = 'J(\\\\.|oh\\\\.|ohan|ohann) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Flick|Schweighauser|
    (B|b)e(y|i)(|m) (B\\\\.|Hrn\\\\.|Herrn|) (Peter Scherb|Heinrich Haag|Eucharius Haag|
    Wilhelm Haas|J(\\\\.|oh\\\\.) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Stupanus|Peter Scherb|
    J(\\\\.|oh\\\\.) J(\\\\.|ak\\\\.|a(c|k)ob) Freyler|Scholer(,|) auf der Rheinbruck|
    Daniel Haag|Joh\\\\. Rudolf Pistorius|C\\\\. A\\\\. Serini|Ja(c|k)ob Bernoulli im
    Engelh(o|oo)f|J\\\\. Decker am Spithalsprung|(|B\\\\.) Bolli|den B\u00fcrgern Bolli|
    B\u00fcrger Haas, Sohn auf dem Leonhardsgraben|Emanuel Thurneysen|Johann Rudolf(|f)
    Im H(off|oof)|J(\\\\.|oh\\\\.) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Schorndorff|J\\\\. Decker
    und Wilhelm Haas|Emanuel Hoffmann)|Bey Frau Wittib Schorndorf',
    place = 'Buchhand*.|Buchdruckere(y|i)|Buchladen|Druckere(i|y)'
  )
  dict$neg <- list(
    bible = 'Buch Mose',
    work = 'Platz als',
    lottery = '(Lo(|o)(s|\u00df)|Lotterie)',
    other = 'Muster(b|-B)(u|\u00fc)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin|
    Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Tabacks-Buchs|Tabacksbuchs|
    Anfangsbuchstabe(n)|Buchstabe(n)|Buchf\u00fchrung|Buchhaltung|Sand(b|-B)uchse(|n)|
    Handels(b|-B)\u00fccher(|n)|Buchsbaum|Buchenholz|Pergamenter|Foulard|N\u00e4harbeit|
    Buchf\u00fchrer(|n)|Buchhalter|englisch(|e|en) gedruckt(|e|en)|franz\u00f6sisch(|e|en)
    gedruckt(|e|en)|Behausung|Buchbinder-Werckz'
    )
  dict$include <- list(
    'cf9c106c-ec5f-5e14-a74e-1f6228fa248b/t34', 'db99a10c-a6b0-5689-9ef0-74336dbd2da3/t30', # book titles that contain 'Lotterie' etc. See complete list at bottom
    'c8ef15ad-a52d-5396-b8e2-30c10e3557b0/t20', '73827605-442a-5c56-b23b-a9f13a885e9f/t6',
    'cfd1358e-ef18-5bc9-90f5-d204528f668c/t2', 'fae377f7-7756-5317-acea-cc0c95792b0c/t10',
    '1e1cd3eb-eb5b-501a-bb75-a8eee1a12cb2/a1', '29c7d8ab-3e6c-5070-814b-af8d99aedaa2/a10',
    '011550b7-0e34-5b42-9245-ff3a6a4c0df4/t18', '8cf75f5d-ba0b-5e68-a280-59eeb5e2a49b/t6',
    '93dccc0b-3541-54df-8776-c0c852afbede/t3', 'e3510f92-2f6d-5192-9815-5850ee07e631/t8',
    '578b8710-d104-5409-ae4d-b11d04a4ba65/t7', 'cdb7d1fd-5b56-5834-b849-b3fb17cc08bd/t8',
    '8ca55bbf-4adc-5cb4-bed7-034d3ef312bf/t12', 'd1424e64-d176-531d-90fe-7d247e471cc9/t14',
    '264925f3-f7e1-530b-aed2-92a75c213b97/t6', 'dbe46241-0df8-5092-b6d9-19f8f78096a4/t9'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Print outside bookstore section, non-commercial sellers
#' @rdname tagfilter_print
#' @export
tagfilter_print3 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'ps', 'offering')
  dict$pos <- list(
    book = 'Buch\\\\b|B\u00fccher(n|)|Bucher',
    edition = 'Auflage|Ausgabe|Prachtausgabe|Bdchen',
    material = '\\\\bPergament',
    language = 'teutsch und latein(|isch)',
    format_1 = '\\\\bin Fol(\\\\.|io)|\\\\binFol|\\\\bin Median-Folio|\\\\sfol\\\\.|gro\u00dfOctav',
    format_2 = '4to|4t\u00f2|8v\u00f2|8vo|8v0|Quarto|Quart-B(a|\u00e4)nd',
    format_3 = '4\u00b0|8\u00b0|Tom\\\\.\\\\b|tom\\\\.\\\\b|Tomis|Tomes|Tome|Tomi|(O|o)ctavo|\\\\bBogen|^(1-9) B\u00f6gen|Halbfranzband|^(ein|un)gebunden.',
    format_4 = 'in (1-9) B(\u00e4n|)den|Fran(|t)z\u00f6sische(|n|m) B(\u00e4|a)nd(|en|n)',
    format_5 = '^gedruckte(n) Fortsetzung|vermehrt(e|) Edition',
    format_6 = 'Band gebunden|(Pergament|Leder|Carton) gebunden|ill.* und gebunden|sauber gebunden|Schweinsleder gebunden',
    #format_7 = '\\\\bbrosch(.|iert)',
    feautures = '^Kupf(f)er(|n)|Holzschnitt|Stahlstich',
    #catalog = 'Catalogus|Katalog',
    types_1 = 'W\u00f6rterbuch|Dictionarium|Lexikon|Lexicon',
    types_2 = 'Bibel|Biblen|Biblia',
    prayerbooks = 'Psalmb(u|\u00fc)ch(|lein|s|e)|Psalm-B(u|\u00fc)ch(|lein|s|e)|(g|G)esangb(u|\u00fc)ch(|lein|s|e)'
  )
  dict$neg <- list(
    region = 'Entlibuch|Sch\u00f6nenbuch',
    bible = 'Buch Mose',
    work = 'Platz als',
    auction = 'vergant(|h)e(n|t)',
    subscription = 'Pr(\u00e4|ae)numeration',
    person = 'Buchh\u00e4ndl|Buch-H\u00e4ndl|Buchdruck|Buchb(e|i)nd|Buch bind|Buchh\u00e4nd ler',
    names_1 = 'J(\\\\.|oh\\\\.|ohan|ohann) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Flick|Schweighauser.',
    names_2 = '(B|b)e(y|i)(|m) (B\\\\.|Hrn\\\\.|Herrn|) (Peter Scherb|Heinrich Haag|Eucharius Haag|Wilhelm Haas|
    J(\\\\.|oh\\\\.) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Stupanus|Peter Scherb|J(\\\\.|oh\\\\.) J(\\\\.|ak\\\\.|a(c|k)ob) Freyler|
    Scholer(,|) auf der Rheinbruck|Daniel Haag|Joh\\\\. Rudolf Pistorius|C\\\\. A\\\\. Serini|
    Ja(c|k)ob Bernoulli im Engelh(o|oo)f|J\\\\. Decker am Spithalsprung|(|B\\\\.) Bolli|den B\u00fcrgern Bolli|
    B\u00fcrger Haas, Sohn auf dem Leonhardsgraben|Emanuel Thurneysen|Johann Rudolf(|f) Im H(off|oof)|
    J(\\\\.|oh\\\\.) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Schorndorff|J\\\\. Decker und Wilhelm Haas)|
    Bey Frau Wittib Schorndorf',
    place_1 = 'Buchhand.|Buchdruckere(y|i)|Buchladen|Druckere(i|y)',
    place_2 = 'Weiber(-Si|si)|Mannen(-Si|si)|(Weiber|Mannen)-Anhen(|c)ker', #to exclude 'Bogen' as specification for the location of a church seat
    other_1 = 'Muster(b|-B)(u|\u00fc)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin',
    other_2 = 'Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Mousselin',
    other_3 = 'Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe(n)|Buchstabe(n)',
    other_4 = 'Buchf\u00fchrung|Buchhaltung|Sand(b|-B)uchse(|n)|Handels(b|-B)\u00fccher(|n)',
    other_5 = 'Buchsbaum|Buchenholz|Pergamenter|Foulard|N\u00e4harbeit|Buchf\u00fchrer(|n)',
    other_6 = 'Buchhalter|buchen(|e|es|er|en|em)|reiner Buchs|Musicpapier|B\u00fcchsen|Pfundbuchsen',
    other_7 = 'englisch(|e|en) gedruckt(|e|en)|franz\u00f6sisch(|e|en) gedruckt(|e|en)',
    other_8 = 'buchen Holz|Buchenholz|gener Buchs|Buchsen|Buchs-|Gartenbuchs',
    other_9 = 'mit Beschl\u00e4g*. zu einem Bogen|Kanzel-Bogen|steinerner Bogen|samt Bogen|
    Armbrust-Bogen|mit einem Bogen|Schwi bogen|Bogen-Liecht|Fenster(|-) Bogen|Violin-Bogen|
    Pfeil und Bogen|aller Auflagen *. frey|generalAuflagen|Al(|c)kofen'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Print outside bookstore section, non-commercial buyers
#' @rdname tagfilter_print
#' @export
tagfilter_print4 <- function(){
  dict <- list()
  dict$applicable <- list('saledemand', 'ps', 'demanding', 'othernews')
  dict$pos <- list(
    book = 'Buch\\\\b|B\u00fccher(n|)|Bucher',
    edition = 'Auflage|Ausgabe|Prachtausgabe|Bdchen',
    material = '\\\\bPergament',
    language = 'teutsch und latein(|isch)',
    format_1 = '\\\\bin Fol(\\\\.|io)|\\\\binFol|\\\\bin Median-Folio|\\\\sfol\\\\.|gro\u00dfOctav',
    format_2 = '4to|4t\u00f2|8v\u00f2|8vo|8v0|Quarto|Quart-B(a|\u00e4)nd',
    format_3 = '4\u00b0|8\u00b0|Tom\\\\.\\\\b|tom\\\\.\\\\b|Tomis|Tomes|Tome|Tomi|(O|o)ctavo|\\\\bBogen|^(1-9) B\u00f6gen|Halbfranzband|^(ein|un)gebunden.',
    format_4 = 'in (1-9) B(\u00e4n|)den|Fran(|t)z\u00f6sische(|n|m) B(\u00e4|a)nd(|en|n)',
    format_5 = '^gedruckte(n) Fortsetzung|vermehrt(e|) Edition',
    format_6 = 'Band gebunden|(Pergament|Leder|Carton) gebunden|ill.* und gebunden|sauber gebunden|Schweinsleder gebunden',
    #format_7 = '\\\\bbrosch(.|iert)',
    feautures = '^Kupf(f)er(|n)|Holzschnitt|Stahlstich',
    #catalog = 'Catalogus|Katalog',
    types_1 = 'W\u00f6rterbuch|Dictionarium|Lexikon|Lexicon',
    types_2 = 'Hand-Bibel|Bibel|Biblen|Biblia',
    prayerbooks = 'Psalmb(u|\u00fc)ch(|lein|s|e)|Psalm-B(u|\u00fc)ch(|lein|s|e)|(g|G)esangb(u|\u00fc)ch(|lein|s|e)'
  )
  dict$neg <- list(
    region = 'Entlibuch|Sch\u00f6nenbuch',
    work = 'Platz als',
    auction = 'vergant(|h)e(n|t)',
    subscription = 'Pr(\u00e4|ae)numeration',
    person = 'Buchh\u00e4ndl|Buch-H\u00e4ndl|Buchdruck|Buchb(e|i)nd|Buch bind|Buchh\u00e4nd ler',
    names_1 = 'J(\\\\.|oh\\\\.|ohan|ohann) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Flick|Schweighauser.',
    names_2 = '(B|b)e(y|i)(|m) (B\\\\.|Hrn\\\\.|Herrn|) (Peter Scherb|Heinrich Haag|Eucharius Haag|Wilhelm Haas|
    J(\\\\.|oh\\\\.) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Stupanus|Peter Scherb|J(\\\\.|oh\\\\.) J(\\\\.|ak\\\\.|a(c|k)ob) Freyler|
    Scholer(,|) auf der Rheinbruck|Daniel Haag|Joh\\\\. Rudolf Pistorius|C\\\\. A\\\\. Serini|
    Ja(c|k)ob Bernoulli im Engelh(o|oo)f|J\\\\. Decker am Spithalsprung|(|B\\\\.) Bolli|den B\u00fcrgern Bolli|
    B\u00fcrger Haas, Sohn auf dem Leonhardsgraben|Emanuel Thurneysen|Johann Rudolf(|f) Im H(off|oof)|
    J(\\\\.|oh\\\\.) J(\\\\.|a(c|k)\\\\.|a(c|k)ob) Schorndorff|J\\\\. Decker und Wilhelm Haas)|
    Bey Frau Wittib Schorndorf',
    place_1 = 'Buchhand.|Buchdruckere(y|i)|Buchladen|Druckere(i|y)',
    place_2 = 'Weiber(-Si|si)|Mannen(-Si|si)|(Weiber|Mannen)-Anhen(|c)ker', #to exclude 'Bogen' as specification for the location of a church seat
    other_1 = 'Muster(b|-B)(u|\u00fc)ch(|er|ern)|Haushaltungsbuch|(Z|z)uber|Fischbeckin',
    other_2 = 'Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Mousselin',
    other_3 = 'Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe(n)|Buchstabe(n)',
    other_4 = 'Buchf\u00fchrung|Buchhaltung|Sand(b|-B)uchse(|n)|Handels(b|-B)\u00fccher(|n)',
    other_5 = 'Buchsbaum|Buchenholz|Pergamenter|Foulard|N\u00e4harbeit|Buchf\u00fchrer(|n)',
    other_6 = 'Buchhalter|buchen(|e|es|er|en|em)|reiner Buchs|Musicpapier|B\u00fcchsen|Pfundbuchsen',
    other_7 = 'englisch(|e|en) gedruckt(|e|en)|franz\u00f6sisch(|e|en) gedruckt(|e|en)',
    other_8 = 'buchen Holz|Buchenholz|gener Buchs|Buchsen|Buchs-|Gartenbuchs',
    other_9 = 'mit Beschl\u00e4g*. zu einem Bogen|Kanzel-Bogen|steinerner Bogen|samt Bogen|
    Armbrust-Bogen|mit einem Bogen|Schwi bogen|Bogen-Liecht|Fenster(|-) Bogen|Violin-Bogen|
    Pfeil und Bogen|aller Auflagen *. frey|generalAuflagen|Al(|c)kofen'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: shared newspaper subscriptions
#' @rdname tagfilter_print
#' @export
tagfilter_print5 <- function(){
  dict <- list()
  dict$applicable <- list('saledemand', 'saleoffer', 'offering', 'ps', 'demanding', 'exchange')
  dict$pos <- list(
    participant = 'Mithalte.*|(a|A)bonnent.*',
    types = 'Zeitung|Zeitschrift',
    title_1 = 'Rauracher|Rau-racher|Raura-cher',
    title_2 = 'Allgemeine(n) Zeitung',
    title_3 = 'Christliche(r|n) Volksbote.*',
    title_5 = 'Annalen',
    title_6 = 'Missions-Magazin',
    title_7 = 'Basler-Zeitung|Basler Zeitung',
    title_8 = 'Wochenblatt'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: lost books
#' @rdname tagfilter_print
#' @export
tagfilter_print6 <- function(){
  dict <- list()
  dict$applicable <- list('lostandfoundheader')
  dict$pos <- list(
    book = '\\\\bBuch|\\\\bB\u00fcchlein|\\\\bB\u00fccher(|n)',
    prayerbooks = 'Psalmb(u|\u00fc)ch(|lein|s|e)|Psalm-B(u|\u00fc)ch(|lein|s|e)|(g|G)esangb(u|\u00fc)ch(|lein|s|e)',
    material = '\\\\bPergament|mit Kup(f|ff)er(|n)'
  )
  dict$neg <- list(
    to_buy_ps = 'zu kauf(f)en|zu haben',
    job = 'Buchhalter',
    other_objects = 'Muster(b|-B)(u|\u00fc)ch(er|ern)|Tabacks-Buchs|Tabacksbuchs|Sand(b|-B)uchse(|n)|Handels(b|-B)\u00fccher(|n)|Buchs Samuelis',
    other = 'Lotterie(|n)|Buchstabe(|n)|Buchsbaum|Buchenholz|(b|B)uchene|Buchsener|Pergamenter'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: books/journals in libraries
#' @export
tagfilter_print7 <- function(){
  dict <- list()
  dict$applicable <- list('lostandfoundheader', 'ps', 'lendoffer', 'lend', 'demanding', 'othernews')
  dict$pos <- list(
    institutions = 'Leih(b|-B)ibl.|Leseanstalt'
    #book = '\\\\bBuch|\\\\bB\u00fcchlein|\\\\bB\u00fccher(|n)',
    #material = '\\\\bPergament|mit Kup(f|ff)er(|n)'

  )
  dict$neg <- list(
    prayerbooks = 'Psalm(en)(b|-B)(u|\u00fc)ch(|lein|s|e)|(g|G)esangb(u|\u00fc)ch(|lein|s|e)',
    participation = 'Subscription(|es)|Pr(ae|\u00e4)numerant(|en)|Subs(c|k)ribent(|en)|Mithalter(|n)',
    job = 'Buchhaltung',
    selling = 'Prei(s|\u00df)|zu (ver|)kauf(f)en|vergant(|h)e(t|n)|Francken|Buchbinder(|ei)|Buch(-H|h)andlung|Buch(-H|h)\u00e4ndler|Buch(-D|d)rucker(ei)',
    other_objects = 'Muster(b|-B)(u|\u00fc)ch(|er|ern)|Tabacks-Buchs|Tabacksbuchs|Sand(b|-B)uchse(|n)|Handels(b|-B)\u00fccher(|n)|Buchs Samuelis',
    other = 'mithalten|Lotterie(|n)|Buchstabe(|n)|Buchsbaum|Buchenholz|(b|B)uchene|Buchsener|Pergamenter|Buchf\u00fchrer(|n)|Buchhalter'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: subscription
#' @rdname tagfilter_print
#' @export
tagfilter_print8 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'ps', 'demanding', 'offering', 'othernews')
  dict$pos <- list(
    participant = 'S(o|)u(b|p|)s(c|k)ription|Pr(ae|\u00e4)num(m|)erant|Pr(ae|\u00e4)num(m|)eration|
    S(o|)u(b|p|)s(c|k)ribent',
    participate = 's(o|)u(b|p|)s(c|k)ribi(e|)r(t|)|pr\u00e4numeri(|e)r(t|)'
  )
  dict$neg <- list(
    lottery = 'Lotterie',
    mine = ' Berg(-W|w)erck'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: co-subscription
#' @rdname tagfilter_print
#' @export
tagfilter_print9 <- function(){
  dict <- list()
  dict$applicable <- list('saledemand', 'saleoffer', 'offering', 'ps', 'demanding', 'exchange')
  dict$pos <- list(
    participant = 'Mi(t|tt)halte.*|mi(t|tt)halten|Gemeinder.{0,5}|Liebhaber.*(J|j)ournal',
    with = 'mit einem oder mehr Freunden in Compagnie|Zeitung.*mittheilen|
    (mit andern|gemeinschaftlich)(| zu) halten|mitzuhalten|\\\\bmit ander(n|en).*zu halten|
    zum behalten.*zu haben|Theil an dem.*zu haben',
    money = 'Geb\u00fchr zum Durchlesen'
  )
  dict$neg <- list(
    publication = 'AVERTISSEMENT|Avertissement',
    milk = 'Eselsmilch|Schotte',
    lotterie = 'Lo(|o)(s|\u00df)|Lotterie',
    council = 'Gemeinderat.*'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: book lottery
#' @rdname tagfilter_print
#' @export
tagfilter_print10 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'offering', 'ps', 'exchange', 'othernews')
  dict$pos <- list(
    lotterie = '(Buch\\\\b|B\u00fccher(n|)|Bucher|Bibel)(|\\\\s|-)(|\\\\s|-)(Lo(|o)(s|\u00df)|Lotterie)',
    several_lotteries = '(Buch\\b|B\u00fccher(n|)|Bucher)(-| |)(?= und .*-Lotterie)'
  )
  dict$neg <- list(
    publication = 'AVERTISSEMENT|Avertissement',
    book_titles = 'Lotteriesucht|gl\u00fcckliche(r|) Lottospieler|Fromme(n|) Lotterie|Lotterie der
                  (Frommen|Lieben)| gl\u00fcckliche Lotterielo(o|)(\u00df|s)|(Vogel|Bilder)-Lotterie|
                  Rechtm\u00e4\u00dfigkeit der Lotterien| Lottokenntni\u00df|kosmographische Lotterie|
                  Gl\u00fcck in der Lotterie gemacht|Lotterie f\u00fcr die Herren|Traum(-B|b)\u00fcchlein|
                  Die Sch\u00e4dlichkeit der Zahlen-Lotterie|(C|K)abbalistische Tabellen|
                  Lottologie|Gedanken vom gro\u00dfen Lo(o|)se in der Lotterie|Geheimni\u00df der
                  Itali(\u00e4|e)nischen Zahlen-Lotterien|Der Lotteriespieler|Anmerkungen \u00fcber
                  die Zahlen-Lotterien|Lotterie der Frommen|Auslegung aller Tr\u00e4ume|
                  Privat(-G|g)edan(c|)ken|Wasserfart(h|)-Lotterie|Rechtm\u00e4ssigkeit der
                  Lotterien|Die Losende Welt'
  )
  create_filter_output(dict)
}

### book titles containing 'lotterie' etc.:
### 'Lotteriesucht|(G|g)l\u00fcckliche(r|) Lottospieler|(F|f)romme(n|) Lotterie|Lotterie der (Frommen|Lieben)|
### (G|g)l\u00fcckliche Lotterielo(o|)(\u00df|s)|(Vogel|Bilder)-Lotterie|Rechtm\u00e4\u00dfigkeit der Lotterien|Lottokenntni\u00df|
### kosmographische Lotterie|Gl\u00fcck in der Lotterie gemacht|Lotterie f\u00fcr die Herren|jeder (L|l)otto(-|\\\\s|)spielende(r|)
### Liebhaber|jeder Lotteriefreund|Die Sch\u00e4dlichkeit der Zahlen-Lotterie|Lotteriespiel n\u00fctzlich zu gebrauchen|Lottologie|
### Gedanken vom gro\u00dfen Lo(o|)se in der Lotterie|Geheimni\u00df der (I|i)tali(\u00e4|e)nischen Zahlen(-|)(L|l)otterien|
### Der Lotteriespieler|Anmerkungen \u00fcber die Zahlen(-|)(L|l)otterien|Lotterie der Frommen|
### Lotterien anwendbare Auslegung aller Tr\u00e4ume|Einer Reichs(-|)(L|l)otterie (E|e)in (P|p)ro|
### Wasserfa(rth|hrt)(-|)(L|l)otterie|Rechtm\u00e4ssigkeit der Lotterien|Die Losende Welt|(G|g)eistliche(n|) Lotterie'
