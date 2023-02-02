#' Tagfilter Animals
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads concerning animals, pets (mostly dogs, 
#' cats, and birds) as well as livestock, and related objects. 
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_animal
#' @seealso tagfilters
NULL


#' Tagfilter for dogs
#' @rdname tagfilter_animal
#' @export
tagfilter_dogs <- function(){
  dict <- list()
  dict$pos <- list(
    general = "(H|h)(u|ü)nde?(n|s)?\\b", # added "e?(n|s)?\\b", otherwise finds Hundert [AE]
    Federhund = "(F|f)edern(-|\\s)(H|h)(u|ü)nd",
    Haushund = "(H|h)aus(-|\\s)(H|h)(u|ü)nd",
    Kettenhund = "(K|k)etten(-|\\s)(H|h)(u|ü)nd",
    EpagneulHund = "(E|e)pagneul(-|\\s)(H|h)(u|ü)nd",
    Huhnerhund = "(H|h)(u|ü)(h|)ner(-|\\s)(H|h)(u|ü)nd",
    Pudelhund = "(P|p)udel(-|\\s)(H|h)(u|ü)nd|Pudel",
    Budelhund = "(B|b)udel(-|\\s)(H|h|B|b)(u|ü)nd",
    Dockenhund = "(D|d)ocken(-|\\s)(H|h)(u|ü)nd|(D|d)ec,n(-|\\s)(H|h)(u|ü)nd",
    Wachtelhund = "(W|w)achtel(-|\\s)(H|h)(u|ü)nd",
    Doque = "(D|d)oquen|(D|d)o(g|q)u(e|en)(-|\\s)(H|h)(u|ü)nd",
    Arlequin = "(A|a)rle(q|g)uin(-|\\s)(H|h)(u|ü)nd",
    Stauphund = "(S|s)t(ä|a)up(-|\\s)(H|h)(u|ü)nd",
    Mops = "(M|m)o(p|b)s|(M|m)o(p|b)s(-|\\s)(H|h)(u|ü)nd",
    Danischhund = "(D|d)(ä|a)nisch(-|\\s)(H|h)(u|ü)nd",
    Bummerhund = "(B|b)ummer(-|\\s)(H|h)(u|ü)nd",
    Vorsteherhund = "(V|v)orsteher(-|\\s)(H|h)(u|ü)nd",
    Fotzelhund = "(F|f)otzel(-|\\s)(H|h)(u|ü)nd",
    Spitzpommer = "(S|s)pitz(p|b)ommer(-|\\s)(H|h)(u|ü)nd|(P|p|b|B)ommer|(P|p|B|b)ommer(-|\\s)(H|h)(u|ü)nd"
  )

  dict$neg <- list(
    raw_material = "(S|s)eehund",
    animal = "Chaise-Pferd|Reisepferd|Chaisepferd",
    knife = "(S|s)ackmesser"
  )
  create_filter_output(dict)
}

#' Tagfilter for birds
#' @rdname tagfilter_animal
#' @export
tagfilter_birds<- function(){
  dict <- list()
  dict$pos <- list(
    general= "(V|v)(o|ö)g(el|in|elein)|(V|v)(o|ö)g(el|in|elein)(\\s|-)((W|w)eib(chen|lein)|(M|m)(a|ä)(n|nn)(chen|lein))|(V|v)(o|ö)g(el|in|elein)((W|w)eib(chen|lein)|(M|m)(a|ä)(n|nn)(chen|lein))",
    canary="(C|K)an(g|a)r(i|l|ie|ien|ios)|(C|K)an(g|a)r(i|l|ie|ien|ios)(V|v)(o|ö)g(el|in|elein)|(C|K)an(g|a)r(i|l|ie|ien|ios)(\\s|-)((W|w)eib(chen|lein)|(M|m)(a|ä)(n|nn)(chen|lein))",
    mockingbird="Drossel|Drostel",
    dove="(T|t|d|D)(a|ä)ub(e|en|in|li|lei)",
    finch="Distel|Frackrock",
    parrot="Papage(i|y)",
    robin="(R|r)o(th|t)kehlchen|(Z|z)ei(s|ss)(ig|gen|chen)",
    magpie="(E|e)lster|(A|a)tzel|Hetzel",
    blackbird="(A|a)msel",
    nightingale="Nachtigall",
    ocr="Eanarie",
    flew="verflogen"
  )

  dict$neg <- list(
    name = "Freyvogel ", #exclude family name
    animal = "(S|s)t(a|ä)uber" #exclude "Stäuberhund"

  )
  create_filter_output(dict)
}


#' Tagfilter for cats
#' @rdname tagfilter_animal
#' @export
tagfilter_cats<- function(){
  dict <- list()
  dict$pos <- list(
    general= "(K|k|R)(a|ä)tz(e|chen)|Kaz",
    malecat="(K|k)ater"
  )

  dict$neg <- list(
    household = "Matratze|Matratzen", #exclude mattress in Ratze ("R" because of OCR error)
    scabies = "Krätze" #exclude name of disease

  )
  create_filter_output(dict)
}


#' Tagfilter for other pets
#' @rdname tagfilter_animal
#' @export
tagfilter_otherpets<- function(){
  dict <- list()
  dict$pos <- list(
    squirrel= "Eichh(o|ö)rn",
    fox="\\bFuchs\\b"

  )

  dict$neg <- list(
    dog = "(D|d)achsh(u|ü)nd|(D|d)achs(farbig|artig|füssen)|(F|f)uchsfarbner"
  )
  create_filter_output(dict)
}


#' Tagfilter for livestock
#' @rdname tagfilter_animal
#' @export
tagfilter_livestock<- function(){
  dict <- list()
  dict$pos <- list(
    goose= "\\sG(a|ä)ns",
    goat="\\sG(e|a)i(s|ss|se|sse|ß|ße)",
    sheep="\\sSch(aa|a)(f|ff)",
    horse="\\sPferd(tlein|ein)|\\sPferd|\\sStutte|\\sRoss|\\sM(u|ü)nken",
    chicken="\\s(H|h)(u|ü)(hn|n)|\\sHahn|\\sHaan|\\sHenne",
    rabbit="\\sHase\\s|Ka(nn|n)in(ch|g)en",
    pig="\\sSchwein|\\sSau|\\sMohr",
    duck="\\sEn(t|dt)e",
    donkey="\\sEsel",
    bee="\\sBienen",
    cow="\\sKu(h|he)"
  )

  dict$neg <- list(
    shaft = "Schaft",
    name="Saumi|Sauter|Hühnerwadel|Kuhm",
    dog="(H|h)(u|ü)nd",
    tool="Schaaf-Schär|(S|s)aum|Pferd-Zaum",
    food="Schwein-Schm(a|al)tz|Kuhfleisch",
    clock="Sackuhr", #includes Kuh
    location="Schweinmarkt|Bubenteüch|H(ü|u)nin(ge|gue)|Hüni|Sch(aa|a)fg(ä|a)ß|Schaaf-Gäßlein|hinter\\sdie\\sGäns",
    location2="Sch(aa|a)(ff|f)haus|Eselth(u|ü)r(m|n)|zur\\sGäns",
    horse_blanket="Pfer(d|de)decke|Pferdohrenquatsche|Pferdgeschirr|Pferd(M|m)aschen|Pferdkobel",
    horse_transport="ab\\seinem\\sPferd|aufs\\sPferd",
    profession="Pferdt(K|k)necht|Pferde-Arzt|Schaffne(y|r)|Schwein(en|e)-Metzger|Saugamme|Roß-Verk(a|ä)ufer",
    description="sauber|schaffen|sau(er|r)|rossenfarb|Rossmi|Rossolis|gansaugen", #includes "Sau"
    duck1="Mente|obbedente|ententweder|etenter|sauvage|Saussures",
    duck2="differente|vente|presentes|Sentenzen",
    duck3="trente|Dentelles|Schiflente",
    duck4="Passamente|Taffente|barchente",
    duck5="scribente|essente",
    duck5="(ver|be|Be|\\s)diente",
    textile="Kaninchen(H|h)aar",
    theft="ententwendet"

  )
  create_filter_output(dict)
}


#' Tagfilter for objects related to animals
#' @rdname tagfilter_animal
#' @export
tagfilter_animalobjects<- function(){
  dict <- list()
  dict$pos <- list(
    dog_collar= "Halsb(a|ä)nd|Lederb(ä|a)nd|(H|h)un(ds|)-Cuple",
    birdcage="Paarh(a|ä)us|Paar-h(a|ä)us",
    birdcage2="K(ä|e)(f|s)i(g|ch)",
    birdcage3="((T|D|t|d)auben-|(T|D|t|d)auben|Vogel-|Vogel)(S|s)chlag",
    dogs="M(a|ä)ulkorb"
  )

  dict$neg <- list(
    name = "Freyvogel " #exclude family name

  )
  create_filter_output(dict)
}
