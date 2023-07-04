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
    general = "(H|h)(u|\\u00fc)nde?(n|s)?\\\\b",
    Federhund = "(F|f)edern(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Haushund = "(H|h)aus(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Kettenhund = "(K|k)etten(-|\\\\s)(H|h)(u|\\u00fc)nd",
    EpagneulHund = "(E|e)pagneul(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Huhnerhund = "(H|h)(u|\\u00fc)(h|)ner(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Pudelhund = "(P|p)udel(-|\\\\s)(H|h)(u|\\u00fc)nd|Pudel",
    Budelhund = "(B|b)udel(-|\\\\s)(H|h|B|b)(u|\\u00fc)nd",
    Dockenhund = "(D|d)ocken(-|\\\\s)(H|h)(u|\\u00fc)nd|(D|d)ec,n(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Wachtelhund = "(W|w)achtel(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Doque = "(D|d)oquen|(D|d)o(g|q)u(e|en)(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Arlequin = "(A|a)rle(q|g)uin(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Stauphund = "(S|s)t(\\u00e4|a)up(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Mops = "(M|m)o(p|b)s|(M|m)o(p|b)s(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Danischhund = "(D|d)(\\u00e4|a)nisch(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Bummerhund = "(B|b)ummer(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Vorsteherhund = "(V|v)orsteher(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Fotzelhund = "(F|f)otzel(-|\\\\s)(H|h)(u|\\u00fc)nd",
    Spitzpommer = "(S|s)pitz(p|b)ommer(-|\\\\s)(H|h)(u|\\u00fc)nd|(P|p|b|B)ommer|(P|p|B|b)ommer(-|\\\\s)(H|h)(u|\\u00fc)nd"
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
    general = "(V|v)(o|\\u00f6)g(el|in|elein)|(V|v)(o|\\u00f6)g(el|in|elein)(\\\\s|-)((W|w)eib(chen|lein)|(M|m)(a|\\u00e4)(n|nn)(chen|lein))|(V|v)(o|\\u00f6)g(el|in|elein)((W|w)eib(chen|lein)|(M|m)(a|\\u00e4)(n|nn)(chen|lein))",
    canary = "(C|K)an(g|a)r(i|l|ie|ien|ios)|(C|K)an(g|a)r(i|l|ie|ien|ios)(V|v)(o|\\u00f6)g(el|in|elein)|(C|K)an(g|a)r(i|l|ie|ien|ios)(\\\\s|-)((W|w)eib(chen|lein)|(M|m)(a|\\u00e4)(n|nn)(chen|lein))",
    mockingbird = "Drossel|Drostel",
    dove = "(T|t|d|D)(a|\\u00e4)ub(e|en|in|li|lei)",
    finch = "Distel|Frackrock",
    parrot = "Papage(i|y)",
    robin = "(R|r)o(th|t)kehlchen|(Z|z)ei(s|ss)(ig|gen|chen)",
    magpie = "(E|e)lster|(A|a)tzel|Hetzel",
    blackbird = "(A|a)msel",
    nightingale = "Nachtigall",
    ocr = "Eanarie",
    flew = "verflogen"
  )

  dict$neg <- list(name = "Freyvogel ",
                   animal = "(S|s)t(a|\\u00e4)uber")
  create_filter_output(dict)
}


#' Tagfilter for cats
#' @rdname tagfilter_animal
#' @export
tagfilter_cats<- function(){
  dict <- list()
  dict$pos <- list(general = "(K|k|R)(a|\\u00e4)tz(e|chen)|Kaz",
                   malecat = "(K|k)ater")

  dict$neg <- list(household = "Matratze|Matratzen",
                   scabies = "Kr\\u00e4tze")
  create_filter_output(dict)
}


#' Tagfilter for other pets
#' @rdname tagfilter_animal
#' @export
tagfilter_otherpets<- function(){
  dict <- list()
  dict$pos <- list(squirrel = "Eichh(o|\\u00f6)rn",
                   fox = "\\\\bFuchs\\\\b")

  list(
    dog = "(D|d)achsh(u|\\u00fc)nd|(D|d)achs(farbig|artig|f\\u00fcssen)|(F|f)uchsfarbner"
  )
  create_filter_output(dict)
}


#' Tagfilter for livestock
#' @rdname tagfilter_animal
#' @export
tagfilter_livestock<- function(){
  dict <- list()
  dict$pos <- list(
    goose = "\\\\sG(a|\\u00e4)ns",
    goat = "\\\\sG(e|a)i(s|ss|se|sse|\\u00df|\\u00dfe)",
    sheep = "\\\\sSch(aa|a)(f|ff)",
    horse = "\\\\sPferd(tlein|ein)|\\\\sPferd|\\\\sStutte|\\\\sRoss|\\\\sM(u|\\u00fc)nken",
    chicken = "\\\\s(H|h)(u|\\u00fc)(hn|n)|\\\\sHahn|\\\\sHaan|\\\\sHenne",
    rabbit = "\\\\sHase\\\\s|Ka(nn|n)in(ch|g)en",
    pig = "\\\\sSchwein|\\\\sSau|\\\\sMohr",
    duck = "\\\\sEn(t|dt)e",
    donkey = "\\\\sEsel",
    bee = "\\\\sBienen",
    cow = "\\\\sKu(h|he)"
  )

  dict$neg <- list(
    shaft = "Schaft",
    name = "Saumi|Sauter|H\\u00fchnerwadel|Kuhm",
    dog = "(H|h)(u|\\u00fc)nd",
    tool = "Schaaf-Sch\\u00e4r|(S|s)aum|Pferd-Zaum",
    food = "Schwein-Schm(a|al)tz|Kuhfleisch",
    clock = "Sackuhr",
    location = "Schweinmarkt|Bubente\\u00fcch|H(\\u00fc|u)nin(ge|gue)|H\\u00fcni|Sch(aa|a)fg(\\u00e4|a)\\u00df|Schaaf-G\\u00e4\\u00dflein|hinter\\\\sdie\\\\sG\\u00e4ns",
    location2 = "Sch(aa|a)(ff|f)haus|Eselth(u|\\u00fc)r(m|n)|zur\\\\sG\\u00e4ns",
    horse_blanket = "Pfer(d|de)decke|Pferdohrenquatsche|Pferdgeschirr|Pferd(M|m)aschen|Pferdkobel",
    horse_transport = "ab\\\\seinem\\\\sPferd|aufs\\\\sPferd",
    profession = "Pferdt(K|k)necht|Pferde-Arzt|Schaffne(y|r)|Schwein(en|e)-Metzger|Saugamme|Ro\\u00df-Verk(a|\\u00e4)ufer",
    description = "sauber|schaffen|sau(er|r)|rossenfarb|Rossmi|Rossolis|gansaugen",
    duck1 = "Mente|obbedente|ententweder|etenter|sauvage|Saussures",
    duck2 = "differente|vente|presentes|Sentenzen",
    duck3 = "trente|Dentelles|Schiflente",
    duck4 = "Passamente|Taffente|barchente",
    duck5 = "scribente|essente",
    duck5 = "(ver|be|Be|\\\\s)diente",
    textile = "Kaninchen(H|h)aar",
    theft = "ententwendet"
  )
  create_filter_output(dict)
}


#' Tagfilter for objects related to animals
#' @rdname tagfilter_animal
#' @export
tagfilter_animalobjects<- function(){
  dict <- list()
  dict$pos <- list(
    dog_collar = "Halsb(a|\\u00e4)nd|Lederb(\\u00e4|a)nd|(H|h)un(ds|)-Cuple",
    birdcage = "Paarh(a|\\u00e4)us|Paar-h(a|\\u00e4)us",
    birdcage2 = "K(\\u00e4|e)(f|s)i(g|ch)",
    birdcage3 = "((T|D|t|d)auben-|(T|D|t|d)auben|Vogel-|Vogel)(S|s)chlag",
    dogs = "M(a|\\u00e4)ulkorb"
  )

  dict$neg <- list(
    name = "Freyvogel " #exclude family name

  )
  create_filter_output(dict)
}
