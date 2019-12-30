#' Dictionary Clothing
#' @export
tagfilter_clothing <- function(){
  dict <- list()
  dict$pos <- list(
    general = "[K|k]le[i|y]d|[R|r]ock|[Ä|ä]rmel|[K|k]ragen",
    apron = "[T|t]scho[b|p|pp]en|Fürtuch",
    trousers = "[H|h]ose",
    underwear = "[S|s]ocke|[K|k|C|c]orset|[S|s]tr[u|ü]mpf",
    uniform = "[U|u]niform",
    costume = "Taufzeug|Tracht",
    dress = "[J|j]un[t|dt]e",
    shirt = "[H|h]emd|[H|h]embd|[C|c]hemise|[K|k|C|c]amis[o|oh]l",
    coat = "[m|M]antel|[C|c]oat|[C|c]otte|[C|c]ols|[F|f]rack|[S|s]abrack"
  )
  dict$neg <- list(
    dry = "trocke|trockne", # adjectives or verbs meaning "dry" but containing "rock"
    furniture = "Kleiderkasten", # furniture for keeping clothing
    accessoire = "knopf|knöpf|träger", # non textile accessoires (Hemdknopf, Hosenträger etc.)
    looking = "[b|B]etracht", # verbs and nouns meaning "looking at" conatining "tracht"
    profession = "Strumpfweber" # profession of making stockings, also excludes tools for profession (e.g. Strumpfweber-Stuhl)

  )
  create_filter_output(dict)
}

#' Dictionary Shoes
#' @export
tagfilter_shoes <- function(){
  dict <- list()
  dict$pos <- list(
    shoes = "[S|s]chuh|[S|s]chüh",
    boots = "[S|s]tiefel",
    soles = "[S|s]ohle|[S|s]öhle"
  )
  dict$neg <- list(
    accessoire = "Handschuh|Handschüh", # textile accessoires (gloves)
    work = "Schuhmacher|Schuster|Schuhster", # occupations concerned with making shoes
    misc = "Radschuh|Schuhkraft|Schuhknech", # other objects and nouns containing "schuh"
    measure_1 = "Schuh\\s[lang|breit|dick|hoch|weit|Länge|hohe]", # removes "Schuh" as measurement, version 1
    measure_2 = "Schuh\\s\\d", # removes "Schuh" as measurement, version 2
    measure_3 = "Nürnberger Maß", # removes "Schuh" as measurement, version 3
    measure_4 = "franz. Maß" # removes "Schuh" as measurement, version 4
  )

  create_filter_output(dict)

}


#' Dictionary Textile Accessoires ### PROBLEMATIC, has to be split up!
#' @export
tagfilter_texaccess <- function(){
  dict <- list()
  dict$pos <- list(
    wig = "[P|p]err[u|ü]cke",
    necktie = "[C|c]rav[e|a]t",
    scarf = "[H|h]alstuch|[F|f]oulard|[Sch|Ch]al",
    handkerchief = "[S|s]nupftuch",
    muff = "[S|s]chl[u|ü]pfer",
    umbrella = "[P|p]araplui|Regenschirm",
    gloves = "[H|h]andschuh",
    headdress = "[K|k]appe|[H|h]aube|Hut|Hüt|[C|c]hapeau",
    epaulets = "[E|e]paulett"
  )
  dict$neg <- list(
    factory = "[h|H]utfabri", # Factory for hats
    verb = "thut|erhalt|halt", # verbs (doing)
    place = "Hutting|Schönhut|Waldshut|Hutgasse", # placenames containing "hut"
    work = "Feuerschütz|Schütze|Hutmacher", # churchchairs are in another category
    object = "Fingerhut|Fingerhüt|Strumpfwoll|Strümpfwoll|Strumpf-Woll|Strümpf-Woll" # other objects including "hut" or "strumpf"
  )

  create_filter_output(dict)

}


#' Dictionary Cloth and Fabric
#' @export
# problematic category with dictionary, cause words describing fabrics are often
# given as part of the description of parts of clothing or other ready-made textiles
# examples are: Jupon, Spitzen, Wollenwaaren, Stoff, Taffet, Mousseline, Thuhlle,
# Linon, Gewebe, Bast, Nansu, Marceline, Fichus, Palatine
tagfilter_cloth <- function(){
  dict <- list()
  dict$pos <- list(
    general = "[H|h]austuch|[F|f]uttertuch|[R|r]este|[L|l]einwa[nd|th]|Zeug|[S|s]acktuch",
    wool = "[W|w]ollw[aa|ah]r|[W|w]oll-[W|w][aa|ah]r"
  )
  dict$neg <- list(
    work = "Zeugnis", # references for work advertisements
    occupation = "Zeugwar", # Occupation "Zeugwart" containing "zeug"
    verb = "überzeug", # verb containing "zeug"
    household = "Bettzeug|Tischzeug", # household textile
    noun = "bezeug", # nouns containing "zeug"
    tool = "[W|w]erkzeug", # tool
    place = "Zeughaus", # name of a house in Basel
    paper = "Papierrest", # scrap paper
    date = "Jahrestermin" # refers to a date
  )

  create_filter_output(dict)

}



#' Dictionary Yarn
#' @export
tagfilter_yarn <- function(){
  dict <- list()
  dict$pos <- list(
    general = "[F|f]lachs|[G|g]arn|[F|f]aden",
    embroidery = "[S|s]tickseide|[S|s]tickwolle|[S|s]tickbaum",
    knitting = "[S|s]trickseide|[S|s]trickwolle|[S|s]trickbaum"
  )
  dict$neg <- list(
    adjectives = "garnies|garniert|garnirt|\\sfadene\\s", # adjectives for decorated with
    decoration = "Garnitur|Garnirung", # decoration on objects
    deco_description = "Faden durchwirkt", # decorated with yarn, not yarn itself
    military = "Garnison" # military garrison
  )

  create_filter_output(dict)

}

#' Dictionary Animal Raw Materials
#' @export
# problematic category with dictionary, cause words like Wolle or Leder are often
# given as part of the description of clothing or other textiles
# therefore here only unambiguous words so far
tagfilter_animalraw <- function(){
  dict <- list()
  dict$pos <- list(
    mirror = "Bettfehde|Bethfehde|Bettfede|Bettfehde|Rosshaar|Roßhaar"

  )
  dict$neg <- list(
    # no negative list necessary so far
  )
  create_filter_output(dict)

}



#' Dictionary Non Textile Accessoires
#' @export
tagfilter_nontexaccess <- function(){
  dict <- list()
  dict$pos <- list(
    buckles = "[S|s]chnalle",
    suspenders = "Hosenträger",
    belt = "[G|g]ürtel|[C|c]eintur",
    buttons = "[K|k]n[o|ö]pf"
  )
  dict$neg <- list(
    profession = "[K|k]nopfmacher" # profession of button makers
  )

  create_filter_output(dict)

}

#' Dictionary Bags and Purses
#' @export
tagfilter_bag <- function(){
  dict <- list()
  dict$pos <- list(
    general = "[T|t]asche|[S|s]eckel|[B|b]eutel|[S|s]äcklein"
  )
  dict$neg <- list(
    # negative list has to be extended
    books = "Taschenkalender|Taschenbuch|Taschenbüch", # pocket books and calendars
    pocketsize = "Taschen-Perspektiv|Taschenspiel|Taschenfeuer|Taschenuhr", # pocketsize versions of other objects
    profession = "Seckelmeister", # professions containing "Seckel"
    description = "Taschenformat" # description of pocketsize of an object
  )

  create_filter_output(dict)

}
