#' Dictionary Clothing and Garments (General)
#' @export
tagfilter_clothing <- function(){
  dict <- list()
  dict$pos <- list(
    general = "[K|k]leid|[K|k]leyd|[R|r]ock|[R|r]öck|[Ä|ä]rmel|Weste",
    apron = "[T|t]scho[b|p|pp]en|Fürtuch",
    trousers = "[H|h]ose",
    dress = "[J|j]unte|[J|j]undte|Juppe",
    shirt = "[H|h]emd|[H|h]embd|[C|c]hemise|[K|k]amisol|[K|k]amisohl|[C|c]amisol|[C|c]amisohl"
  )
  dict$neg <- list(
    straw = "Rockstroh", # special kind of straw
    carneval = "Milchjoggi|Fastnachtkleid|Fastnachtskleid|Polichinel-Kleid|Polichinelkleid|Maskenkleid", # costumes for carneval, see dictionary "costume"
    other = "Brockel", # describes appearance of different objects (small chunks)
    occupation = "Kleiderputz", # cleaning of clothes
    name = "Bolingbrocke", # family name
    military = "Offiziers-Rock|Uniform-Rock|Exerzier-Weste", # see dictionary "uniform"
    shroud = "Todtenröck|Todtenrock", # see dictionary "costume"
    underwear = "Leibchen|Unterärmel|Unterkleid|Unterrock|Unterröck", # see dictionary "underwear"
    sleapwear = "Schlafrock|Schlafröck|Nachtärmel", # see dictionary "sleapware"
    adjective = "gekleidet", # description of other object (mostly dolls)
    sister = "Schwester", # contains "weste"
    dry = "trocke|trockne", # adjectives or verbs meaning "dry" but containing "rock"
    furniture = "Kleiderkasten|Kleiderkäst|Kleiderschrank|Kleider-Kasten|Kleider-Kästen", # furniture for keeping clothing, see dictionary cabinet
    accessoire = "knopf|knöpf|träger" # non textile accessoires (Hemdknopf, Hosenträger etc.)

  )
  create_filter_output(dict)
}


#' Dictionary Sleepwear
#' @export
tagfilter_sleapwear <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Schlafrock|Schlafröck|Nachtärmel"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)
}



#' Dictionary Military Clothing/ Uniforms
#' @export
tagfilter_uniform <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Offiziers-Rock|Uniform-Rock|Offiziers-Roöck|Uniform-Röck|Exerzier-Weste|Uniformrock|Uniformröck",
    uniform = "Uniform",
    epaulets = "Epaulett"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Underwear
#' @export
tagfilter_underwear <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Lingerie|Leibchen|Unterärmel|Unterkleid|Unterrock|Unterröck",
    corset = "Korset|Corset",
    socks = "Socke|Strumpf|Strümpf"
  )
  dict$neg <- list(
    profession = "Strumpfweber|Strumpfausbreit|Strumpffach", # profession of making stockings, also excludes tools for profession (e.g. Strumpfweber-Stuhl)
    raw = "Strumpfwolle|Strümpfwolle|Strumpf-Wolle|Strümpf-Wolle" # yarn for making socks
  )
  create_filter_output(dict)
}


#' Dictionary Outerwear
#' NEU
#' @export
tagfilter_outerwear <- function(){
  dict <- list()
  dict$pos <- list(
    male = "Frack|Talar",
    general = "Mantel|Mäntel|Coat|Cotte|Schabrack|Mantille|Kittelein|Pellerine"
  )
  dict$neg <- list(
    fabric = "Mantelzeug", # fabric for coats
    mercery = "Mantelhaft", # clasps for coats
    music = "Instrumentalartikel" # musical instruments (contains "talar")
  )
  create_filter_output(dict)
}


#' Dictionary Garments for Special Occassions and Costumes
#' @export
tagfilter_costume <- function(){
  dict <- list()
  dict$pos <- list(
    carneval = "Milchjoggi|Fastnachtkleid|Fastnachtskleid|Polichinel-Kleid|Polichinelkleid|Maskenkleid",
    shroud = "Todtenröck|Todtenrock|Totenröck|Todtenrock",
    costume = "Bauerntrocht|-Tracht",
    baptism = "Taufzeug"
  )
  dict$neg <- list(
    looking = "[b|B]etracht" # verbs and nouns meaning "looking at" conatining "tracht"
  )
  create_filter_output(dict)
}


#' Dictionary Shoes
#' @export
# some "schuh" as measurement remain, e.g. "Länge 3 1/ 2 Schuh"
# excluding these through regex of "Länge, Breite etc." in proximity of 3 words to "Schuh"?
tagfilter_shoes <- function(){
  dict <- list()
  dict$pos <- list(
    slippers = "[P|p]antoffel",
    shoes = "[S|s]chuh|[S|s]chüh",
    boots = "[S|s]tiefel",
    soles = "[S|s]ohle|[S|s]öhle"
  )
  dict$neg <- list(
    name = "Guldenschuh", # family name
    accessoire = "[H|h]andschuh|[H|h]andschüh", # textile accessoires (gloves)
    work = "Schuhmacher|Schuster|Schuhster", # occupations concerned with making shoes
    # PROBLEM: sometimes filters out relevant ads - exclusion of work and immo ads probably better solution
    misc = "Radschuh|Schuhkraft|Schuhknech", # other objects and nouns containing "schuh"
    measure_1 = "Schuh\\s[lang|breit|dick|hoch|weit|Länge|hohe]", # removes "Schuh" as measurement, version 1
    measure_2 = "Schuh\\s\\d", # removes "Schuh" as measurement, version 2
    measure_3 = "Nürnberger Maß", # removes "Schuh" as measurement, version 3
    measure_4 = "franz. Maß", # removes "Schuh" as measurement, version 4
    measure_5 = "Schuhlänge|SchuhLänge|Schuh-Länge", # removes "Schuh" as measurement, version 5
    measure_6 = "Schuh Breite", # removes "Schuh" as measurement, version 6
    measure_7 = "Schuh Länge", # removes "Schuh" as measurement, version 7
    measure_8 = "Schuh Höhe" # removes "Schuh" as measurement, version 8
  )

  create_filter_output(dict)

}


#' Dictionary Handkerchiefs
#' @export
tagfilter_handkerchief <- function(){
  dict <- list()
  dict$pos <- list(
    handkerchief = "Schnupftuch|Mouchoir|Sacktuch|Sacktüch"

  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )

  create_filter_output(dict)

}


#' Dictionary Umbrellas and Related Objects (umbrella cases and umbrella cloth)
#' @export
tagfilter_umbrella <- function(){
  dict <- list()
  dict$pos <- list(
    rain = "Paraplui|Regenschirm",
    sun = "Sonnenschirm|Ombrelle|Parasol|Parresol"

  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )

  create_filter_output(dict)

}


#' Dictionary Gloves and Muffs
#' @export
tagfilter_hand <- function(){
  dict <- list()
  dict$pos <- list(
    muff = "Schlupfer|Schlüpfer",
    gloves = "Handschuh"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)
}



#' Dictionary Scarves, Colars, and Neckties
#' @export
tagfilter_neck <- function(){
  dict <- list()
  dict$pos <- list(
    colar = "Palatine|Kragen|Krägen|Cols",
    necktie = "Cravete|Cravate|Cravatte|Cravette",
    scarf = "Halstuch|Foulard|Schal|Schawl|Shaul|Chal|Fichu"
  )
  dict$neg <- list(
    sound = "Schalles|Schalle", # sound of something
    place = "Schaltenbrand|Mönchaltdorf", # placenames
    fabric = "Chalis|Chaly|Chally", # special kind of fabric; PROBLEM: sometimes description for fabric of a scarve...
    measure = "Waagschale", # measurement containing "schal
    bowls = "Schalen|Schale" # bowls
  )
  create_filter_output(dict)
}




#' Dictionary Headdresses and Wigs
#' @export
tagfilter_headdress <- function(){
  dict <- list()
  dict$pos <- list(
    wig = "Perruck|Perrück|Perück|Peruck",
    cap = "Kappe|Capotte|Bonnet|Mütze",
    general = "Hut|Hüt|Chapeau",
    straw = "Strohhut|Strohhüt",
    female = "Haube"
  )
  dict$neg <- list(
    mind = "zu//shüte", # minding something/someone (usally children)
    plant = "Zuckerhut", # name of a plant
    dump = "hütten", # verbs meaning dumping something
    beware = "verhüte", # verb meaning beware
    name = "Schaubhut|Hauber|Schaubelt", # family names
    religion = "Herrenhut", # religious group
    unclear = "Bonneterie", # exact meaning unsure, maybe place of making bonnets?
    profession = "Kappenmacher|Hutmacher|Strohhutnähen", # professions, PROBLEM: often in ad including actual hats but not always... - leave out?
    place = "Hutting|Schönhut|Waldshut|Hutgasse|Huthgasse|Schützen|Eisenhut|Schutzen", # placenames containing "hut/hüt"
    verb = "\\sthut", # verbs (doing)
    hut = "Hütte", # small house, hut
    object = "Fingerhut|Fingerhüt|Strumpfwoll|Strümpfwoll|Strumpf-Woll|Strümpf-Woll|Zündhütchen" # other objects including "hut" or "strumpf"
  )
  create_filter_output(dict)
}





#' Dictionary Certain Types of Fabric/ Textile Material
#' This Dictionary is ment to find textiles in ads where no other dictionary is useful,
#' but a lot of ads are of course already found through other dictionaries;
#' qualities of textiles will be explored in other dictionaries in more detail
#' @export
tagfilter_texmaterial <- function(){
  dict <- list()
  dict$pos <- list(
    unclear = "Nappes|Senpareille|Napolitain|Circassien|Stramin",
    fur = "Pelz|Marder|Zobel|Steinmarder",
    marcelline = "Marcelin",
    muslin = "Mousselin",
    lace = "Neiges|Spitzlein|Spitze",
    canvas = "Leinwand|Leinwat|Battist|Batist|Canevas",
    gingham = "Guingham|Gingham",
    semi_silk = "Halbseide",
    silk_origin_1 = "Gros de", # does not seem to work- why? is the space between the problem?
    silk_origin_2 = "Grosde",
    silk = "Marceline|Levantine|Seide|Blonde|Floreth|Floret|Taffent|Taffet",
    cashemere = "Casimir|Cachemir",
    bast = "Bast|Sparterie",
    cotton = "Linon|Baumwoll",
    linnen_1 = "leinenes Gewebe", # does not seem to work- why? is the space between the problem?
    linnen_2 = "Leinenzeug|Futter-Leinen|Halbleinen|leinenes|Steifleinen|leinener|Leinenwaaren|Leinenplunder|halbleinen|Leinenb[a|ä]nd",
    printed = "Indienne|Cattun|Persienne",
    tulle = "Tull|Thulle|Bobinet|Gaze",
    oilcloth = "Wachstuch|Wachstüch|Wachstaffet|Wachslappen",
    merino = "Mérino|Merino",
    flax = "Flachs",
    wool = "Wollenband|-Wolle|\\bWollenwaare|Thibet|Alépin"
  )
  dict$neg <- list(
    # includes some "Pfeifenspitzen", where "Pfeifen" is not right before "Spitzen" - how to exclude those?
    stone = "Alabast[e|a]r", # kind of stone
    bastard = "Bastard", # child out of wedlock
    name = "Sebastian|Sebast", # first name
    animal = "Seidenkaninchen", # certain kind of
    work = "unterzubringen|begehrt|gewesener", # excludes work ads and description of people's jobs
    paper = "Seidenpapier", # kind of paper
    profession = "Seidenzwirnmeister|Seidenwinder", # profession
    workplace = "Seiden-Zwirnerey", # workplace
    tool = "Seidenwind-Maschine|Seidenwindmaschine|Seidenrad|Seinderäder", # tool for winding silk thread
    place = "Geißspitz", # placename
    instrument = "Mundspitze", # part of musical instrument
    ohter = "Pelzfüsse|Pelzsäcke" # meaning unclear, but no textile
  )

  create_filter_output(dict)

}


#' Dictionary Unspecified Cloth and Fabric
#' @export
tagfilter_cloth <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Reste|Zeug|Tuch|Tüch|Geflecht|Etoffe|Gewebe"
  )
  dict$neg <- list(
    work = "Zeugni[s|ß]|tüchtig|Leumundszeug", # filters out work advertisements
    occupation = "Zeugwar", # Occupation "Zeugwart" containing "zeug"
    verb = "überzeug|restera", # verbs containing "zeug" or "rest"
    household = "Bettzeug|Tischzeug", # household textile
    noun = "bezeug", # nouns containing "zeug"
    tool = "Werkzeug|Werkszeug", # tool
    place = "Zeughaus", # name of a house in Basel
    paper = "Papierrest|Rei[ß|ss]zeug|Schreibzeug", # scrap paper, writing and painting tools
    date = "Jahrestermin" # refers to a date
  )

  create_filter_output(dict)

}



#' Dictionary Yarn
#' @export
tagfilter_yarn <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Garn|Faden|Cordon|Litze",
    embroidery = "Stickseide|Stickwolle|Stickbaum",
    knitting = "Strickseide|Strickwolle|Strickbaum"
  )
  dict$neg <- list(
    tool = "Fadenzähler", # tool for counting threads
    adjectives = "garnies|garniert|garnirt|garnie|\\sfadene\\s", # adjectives for decorated with
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
    feather = "Bettfehde|Bethfehde|Bettfede|Bettfehde|Flaum",
    horsehair = "Ro[ss|ß]haar|Pferdhaar"

  )
  dict$neg <- list(
    upholstery = "ausgepolstert", # removes upholstery containing horsehair
    household = "Roßhaarsieb", # strainer from horsehair
    garment = "Roßhaar-Cravat|roßhaarne\\s|roßhaarene\\s", # garments from horsehair
    plums = "Pflaumen\\s" # plums
  )
  create_filter_output(dict)

}

#' Dictionary Plant Raw Materials
#' @export
tagfilter_plantraw <- function(){
  dict <- list()
  dict$pos <- list(
    alpinegrass = "Waldhaar"

  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)

}



#' Dictionary Mercery and Non Textile Accessoires
#' @export
tagfilter_mercery <- function(){
  dict <- list()
  dict$pos <- list(
    buckle = "Schnalle",
    suspender = "Hosenträger",
    belt = "Gürtel|Ceintur",
    button = "Kn[o|ö]pf"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
    # "Knopfmacher" as a profession relevant for category, exclusion of work and immo ads should exclude those irrelevant to textile category
  )

  create_filter_output(dict)

}

#' Dictionary Bags and Purses
#' @export
tagfilter_bag <- function(){
  dict <- list()
  dict$pos <- list(
    bag = "Tasche|Seckel|Beutel|Säck|Ridicule",
    rucksack = "Felleisen"
  )
  dict$neg <- list(
    # negative list has to be extended
    fruit = "Früchte", # filters out ads with bags of fruit
    coffee = "Kaffee", # filters out ads with bags of coffee
    cloth = "Beuteltuch", # cloth for bags
    place = "Säckingen", # placename
    potato = "Erdäpfel", # filters out ads with bags of potatoes
    oat = "Habersäck", # bags of oats
    travel = "Fußsäck|Nachtsäck", # leather blanket for carriage passengers and sleeping bags
    books = "Taschenkalender|Taschenbuch|Taschenbüch", # pocket books and calendars
    pocketsize = "Taschen-Perspektiv|Taschenspiel|Taschenfeuer|Taschenuhr", # pocketsize versions of other objects
    administration = "Seckelmeister|Seckelrechnung", # administrative positions and words containing "Seckel"
    description = "Taschenformat" # description of pocketsize of an object
  )

  create_filter_output(dict)

}
