#' Dictionary Clothing and Garments (General)
#' @export
tagfilter_clothing <- function(){
  dict <- list()
  dict$pos <- list(
    general = "[K|k]leid|[K|k]leyd|[R|r]ock|[R|r]öck|[Ä|ä]rmel|Weste",
    apron = "[T|t]scho[b|p|pp]en|Fürtuch",
    trousers = "[H|h]ose",
    dress = "[J|j]unte|[J|j]undte|Juppe|Jüppe",
    shirt = "[H|h]emd|[H|h]embd|[C|c]hemise|[K|k]amisol|[K|k]amisohl|[C|c]amisol|[C|c]amisohl"
  )
  dict$neg <- list(
    activity_1 = "Kleider machen", # activity with clothes, v1
    activity_2 = "Kleider w[a|ä]schen", # activity with clothes, v2
    activity_3 = "Kleider glätten", # activity with clothes, v3
    activity_4 = "Kleider nähen", # activity with clothes, v4
    activity_5 = "Kleider putzen", # activity with clothes, v5
    activity_6 = "Kleider zu machen", # activity with clothes, v6
    activity_7 = "Kleider zu w[a|ä]schen", # activity with clothes, v7
    activity_8 = "Kleider zu glätten", # activity with clothes, v8
    activity_9 = "Kleider zu nähen", # activity with clothes, v9
    activity_10 = "Kleider zu putzen", # activity with clothes, v10
    activity_11 = "zu stricken", # activity with clothes, v11
    activity_12 = "einen Platz", # activity with clothes, v12
    activity_13 = "zum Putzen", # activity with clothes, v13
    activity_14 = "zu vertilgen", # activity with clothes, v14
    animal = "Federnhund", # filters out animal related clothing
    book = "Fleckenbüchlein", # book with instructions for cleaning
    death = "gewesen", # filters out death notices
    cleaning = "Waschwasser|Flecken-Kugelen|Kleiderputz", # detergent for cleaning clothes
    lime = "Kalchosen", # limestone
    fragrance = "Rosenöl", # fragrance for laundry
    immo = "Bauchosen|Alickhosen|Gebäude", # immo ads
    place = "Oberhosen|Freyhosen|Waltighosen", # placename
    position = "einen Platz", # filters out labour ads for servants
    name = "Bachosen", # family name
    straw = "Rockstroh|Rockenstroh", # special kind of straw
    carneval = "Milchjoggi|Fastnachtkleid|Fastnachtskleid|Polichinel-Kleid|Polichinelkleid|Maskenkleid", # costumes for carneval, see dictionary "costume"
    other = "Brockel", # describes appearance of different objects (small chunks)
    work = "Lehre|Unterricht|Haußbedient|Zeugnisse", # cleaning of clothes
    name = "Bolingbrocke", # family name
    military = "Offiziers-Rock|Uniform-Rock|Exerzier-Weste", # see dictionary "uniform"
    shroud = "Todtenröck|Todtenrock", # see dictionary "costume"
    underwear = "Leibchen|Unterärmel|Unterkleid|Unterrock|Unterröck", # see dictionary "underwear"
    sleapwear = "Schlafrock|Schlafröck|Nachtärmel", # see dictionary "sleapware"
    adjective = "gekleidet", # description of other object (mostly dolls)
    sister = "Schwester", # contains "weste"
    dry = "trocke|trockne", # adjectives or verbs meaning "dry" but containing "rock"
    furniture_1 = "Kleiderkasten|Kleiderkäst|Kleiderschrank|Kleider-Kasten|Kleider-Kästen|Kleidermange|Kasten|Kleider-Kästlein|Blunderschafft", # furniture for keeping clothing, see dictionary cabinet, v1
    furniture_2 = "kleider dienlich", # filters out cabinets for clothes
    accessoire = "knopf|knöpf|träger|Hemdengufe" # non textile accessoires (Hemdknopf, Hosenträger etc.)

  )
  create_filter_output(dict)
}


#' Dictionary Sleepwear
#' @export
tagfilter_sleepwear <- function(){
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
    general = "Offiziers-Rock|Uniform-Rock|Offiziers-Roöck|Uniform-Röck|Exerzier-Weste|Uniformrock|Uniformröck|Tschako",
    uniform = "Uniform",
    epaulets = "Epaulett",
    general = "Militär-Effekt|Militäreffekt"
  )
  dict$neg <- list(
    militia = "Miliz-Aufgebot" # muster for the militia
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
    immo = "Losament|Behausung|Gelegenheit", # excludes related immo ads
    book = "Heft|Kalender", # excludes related prints
    service = "Kundenhäuser|Kundenhaus", # excludes ads for related services
    board = "Kost", # excludes ads for board with additional services
    profession = "Strumpfw[ä|e]b|Strumpffw[ä|e]b|Strumpfausbreit|Strumpffach|Str[u|ü]mpf-Fabri|Str[u|ü]mpff-Fabri", # profession of making stockings, also excludes tools for profession (e.g. Strumpfweber-Stuhl)
    raw = "Strumpfwolle|Strümpfwolle|Strumpf-Wolle|Strümpf-Wolle" # yarn for making socks
  )
  create_filter_output(dict)
}


#' Dictionary Outerwear
#' @export
tagfilter_outerwear <- function(){
  dict <- list()
  dict$pos <- list(
    male = "Frack|Talar",
    general = "Mantel|Mäntel|Coat|Cotte|Mantille|Kittelein|Pellerine"
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
    slippers = "[P|p]antoffel|Chauffe-pied",
    shoes = "[S|s]chuh|[S|s]chüh",
    boots = "[S|s]tiefel|Stüfel",
    soles = "[S|s]ohle|[S|s]öhle"
  )
  dict$neg <- list(
    immo = "Liegenschaft", # filters out immo ads with measurements in "Schuh"
    wood = "Bodenholz|Faßdaugen|Dielen|Daugenholz", # wood, measured in "Schuh"
    polish = "Wichse", # shoe polish
    fountain = "Brunnstiefel|Ziehbrunn", # fountains (one part is also called "Schuh")
    name = "Guldenschuh", # family name
    accessoire = "[H|h]andschuh|[H|h]andschüh", # textile accessoires (gloves)
    work = "Schuhmacher|Schuster|Schuhster", # occupations concerned with making shoes
    # PROBLEM: sometimes filters out relevant ads - exclusion of work and immo ads probably better solution
    misc = "Radschuh|Schuhkraft|Schuhknech", # other objects and nouns containing "schuh"
    measure_1a = "Schuh\\s[lang|breit|dick|hoch|weit|Länge|hohe]", # removes "Schuh" as measurement, version 1a
    measure_1b = "Schuhe\\s[lang|breit|dick|hoch|weit|Länge|hohe]", # removes "Schuh" as measurement, version 1b
    measure_2 = "Schuh\\s\\d", # removes "Schuh" as measurement, version 2
    measure_3 = "Nürnberger Maß", # removes "Schuh" as measurement, version 3
    measure_4 = "franz. Maß", # removes "Schuh" as measurement, version 4
    measure_5 = "Schuhlänge|SchuhLänge|Schuh-Länge|schühig", # removes "Schuh" as measurement, version 5
    measure_6 = "Schuh Breite", # removes "Schuh" as measurement, version 6
    measure_7 = "Schuh Länge", # removes "Schuh" as measurement, version 7
    measure_8 = "Schuh Höhe", # removes "Schuh" as measurement, version 8
    measure_9 = "Länge\\s\\d", # removes "Schuh" as measurement, version 9
    measure_10 = "Breite\\s\\d" # removes "Schuh" as measurement, version 10
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
    rain = "Paraplu[i|y|v|g]|Regenschirm|Pareplu[i|y|v|g]",
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
    muff = "Schlupfer|Schlüpfer|Schlupffer|Schlüpffer",
    gloves = "Handsch[u|ü]h"
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
    person = "Marschal", # military rank
    ocr = "sichals|auchal|welchal|durchal|gleichal|Schaltem|sichall", # ocr mistakes (whitespace is missing)
    sound = "Schalles|Schalle|Schalsconservirung", # sound of something
    place = "Schaltenbrand|Mönchaltdorf", # placenames
    fabric = "Chalis|Chaly|Chally|Chalon", # special kind of fabric; PROBLEM: sometimes description for fabric of a scarve...
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
    wig = "Perruck|Perrück|Perück|Peruck|H[a|aa]r-Tours",
    cap = "Kappe|Capotte|Bonnet|Mütze",
    general = "Hut|Hüt|Chapeau",
    straw = "Strohhut|Strohhüt",
    female = "Haube|Häubchen|Kopfputz"
  )
  dict$neg <- list(
    animal = "[D|T]aube", # description of animal with "kappe"
    mind = "zu//shüte", # minding something/someone (usally children)
    plant = "Zuckerhut", # name of a plant
    dump = "hütten", # verbs meaning dumping something
    beware = "verhüte", # verb meaning beware
    name = "Schaubhut|Hauber|Schaubelt", # family names
    religion = "Herrenhut", # religious group
    unclear = "Bonneterie", # exact meaning unsure, maybe place of making bonnets?
    # profession = "Kappenmacher|Hutmacher|Strohhutnähen", # professions, PROBLEM: often in ad including actual hats but not always... - leave out?
    place = "Hutting|Schönhut|Waldshut|Hutgasse|Huthgasse|Hutgaß|Schützen|Eisenhut|Schutzen|Brodthauß", # placenames containing "hut/hüt"
    immo = "Losament|Behausung|Stube|Stüblein|Wohnung", # filters out immo ads
    work = "Gesell|Profeßion|Kamerdiener", # filters out work ads
    other = "Hüter", # supervisor
    verb_1 = "\\sthut", # verbs (doing)
    verb_2 = "sch[u|ü]tten|schutt[e|i]n|schütt[e|i]n|Schut", # to throw smth
    hut = "Hütte", # small house, hut
    object = "Fingerhut|Fingerhüt|Strumpfwoll|Strümpfwoll|Strumpf-Woll|Strümpf-Woll|Zündhütchen|Schutt" # other objects including "hut" or "strumpf"
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
    atlas = "gestreifter Atlas",
    unclear = "Nappes|Senpareille|Napolitain|Circassien|Stramin|Alépin|Etamin|Kamelott",
    fur = "Pelz|Marder|Zobel|Steinmarder",
    marcelline = "Marcelin",
    muslin = "Mousselin",
    lace = "Neiges|Spitzlein|Spitze",
    canvas = "Leinwand|Leinwat|Battist|Batist|Canevas|Halblein",
    gingham = "Guingham|Gingham",
    semi_silk = "Halbseide",
    silk_origin_1 = "Gros de", # does not seem to work- why? is the space between the problem?
    silk_origin_2 = "Grosde",
    silk = "Marceline|Levantine|Seide|Blonde|Floreth|Floret|Taffent|Taffet|Crepvan|Creptamin",
    cashemere = "Casimir|Cachemir",
    bast = "Bast|Sparterie",
    wicker = "Wiener Rohr",
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
    book = "Bücher|Buch|Gespräch", # excludes ads for prints
    death = "Ehefrau|beerdigt", # excludes death notices
    umbrella = "Paraplu[i|y|v]|Regenschirm|Pareplu[i|y|v]|Sonnenschirm|Ombrelle|Parasol|Parresol", # excludes umbrellas, since they are not only textile objects
    immo = "Losament|Wohnung|Ziehbrunn", # filters out immo ads
    stone = "Alabast[e|a]r|Bernstein", # kind of stones
    bastard = "Bastard", # child out of wedlock
    name = "Sebastian|Sebast", # first name
    animal = "Seidenkaninchen", # certain kind of
    work = "unterzubringen|begehrt|gewesener|Lehrgelt|placieren|Handschrift|Zeugnis|Zeugniß", # excludes work ads and description of people's jobs
    service = "waschen|flicken", # filters out ads for services related to textiles
    paper = "Seidenpapier|Seindeppr|Papier", # kind of paper
    profession = "Seidenzwirnmeister|Seidenwinder", # profession
    workplace = "Seiden-Zwirnerey", # workplace
    tool = "Seidenwind-Maschine|Seidenwindmaschine|Seidenrad|Seinderäd|Seindenwaage", # tool for winding silk thread
    place = "Geißspitz", # placename
    instrument = "Mundspitze", # part of musical instrument
    medicine = "Balsam|Heilbalsam", # medicine with instructions to put it on a kind of cloth
    ohter = "Pelzfüsse|Pelzsäcke|Brennkessel" # other objects
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
    print = "Subscription|Buch|Büch|Papier", # print and paper ads
    carriage = "Chaise", # carriages with textile decorations
    animal = "Spitzpommer|Hund", # excludes ads with animal descriptions
    death = "begraben", # excludes death notices
    immo = "Juchart", # excludes immo ads
    election = "Wahl", # excludes election notices
    other = "Ratte|Schriften|Bibliothek|Feuerzeug|Rosenöl", # excludes other unrelated objects
    bag = "T[a|ä]sch|Seckel|Beutel|S[ä|a]ck|Ridicule", # excludes bags and purses
    umbrella = "Paraplu[i|y|v]|Regenschirm|Pareplu[i|y|v]|Sonnenschirm|Ombrelle|Parasol|Parresol", # excludes umbrellas, since they are not only textile objects
    horse = " Pferdtzeug|Reitzeug", # objects for riding etc.
    ocr = "aufhaltetofferirt", # ocr mistake (whitespace missing)
    work = "Zeugni[s|ß]|tüchtig|Leumundszeug|Lehrling|Bedingnis|Lehre|Ladendiener|Reisender|ledig", # filters out work advertisements
    occupation = "Zeugwar|Tuchhandler|Tuchscherer|putzen|Flecken|nähen|stricken", # Occupation "Zeugwart" containing "zeug"
    verb = "überzeug|restera", # verbs containing "zeug" or "rest"
    household = "Bettzeug|Tischzeug|Preß|Lampe|Betteinguß|Tapete", # household textiles and objects
    noun = "bezeug|erzeug", # nouns containing "zeug"
    tool = "Werkzeug|Werkszeug|Werckzeug|W[e|ä]bstuhl|W[e|ä]bstühl|Presse", # tool
    place = "Zeughaus|Zeug-Hauß", # name of a house in Basel
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
    print = "Buch|Bücher", # excludes print ads
    hunt = "Jagd|Jagds[ä|a]ck|Fischgarn", # hunting with yarn
    tool = "Fadenz[ä|a]hler|Oehren|Waarenzähler", # tool for counting threads, needles for yarn
    adjectives = "garnies|garniert|garnirt|garnie|fadene|garnire", # adjectives for decorated with
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
    feather = "Be(tt|th)f(eh|e)de|Flaum|Eitherdaun|Ederdun|Federb[u|ü]sch",
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
    pin = "Vorstecknadel",
    buckle = "Schnalle",
    suspender = "Hosenträger",
    belt = "Gürtel|Ceintur",
    button = "Kn[o|ö]pf"
  )
  dict$neg <- list(
    immo = "Losament|Gelegenheit", # excludes immo ads
    cane = "Spannisches Rohr", # walking canes with "Knopf"
    other = "Waidsack|Säcke|Ofenstange|Deckel|Redincorte" # other objects with buckles or buttons
    # "Knopfmacher" as a profession relevant for category, exclusion of work and immo ads should exclude those irrelevant to textile category
  )

  create_filter_output(dict)

}

#' Dictionary Bags and Purses
#' @export
tagfilter_bag <- function(){
  dict <- list()
  dict$pos <- list(
    bag = "Tasche|Seckel|Beutel|S[ä|a]ck|Ridicule|Täschlein|N[é|e]cessaire",
    rucksack = "Felleisen"
  )
  dict$neg <- list(
    immo = "Wohnung|Losament|Gelegenheit", # excludes immo ads
    bed = "Strohs[ä|a]ck", # bedding
    measure_1 = "\\d.\\sSäck", # removes "Säck" as measurement, v1
    measure_2 = "\\d\\sSäck", # removes "Säck" as measurement, v2
    measure_3 = "\\d.\\sSack", # removes "Sack" as measurement, v3
    measure_4 = "\\d\\sSack", # removes "Sack" as measurement, v4
    measure_5 = "hundert Säck", # removes "Säck" as measurement, v5
    fruit = "Früchte", # filters out ads with bags of fruit
    coffee = "Kaffee", # filters out ads with bags of coffee
    cloth = "Beuteltuch", # cloth for bags
    place = "Säckingen", # placename
    potato = "Erdäpfel", # filters out ads with bags of potatoes
    oat = "Habersäck", # bags of oats
    travel = "Fußsäck|Nachtsäck", # leather blanket for carriage passengers and sleeping bags
    books = "Taschenkalender|Taschenbuch|Taschenbüch|Hauskalender", # pocket books and calendars
    pocketsize = "Taschen-Perspektiv|Taschenspiel|Taschenfeuer|Taschenuhr|Taschen-Latern|Taschenlatern|Taschenmesser|Federmesser", # pocketsize versions of other objects
    administration = "Seckelmeister|Seckelrechnung|Seckel-", # administrative positions and words containing "Seckel"
    description = "Taschenformat|Taschenform" # description of pocketsize of an object
  )

  create_filter_output(dict)

}
