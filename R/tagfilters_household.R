#' Dictionary Bed
#' @export
tagfilter_bed <- function(){
  dict <- list()
  dict$pos <- list(
    bed = "Beth|Bett",
    cradle = "Wiege"
  )
  dict$neg <- list(
    date = "Bettag", # holiday
    book = "Schriften|Schreiben", # filters out book ads
    measure = "wiegend", # measurement
    immo = "Zimmer|Küche|Losament|Kammer|Wohnung|Stüblein|Stube", # filters out immo ads
    garden = "Frühbett", # garden object
    household = "Bethpfanne|Betteinguß", # household objects
    alphabet = "Alphabeth", # alphabet
    birth = "Kindbetterin|Kindbet", # filters out ads for work around birth
    verb = "gebetten|gebethen", # verbs containing "bett/beth"
    name = "Elisabet|Babett|Elßbeth|Eizbeth|Ellbeth|Lisabeth", # names of people and places including "bett/beth"
    placename = "Bettigen|Bettingen|Bettwil|Baselgebeth", # placenames containing "Bett"
    textiles = "Bettdeck|Deckbet|Bettwerk|Bettwerkh|Bettzeug|Bethzeug|Bettsack|Bethsack|Bett-Tril|Betttril|Bethtril", # these are household textiles
    feathers = "Bettfede|Bethfede",
    misc ="verschwiegen|verchwiegen|bettel|wiegewohnt|Regenwetter" # words containing "bett" or "wiege" not referring to objects
  )
  create_filter_output(dict)
}

#' Dictionary Childrens Pushchairs (some may be toys!)
#' @export
tagfilter_pushchair <- function(){
  dict <- list()
  dict$pos <- list(
    pushchair = "Korbw(a|ä|ae)g|Kinderw(a|ä|ae)g|Kinder(chais|schäs)|Korb-W(ag|äg|ae)|Kinder-W(a|ä|ae)g|Kinder-(Chais|Schäs)"
  )
  dict$neg <- list(
    toy = "Puppen-Korb" # toy for children
  )
  create_filter_output(dict)
}

#' Dictionary Bedding
#' @export
tagfilter_bedding <- function(){
  dict <- list()
  dict$pos <- list(
    bedding = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|Federbe(th|tt)|
    Bethzeug|Bettsack|Bethsack|Decke|Strochsack|Strohsäck|Kissen|Unterbe(tt|th)|Nachtsack|Betteingu(ß|ss)"
  )
  dict$neg <- list(
    carriage = "Schlitten|Packkissen|Rei(s|se)kissen|Wägelein|Kütschlein|Cabriolet|Kummeter", # descriptions of carriages
    cover = "zu decken", # verb
    book = "Goldschnitt", # excludes descriptions of books
    service = "ausbessern|flicken", # services concerning household textiles
    furniture = "Canapee|Fortepiano", # excludes furniture
    discover = "entdecke", # meaning discovery of smth
    name = "Neudecker|Decker", # family name
    horse = "Pferdedecke|Pferddecke|bedecken", # horse covers, will be included in category for riding
    other = "Deckenstock", # unsure of meaning, but no household textile
    wood = "Holzdecke", # pieces of wood
    lid = "Deckel", # lid of a container
    ocr = "zugleichzudecke|Enideckend" # ocr mistake
    )

  create_filter_output(dict)

}

#' Dictionary Table Linen
#' @export
tagfilter_tablelinen <- function(){
  dict <- list()
  dict$pos <- list(
    tablelinen = "Tisch(zeug|deck)|(Tisch|Tafel)t(ü|u)ch",
    napkin = "Serviette|Handt(ü|u)ch"

  )
  dict$neg <- list(
    napkinring = "Serviette-Ring|Serviette-Schieber" # placeholder
  )

  create_filter_output(dict)

}

#' Dictionary Carpets and Curtains
#' @export
tagfilter_carpet <- function(){
  dict <- list()
  dict$pos <- list(
    carpet = "T(e|a)(pp|p)i|Bodent(u|ü)ch",
    curtain = "Vorh(a|ä)ng"
  )
  dict$neg <- list(
    food = "Tapioca", # names for food products
    instrument = "Fortepiano" # excludes musical instruments (containig "tepi")
  )

  create_filter_output(dict)

}

#' Dictionary Chair
#' @export
tagfilter_chair <- function(){
  dict <- list()
  dict$pos <- list(
    chair = "St(u|ü)hl|Sitz",
    bank = "B(a|ä)nk"
    # maybe exclude Sitz later, as it only finds two ads also containing Sessel (which are upholstery)
  )
  dict$neg <- list(
    immo = "Baurenhauß|Losament", # filters out immo ads
    verb = "sitzen können", # description of seating
    measure = "Stühle voll", # kind of measurement
    tool = "Drehstuhl|Strumpfweber-St(u|ü)hl|Strumpffweber-St(u|ü)hl|Strumpffwäberstuhl", # tools for certain professions
    loom = "Wäbst(u|ü)hl|Webst(u|ü)hl|Bandstuhl|Po(s|ss|ß)amentstuhl|Weberstuhl|Pa(s|ss|ß)amenter-Stuhl|Posamentier", # looms for weaving
    profession = "Stuhlschreiner|Strumpffwäber", # professions including "stuhl"
    possession = "besitz|Besitz", # words meaning possession including "sitz"
    meeting = "Sitzung|Be(y|i)sitzer", # word for meetings
    churchchairs = "Frauensitz|Weibersitz|Mannensitz|Kirchensitz|Mannssitz|Männersitz|Weiber-Sitz|
    Mannen-Sitz|Frauenst(u|ü)hl|Weiberst(u|ü)hl|Mannst(u|ü)hl|Mannsst(u|ü)hl|Goldst(u|ü)hl", # describing churchchairs (other category)
    church_1 = "St. Leonhardts Kirch", # filters out remaining churchchairs, v1
    church_2 = "Münsterkirch", # filters out remaining churchchairs, v2
    description = "sitzig|sitzend", # description of other objects (how many seats)
    domicile = "Wohnsitz", # word for domicile
    carriage = "(K|k)utsche|(W|w)agen|(W|w)ägelein|(W|w)ägelin|banc|bank" # pointing to carriages or carriage parts (seats for carriages)
  )

  create_filter_output(dict)

}

#' Dictionary Cupboards and Cabinets
#' @export
tagfilter_cabinet <- function(){
  dict <- list()
  dict$pos <- list(
    cupboard = "B(u|ü)ffet|Büffert",
    dresser = "(K|C)ommode|Trumeau",
    cabinet = "K(a|ä)sten|(C|K)orpus|Schrank|Kästlein|Kensterlin",
    book = "Bücherkäst|Büchersch"
  )
  # "Sch(a|ä)fft" finds a lot of unrelated verbs (e.g. "angeschafft") and no ads in 1734/1834
  # (therefore here excluded, maybe put it in later); exlusion of verbs through negative list has to be made
  # if Schafft is necessary for other time periods
  dict$neg <- list(
    building = "Wasserkasten", # waterstorage
    textile = "Wachsdecken|Wachstücher|Kommodendecke", # cover of dressers
    measure = "auf den Kasten", # measuring of something in boxes
    tool = "Schwefelkasten", # tool for working with textiles
    instrument = "Piano", # can be described with a "Kasten"
    authority = "Kasten des Klingenthals", # salt depot
    book = "Anleitung|Juris|Grammatik", # filters out titles of books
    storage = "Haberk(a|ä)st", # small storage for grains
    carriage = "Chais(e|en)k(a|ä)st|Chais(e|en)-K(a|ä)st|Schäsen|Kutschen-Kasten", # carriages
    bath = "Baadk(a|ä)st|Badk(a|ä)st", # bath tub
    clock = "Büffet-Uhr", # timepiece
    lottery = "Kasten-Ampts-Lotterie|KastenAm(b|d)ts-Lotterie|Kastenamt", # filters out lottery ads
    immo = "Kammer|Keller|Bodenzin(ß|s|ss)", # filters out immo ads
    toy = "Baukästen", # box for building bricks
    dollbox = "Polichinell", # finds pop up doll boxes
    paintbox = "Farbkästen|Farbkasten", # describes paintboxes
    key = "(Schrank|Kasten)schlüssel" # means key to a cabinet
  )

  create_filter_output(dict)

}


#' Dictionary Stoves and Related Objects
#' @export
tagfilter_stove <- function(){
  dict <- list()
  dict$pos <- list(
    stoves = "(O|Ö|Oe)fen|(K|C)amin|(Oe|Ö|O)fel",
    grates = "Feu(er|r)h(u|ü)nd|K(u|ü)nstblech",
    bedpan = "Be(th|tt)pfanne"
  )
  dict$neg <- list(
    kitchen = "Bratenöfelein", # kitchen utensil
    weapon = "Oefension", # description of use of weapons
    factory = "Schmöltzöfen", # factory equipment
    book = "Stadtbibliothek", # filters out book ads
    work = "Kaminfeger", # filters out work ads
    immo_1 = "Güter|Kaminküche|(K|C)amin-Kammer|Losament", # filters out immo ads, v1
    immo_2 = "ohne (K|C)amin", # filters out immo ads, v2
    immo_3 = "mit (K|C)amin", # filters out immo ads, v3
    oil = "Tropfen", # fragrant oil for pouring on ovens
    iron = "Glätteöfelein", # ironing
    placename = "Oberhofen", # placenames containing "ofen"
    alecove = "Alekofen|Alickofen|Alckhofen", # alecoves if written with an "f"
    familyname = "Bachofen", # family names containing "ofen",
    ocr = "kanovenst" # ocr mistakes
  )

  create_filter_output(dict)
}

#' Dictionary Mirrors
#' @export
tagfilter_mirror <- function(){
  dict <- list()
  dict$pos <- list(
    mirror = "Spiegel"
    # no negative list necessary so far
  )
  dict$neg <- list(
    carriage = "Kutch|Berline", # filters out windows for carriages
    uni = "Vorlesung|Experiment", # lectures for physics etc.
    book = "Tugendspiegel|Fürstenspiegel|Taschenkalender|Schriften|Druckere(y|i)", # filters out book ads
    instrument = "Spiegel-Telescop", # measuring instrument
    quality = "Spiegelglanz", # describes a quality of something, not the object
    placename = "Spiegelga(ss|ß)|Spiegelgä(ss|ß)" # a street in Basel
  )
  create_filter_output(dict)

}

#' Dictionary Timepieces
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$pos <- list(
    timepiece = "(U|Ü|Ue)hr|Pendul"
  )
  dict$neg <- list(
    book = "Schriften", # filters out book ads
    official = "Polizeigericht", # official notices (e.g. lotteries of timepieces)
    shoe = "Schuhrin", # clasp for shoes
    verb = "führen|ausgeführ|herrühr|verspühr|derührt|gerührt|geführ|berühr|spühr|rühr|ührt|ühret", # verbs containing "uhr/ühr"
    other = "Ge(b|d)(ü|u)hr|Aufführ|Führlohn|Wühren|ge(n|b)ührend", # other words containing "uhr/ühr"
    key = "Uhrenschlüssel|Uhrschlüssel", # key for a clock, very often lost & found, maybe include later or in another category?
    chain_1 = "Uhrkette|Uhrenkette|Uhrhafte|Uhrenhafte|Uhrenband|Uhrband|Uhrenbänd|Uhrbänd|Schnüre|Uhren-Kette", # chain for pocketwatches, v1
    chain_2 = "Uhren Kette", # chain for pocketwatches, v2
    lost = "verloren|gefunden|Finder", # filters out lost and found ads (pocketwatches)
    book = "Uhrfeder", # booktitle
    time_1 = "Nachts Uhr", # nighttime, not object
    time_2 = "um Uhr", # specific time (if number not recognised)
    time_3 = "ein Uhr", # specific time, number 1
    time_4 = "zwe(i|y) Uhr", # specific time, number 2
    time_5 = "dre(i|y) Uhr", # specific time, number 3
    time_6 = "vier Uhr", # specific time, number 4
    time_7 = "fün(f|ff) Uhr", # specific time, number 5
    time_8 = "sechs Uhr", # specific time, number 6
    time_9 = "sieben Uhr", # specific time, number 7
    time_10 = "acht Uhr", # specific time, number 8
    time_11 = "neun Uhr", # specific time, number 9
    time_12 = "zehn Uhr", # specific time, number 10
    time_13 = "elf Uhr", # specific time, number 11
    time_14 = "zwölf Uhr", # specific time, number 12
    time_15 = "zwey Uhr", # specific time, number 13
    time_16 = "\\d\\sUhr", # specific times with numbers, mostly meaning a time and not an object, version 1
    #### this regex does not seem to always work - what is the problem? I think the problem is the whitespace... It should catch e.g. "5 Uhr" or "12 Uhr"
    time_17 = "\\dUhr", # specific times with numbers, mostly meaning a time and not an object, version 2
    time_18 = "tags Uhr", # specific time
    time_19 = "früh Uhr", # specific time
    time_20 = "Uhrzeit", # specific time
    time_21 = "Uhr vormittag", # specific time
    time_22 = "Uhr Abend", # specific time
    time_23 = "\\d.\\sUhr", # specific time
    time_24 = "Versteigerung|Versammlung|Tanz-Anzeige", # auctions and meetings with information about time
    pocketwatch_1 = "Taschenuhr|Sackuhr|Repetiruhr|Frauenzimmeruhr|Zylinderuhr|Repetir-uhr|Frauenzimmer-uhr|
    Zylinder-uhr|Sack-Uhr|Frauenzimmer(ue|u|ü)hr", # pocketsize watches (accessoire), v1
    pocketwatch_2 = "Sack Uhr", # pocketsize watches (accessoire), v2
    pocketwatch_3 = "Uhr mit der Kette", # pocketsize watches (accessoire), v3
    profession = "(U|u)hrmach|Uhrenmach", # watchmakers, also excludes some relevant ads
    place = "\\bWuhr", # placename
    name = "Puhrmann", # family name
    driving = "F(u|ü)hr" # words associated with "driving" ("fuhr")
  )

  create_filter_output(dict)

}

#' Dictionary Tables
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$pos <- list(
    kitchentable = "Küche-Tafel|Küchetafel|Küchentafel|Küchen-Tafel",
    table = "(T|t)isch"
  )
  dict$neg <- list(
    knife = "Tischmesser|Tranchiermesser", # knives
    board = "Tischg(ä|a)ng", # board and lodging
    building = "Holzdecke", # pieces of wood
    ironing = "Glättetisch|Glätte-Tisch", # ironing board
    light = "Wachskerze|Wachslicht|Tischlampe", # lighting for on tables
    plant = "Artischock", # plant
    key = "Schlüssel|Schlüßlen", # keys for tables
    immo = "Keller", # filters out immo ads
    book = "Buchdrucker|Schriften|Buchbinder", # filters out book ads
    adjective = "praktisch|sch(o|ö)ttisch|optisch|homeopatisch|helvetisch|
    romatisch|dramatisch|authentisch|brittisch|politisch|kosmetisch|rheumatisch|
    elastisch|theoretisch|levantisch|alphabetisch|gichtisch|aromatisch|poetisch|zantisch|
    orientalisch|elastisch|corbutisch|astatisch|juristisch|achromatisch|städtisch", # multiple adjectives containing "tisch"
    textile = "Tischzeug|Tischtepp|Tischtuch|Tischt(ü|u)ch|Tischdeck|Tischlachen|Unters(a|ä)tz", # table linen is in another category
    profession = "Tischler|Tischmacher", # professions containing "tisch"
    person = "Tischgenos" # persons not objects
  )

  create_filter_output(dict)

}

#' Dictionary Tableware
#' @export
tagfilter_tableware <- function(){
  dict <- list()
  dict$pos <- list(
    plate = "Teller",
    tableware = "Geschir|Biergläs|Bierglas|Tasse|Humpe|Becher",
    material = "Porzel|Porcel|Steingut|Fayence|Krystallwaa|Krystall-Waa|
    Zin(n|nen)geschirr|Zin(n|nen)-Geschirr|Zin(n|nen)waare|Zin(n|nen)-Waare|Blechwaare|Blech-Waare",
    jug ="(K|C)anne|(K|C)arafe",
    beverage = "Caffeti(e|è)r|Kaffeti(e|è)r|Kaffeeti(e|è)r|Cr(e|ê)mier"
  )
  dict$neg <- list(
    cutlery = "Silbergeschir", # cutlery
    toy = "Soldaten", # tin soldiers
    washing = "Bauc(hg|heg)eschir|Waschgeschir", # washing utensils
    bird = "Tauben", # pidgeaon "Geschirr"
    churchchair = "Cannel-Roost", # filters out churchchairs
    profession = "Kannengiesser", # profession
    tool = "Schmiedegeschir|Bierbrauere(y|i)", # tools
    polish = "Wichse|Schmiere", # polish for harnesses and cutlery
    immo = "Acker|Reben|Zimmer|Losament|Landg(u|ü)t", # filters out immo ads
    wine = "Herbsgeschir|Trettgeschir|Kellergeschir", # tools for wine making
    french = "couteller", # cutlery in French
    measure_1 = "Becherweise", # measuring of something in cups, v1
    measure_2 = "be(y|i)m Becher", # measuring of something in cups, v2
    measure_3 = "zwe(y|i) Becher", # measuring of something in cups, v3
    measure_4 = "Becher à", # measuring of something in cups, v4
    wine = "Muscateller", # special kind of wine
    letter = "Briefsteller", # writer of a letter
    order = "Besteller", # ordering
    pipe = "Pfeifenkopf|Pfeifenköpf", # parts of tobacco pipes from porcelaine
    harness = "Pferdgeschir|Pferdegeschir|Fuhrgeschir|Kutschengeschir|Kutschen-Geschir|
    Chaisegeschir|Chaise-Geschir|Kummetgeschir|Kumetgeschir|Pferd|Geschell|Hindergeschir|
    Fuhrwerkgeschir|Sillengeschir|Brustplatt-Geschir|Reitgeschir|Traggeschir", # different words containing "geschir" in the meaning of harness
    tavern_1 = "Wirtshaus zur Kanne", # an inn in Basel is called "Zur Kanne"
    tavern_2 = "Zur Kanne" # an inn in Basel is called "Zur Kanne"
  )

  create_filter_output(dict)

}


#' Dictionary Bureaus and Work Desks
#' @export
tagfilter_bureau <- function(){
  dict <- list()
  dict$pos <- list(
    bureau = "(S|s)ekretär|(S|s)ecretär|(S|s)ecretair|(S|s)ekretair", # one false positive for "Secretär" as a job
    workdesk ="P(u|ü)lt"
  )
  dict$neg <- list(
    key = "Schlüssel", # keys for bureaus
    profession = "PolizeySekretär|Commission", # secretaries of certain commissions
    catapult = "Katapult|tapult" # catapults, sometimes the K is not correctly recognized
  )

  create_filter_output(dict)

}

#' Dictionary Storage
#' @export
tagfilter_storage <- function(){
  dict <- list()
  dict$pos <- list(
    basket = "Korb|Körb",
    box ="K(i|ü)ste|K(i|ü)stch|Kästchen",
    bag = "Sack|Säcke|Säckch",
    tub = "Trog\\b"
  )
  dict$neg <- list(
    place = "Goldküste", # placename
    death = "beerdigt", # death notices
    bed = "Strohs(ä|a)ck", # bedding
    book = "Buchdrucker|Bibel", # filters out book ads
    carriage = "Chaise|Schäse", # excludes carriages with baskets or boxes
    stroller = "Kinderwagen|Kinderwäg", # excludes strollers with baskets
    pocketknife = "Sackmesser", # pocketknife
    travel= "Fussack|Fußsack|Reissack|Reisesack|Reis-Sack|Reise-Sack", # foot rest for carriages and bags for travel
    bed = "Strohsack|Bettsack|Nachtsack", # bag full of straw, used as bedding
    textile = "Sacktuch", # textile
    work = "lesen|schreiben|rechnen|Lehrt(o|ö)chter", # filtering out work ads
    grain = "Kernen", # filters out ads selling grain in bags
    food = "Erdäpfel|Grundbirne|Setzerdäpfel|Habern\\b|Haber\\b", # filters out ads selling different kind of food in bags
    measure_1 = "korbweis|kistchenweis|kistenweis", # measuring something by baskets or boxes, variant 1
    measure_2 = "Kistchen zu", # measuring something by boxes, variant 2
    meausure_3 = "Kistchen von", # measuring something by boxes, variant 3
    measure_4 = "//d//sKistchen", # measuring something by boxes, variant 4
    meausure_5 = "Kistchen à", # measuring something by boxes, variant 5
    meausure_6 = "Kistchen v.", # measuring something by boxes, variant 6
    meausure_7 = "pr. Kistchen", # measuring something by boxes, variant 7
    measure_8 = "der Sack zu",  # measuring something by bags, variant 8
    measure_9 = "\\d\\sSack", # number of bags of something, v 9
    measure_10 = "\\d\\sSäck", # number of bags of something, v 10
    measure_11 = "\\dSäck", # number of bags of something, v 11
    measure_12 = "in K(o|ö)rb", # number of bags of something, v 12
    measure_13 = "be(i|y)m Korb", # number of bags of something, v 13
    measure_14 = "Sack à", # number of bags of something, v 14
    measure_15 = "Sack um", # number of bags of something, v 15
    measure_16 = "Sack von", # number of bags of something, v 16
    measure_17 = "Sackvon", # number of bags of something, v 17
    measure_18 = "sackweis", # number of bags of something, v 18
    beehive = "Bienenkorb|Bienenkörb", # bee hives
    stroller = "Korbwage|Korbwäge", # strollers and prams (own category)
    profession = "Korbmacher|Korbhändler|Korbladen|KorbLaden", # professions and shops
    cabinet = "Bücherkäst|Glaskäst", # differet kinds of cabinets
    watch_1 = "Sackuhr|Sack-Uhr", # pocketwatch, v1
    watch_2 = "Sack Uhr", # pocketwatch, v1
    pistol = "Sack-Pistol|Sackpistol", # pocket pistols
    telescope = "Sack-Perspek|Sackpersp", # pocket telescope
    light = "Sackfeuer|Sacklatern", # pocket lighter and lantern
    graveyard = "Gottesacker", # graveyard
    other = "Korbgitter", # unknwon object, but no storage
    textile = "Sacktuch|Sacktüch" # name for specific kind of cloth
  )
  # maybe exclude Bettsack and add it to bed dictionary?

  create_filter_output(dict)
}

#' Dictionary Toys for Children
#' @export
tagfilter_toy <- function(){
  dict <- list()
  dict$pos <- list(
    learning = "ABC-Spiel",
    doll = "Puppe|Puppen-Korbw(a|ä)g|Puppenkorbw(a|ä)g",
    bricks = "Bauhölz(chen|lein)|Baukasten",
    castle = "Felsenburg",
    boy = "Spiel-Boit|Spielboit|Aufstellschachtel",
    horse = "(Schwung|Steck|Stecken)pferd",
    general = "Spiel(waa|a)re|Kinderspiel",
    tin_1 = "zinn(erne|ern|erner|ernes) Soldat",
    tin_2 = "Zin(n|nen)soldat|Zin(n|nen)-Soldat",
    tin_3 = "Zin(n|nen)w(waa|a)ren zum Aufstellen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )
  create_filter_output(dict)
}

#' Dictionary Games
#' @export
tagfilter_game <- function(){
  dict <- list()
  dict$pos <- list(
    billard = "Billard|Billiard|Bilard|Biliard",
    chess = "Schachbret|Schachspiel",
    pocketgame = "Taschenspiel",
    domino = "Domino",
    lotto = "Lottospiel",
    cards = "Spielkarten|Whist",
    dice = "Würfelspiel",
    bowling = "Kegelspiel|Kegelries",
    general = "Spiele"
  )
  dict$neg <- list(
    marionette = "Margonetten-Spiel", # marionettes
    book = "Schriften", # filters out book ads
    competition = "ausgekegelt", # competition in certain games (bowling)
    authority = "Gesetz|Verordnung", # related official notices
    other = "Ausspiel|Beispiel", # other unrelated words containing "spiel"
    children = "ABC-Spiel", # toys and games for children
    work = "Spielkarten-Arbeit|Anstellung|kundig", # filters out related work ads
    fireworks = "Feuerwerkspiel", # fireworks
    music = "Pianofortespiel", # playing music, musicians
    thief = "Taschenspieler", # tricksters and thieves
    play = "spielend|gespielt|Spielens\\b", # meaning to play something
    theater = "Traurspiel" # ads for theater plays

  )

  create_filter_output(dict)

}

#' Dictionary Kitchen Utensils
#' @export
# I excluded "Krug" because of a lot of false negatives (family name, place name) difficult to filter out
tagfilter_kitchen <- function(){
  dict <- list()
  dict$pos <- list(
    distilling = "Brennhafen",
    washingup = "Wasserstein",
    preparation = "Mörser|Wahlholz|Wahlh(ö|o)lz|Krauthobel|Messerwetz|Milchsecht|(Pfropfen|Zapfen)zieher|Beizebütte|Kaffeebrett|
    Ca(ff|f)ebrett",
    cooking = "Kochlöffel|Kochhafen|Schwenkkesse|Kochgeschir|Pfanne|Pfännlein",
    cooker = "Kaffeh(ee|e)rd|Sparh(ee|e)rd|Kochhafen|Kaffe(e-D|ed)ampfmaschine|Braten(ö|o)fe|Dre(y|i)fu(ß|ss)",
    storage ="Salzfäss|Salzfass|Milchflasche|Fleischbütte|Sauerkrautstand|Zucker(dose|buchs)|Kühlstand|Krautstand|Fischtrog|
    Bro(d|t)korb|Bro(d|t)körb|T(hee|hée|ee)-Boit|Wasserzuber|Fischbeck(i|e)n",
    pan = "Casserolle|Bressière|Daubière|Bratenwender|Federbrä(t|th)er",
    baking = "Mödel|Waffleneisen|Gugelhopfform",
    press = "Pre(ss|ß)spindel|Pre(ss|ß)-Spindel|Pre(ss|ß)stang|Pre(ss|ß)-Stang|Handpre(ss|ß)|Stockpre(ss|ß)",
    mill = "(Oe|Ö)lmühl|Erdäpfelmühle|Erdäpfel-Mühle|(K|C)a(ff|f)emühle|(K|C)a(ff|f)e-Mühle|Kirschenmühle|Kirschen-Mühle|
    Mahlmühle|Mahl-Mühle"
  )
  dict$neg <- list(
    death = "von Basel", # excludes death notices
    measure_1 = "\\d.\\sZ(ü|u)ber", # measurement of something, v1
    measure_2 = "in Krügen", # measurement of something, v2
    measure_3 = "Z(ü|u)ber Wasser", # measurement of water, v3
    tool = "F(a|ä)rbkessel|F(a|ä)rb-Kessel", # tools
    name = "Her(rn|r) Kr(ü|u)g", # family name
    textile = "gemödelt", # certain kind of decoration of a textile
    mineralwater = "Selterser|Selter", # mostly stored mineral water (bevarage), very few instances used for bottles only (these will get lost with this negative)
    immo = "ablaufen|Kammer|H(a|ä)u(s|ß)", # filters out immo ads with kitchen and a "ablaufendem Wasserstein", "Kammer" filters out the rest of the immo ads
    weapon = "Messing-Mörser" # description of a weapon

  )

  create_filter_output(dict)

}


#' Dictionary Lighting
#' @export
tagfilter_lighting <- function(){
  dict <- list()
  dict$pos <- list(
    lighting = "Leuchter|Chandelier|Lampe|Latern|Nachtlicht|Lichtstock|Lichtstöck|Lamepngl(a|ä)s|
    Gingette",
    oil = "Lampe(nö|noe|n-Ö|n-Oe)hl",
    candle = "Wachskerzen|Wachslichter|Kerze",
    wick = "Lamendocht",
    other = "Lichtscheer"

  )
  dict$neg <- list(
    book = "Welttheater", # filters out book ads
    fireworks = "Feuerwerk", # fireworks
    carriage = "einsp(ä|a)nnig|zweisp(ä|a)nnig", # excludes descriptions of carriages
    magica = "magica", # excludes laterna magica
    lecture = "Physik" # description of university lectures

  )

  create_filter_output(dict)

}

#' Dictionary Musical Instruments
#' @export
tagfilter_instrument <- function(){
  dict <- list()
  dict$pos <- list(
    tuning = "Stimmgabel",
    keyboard = "Flügel|Piano|Clavier|Klavier|Spinet|Spi(nn|n)eth",
    drum = "Trommel", # maybe exclude here drums specifically for children (toys)?
    wind = "Flöte|Clarinet|Fagot|Trompet|Oboe|Posaune|Posth(o|ö)rn|Waldh(o|ö)rn",
    guitar = "Gui(t|tt)ar",
    string = "Violin|Contrebass|Cell(o|e)|Harfe",
    resin = "Colophonium|Geigenharz",
    strings = "Saiten",
    organ = "Aeoloti(k|c)on|Harmonika|Haus(ö|o)rgel",
    misc = "Instrument"
  )
  dict$neg <- list(
    form = "wie eine Trompete", # formed like a trumpet
    invention = "erfunden", # technical instruments
    official = "Bewilligung|Ordnung", # official notices, prohibition of music etc.
    book = "Vorwort|Gedicht", # book ads
    food = "Vermicelle", # food
    adjective = "scharfen|excellent", # contains "harfe" or "celle"
    work = "Clavierschlagen", # excludes work ads
    divider = "Spanische Wand", # dividers with "Flügel"
    notes = "instrumental|Genre", # music notes
    concert_1 = "Conzert|Concert", # excludes announcements of concerts, v1
    concert_2 = "Freunde des", # excludes announcements of concerts, v2
    physics = "physikal|mechani|chirurgi|opti", # excludes optical, mechanical, chirurgical and physical instruments
    domestic = "Sticktrommel", # instrument for embroidery (see domestic appliances)
    immo = "Flügelgebäude|Fensterflügel|Fenster|Parcelle", # description of certain kind of building or building material
    food = "Geflügel", # chicken and oder poultry
    print = "abonniren|Magazin|Composition|Ges(a|ä)ng|Auszüg|Auszug|Heft|Begleitung", # excludes printed matter (musical notes)
    profession = "Unterricht|lecon" # excludes work-related ads (music teaching)

  )

  create_filter_output(dict)

}

#' Dictionary Building Components
#' @export
tagfilter_building <- function(){
  dict <- list()
  dict$pos <- list(
    well_1 = "Br(u|ü)(nn|n)stein",
    well_2 = "Br(u|ü)(nn|n) Stein",
    door = "(Th|T)üre|Vor(th|t)üre|(Th|T)ürschlinge", # possibly too many immo ads with "Thüre", negatives are tricky
    window = "Fenster|Kreuzstock|Kreuzstöck|Fensterflügel|Fensterpritsch|Vorfenster",
    wood = "Bauholz|Latten|Diele",
    stone = "Backstein|Quaderstein",
    metal = "Dachk(a|ä)nel|Dachk(a|ä)nal|Rohr|T(ei|eu)chel|Abtrittrohr",
    shingle = "Dachschindel|Schind(eln|len)",
    tile = "Bodenpl(ä|a)ttl|Plättlein",
    glas = "Glas in Scheibe",
    other = "Gerüststange|Wetterdach",
    pipe = "Rohr"
  )
  dict$neg <- list(
    news = "Pulver-Mühl|Stockwerck|Mordthat", # excludes news containing "Hauß" and "Fenster"
    umbrella = "Parapluie", # descriptions of umbrellas with "rohr"
    lost = "verloren", # excludes ads for lost goods (very rare with building components)
    churchchair = "(Frauen|Mannen|Weiber)sitz|(Frauen|Mannen|Weiber)-Sitz", # church chairs (close to a door)
    telescope = "Fernrohr", # telescope
    instrument = "Fagotrohr|Instrument", # musical instruments
    material_1 = "M(eh|ee)rrohr|Pfefferrohr|Wienerrohr", # materials for walking canes, v1
    material_2 = "Wiener Rohr", # materials for walking canes, v2
    measure = "in Platte", # measurement for something
    carriage = "einsp(ä|a)nnig|zwe(i|y)sp(ä|a)nnig", # carriages with windows
    work = "Kenntnis|Lehre", # filters out work ads
    plates = "Platten", # plates
    place = "Winterthür", # placenames
    cabinet = "Schrank|Kästchen", # description of cabinet doors
    book = "Kupfferblatt", # book ads
    weapon = "Flin(th|t)e", # weapons with a "rohr"
    tool = "Glaserwerckzeug", # tools
    tobacco = "Bla(ß|ss|s)rohr-Steck|Pfeifenrohr", # tobacco utensil
    walking = "Spannisc(h|he|hes) Rohr", # walking cane
    adjective = "glatt", # containing "latt"
    divider = "Scheidwand", # excludes doors as part of room dividers
    cabinet = "Kasten", # excludes doors as a part of cabinets
    immo = "Ladenthüre|beschlüssig|beschlossen|Estrich|Küche|Hinterthür|Fasnachtfronfast", # words indicating immo-ads
    ocr = "dieletze" # ocr mistakes

  )

  create_filter_output(dict)

}

#' Dictionary Wallpaper
#' @export
tagfilter_wallpaper <- function(){
  dict <- list()
  dict$pos <- list(
    wallpaper = "Tape(t|z)"
  )
  dict$neg <- list(
    attraction = "vorstellen", # advertisments for attraction at the place of a "Tapezierer"
    service = "waschen", # services from "Tapezirer" unrelated to wallpaper
    furniture = "Fauteuil", # furniture with descriptions or from "Tapezirer"
    immo = "Behausung|Küche|austapezirt|tapezirt", # excludes immo ads with description of wallpaper
    sewing = "Nadeln" # sewing needles

  )

  create_filter_output(dict)

}

#' Dictionary Suitcases
#' @export
tagfilter_suitcase <- function(){
  dict <- list()
  dict$pos <- list(
    suitcase = "Koffer|Coffre|Coffer",
    travelbag = "Reissack|Reisesack|Reissäck|Reisesäck|Reis-Sack|Reis-Säck|Reise-Sack|Reise-Säck|Rei(ss|ß)zeug"
  )
  dict$neg <- list(
    tool = "Kofferwinde" # name for a winding tool

  )

  create_filter_output(dict)

}

#' Dictionary Cutlery
#' @export
tagfilter_cutlery <- function(){
  dict <- list()
  dict$pos <- list(
    cutlery = "Löfel|Löffel|Gabel|Messer",
    misc ="Silbergarnitur|Besteck"
  )
  dict$neg <- list(
    measure = "Messerspi(ß|tz)", # measuring something
    shooting = "Schützen", # ads for "Schützenfest" with cutlery as prices
    death = "gewesen|hinterlassen", # filters out death notices
    ocr = "Zurückgabel", # ocr mistake (actually "Zurückgabe")
    saddle = "Löffel-Sattel", # certain kind of saddle
    measure = "Augenmesser", # tools for measuring
    kitchen = "Transchiermesser|Kochlöffel|Messerwetzer", # kitchen tools
    music = "Stimmgabel", # tunig fork
    pocket = "Sackmesser", # pocketknife
    agriculture = "Heugabel|Ladgabel|Obstmesser|Mattenmesser|Matten-Messer",
    work = "Kornmesser|messerey|Mehlmesser", # profession
    stove = "Kachelöfel", # small stove containing "löfel"
    hunting = "Waidmesser", # hunting knife
    diameter = "Durchmesser", # diameter
    stationary = "Federmesser|Falzmesser|Rastermesser", # tools for writing and for use of paper in general
    name = "Langmesser", # family name
    razor = "Rasiermesser|Barbiermesser" # razors and

  )

  create_filter_output(dict)

}

#' Dictionary Measuring Instruments
#' @export
tagfilter_measure <- function(){
  dict <- list()
  dict$pos <- list(
    meter = "Barometer|Thermometer"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Room Dividers
#' @export
tagfilter_divider <- function(){
  dict <- list()
  dict$pos <- list(
    spanish_1 = "spa(n|nn)ische W(a|ä)nd",
    spanish_2 = "spa(n|nn)isch W(a|ä)nd",
    divider = "Scheidwand|Kunstwand"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Objects for Pets
#' @export
tagfilter_petobject <- function(){
  dict <- list()
  dict$pos <- list(
    bird = "Paarhäus|Käfig|Paarh(a|ä)us|Anh(a|ä)ngköfig|Flugbret|Maisenschlag|Taubennest|Taubenschlag|Vogelschlag",
    dog ="Hundesst(ä|a)ll|Hundest(ä|a)ll|Hundsst(ä|a)ll|Halsband",
    fish = "Goldfischglas"
  )
  dict$neg <- list(
    lost = "abhanden|verloren|verloffen|entloffen|zugeloffen|entlief|entflogen" # excludes lost and found animals with descriptions

  )

  create_filter_output(dict)

}


#' Dictionary Upholstery
#' @export
tagfilter_upholstery <- function(){
  dict <- list()
  dict$pos <- list(
    couch = "Canapé|Kanapse|Kanape|Canape|Kanefa",
    armchair ="(S|s)essel|(F|f)auteil|(F|f)uateuil|(F|f)auteuil"
  )
  dict$neg <- list(
    work = "Sesselfabrik", # occupation or place of manufacture for upholstery
    carpet = "Vorl(a|e)g(et|t)eppich" # carpets ment to be placed before a couch

  )

  create_filter_output(dict)

}



#' Dictionary Domestic Appliances
#' @export
tagfilter_domestic <- function(){
  dict <- list()
  dict$pos <- list(
    embroidery = "Sticktrommel",
    sewing = "Nadeln|Nadlerwaaren|Nadler-Waaren|Nähk(ä|a)st|Steckgufen|Stecknadel|Fingerh(u|ü)t",
    knitting = "Stricknadel|Strickseckel|Stricksstiefel",
    iron = "Bügeleisen|Glätteisen|Glättetisch|Glätte(öfelein|ofen)|Kleidermange",
    washing ="Waschkessel|Wasch-Kessel|Waschbütte|Wasch-Bütte|Bauc(he|h)geschir|Bauc(he|h)-Geschir|Bauc(he|h)bütte|Bauc(he|h)-Bütte|
    Plunderstang",
    spinning = "Spinnrad|Spinnräd|Spuhlrad|Schlumpstock"
  )
  dict$neg <- list(
    accessoire_1 = "Vorstecknadel|Broche", # accessoire, v1
    accessoire_2 = "goldene Stecknadel" # accessoire, v2

  )

  create_filter_output(dict)

}


#' Dictionary Garden Tools, Furniture and Other Objects
#' @export
tagfilter_garden <- function(){
  dict <- list()
  dict$pos <- list(
    trellis = "Spalier",
    bench = "Gartenbank|Bänklein|Gartenbänk",
    pot = "Gärtner-Cloches|Blumengestel",
    tool ="Baum-Sch(ee|e)re|Baumsch(ee|e)re|Haag-Sch(ee|e)re|Haagsch(ee|e)re|Baum-S(a|ä)ge|
    Baums(a|ä)ge|Gertel|Gartengeschirr"
  )
  dict$neg <- list(
    immo = "Landg(u|ü)t|Juchart|Wohnung" # filters out immo ads

  )

  create_filter_output(dict)

}

#' Dictionary Home Decoration
#' @export
tagfilter_homedeco <- function(){
  dict <- list()
  dict$pos <- list(
    vase = "Vase",
    general = "Zimmerzierrat"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Art
#' @export
tagfilter_art <- function(){
  dict <- list()
  dict$pos <- list(
    art = "Aquarel|Handzeichnung|(Oe|Ö)lgemäld|(Oe|Ö)lbild|Kupferstiche|Bilder|Gem(ä|äh)lde|M(a|ah)lere(y|i)en"
  )
  dict$neg <- list(
    embroidery = "Sticken", # description of embroidery
    collection = "Kabinet|Ausstellung|Sammlung|Vorstellung", # collections of art for visiting
    women = "Weibsbild", # meaning woman/women
    weapon = "Degen", # descriptions of weapons with decorations
    book = "Zeitschrift|Katalog|Schriften|Predigt|Fabel|Titel|Leseb(u|ü)ch|Erklärung|Erzählung|Beschreibung" # filters out book ads

  )

  create_filter_output(dict)

}

#' Dictionary Bathing Objects
#' @export
tagfilter_bathobject <- function(){
  dict <- list()
  dict$pos <- list(
    bath = "B(a|aa)d(bütt|kast|käst)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Misc Household Goods (Unspecified)
#' @export
tagfilter_mischousehold <- function(){
  dict <- list()
  dict$pos <- list(
    misc_1 = "hausräthlich|hausrätlich",
    misc_2 = "Hausgerät"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}
