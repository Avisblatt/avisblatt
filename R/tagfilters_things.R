#' Dictionary Mercery and Non Textile Accessoires
#' @export
tagfilter_mercery <- function(){
  dict <- list()
  dict$pos <- list(
    pin = "Vorstecknadel",
    buckle = "Schnalle",
    suspender = "Hosenträger",
    belt = "Gürtel|Ceintur",
    button = "Kn(o|ö)pf"
  )
  dict$neg <- list(
    work = "Lehre", # excludes work ads
    clothes = "Kn(o|ö)p(f|ff)l(o|ö)ch", # descriptions of clothes
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
    bag = "Tasche|Seckel|Beutel|S(ä|a)ck|Ridicule|Täschlein|N(é|e)cessaire",
    rucksack = "Felleisen"
  )
  dict$neg <- list(
    immo = "Wohnung|Losament|Gelegenheit", # excludes immo ads
    bed = "Strohs(ä|a)ck", # bedding
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
    pocketsize = "Taschen-Perspektiv|Taschenspiel|Taschenfeuer|(Taschen|Sack)uhr|(Taschen|Sack)-Uhr|Taschen-Latern|Taschenlatern|Taschenmesser|Federmesser", # pocketsize versions of other objects
    administration = "Seckelmeister|Seckelrechnung|Seckel-", # administrative positions and words containing "Seckel"
    description = "Taschenformat|Taschenform" # description of pocketsize of an object
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
    leather = "Sohl(leder|h(a|ä)ut)|Sohl-(Leder|H(a|ä)ut)|Zeugleder",
    feather = "Be(tt|th)f(eh|e)de|Flaum|Eitherd(aun|un)|Federb(u|ü)sch|Federen|(Pf|F)laumfed",
    horsehair = "(Ro(ss|ß)|Pfer(d|de))haar"

  )
  dict$neg <- list(
    carriage = "Chaise", # description of carriages ("Federen")
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

#' Dictionary Umbrellas and Related Objects (umbrella cases and umbrella cloth)
#' @export
tagfilter_umbrella <- function(){
  dict <- list()
  dict$pos <- list(
    rain = "Par(a|e)plu(i|y|v|g)|Regenschirm|genschirm",
    sun = "Sonnenschirm|Ombrelle|Pa(r|rr)(a|e)sol"

  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )

  create_filter_output(dict)
}

#' Dictionary Carriages and Related Objects
#' @export
tagfilter_carriage <- function(){
  dict <- list()
  dict$pos <- list(
    whip = "(Chai|Schä)sepeitsche|(Chai|Schä)se-Peitsche",
    harness = "(K(u|ü)mmet|Chaise|Schäse)-Geschir|(K(u|ü)mmet|Chaise|Schäse)geschir|Kutsc(h|he|hen)geschir|Kutsc(h|he|hen)-Geschir",
    coach = "W(a|aa|ä)gen",
    sleigh = "(Familien|Hau(ß|s|ss))schlitten|Schlitten",
    family = "Familie-W(a|aa)gen|Familienw(a|aa)gen",
    travel = "Reisew(a|aa)gen",
    riding = "Reitw(a|aa)gen|Reitwägelein",
    charabanc = "Char\\-(à|a)\\-banc",
    carriage = "Malaben|Malborough|Berline//b|(K|C)abriolet|Coupe|Kutsch//b|Kutsche//b|Kutschen//bKütschlein|(T|D)roscheke|
    (T|D)ro(sch|tsch)ke|Trosque|Pascule|Pout//b|Kutschen-Berlin|Rei(ße|se|ß|s)kutsch|Rei(ße|se|ß|s)-Kutsch|Engglai-Kutsch|
    Bernerw(a|ä)ge|Berner-W(a|ä)ge",
    chaise = "Chai(se|s)|Chai(se|s)lein|Schäse|Schä(s|se)lein",
    characteristic = "ein(spännig|rädrig)|zwe(y|i)(spännig|rädrig)|(1|2|3|4)(spännig|rädrig)",
    parts = "Schwanenhäls|Geschell|Kutschen-(Kasich|Kasten)|Kutschen(kasich|kasten)"
  )
  dict$neg <- list(
    trolley = "Leiterwagen|Bauernwagen|Bauern-Wagen|Dielenwagen|Dielen-Wagen|Bauernwägel", # trolleys, see other category
    verb_1 = "wagen wir", # meaning of to dare something
    burial = "Todtenwagen", # transportation of corpses
    immo_1 = "Wagenschopf|W(a|aa)genremise|W(a|aa)gen-Remise", # shelter for a carriage
    mail = "Post(a|aa|ä)gen|Postkutsche|Post-Anzeige|Postanzeige|Post-Chaise|Postillon", # mail coach
    measure_1 = "W(a|aa|ä)gen voll", # measurement of something in coaches, v1
    measure_2 = "Grube|w(a|aa|ä)genwei(s|ß)|Heu|Emd|verwährt", # carloads and objects measured in carloads
    measure_3 = "Loth wägen", # measure v3
    scale = "Schnell-W(a|aa|ä)g|Schnellw(a|aa|ä)g|Waagbalcken|Romaine", # scales
    tool = "W(a|aa|ä)genwinde|W(a|aa|ä)gen-Winde|Winde|Zentner", # tool for lift heavy loads
    lost_1 = "ab einem W(a|aa)gen", # losing something from a carriage, v1
    lost_2 = "ab seinem W(a|aa)gen", # losing something from a carriage, v2
    lost_3 = "verloren|verlohren|verlohrne", # losing something from a carriage, v3
    lost_4 = "fallen lassen", # losing something from a carriage, v4
    stroller = "Kinderwagen|Korbwagen", # stroller for children or as toys
    metal = "Berlinereisen|Berliner-Eisen", # specific kind of metal
    worker = "Kutscher|Lohnkutscher|F(ü|u)hrmann", # coachmen
    service_1 = "erbietet|Tanz-Anzeige", # services with a carriage, v1
    service_2 = "bereit stehen", # services with a carriage, v2
    service_3 = "hin zu führen", # services with a carriage, v3
    books = "Buchhandlung", # books containing instructions for carriage-making
    wallpaper = "Tapete|tapezieren", # wallpaper decoration for carriages
    travel_1 = "Retour-(Chais|Kutsch)|verreisen|Retour-èferd", # travel by carriage v1
    travel_2 = "Platz haben", # travel by carriage, v2
    travel_3 = "zu fahren", # travel by carriage, v3
    travel_4 = "verlangt Platz", # travel by carriage, v4
    travel_5 = "diese Woche", # travel by carriage, v5
    travel_6 = "gewidmet|Einkehr", # travel by carriage, v6
    travel_7 = "dahin gehend", # travel by carriage, v7
    travelcompanion = "Reisegesellscha(f|ff)t|Comagnie|Gesellschafft|Gelegenheit|Compagnie", # searching and offering of company
    other = "Pantzer|Flaschenkette", # other small objects
    ocr = "wägenkann", # ocr mistake
    verb_2 = "zu wägen", # verbs
    noobject = "wägende|Wagenhau(ß|s)", # no objects
    toy = "Kinder-|Kinder(chais|wag|kutsch)", # excludes toy carriges for children
    animal = "Chaise-Pferd|Reisepferd|Chaisepferd" # horses for drawing carriages,
  )
  create_filter_output(dict)
}

#' Dictionary Childrens Pushchairs (some may be toys!)
#' @export
tagfilter_pushchair <- function(){
  dict <- list()
  dict$pos <- list(
    pushchair = "Korbw(a|ä|ae)g|Kinderw(a|ä|ae)g|Kinder(chais|schäs)|
    Korb-W(ag|äg|ae)|Kinder-W(a|ä|ae)g|Kinder-(Chais|Schäs)"
  )
  dict$neg <- list(
    toy = "Puppen-Korb" # toy for children
  )
  create_filter_output(dict)
}

#' Dictionary Storage
#' @export
tagfilter_storage <- function(){
  dict <- list()
  dict$pos <- list(
    basket = "K(o|ö)rb",
    box ="K(i|ü)st(e|ch)|Kästchen",
    bag = "S(a|ä)(ck|cke|ckch)",
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

#' Dictionary Building Components
#' @export
tagfilter_building <- function(){
  dict <- list()
  dict$pos <- list(
    door = "steinerner Bogen",
    well_1 = "Br(u|ü)(nn|n)stein",
    well_2 = "Br(u|ü)(nn|n) Stein",
    door = "(Th|T)üre|Vor(th|t)üre|(Th|T)ürschlinge", # possibly too many immo ads with "Thüre", negatives are tricky
    window = "Fenster|(K|C)reu(z|tz)st(o|ö)ck|Fenster(flügel|pritsch)|Vorfenster|Jalousie-L(ä|a)den|Jalousiel(ä|a)den",
    wood = "Bauhol(z|tz)|Latten|Diele|Stiege|Kellerb(ö|o)gen",
    stone = "(Back|Quader)stein|Steinplatte",
    metal = "Dachk(a|ä)n(e|a)l|Rohr|T(ei|eu)chel|Abtrittrohr",
    shingle = "Dachschindel|Schind(eln|len)|Ziegel\\b|Deichel",
    tile = "Bodenpl(ä|a)ttl|Plättlein",
    glas = "Glas in Scheibe",
    other = "Gerüststange|Wetterdach|G(egi|a)tter",
    pipe = "Rohr"
  )
  dict$neg <- list(
    verb = "gestiegen", # verb containing "stiege"
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
    ocr = "dieletze", # ocr mistakes
    place_1 = "Ziegelh(oo|o)f", # places in Basel
    place_2 = "Stiege hoch" # description of location of something

  )

  create_filter_output(dict)

}

#' Dictionary Suitcases
#' @export
tagfilter_suitcase <- function(){
  dict <- list()
  dict$pos <- list(
    suitcase = "(K|C)off(er|re)",
    travelbag = "Rei(ss|se|ß|s)(a|ä)ck|Rei(s|ss|se|ß)-S(a|ä)ck|Rei(ss|ß|s|se)zeug"
  )
  dict$neg <- list(
    place = "Karrenhof", # placename
    tool = "Kofferwinde" # name for a winding tool

  )

  create_filter_output(dict)

}

#' Dictionary Measuring Instruments
#' @export
tagfilter_measure <- function(){
  dict <- list()
  dict$pos <- list(
    meter = "(Baro|(T|Th)ermo)meter"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Trolleys (Carriages for Transportation of Objects, not People)
#' @export
tagfilter_trolley <- function(){
  dict <- list()
  dict$pos <- list(
    handcart = "(Hand|Leiter)w(ä|a)ge(l|n)|(Sto(ss|ß|s)|Trog)ka(rr|r)en|K(a|ä)(rr|r)en",
    trolley = "(Bauern|Dielen)-W(a|ä)ge|(Bauern|Dielen)w(a|ä)ge|Fuhrwerk",
    harness = "Sillen-Geschir|Sillengeschir",
    objects = "Wagenkette|(Leit|Zug)seil"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Health, Cosmetics and Drugstore Products
#' @export
tagfilter_health <- function(){
  dict <- list()
  dict$pos <- list(
    medicine_1 = "Bal(s|ss)am|Heilmittel|Hustentäfel|Salbe|Tin(k|c)tur|Gichtpapier|Zahn-Pulver|Zahnpulver
    |Ar(tz|tzt|zt)ne(y|i)|Tabletten|Medi(z|c)in|Pulver|Mittels|Heilkraft|Mittel(wider|gegen|für)|Augenmittel|
    Kr(e|ä)uter-(Oeh|Oe|Ö|Öh)l",
    medicine_2 = "Pate pectoral",
    medicine_3 = "Mitte(l|ls) gegen",
    medicine_4 = "Mitte(l|ls) wider",
    medicine_5 = "Mitte(l|ls) für",
    medicine_6 = "bew(ä|e)hr(tes|ts|trs) Mitte(l|ls)",
    medicine_7 = "dicke Hälse",
    medicine_8 = "Zahn Pulver",
    medicine_9 = "gegen (Schnupfen|Kattarh|Engbrüstigkeit|Heiserkeit)",
    medicine_10 = "unfehlbar(es|e) Mittel",
    medicine_11 = "vortrefflich(en|es|e) Mittel",
    bandage = "Bandage|Bruchb(a|ä)nd",
    soap = "S(e|a)i(f|ff)e",
    washing = "Flecken Kug(el|le)",
    hygiene_1 = "Waschschw(a|ä)mm|Wasch-Schw(a|ä)mm|Handwasch|Waschwasser",
    hygiene_2 = "englische Erde",
    hair = "Bürste|(Chignon|Haar|Frauenzimmer|Locken|Fris(ie|i)r)k(ä|a)mm|Haarnadel|(Chignon|Haar|Frauenzimmer|Locken|Fris(ie|i)r)k(ä|a)mm|
    Haarpuder|Haar-Puder",
    cosmetic_1 = "Po(mm|m)ade|Puderlade|Maca(ss|s)ara|(K|C)osmeti(k|c)|Quaste",
    cosmetic_2 = "kosmetische Mittel",
    shaving = "Rasiermesser",
    perfume_1 = "Parf(ü|u)m|Parf(ü|u)merie|Fla(c|k)on|Rosenöl",
    perfume_2 = "eau de",
    perfume_3 = "(K|C)(o|ö)(l|ll)ni(sch|sches) Wasser",
    chemical_1 = "Ameisengeist|A(mm|m)oni(ak|um)|(Ch|C)onchil|Salpeter|Bittererde|
    Brocke(l|li)-A(mm|m|ml|mml)ung|Brocke(l|li)a(mm|m|ml|mml)ung|Far(be|b)kraut",
    chemical_2 = "Chemische Pr(ä|e)parate",
    chemical_3 = "Chemisches Pr(ä|e)parat",
    polish = "Schmiere|Wichse|Schella(ck|k)"
  )
  dict$neg <- list(
    food = "Cacao|Kaffee", # food in form of powder
    object = "Mittelstück|Pulverh(o|ö)rn|Pulverflaschen", # other objects
    book = "Abhandlung|Jahrb(u|ü)ch", # excludes book titles
    mill = "Pulvermühl|Pulver-Mühl", # mill for grinding things into powder
    other = "vermittel|mittelst|Medizinalbehörde|Medizinal-Kollegien|mittelschwer", # words containing "mittel" (no objects)
    textile = "Gel(d|t)beute-Quaste|Quastenspitz", # decoration for bags
    profession = "Bürstenbinder", # profession
    place = "Seifensiederey", # place for making sopa
    eau_1 = "bateau de", # stick
    eau_2 = "B(u|ü)reau", # cabinet
    eau_3 = "Eau de Noyeaux", # liquor, v1
    eau_4 = "Eau deNoyeaux", # liquor, v2
    eau_5 = "Eau de Vie", #liquor, v3
    eau_4 = "Tableau de" # picture of something
  )
  create_filter_output(dict)
}


#' Dictionary Weapons and Related Objects
#' @export
tagfilter_weapon <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Waffe|Ordonnanz-G",
    crossbow = "Armbrust",
    bayonet = "Bajo(nn|n)et",
    sword = "Degen|S(a|ä)bel|(K|C)arabiner",
    gun = "Flin(t|th)e|Gewehr|Pis(t|th)(o|oh)l|Büchse|Stutzer",
    cartridge = "Patron|Gibern|Kuge(l|ln)model|Kuge(l|ln)-Model",
    sheath = "Scheide"
  )
  dict$neg <- list(
    other = "Abscheiden|unterscheiden|bescheiden|Scheideweg", # no objects (containing "scheide")
    profession = "Scheidenmacher" # profession
  )
  create_filter_output(dict)
}

#' Dictionary Shop Equipment
#' @export
tagfilter_shopequip <- function(){
  dict <- list()
  dict$pos <- list(
    storage = "(C|K)omptoir-K(a|ä)sten|(C|K)omptoirk(a|ä)sten|W(aa|a)renk(ä|a)st|W(aa|a)ren-K(ä|a)st",
    display = "Gl(a|ä)sk(a|ä)st|Gl(ä|a)ser-K(a|ä)st|Glasglocke",
    desk = "Ladent((c|k)orpus|tisch)",
    cash = "Gel(d|t)((k|c)ass|trog|st(o|ö)ck)|Gel(d|t)-((K|C)ass|Trog|St(o|ö)ck)|Cassa",
    scale = "(Fuhr|Gold|Magazin|Schnell|W(a|aa)ren|Fl(a|ä)schen)w(ä|aa|a)g|(Fuhr|Gold|Magazin|Schnell|W(a|aa)ren|Fl(a|ä)schen)-W(ä|aa|a)g|
    (Einsatz|Eisen)gewicht|(Einsatz|Eisen)-Gewicht|lbstein|Messinggewicht|Waage|W(ä|aa|a)g(kengel|bal(ck|k)e)|W(ä|aa|a)g-(Kengel|Bal(ck|k)e)",
    scale_phrase = "Einsatz Gewicht",
    counting = "W(aa|a|ä)renzähl",
    general = "Handelsutensil|Ladengerä(th|t)scha(f|ff)t"
  )
  dict$neg <- list(
    storage = "Cassette|Cassettlein" # other storage objects
  )
  create_filter_output(dict)
}

#' Dictionary Tools and Instruments
#' @export
tagfilter_tool <- function(){
  dict <- list()
  dict$pos <- list(
    fire = "Bl(a|ä)(s|se|ß|ss)balg",
    woodworking = "Bohrer|Brenneisen|Drehstuhl|H(o|ö)bel|Hackb(a|ä)nk|Hack-B(a|ä)nk|H(a|ä)mmer|
    Säge|Schneideisen|Schraubst(o|ö)ck|Schreinerwerckzeug|Schneidmaschine|F(u|ü)gbl(o|ö)ch|Anhau",
    screw = "Leimschraube",
    knife = "Barbiermesser",
    metalworking = "Ambo(s|ß|ss)|Feldschmidt|Reibstein",
    stoneworking = "Schleifstein|Steinschleife",
    sharpening = "Streichriemen|Abziehleder",
    textileworking = "Strump(f|ff)w(e|ä)ber-Stuhl|Strump(f|ff)w(e|ä)berstuhl|Tuch-Pre(ss|ß)|Tuchpre(ss|ß)|Zwirnmaschine|Fadenzähler|
    Schwefelk(a|ä)st|se(i|y)denrad|Leinenwe(ber|b)stuhl|P(a|o)ssamen(ter|t)stuhl|P(a|o)ssamen(ter|t)-Stuhl|Zwirnmühl|Zwirn-Mühl|Se(i|y)denmühl|
    Se(i|y)den-Mühl|W(e|ä)bst(u|ü)l",
    textiles_1 = "eisernes Rädlein",
    mill = "Kammr(ä|a)d|Wasserr(a|ä)d|se(i|y)denwaage|se(i|y)den-Waage|se(i|y)denwindmaschine|se(i|y)denwind-Maschine|Schwungrad|Schwung-Rad|M(ü|a)hlstein|
    Wendelbaum",
    paper = "Siegelpre(ss|ß)",
    printing = "Kupferpre(ss|ß)|Kupferdruckerpre(ss|ß)|Drucktisch",
    straw = "Strohschneid",
    distilling = "Brenn(hafen|kessel|häu(s|ß)lein)",
    other = "Stemmeisen|Zange|Wasserwaag|Zirkel|Zollstab",
    general = "Wer(k|c|ck)zeug|Quincaillerie"
  )
  dict$neg <- list(

    strawberry = "Frambose", # strawberries and liquors (french)
    adjective = "ungehobelt" # description of wood
  )
  create_filter_output(dict)
}

#' Dictionary Stationary and Paperware
#' @export
tagfilter_stationary <- function(){
  dict <- list()
  dict$pos <- list(
    pen = "Bleistift|Griffel|Schreib(feder|zeug)",
    drawing = "Zeichnungs(kreide|papier)|Pinsel|Far(ben|b)k(ä|a)st|Far(ben|b)-K(ä|a)st",
    ink = "(T|D)inte",
    notebook = "Carnet|(Notiz|Schreib)(b(u|ü)ch|mappe)",
    paper = "Briefpre(ss|ß)|(Musik|Kreu(z|zlein)|(C|K)on(c|z)ept|Stab|Schreib|Druck|Post|Karten|Marmor|Noten|Pack|se(i|y)den|Brief|Stempel|Flie(ß|ss|s))pa(p|pp)ier|Pa(p|pp)ierrest|Pappier|
    (Musik|Kreu(z|zlein)|(C|K)on(c|z)ept|Stab|Schreib|Druck|Post|Karten|Marmor|Noten|Pack|se(i|y)den|Brief|Stempel|Flie(ß|ss|s))-Pa(p|pp)ier|Schreibkarte|Schreibrolle|Ma(k|c)ulatur",
    paper_2 = "Bögen Papier",
    paper_3 = "Resten Papier",
    paper_4 = "neu(e|en|es) Papier",
    cardboard = "Karton",
    slate = "Schieferta(f|v)el",
    seal = "P(e|i)(t|tt)scha(f|ff)t|Siegellack",
    other = "Lineal|Schreibunterlage|Federmesser|Sch(ee|e)re",
    general = "Schreibzeug"
  )
  dict$neg <- list(
    profession = "Tuchscheer", # profession
    garden = "(Haag|Baum)scheere|(Haag|Baum)-Scheere" # for cutting plants
  )
  create_filter_output(dict)
}

#' Dictionary Jewellery
#' @export
tagfilter_jewellery <- function(){
  dict <- list()
  dict$pos <- list(
    flowers = "(Ball|Kunst)blume|(Braut|To(dt|t)en)kr(a|ä)nz",
    necklace = "Hals(kette|band)", # ad "Kette" when completing dictionary (a lot of negatives)
    earring = "Oh(ren|r)(beh(a|ä)ng|ring)",
    bracelet = "Bracelet",
    pin = "Vorstecknadel",
    general = "Bijouteri"
  )
  dict$neg <- list(
    dog = "Hund" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Wood
#' @export
tagfilter_wood <- function(){
  dict <- list()
  dict$pos <- list(
    firewood = "Brennhol(tz|z)",
    coal_1 = "tannene Kohlen",
    coal_2 = "Par(th|t)e(y|i) Kohlen",
    material = "(Buchen|Tannen|Ahorn|Linden)(hol(tz|z)|st(a|ä)mm)|(Buchen|Tannen|Ahorn|Linden)-(St(a|ä)mm|Hol(tz|z))|Weichselrohr|Weichsel-Rohr",
    form = "(Boden|Scheit|(T|D)augen)hol(tz|z)|Fleckling|Führling|Hol(tz|z)-Raspel|Hol(tz|z)bock|Pf(a|ä)hl|(Prügel|Knebel)hol(tz|z)|Lohst(o|ö)ck|
    S(a|ä)gsp(ä|a|äh|ah)n|Stämme|Stammhol(tz|z)|Wellen|Fa(ss|ß)(d|t)auge|Plütschi|Sägb(a|ä)um|Drehsp(äh|ä)n",
    building = "Bauhol(tz|z)|Latten|Diele",
    unknown = "Kammhol(tz|z)"
# maybe include "hol(tz|z)" and "Klafter", but negatives are tricky
  )
  dict$neg <- list(
    copper = "Kup(ff|f)er(b|p)latte", # copper prints
    description = "unter Platten", # description of furniture
    adjective = "glatte", # adjective
    stone = "Marmor-Platte|Marmorplatte|Sandsteinplatte|Sandstein-Platte", # type of stones
    textiles = "Glatten|Bastplatten|Garn", # type of textiles
    ocr = "dieLehre", # ocr mistakes
    verb = "zuwellen", # verbs
    place_1 = "Schwar(z|tz)enpfahl|Schwar(z|tz)pfahl|Schwar(z|tz)en-Pfahl|Grünpfahlg(a|ä)", # placenames
    place_2 = "Schwar(z|tz)en Pfahl", # placenames
    immo = "Brennhol(tz|z)-Magazin|Behausung|Losament|Kammer|Stube" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Barrels and Bottles
#' @export
tagfilter_barrel <- function(){
  dict <- list()
  dict$pos <- list(
    bottle = "Bouteille(n|s)|Pi(è|e|é)ces|Gutter|Selterserwasser-Kr(ü|u)ge",
    barrel_1 = "Fa(ss|ß)\\b|Fä(ss|ß)er\\b|(Ö|Oe)hlst(u|ü)cklein",
    barrel_2 = "Stücklein Fa(s|ß|ss)",
    barrel_2 = "Stucklein Fa(s|ß|ss)"
    )
  dict$neg <- list(
    immo_1 = "Losament|Stube", # excludes immo ads, v1
    immo_2 = "Platz für", # excludes immo ads, v2
    wood = "(D|T)augenhol(tz|z)", # wood for barrels
    carneval = "Fa(ss|ß)nacht", # carneval
    wine = "\\bin (Bouteillen|Pi(è|e|é)ces|Gutter|Fa(ss|ß)|Fä(ss|ß)er|Stücklein)" # full barrels and bottles
  )
  create_filter_output(dict)
}

#' Dictionary Tobacco and Related Objects
#' @export
tagfilter_tobacco <- function(){
  dict <- list()
  dict$pos <- list(
    tobacco = "Taba(k|ck|c)|(Rauch|Schnupf)taba(k|ck|c)",
    pipe = "Pfeife|Taba(k|ck|ks|cks|c|cs)pfeife|Pfeifen(kopf|raumer|rohr)",
    storage = "((C|Z)iga(rr|r)en|Taba(k|ck|ks|cks|c|cs))-(B(ü|u)chs|Bux|Etuis|Dose|Beutel)|
((C|Z)iga(rr|r)en|Taba(k|ck|ks|cks|c|cs))(beutel|b(ü|u)chs|bux|etuis|dose)|
    Taba(k|ck|ks|cks|c|cs)kasten|Taba(r|k|ck|ks|cks|c|cs)ier|Tabattier|
    Taba(r|k|ck|ks|cks|c|cs)ti(e|è)r"
  )
  dict$neg <- list(
    immo = "Losament|Kuchin|Küche|Stube" # filters out immo ads
  )
  create_filter_output(dict)
}

#' Dictionary Hay and Straw
#' @export
tagfilter_hay <- function(){
  dict <- list()
  dict$pos <- list(
    hay = "H(e|ö)(u|ü|w)\\b|\\bEm(d|bd|db)\\b|H(e|ö)(u|ü|w)gra(ss|ß|s)",
    straw = "Stroh\\b",
    pasture_1 = "Klee|Herbstweid",
    pasture_2 = "Gra(s|ss|ß) auf"
  )
  dict$neg <- list(
    objects = "Stroh-(Sessel|Sack)|Stroh(sack|sessel)|Rockstroh|Stroh-Bord", # objects out of straw
    ocr = "französischeu|Kücheu", # ocr mistakes
    place = "auf dem Heu" # placename (Heuberg, finds those not in one word)
  )
  create_filter_output(dict)
}

#' Dictionary Unspecified Wooden Objects
#' @export
tagfilter_woodobject <- function(){
  dict <- list()
  dict$pos <- list(
    woodturning = "Drechslerw(aa|a)r|Drechsler-W(aa|a)r",
    carving = "Schnitzw(aa|a)r|Schnitz-W(aa|a)r",
    general = "H(o|ö)l(tz|z|zen)(decke|werk|w(a|aa)r)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Dung
#' @export
tagfilter_dung <- function(){
  dict <- list()
  dict$pos <- list(
    dung_1 = "(K(ü|u)h|Pfer(d|de)|Schwei(n|ne)|Tauben|Ziegen)(bau|mist)", # possibly add "\\bBau\\b", but a lot of negatives at this point
    dung_2 = "verwährter Bau"
  )
  dict$neg <- list(
    other = "Wasser-Kunst", # "Bau- und Wasserkunst"
    work = "Maschinen-Bau", # work
    authorities = "Bau-Colleg" # insitution
  )
  create_filter_output(dict)
}

#' Dictionary Plants
#' @export
tagfilter_plant <- function(){
  dict <- list()
  dict$pos <- list(
    tree = "B(a|ä)um|Obstb(ä|a)um",
    bush_1 = "Gar(th|t)enbu(chs|x)|Oleander",
    bush_2 = "grüner Bu(chs|x)",
    flower = "Blumenzwiebel|Blumen-Zwiebel|Nägelin",
    flower_2 = "Blumen Zwiebel",
    seed = "S(a|aa)men\\b",
    seedling = "Setzling",
    root = "(Dahlien|Sparglen)-Wur(tz|z)(el|e)n",
    general = "Pflan(z|tz)e|Gewächs"

  )
  dict$neg <- list(
    name_1 = "Wittwe Nägelin", # family name 1
    name_2 = "Jakob Nägelin", # family name 2
    name_3 = "Adam Nägelin", # family name 3
    fair = "Bude|Messe", # descriptions of fair stalls by trees
    wine = "Wein", # sometimes described as "bestes Gewächs"
    book = "Tafeln|Meisterstück|Schriften|Prospekt|Buchbinder", # excludes book illustrations
    adjective = "arbeitsam|heisam|heilsam|gemeinsam|grausam|zusamen|ehrsam|erwerbsam", # adjectives with "samen"
    verb = "samen halten", # verb
    name = "Baumgartner|Baumann|Baumeister", # family names
    textile = "Baumwoll", # cotton
    other_1 = "Baum-Trotte|Baumtrotte|Weinb(a|ä)um|Baumleiter|Baum-Messer|Baummesser|Baumaterial|
    Brosamen|Pflanzenschleim|Wendelbaum|Wendel-Baum|Brennbaum|Schlagb(a|ä)um|Baum-Garten", # other object, v1
    other_2 = "Baum Trotte", # other objects, v2
    place_1 = "Maulbaum|Bauma\\b", # place names, v1
    place_2 = "dem Bäumlein", # place names, v2
    immo = "Matten|Feld|Baumgarten|Liegenscha(f|ff)t|Losamen|Wohnh(a|ä)us|Wohnung|Wirtshaus|Scheuer|Juchart", # excludes immo ads with plants
    description = "nu(ß|ss)b(ä|a)umern|nu(ß|ss)b(ä|a)umen|baumrund|kirschbaum|Nu(ss|ß)baumhol(tz|z)|Buchsbaumhol(tz|z)|Nu(ß|ss)baumgehäus" # description of furniture and clothes
  )
  create_filter_output(dict)
}

#' Dictionary Glasses and Optical Instruments
#' @export
tagfilter_glasses <- function(){
  dict <- list()
  dict$pos <- list(
    glasses = "Brille|(Augen|Lese)gl(a|ä)s",
    lens = "Luppe",
    opera = "Lorgnette|Perspektiv|Lorgnon",
    telescope = "Telescop|Fernr(o|ö)hr",
    microscope = "Micros(c|k)op",
    laterna = "Laterna magica",
    optical_1 = "optische Instrumente",
    optical_2 = "Optik\\b",
    other = "Landscha(f|ff)tsspiegel"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Soil, Gravel, Lime and Related Goods
#' @export
tagfilter_soil <- function(){
  dict <- list()
  dict$pos <- list(
    soil = "(Garten|Matten)grund|(Garten|Matten)-Grund",
    gravel = "Birsgrien|Kieselsteine|Asphalt",
    lime = "Kal(k|ch)",
    red = "ro(t|th)e W(aa|a)re",
    plaster_1 = "gebr(an|au)nter G(y|i)ps",
    ash = "(gute|buchene) Asch(e|en)\\b"
  )
  dict$neg <- list(
    name = "Kalkbrenner", # family name
    pharmacy = "Chlorkalk", # pharmacy object
    immo_2 = "Wohnung|Kammer|Losament|Stube|Küche|Laden|beziehen|Fronfast|Keller|Zimmer" # excludes immo ads
  )
  create_filter_output(dict)
}

#' Dictionary Agriculture
#' @export
tagfilter_agriculture <- function(){
  dict <- list()
  dict$pos <- list(
    tool = "Dreschflegel|(Heu|Lad|Lade)gabel|Pfl(u|ü)g|Sense|Rechen|Mattenmesser|Matten-Messer",
    trolley = "Güllenkarren|Heuwagen",
    storage = "Obsthurte|Obstk(o|ö)rb",
    wine = "Rebsteck",
    animal = "Mastbütte|Bienenst(o|ö)ck"
  )
  dict$neg <- list(
    name = "Pflugg(ä|a)", # place name
    ocr = "senseit|wachsense", # ocr mistakes (contain "sense")
    name = "Rechenmacher", # family name
    other = "Rechenscha(f|ff)t|Rechenrat|Rechenmeister", # no objects
    verb = "sprechen|brechen", # berbs containing "rechen"
    maths = "Rechen(kunst|tafel)|Rechen-(Tafel|Kunst)" # mathematics
  )
  create_filter_output(dict)
}

#' Dictionary Riding
#' @export
tagfilter_riding <- function(){
  dict <- list()
  dict$pos <- list(
    saddle = "S(a|ä)ttel|Steigbügel",
    bridle = "(Pfer(de|d)|Reit)((b|ge)iss|geschir)|(Pfer(de|d)|Reit)-((B|Ge)iss|Geschir)",
    whip = "Peitsche",
    spur = "Sporren",
    blanket = "Pfer(ded|dd)ecke"
  )
  dict$neg <- list(
    toy = "Schwungpferd" # hobby horse
  )
  create_filter_output(dict)
}

#' Dictionary Objects Related to Wells and Fountains
#' @export
tagfilter_well <- function(){
  dict <- list()
  dict$pos <- list(
    well = "(Brun(n|nen)|Wasser)stein|(Brun(n|nen)|Wasser)-Stein|Brun(n|nen)trog|Brun(n|nen)-Stein|Brun(n|nen)-Trog|
    Brunnstiefel|Brunn-Stiefel",
    pump = "Wasserpumpe|Wasser-Pumpe",
    parts = "Zu(ge|be)hörde zu einem (Ziehbrunn|Brunn|Wasser)"
  )
  dict$neg <- list(
    immo = "Wohnung|Kammer|Losament|Stube|Küche|Laden|beziehen|Fronfast|Keller|Zimmer" # excludes immo ads
  )
  create_filter_output(dict)
}


#' Dictionary Naturalia and Minerals
#' @export
tagfilter_naturalia <- function(){
  dict <- list()
  dict$pos <- list(
    naturalia = "Muscheln|Schmetterling",
    minerals = "Versteinerung|Mineralien"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Containers
#' @export
tagfilter_container <- function(){
  dict <- list()
  dict$pos <- list(
    container = "Z(u|ü)ber|Eimer|Trog|B(ö|o|u)(ck|k|ckt|kt)(lin|re)"
  )
  dict$neg <- list(
    ocr = "beimerian", # ocr mistake
    name_1 = "Fr. Böcklin", # family name, v1
    name_2 = "Fr.Böcklin", # family name, v2
    name_3 = "Seimer", # family name, v3
    place = "Arlesheimer|Türckheimer", # place names
    cash = "Gel(d|t)trog", # object for holding cash
    fire = "Feuereimer|Feuer-Eimer", # fire prevention
    well = "Brunntrog", # well
    other = "Geheimerat|Zubereitung", # no objects
    trolley = "Trogkarren", # trolley
    verbs = "betrogen|zubereite|hinzuberuf" # verbs
  )
  create_filter_output(dict)
}

#' Dictionary Fire Starters
#' @export
tagfilter_firestart <- function(){
  dict <- list()
  dict$pos <- list(
    lighter = "Feuer(stahl|zeug|schw(a|ä)mm)|Zündmaschine",
    match = "(Schwefel|Zünd)h(o|ö)l(z|tz)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Fire Protection and Fire Extinguishers
#' @export
tagfilter_extinguisher <- function(){
  dict <- list()
  dict$pos <- list(
    extinguisher = "Feuerspritze|Feuer-Spritze",
    bucket_1 = "Feuereimer|Feuer-Eimer",
    bucket_2 = "Feuer Eimer"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Fireworks
#' @export
tagfilter_firework <- function(){
  dict <- list()
  dict$pos <- list(
    firework = "Feuerwerk"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Antiques
#' @export
tagfilter_antique <- function(){
  dict <- list()
  dict$pos <- list(
    antique_1 = "Antique-K(o|ö)pf|Antiquek(o|ö)pf|Statue",
    antique_2 = "römisch(e|en) Figur",
    antique_3 = "Antique K(ö|o)pf"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Keys
#' @export
tagfilter_key <- function(){
  dict <- list()
  dict$pos <- list(
    home = "Hausschlüssel",
    furniture = "Schrankschlüssel|Uhrenschlüssel"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Walking Canes
#' @export
tagfilter_cane <- function(){
  dict <- list()
  dict$pos <- list(
    cane = "Spazierst(o|ö)ck"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Objects Related to Wine
#' @export
tagfilter_wineobject <- function(){
  dict <- list()
  dict$pos <- list(
    production = "Weintrott|Z(a|ä)pfen",
    storage = "Weinb(ü|u)(k|ck)te",
    consumption = "Wei(nhah|nhäh|nha|nhä)nen|Wein-H(ah|a)nen|Wein-H(äh|ä)nen|Wein(krause|schild)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Ropes
#' @export
tagfilter_rope <- function(){
  dict <- list()
  dict$pos <- list(
    rope = "(Spann|Zug|(B|P)lunder)(strick|seil)|(Spann|Zug|(B|P)lunder)-(Strick|Seil)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Objects for Taverns and Inns
#' @export
tagfilter_tavernobject <- function(){
  dict <- list()
  dict$pos <- list(
    sign = "Wir(th|t)sschild",
    general = "Wir(t|th)scha(f|ff)tsgerät"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Animal Feed
#' @export
tagfilter_feed <- function(){
  dict <- list()
  dict$pos <- list(
    potatoes = "Schwei(n|ns)erdäpfel|Schwei(n|ns)-Erdäpfel",
    feed_1 = "Fütterung der",
    feed_2 = "Mästung der",
    feed_3 = "zum verfüttern",
    ocr = "zumverfüttern"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Misc Objects (Unspecified)
#' @export
tagfilter_miscobject <- function(){
  dict <- list()
  dict$pos <- list(
    misc = "Gegenstände|Waarenlager",
    magnets = "Magnet"
  )
  dict$neg <- list(
    other = "Berathungsgegenständ|magnetisch|Magnetismus" # no objects
  )
  create_filter_output(dict)
}
