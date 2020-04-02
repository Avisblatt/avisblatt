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

#' Dictionary Umbrellas and Related Objects (umbrella cases and umbrella cloth)
#' @export
tagfilter_umbrella <- function(){
  dict <- list()
  dict$pos <- list(
    rain = "Paraplu[i|y|v|g]|Regenschirm|Pareplu[i|y|v|g]|genschirm",
    sun = "Sonnenschirm|Ombrelle|Parasol|Parresol"

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
    whip = "Chaisepeitsche|Schäsepeitsche|Chaise-Peitsche|Schäse-Peitsche",
    harness = "K(u|ü)mmet-Geschir|K(u|ü)mmetgeschir|Chaisegeschir|Chaise-Geschir|Schäsegeschir|Schäse-Geschir|
    Kutsc(h|he|hen)geschir| Kutsc(h|he|hen)-Geschir",
    coach = "W(a|aa)gen|Wägen",
    sleigh = "Familienschlitten|Schlitten|Haußschlitten",
    family = "Familie-W(a|aa)gen|Familienw(a|aa)gen",
    travel = "Reisew(a|aa)gen",
    riding = "Reitw(a|aa)gen|Reitwägelein",
    charabanc = "Char-à-banc|Char-a-banc",
    carriage = "Malaben|Malborough|Berline//b|(K|C)abriolet|Coupe|Kutsch//b|Kutsche//b|Kutschen//bKütschlein|(T|D)roscheke|
    (T|D)ro(sch|tsch)ke|Trosque|Pascule|Pout//b|Kutschen-Berlin|Rei(ße|se|ß|s)kutsch|Rei(ße|se|ß|s)-Kutsch|Engglai-Kutsch|
    Bernerw(a|ä)ge|Berner-W(a|ä)ge",
    chaise = "Chai(se|s)|Chaiselein|Chaislein|Schäse|Schäslein|Schäselein",
    characteristic = "einspännig|zwe(y|i)spännig|4rädrig|(1|2|3|4)spännig|zwe(i|y)rädrig",
    parts = "Schwanenhäls|Geschell|Kutschen-Kasich|Kutschenkasich"
  )
  dict$neg <- list(
    trolley = "Leiterwagen|Bauernwagen|Bauern-Wagen|Dielenwagen|Dielen-Wagen|Bauernwägel", # trolleys, see other category
    verb = "wagen wir", # meaning of to dare something
    burial = "Todtenwagen", # transportation of corpses
    immo = "Wagenschopf|W(a|aa)genremise|W(a|aa)gen-Remise", # shelter for a carriage
    mail = "Post(a|aa|ä)gen|Postkutsche|Post-Anzeige|Postanzeige|Post-Chaise|Postillon", # mail coach
    measure_1 = "W(a|aa|ä)gen voll", # measurement of something in coaches, v1
    measure_2 = "Grube|w(a|aa|ä)genwei(s|ß)|Heu|Emd|verwährt", # carloads and objects measured in carloads
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
    toy = "Kinder-|Kinder(chais|wag|kutsch)", # excludes toy carriges for children
    animal = "Chaise-Pferd|Reisepferd|Chaisepferd" # horses for drawing carriages
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

#' Dictionary Building Components
#' @export
tagfilter_building <- function(){
  dict <- list()
  dict$pos <- list(
    well_1 = "Br(u|ü)(nn|n)stein",
    well_2 = "Br(u|ü)(nn|n) Stein",
    door = "(Th|T)üre|Vor(th|t)üre|(Th|T)ürschlinge", # possibly too many immo ads with "Thüre", negatives are tricky
    window = "Fenster|(K|C)reu(z|tz)stock|(K|C)reu(z|tz)stöck|Fensterflügel|Fensterpritsch|Vorfenster",
    wood = "Bauhol(z|tz)|Latten|Diele",
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

#' Dictionary Trolleys (Carriages for Transportation of Objects, not People)
#' @export
tagfilter_trolley <- function(){
  dict <- list()
  dict$pos <- list(
    handcart = "Handwägel|Leiterwägel|Leiterwagen|Sto(ss|ß)karren|Trogkarre|K(a|ä)rren",
    trolley = "Bauern-W(a|ä)ge|Bauernw(a|ä)ge|Dielenwagen|Dielen-Wagen|Fuhrwerk",
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
    soap = "Sei(f|ff)e|Sai(ff|f)e",
    washing = "Flecken Kug(el|le)",
    hygiene_1 = "Waschschw(a|ä)mmWasch-Schw(a|ä)mm|Handwasch|Waschwasser",
    hygiene_2 = "englische Erde",
    hair = "Bürste|Chignonk(ä|a)mm|Frauenzimmerk(ä|a)mm|Haark(ä|a)mm|Lockenk(ä|a)mm|Fris(ie|i)rk(ä|a)mm|
    Chignon-K(ä|a)mm|Frauenzimmer-K(ä|a)mm|Haar-K(ä|a)mm|Locken-K(ä|a)mm|Fris(ie|i)r-K(ä|a)mm|Haarnadel|
    Haarpuder|Haar-Puder",
    cosmetic_1 = "Po(mm|m)ade|Puderlade|Maca(ss|s)ara|(K|C)osmetik|Kosmeti(k|c)|Quaste",
    cosmetic_2 = "kosmetische Mittel",
    shaving = "Rasiermesser",
    perfume_1 = "Parf(ü|u)m|Parf(ü|u)merie|Fla(c|k)on|Rosenöl",
    perfume_2 = "eau de",
    perfume_3 = "(K|C)(o|ö)(l|ll)ni(sch|sches) Wasser",
    chemical_1 = "Ameisengeist|A(mm|m)oniak|A(mm|m)onium|(Ch|C)onchil|Salpeter|Bittererde|
    Brockel-A(mm|m)ung|Brockela(mm|m)ung|Far(be|b)kraut",
    chemical_2 = "Chemische Präparate",
    chemical_3 = "Chemisches Präparat",
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
    general = "Waffe",
    crossbow = "Armbrust",
    bayonet = "Bajo(nn|n)et",
    sword = "Degen|S(a|ä)bel|(K|C)arabiner",
    gun = "Flin(t|th)e|Gewehr|Pis(t|th)ol|Büchse",
    cartridge = "Patron|Gibern",
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
    storage = "(C|K)omptoir-Kasten|(C|K)omptoirkasten|(C|K)omptoir-Kästen|(C|K)omptoirkästen|W(aa|a)renkäst|
    W(aa|a)renkast|W(aa|a)ren-Käst|W(aa|a)ren-Kast",
    display = "Glask(a|ä)st|Gläser-K(a|ä)st|Glaser-K(a|ä)st|Glasglocke",
    desk = "Ladent(c|k)orpus|Ladentisch",
    cash = "Gel(d|t)(k|c)ass|Gel(d|t)-(K|C)ass|Gel(d|t)trog|Gel(d|t)st(o|ö)ck|Cassa",
    scale = "Fuhrw(ä|aa|a)g|Einsatzgewicht|Einsatz-Gewicht|Goldw(ä|aa|a)g|lbstein|Magazin-W(ä|aa|a)g|
    Schnellw(ä|aa|a)g|Waaren-W(ä|aa|a)g|Waarenw(ä|aa|a)g|Waren-W(ä|aa|a)g|Warenw(ä|aa|a)g|Messinggewicht|
    Schnell-W(ä|aa|a)g|Flaschen-W(ä|aa|a)g|Flaschenw(ä|aa|a)g",
    counting = "W(aa|a)renzähl",
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
    fire = "Bla(s|se|ß|ss)balg",
    woodworking = "Bohrer|Brenneisen|Drehstuhl|H(o|ö)bel|Hackb(a|ä)nk|Hack-B(a|ä)nk|H(a|ä)mmer|
    Säge|Schneideisen|Schraubst(o|ö)ck|Schreinerwerckzeug|Schneidmaschine|F(u|ü)gbl(o|ö)ch",
    screw = "Leimschraube",
    knife = "Barbiermesser",
    metalworking = "Ambo(s|ß|ss)|Feldschmidt|Reibstein",
    stoneworking = "Schleifstein|Steinschleife",
    sharpening = "Streichriemen|Abziehleder",
    textileworking = "Strump(f|ff)w(e|ä)ber-Stuhl|Strump(f|ff)w(e|ä)berstuhl|Tuch-Pre(ss|ß)|Tuchpre(ss|ß)|Zwirnmaschine|Fadenzähler|
    Schwefelk(a|ä)st|se(i|y)denrad|Leinenwe(ber|b)stuhl|P(a|o)ssamen(ter|t)stuhl|P(a|o)ssamen(ter|t)-Stuhl|Zwirnmühl|Zwirn-Mühl|Se(i|y)denmühl|Se(i|y)den-Mühl",
    mill = "Kammr(ä|a)d|Wasserr(a|ä)d|se(i|y)denwaage|se(i|y)den-Waage|se(i|y)denwindmaschine|se(i|y)denwind-Maschine|Schwungrad|Schwung-Rad|M(ü|a)hlstein|
    Wendelbaum",
    paper = "Siegelpre(ss|ß)",
    printing = "Kupferpre(ss|ß)|Kupferdruckerpre(ss|ß)|Drucktisch",
    straw = "Strohschneid",
    distilling = "Brennhafen|Brennkessel|Brennhäu(s|ß)lein",
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
    pen = "Bleistift|Griffel|Schreibfeder|Schreibzeug",
    drawing = "Zeichnungs(kreide|papier)|Pinsel|Far(ben|b)käst|Far(ben|b)-Käst|Far(ben|b)kast|Far(ben|b)-Kast",
    ink = "(T|D)inte",
    notebook = "Carnet|Notizb(u|ü)ch|Schreibb(u|ü)ch|Schreibmappe",
    paper = "Briefpre(ss|ß)|(Schreib|Druck|Post|Karten|Marmor|Noten|Pack|se(i|y)den|Brief|Stempel)papier|Papierrest|
    (Schreib|Druck|Post|Karten|Marmor|Noten|Pack|se(i|y)den|Brief|Stempel)-Papier|Schreibkarte|Schreibrolle|Makulatur",
    cardboard = "Karton",
    slate = "Schieferta(f|v)el",
    seal = "P(e|i)(t|tt)scha(f|ff)t|Siegellack",
    other = "Lineal|Schreibunterlage|Federmesser|Scheere",
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
    flowers = "Ballblume|Brautkr(a|ä)nz|Todtenkr(a|ä)nz|Totenkr(a|ä)nz|Kunstblume",
    necklace = "Halskette",
    earring = "Ohrenbeh(a|ä)ng|Ohrenring|Ohrbeh(a|ä)ng|Ohrring",
    bracelet = "Bracelet",
    pin = "Vorstecknadel",
    general = "Bijouteri"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
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
    material = "Buchenhol(tz|z)|Tannenhol(tz|z)|Ahornst(a|ä)mm|Ahorn-St(a|ä)mm|Weichselrohr|Weichsel-Rohr|Lindenst(a|ä)mm|Linden-St(a|ä)mm",
    form = "Bodenhol(tz|z)|Fleckling|Führling|hol(tz|z)-Raspel|hol(tz|z)bock|Pf(a|ä)hl|(Prügel|Knebel)hol(tz|z)|Lohst(o|ö)ck|
    S(a|ä)gsp(ä|a|äh|ah)n|Scheithol(tz|z)|Stämme|Stammhol(tz|z)|Wellen|(D|T)augenhol(tz|z)|Fa(ss|ß)(d|t)auge|Plütschi|Sägb(a|ä)um|Drehsp(äh|ä)n",
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
    bottle = "Bouteillen|Pi(è|e|é)ces|Gutter|Selterserwasser-Kr(ü|u)ge",
    barrel_1 = "Fa(ss|ß)\\b|Fä(ss|ß)er\\b",
    barrel_2 = "Stücklein Fa(s|ß|ss)", # doesn't work - why?
    barrel_2 = "Stucklein Fa(s|ß|ss)" # doesn't work - why?
  )
  dict$neg <- list(
    immo_1 = "Losament|Stube", # excludes immo ads, v1
    immo_2 = "Platz für", # excludes immo ads, v2
    wood = "(D|T)augenhol(tz|z)", # wood for barrels
    carneval = "Fa(ss|ß)nacht", # carneval
    wine = "in (Bouteillen|Pi(è|e|é)ces|Gutter|Fa(ss|ß)|Fä(ss|ß)er|Stücklein)" # full barrels and bottles
  )
  create_filter_output(dict)
}

#' Dictionary Tobacco and Related Objects
#' @export
tagfilter_tobacco <- function(){
  dict <- list()
  dict$pos <- list(
    tobacco = "Taba(k|ck|c)|Rauchtaba(k|ck|c)|Schnupftaba(k|ck|c)",
    pipe = "Pfeife|Taba(k|ck|ks|cks|c|cs)pfeife|Pfeifenkopf|Pfeifenraumer|Pfeifenrohr",
    storage = "(C|Z)igarren-Büchse|(C|Z)igarren-Etuis|(C|Z)igarrenbüchse|(C|Z)igarrenetuis|Taba(k|ck|ks|cks|c|cs)dose|
    Taba(k|ck|ks|cks|c|cs)beutel|Taba(k|ck|ks|cks|c|cs)kasten|Taba(r|k|ck|ks|cks|c|cs)ier|
    Taba(r|k|ck|ks|cks|c|cs)tier|Taba(k|ck|ks|cks|c|cs)b(u|ü)chs| Taba(k|ck|ks|cks|c|cs)-B(u|ü)chs"
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
    hay = "He(u|ü)\\b|\\bEm(d|bd|db)\\b|Heugra(ss|ß|s)",
    straw = "Stroh\\b",
    pasture = "Klee|Herbstweid"
  )
  dict$neg <- list(
    objects = "Stroh-Sessel|Rockstroh|Stroh-Bord", # objects out of straw
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
    woodturning = "Drechslerwaar|Drechsler-Waar",
    carving = "Schnitzwaar|Schnitz-Waar",
    general = "hol(tz|z)decke|H(ö|o)lzenwerk|H(ö|o)lzwerk"
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
    dung_1 = "\\bBau\\b|K(ü|u)hbau|Pfer(d|de)bau|Schwei(n|ne)bau|Taubenmist|Ziegenbau",
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
    tree = "B(a|ä)um|Bäume|Obstb(ä|a)um",
    bush_1 = "Gar(th|t)enbu(chs|x)|Oleander",
    bush_2 = "grüner Bu(chs|x)",
    flower = "Blumenzwiebel|Blumen-Ziebel|Nägelin",
    flower_2 = "Blumen Zwiebel",
    seed = "S(a|aa)men\\b",
    seedling = "Setzling",
    root = "Dahlien-Wurzeln|Sparglen-Wurtz(el|e)n",
    general = "Pflanze|Pflantze|Gewächs"

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
    Brosamen|Pflanzenschleim|Wendelbaum|Wendel-Baum|Brennbaum", # other object, v1
    other_2 = "Baum Trotte", # other objects, v2
    place = "Maulbaum|Bauma\\b", # place names
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
    glasses = "Brille|Augengl(a|ä)s|Lesegl(a|ä)s",
    lens = "Luppe",
    opera = "Lorgnette|Perspektiv|Lorgnon",
    telescope = "Telescope|Fernr(o|ö)hr",
    microscope = "Micros(c|k)op",
    laterna = "Laterna magica",
    optical = "optische Instrumente",
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
    soil = "Gartengrund|Garten-Grund",
    gravel = "Birsgrien|Kieselsteine",
    lime = "Kal(k|ch)",
    red = "ro(t|th)e Waare",
    plaster_1 = "gebrannter G(y|i)ps",
    plaster_1 = "gebraunter G(y|i)ps"
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
    maths = "Rechenkunst|Rechentafel|Rechen-Tafel" # mathematics
  )
  create_filter_output(dict)
}

#' Dictionary Riding
#' @export
tagfilter_riding <- function(){
  dict <- list()
  dict$pos <- list(
    saddle = "S(a|ä)ttel|Steigbügel",
    bridle = "Pferdebiss|Pfer(dg|deg)eschirr|Reitgeschirr|Pfer(d|de)-Geschirr|Reit-Geschirr",
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
    well = "Wasserstein|Wasser-Stein|Brun(n|nen)stein|Brun(n|nen)trog|Brun(n|nen)-Stein|Brun(n|nen)-Trog",
    pump = "Wasserpumpe|Wasser-Pumpe"
  )
  dict$neg <- list(
    immo_2 = "Wohnung|Kammer|Losament|Stube|Küche|Laden|beziehen|Fronfast|Keller|Zimmer" # excludes immo ads
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
    container = "Z(u|ü)ber|Eimer|Trog|Bö(ck|k)lin|Bo(ck|k)te|Bu(ck|k)te"
  )
  dict$neg <- list(
    ocr = "beimerian", # ocr mistake
    name_1 = "Fr. Böcklin", # family name, v1
    name_2 = "Fr.Böcklin", # family name, v2
    place = "Arlesheimer|Türckheimer", # place names
    cash = "Gel(d|t)trog", # object for holding cash
    fire = "Feuereimer|Feuer-Eimer", # fire prevention
    well = "Brunntrog", # well
    other = "Geheimerat|Zubereitung", # no objects
    trolley = "Trogkarren", # trolley
    verbs = "betrogen|zubereite" # verbs
  )
  create_filter_output(dict)
}

#' Dictionary Fire Starters
#' @export
tagfilter_firestart <- function(){
  dict <- list()
  dict$pos <- list(
    lighter = "Feuerstahl|Feuerzeug|Zündmaschine|Feuerschw(a|ä)mm",
    match = "Schwefelh(o|ö)lz|Zündh(o|ö)lz"
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
    antique_2 = "römische Figur"
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
    consumption = "Wei(nhah|nhäh|nha|nhä)nen|Wein-H(ah|a)nen|Wein-H(äh|ä)nen|Weinkrause|Weinschild"
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
    rope = "Spannstrick|Zugseil|(B|P)lunderseil|Spann-Strick|Zug-Seil|(B|P)lunder-Seil"
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
    potatoes = "Schwei(n|ns)erdäpfel|Schwei(n|ns)-Erdäpfel"
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
    misc = "Gegenstände"
  )
  dict$neg <- list(
    other = "Berathungsgegenständ" # no objects
  )
  create_filter_output(dict)
}
