#' Tagfilter Property
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads mentioning things not covered by the 
#' tagfilter families textiles, foodstuff and household. These include everything from
#' different accessories like mercery, umbrellas or purses to storage objects, carriages 
#' and dung.
#'
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_things
#' @seealso tagfilters
NULL



#' @rdname tagfilter_things
#' @export
tagfilter_mercery <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    pin = 'Vorstecknadel',
    buckle = 'Schnalle',
    suspender = 'Hosentr\u00e4ger',
    belt = 'G\u00fcrtel|Ceintur',
    button = 'Kn(o|\u00f6)pf'
  )
  dict$neg <- list(
    cleaning = 'Schnallenb\u00fcrste', # cleaning utensil
    work = 'Lehre', # excludes work ads
    clothes = 'Kn(o|\u00f6)p(f|ff)l(o|\u00f6)ch', # descriptions of clothes
    immo = 'Losament|Gelegenheit', # excludes immo ads
    cane = 'Spannisches Rohr', # walking canes with 'Knopf'
    other = 'Waidsack|S\u00e4cke|Ofenstange|Deckel|Redincorte' # other objects with buckles or buttons
    # 'Knopfmacher' as a profession relevant for category, exclusion of work and immo ads should exclude those irrelevant to textile category
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_things
#' @export
tagfilter_bag <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bag = 'Tasche|Seckel|Beutel|S(\u00e4|a)ck|Ridicule|T\u00e4schlein|N(\u00e9|e)cessaire',
    rucksack = 'Felleisen'
  )
  dict$neg <- list(
    textile = 'Sackzwilch', # textiles
    profession = 'S\u00e4ckler', # profession
    immo = 'Wohnung|Losament|Gelegenheit', # excludes immo ads
    bigbags = '(Stroh|Korn|Be(t|th|tt)|Wa(i|y)d)s(\u00e4|a)ck', # big bags (bedding, straw, corn)
    measure_1 = '\\\\d.\\\\sS\u00e4ck', # removes 'S\u00e4ck' as measurement, v1
    measure_2 = '\\\\d\\\\sS\u00e4ck', # removes 'S\u00e4ck' as measurement, v2
    measure_3 = '\\\\d.\\\\sSack', # removes 'Sack' as measurement, v3
    measure_4 = '\\\\d\\\\sSack', # removes 'Sack' as measurement, v4
    measure_5 = 'hundert S\u00e4ck', # removes 'S\u00e4ck' as measurement, v5
    measure_6 = 'sackweis', # removes 'Sack' as measurement, v6
    fruit = 'Fr\u00fcchte', # filters out ads with bags of fruit
    coffee = 'Kaffee', # filters out ads with bags of coffee
    cloth = 'Beuteltuch', # cloth for bags
    place = 'S\u00e4ckingen', # placename
    potato = 'Erd\u00e4pfel', # filters out ads with bags of potatoes
    oat = 'Habers(\u00e4|a)ck', # bags of oats
    travel = '(Fu(\u00df|ss)|Nacht)s(\u00e4|a)ck', # leather blanket for carriage passengers and sleeping bags
    books = '(Taschen|Haus|Sack)((k|c)alender|b(u|\u00fc)ch)|(Taschen|Haus|Sack)-((k|c)alender|b(u|\u00fc)ch)', # pocket books and calendars
    pocketsize = 'Taschen-Perspektiv|Taschenspiel|Taschenfeuer|(Taschen|Sack)(uhr|pist(o|oh)le)|(Taschen|Sack)-(Uhr|Pist(o|oh)le)|Taschen-Latern|Taschenlatern|(Taschen|Sack|Feder)messer', # pocketsize versions of other objects
    administration = 'Seckelmeister|Seckelrechnung|Seckel-', # administrative positions and words containing 'Seckel'
    description = 'Taschenformat|Taschenform' # description of pocketsize of an object
  )

  create_filter_output(dict)

}



#' @rdname tagfilter_things
#' @export
# problematic category with dictionary, cause words like Wolle or Leder are often
# given as part of the description of clothing or other textiles
# therefore here only unambiguous words so far
tagfilter_animalraw <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    leather = 'Sohl(leder|h(a|\u00e4)ut)|Sohl-(Leder|H(a|\u00e4)ut)|Zeugleder',
    feather = 'Be(tt|th)f(eh|e)de|Flaum|Eitherd(aun|un)|Federb(u|\u00fc)sch|Federen|(Pf|F)laumfed',
    horsehair = '(Ro(ss|\u00df)|Pfer(d|de))haar'

  )
  dict$neg <- list(
    animal = 'Federen-Pudel', # kind of dog
    bedding = 'Deckbett', # bedding
    adjective = 'flaumen(es|e)', # adjective/ description
    carriage = 'Chaise', # description of carriages ('Federen')
    upholstery = 'ausgepolstert', # removes upholstery containing horsehair
    household = 'Ro\u00dfhaarsieb', # strainer from horsehair
    garment = 'Ro\u00dfhaar-Cravat|ro\u00dfhaarne\\\\s|ro\u00dfhaarene\\\\s', # garments from horsehair
    plums = 'Pflaumen\\\\s|Pflaumenb(a|\u00e4)um' # plums
  )
  create_filter_output(dict)

}

#' @rdname tagfilter_things
#' @export
tagfilter_plantraw <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    alpinegrass = 'Waldhaar'

  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)

}

#' @rdname tagfilter_things
#' @export
tagfilter_umbrella <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    rain = 'Par(a|e)plu(i|y|v|g)|Regenschirm|genschirm',
    sun = 'Sonnenschirm|Ombrelle|Pa(r|rr)(a|e)sol'

  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder, no negatives necessary so far
  )

  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_carriage <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    whip = '(Chai|Sch\u00e4)sepeitsche|(Chai|Sch\u00e4)se-Peitsche',
    harness = '(K(u|\u00fc)mmet|Chaise|Sch\u00e4se)-Geschir|(K(u|\u00fc)mmet|Chaise|Sch\u00e4se)geschir|Kutsc(h|he|hen)geschir|Kutsc(h|he|hen)-Geschir',
    coach = 'W(a|aa|\u00e4)gen',
    sleigh = '(Familien|Hau(\u00df|s|ss))schlitten|Schlitten',
    family = 'Familie-W(a|aa)gen|Familienw(a|aa)gen',
    travel = 'Reisew(a|aa)gen',
    riding = 'Reitw(a|aa)gen|Reitw\u00e4gelein',
    charabanc = 'Char\\\\-(\u00e0|a)\\\\-banc',
    carriage = 'Malaben|Malborough|Berline//b|(K|C)abriolet|Coupe|Kutsch//b|Kutsche//b|Kutschen//bK\u00fctschlein|(T|D)roscheke|
    (T|D)ro(sch|tsch)ke|Trosque|Pascule|Pout//b|Kutschen-Berlin|Rei(\u00dfe|se|\u00df|s)kutsch|Rei(\u00dfe|se|\u00df|s)-Kutsch|Engglai-Kutsch|
    Bernerw(a|\u00e4)ge|Berner-W(a|\u00e4)ge',
    chaise = 'Chai(se|s)|Chai(se|s)lein|Sch\u00e4se|Sch\u00e4(s|se)lein',
    characteristic = 'ein(sp\u00e4nnig|r\u00e4drig)|zwe(y|i)(sp\u00e4nnig|r\u00e4drig)|(1|2|3|4)(sp\u00e4nnig|r\u00e4drig)',
    parts = 'Schwanenh\u00e4ls|Geschell|Kutschen-(Kasich|Kasten)|Kutschen(kasich|kasten)'
  )
  dict$neg <- list(
    trolley = 'Leiterwagen|Bauernwagen|Bauern-Wagen|Dielenwagen|Dielen-Wagen|Bauernw\u00e4gel', # trolleys, see other category
    verb_1 = 'wagen wir', # meaning of to dare something
    burial = 'Todtenwagen', # transportation of corpses
    immo_1 = 'Wagenschopf|W(a|aa)genremise|W(a|aa)gen-Remise', # shelter for a carriage
    mail = 'Post(a|aa|\u00e4)gen|Postkutsche|Post-Anzeige|Postanzeige|Post-Chaise|Postillon', # mail coach
    measure_1 = 'W(a|aa|\u00e4)gen voll', # measurement of something in coaches, v1
    measure_2 = 'Grube|w(a|aa|\u00e4)genwei(s|\u00df)|Heu|Emd|verw\u00e4hrt', # carloads and objects measured in carloads
    measure_3 = '(Loth|Pfund) w\u00e4gen', # measure v3
    scale = 'Schnell-W(a|aa|\u00e4)g|Schnellw(a|aa|\u00e4)g|Waagbalcken|Romaine', # scales
    tool = 'W(a|aa|\u00e4)genwinde|W(a|aa|\u00e4)gen-Winde|Winde|Zentner', # tool for lift heavy loads
    lost_1 = 'ab einem W(a|aa)gen', # losing something from a carriage, v1
    lost_2 = 'ab seinem W(a|aa)gen', # losing something from a carriage, v2
    lost_3 = 'verloren|verlohren|verlohrne', # losing something from a carriage, v3
    lost_4 = 'fallen lassen', # losing something from a carriage, v4
    stroller = 'Kinderwagen|Korbwagen', # stroller for children or as toys
    metal = 'Berlinereisen|Berliner-Eisen', # specific kind of metal
    worker = 'Kutscher|Lohnkutscher|F(\u00fc|u)hrmann', # coachmen
    service_1 = 'erbietet|Tanz-Anzeige', # services with a carriage, v1
    service_2 = 'bereit stehen', # services with a carriage, v2
    service_3 = 'hin zu f\u00fchren', # services with a carriage, v3
    books = 'Buchhandlung', # books containing instructions for carriage-making
    wallpaper = 'Tapete|tapezieren', # wallpaper decoration for carriages
    travel_1 = 'Retour-(Chais|Kutsch)|verreisen|Retour-\u00e8ferd', # travel by carriage v1
    travel_2 = 'Platz haben', # travel by carriage, v2
    travel_3 = 'zu fahren', # travel by carriage, v3
    travel_4 = 'verlangt Platz', # travel by carriage, v4
    travel_5 = 'diese Woche', # travel by carriage, v5
    travel_6 = 'gewidmet|Einkehr', # travel by carriage, v6
    travel_7 = 'dahin gehend', # travel by carriage, v7
    travelcompanion = 'Reisegesellscha(f|ff)t|Comagnie|Gesellschafft|Gelegenheit|Compagnie', # searching and offering of company
    other = 'Pantzer|Flaschenkette|Goldwaage', # other small objects
    ocr_1 = 'w\u00e4genkann|wagenoder|Wagenvoll', # ocr mistake
    name = 'Mr. Poaumarchais', # name
    verb_2 = 'zu w\u00e4gen', # verbs
    noobject = 'w\u00e4gende|Wagenhau(\u00df|s)', # no objects
    toy = 'Kinder-|Kinder(chais|wag|kutsch)', # excludes toy carriges for children
    animal = '(Chaise|Cabriolet)-Pferd|(Reise|Chaise|Cabriolet)pferd|Wagenro(s|\u00df)' # horses for drawing carriages,
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_pushchair <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    pushchair = 'Korbw(a|\u00e4|ae)g|Kinderw(a|\u00e4|ae)g|Kinder(chais|sch\u00e4s)|
    Korb-W(ag|\u00e4g|ae)|Kinder-W(a|\u00e4|ae)g|Kinder-(Chais|Sch\u00e4s)'
  )
  dict$neg <- list(
    toy = 'Puppen-Korb' # toy for children
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_storage <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    basket = 'K(o|\u00f6)rb',
    box ='K(i|\u00fc)st(e|ch)|K\u00e4stchen',
    bag = 'S(a|\u00e4)(ck|cke|ckch)',
    tub = 'Trog\\\\b'
  )
  dict$neg <- list(
    handbag = '(Strick|Pist(o|oh)len|Geld|Weiber|Mantel|Nac(ht|h)|Mantel|Pist(o|oh)lenhulftern)s(a|\u00e4)ck|(Strick|Pist(o|oh)len|Geld|Weiber|Mantel|Nac(ht|h)|Mantel|Pist(o|oh)lenhulftern)-s(a|\u00e4)ck',
    pocketsize = 'Sack(fl(a|\u00e4)sch|puffert)|Sack-(fl(a|\u00e4)sch|puffert)', # pocketsize objects
    place_1 = 'Goldk\u00fcste', # placename, v1
    place_2 = 'zum Korb', # placename, v2
    death = 'beerdigt', # death notices
    bed = 'Strohs(\u00e4|a)ck', # bedding
    book = 'Buchdrucker|Bibel|Sack((k|c)alender|b(u|\u00fc)ch)|Sack-((k|c)alender|b(u|\u00fc)ch)', # filters out book ads
    carriage = 'Chaise|Sch\u00e4se', # excludes carriages with baskets or boxes
    stroller = 'Kinderwagen|Kinderw\u00e4g', # excludes strollers with baskets
    pocketsize = 'Sackmesser|Sack-Fernr(o|\u00f6)hr', # pocketsize objects
    travel= 'Fussack|Fu\u00dfsack|Reissack|Reisesack|Reis-Sack|Reise-Sack', # foot rest for carriages and bags for travel
    bed = 'Strohsack|Bettsack|Nachtsack', # bag full of straw, used as bedding
    textile = 'Sacktuch', # textile
    work = 'lesen|schreiben|rechnen|Lehrt(o|\u00f6)chter', # filtering out work ads
    grain = 'Kernen', # filters out ads selling grain in bags
    food = 'Erd\u00e4pfel|Grundbirne|Setzerd\u00e4pfel|Habern\\\\b|Haber\\\\b', # filters out ads selling different kind of food in bags
    measure_1 = 'korbweis|kistchenweis|kistenweis', # measuring something by baskets or boxes, variant 1
    measure_2 = 'Kistchen zu', # measuring something by boxes, variant 2
    meausure_3 = 'Kistchen von', # measuring something by boxes, variant 3
    measure_4 = '//d//sKistchen', # measuring something by boxes, variant 4
    meausure_5 = 'Kistchen \u00e0', # measuring something by boxes, variant 5
    meausure_6 = 'Kistchen v.', # measuring something by boxes, variant 6
    meausure_7 = 'pr. Kistchen', # measuring something by boxes, variant 7
    measure_8 = 'der Sack zu',  # measuring something by bags, variant 8
    measure_9 = '\\\\d\\\\sSack', # number of bags of something, v 9
    measure_10 = '\\\\d\\\\sS\u00e4ck', # number of bags of something, v 10
    measure_11 = '\\\\dS\u00e4ck', # number of bags of something, v 11
    measure_12 = 'in K(o|\u00f6)rb', # number of bags of something, v 12
    measure_13 = 'be(i|y)m Korb', # number of bags of something, v 13
    measure_14 = 'Sack \u00e0', # number of bags of something, v 14
    measure_15 = 'Sack um', # number of bags of something, v 15
    measure_16 = 'Sack von', # number of bags of something, v 16
    measure_17 = 'Sackvon', # number of bags of something, v 17
    measure_18 = '(s(a|\u00e4)ck|korb)(weis|voll)', # number of bags of something, v 18
    measure_19 = 'halbe(n|r) Sack', # number of bags of something, v 19
    beehive = 'Bienenkorb|Bienenk\u00f6rb', # bee hives
    stroller = 'Korbwage|Korbw\u00e4ge', # strollers and prams (own category)
    profession = 'Korbmacher|Korbh\u00e4ndler|Korbladen|KorbLaden|S\u00e4ckler\\\\b|S(a|\u00e4)cktr(a|\u00e4)ger', # professions and shops
    cabinet = 'B\u00fccherk\u00e4st|Glask\u00e4st', # differet kinds of cabinets
    watch_1 = 'Sackuhr|Sack-Uhr', # pocketwatch, v1
    watch_2 = 'Sack Uhr', # pocketwatch, v1
    pistol = 'Sack-Pistol|Sackpistol', # pocket pistols
    telescope = 'Sack-Perspek|Sackpersp', # pocket telescope
    light = 'Sackfeuer|Sacklatern', # pocket lighter and lantern
    graveyard = 'Gottesacker', # graveyard
    other = 'Korbgitter', # unknwon object, but no storage
    textile = 'Sack(t(u|\u00fc)ch|zwilch)' # name for specific kind of cloth


  )
  # maybe exclude Bettsack and add it to bed dictionary?

  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_buildingcomponents <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    door = 'steinerner Bogen',
    well_1 = 'Br(u|\u00fc)(nn|n)stein',
    well_2 = 'Br(u|\u00fc)(nn|n) Stein',
    door = '(Th|T)\u00fcre|Vor(th|t)\u00fcre|(Th|T)\u00fcrschlinge', # possibly too many immo ads with 'Th\u00fcre', negatives are tricky
    window = 'Fenster|(K|C)reu(z|tz)st(o|\u00f6)ck|Fenster(fl\u00fcgel|pritsch)|Vorfenster|Jalousie-L(\u00e4|a)den|Jalousiel(\u00e4|a)den',
    wood = 'Bauhol(z|tz)|Latten|Diele|Stiege|Kellerb(\u00f6|o)gen',
    stone = '(Back|Quader)stein|Steinplatte',
    metal = 'Dachk(a|\u00e4)n(e|a)l|Rohr|T(ei|eu)chel|Abtrittrohr',
    shingle = 'Dachschindel|Schind(eln|len)|Ziegel\\\\b|Deichel',
    tile = 'Bodenpl(\u00e4|a)ttl|Pl\u00e4ttlein',
    glas = 'Glas in Scheibe',
    other = 'Ger\u00fcststange|Wetterdach|G(egi|a)tter',
    pipe = 'Rohr'
  )
  dict$neg <- list(
    unknown = 'Beinrohr', # unknown object, but no building material
    curch = 'Kirchenth\u00fcr', # church doors
    person = 'Ziegel-Jgfr', # name
    factory = 'Manufakt\u00fcre', # factory
    food = 'Blatten-Kraut', # food
    gun = 'Luntenrohr', # weapons
    tobacco = '(Taba(ck|k|kk)|Pfeifen)rohr|(Taba(ck|k|kk)|Pfeifen)-Rohr', # tobacco objects
    knitting = 'Strickrohrlein', # knitting objects
    verb = 'gestiegen', # verb containing 'stiege'
    news = 'Pulver-M\u00fchl|Stockwerck|Mordthat', # excludes news containing 'Hau\u00df' and 'Fenster'
    umbrella = 'Parapluie', # descriptions of umbrellas with 'rohr'
    lost = 'verloren', # excludes ads for lost goods (very rare with building components)
    churchchair = '(Frauen|Mannen|Weiber)sitz|(Frauen|Mannen|Weiber)-Sitz', # church chairs (close to a door)
    telescope = 'Fernrohr', # telescope
    instrument = 'Fagotrohr|Instrument', # musical instruments
    material_1 = 'M(eh|ee)rrohr|Pfefferrohr|Wienerrohr', # materials for walking canes, v1
    material_2 = 'Wiener Rohr', # materials for walking canes, v2
    measure = 'in Platte', # measurement for something
    carriage = 'einsp(\u00e4|a)nnig|zwe(i|y)sp(\u00e4|a)nnig', # carriages with windows
    work = 'Kenntnis|Lehre', # filters out work ads
    plates = 'Platten', # plates
    place = 'Winterth\u00fcr', # placenames
    cabinet = 'Schrank|K\u00e4stchen', # description of cabinet doors
    book = 'Kupfferblatt', # book ads
    weapon = 'Flin(th|t)e', # weapons with a 'rohr'
    tool = 'Glaserwerckzeug', # tools
    tobacco = 'Bla(\u00df|ss|s)rohr|Pfeifenrohr', # tobacco utensil
    walking = 'Spannisc(h|he|hes) Rohr', # walking cane
    adjective = 'glatt', # containing 'latt'
    divider = 'Scheidwand', # excludes doors as part of room dividers
    cabinet = 'Kasten', # excludes doors as a part of cabinets
    immo = 'Ladenth\u00fcre|beschl\u00fcssig|beschlossen|Estrich|K\u00fcche|Hinterth\u00fcr|Fasnachtfronfast', # words indicating immo-ads
    ocr = 'dieletze', # ocr mistakes
    place_1 = 'Ziegelh(oo|o)f|Ziegel-H\u00fctte', # places in Basel
    place_2 = 'Stiege hoch' # description of location of something

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_things
#' @export
tagfilter_suitcase <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    suitcase = '(K|C)off(er|re)',
    travelbag = 'Rei(ss|se|\u00df|s)(a|\u00e4)ck|Rei(s|ss|se|\u00df)-S(a|\u00e4)ck'
  )
  dict$neg <- list(
    place = 'Karrenhof', # placename
    tool = 'Kofferwinde' # name for a winding tool

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_things
#' @export
tagfilter_measure <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    meter = '(Baro|(T|Th)ermo)meter'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_things
#' @export
tagfilter_trolley <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    handcart = '(Hand|Leiter)w(\u00e4|a)ge(l|n)|(Sto(ss|\u00df|s)|Trog)ka(rr|r)en|K(a|\u00e4)(rr|r)en',
    trolley = '(Bauern|Dielen)-W(a|\u00e4)ge|(Bauern|Dielen)w(a|\u00e4)ge|Fuhrwerk',
    harness = 'Sillen-Geschir|Sillengeschir',
    objects = 'Wagenkette|(Leit|Zug)seil'
  )
  dict$neg <- list(
    child = 'Laufkarren', # toy for a child ########### has to be added to toys or strollers ###########
    rope = 'Leitseiler', # rope
    profession = 'Bibliothekar', # professions
    measure_1 = 'Karrenvoll', # measure v1
    measure_2 = 'Karren voll' # measure v2
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_health <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    medicine_1 = 'Bal(s|ss)am|Heilmittel|Hustent\u00e4fel|Salbe|Tin(k|c)tur|Gichtpapier|Zahn-Pulver|Zahnpulver
    |Ar(tz|tzt|zt)ne(y|i)|Tabletten|Medi(z|c)in|Pulver|Mittels|Heilkraft|Mittel(wider|gegen|f\u00fcr)|Augenmittel|
    Kr(e|\u00e4)uter-(Oeh|Oe|\u00d6|\u00d6h)l',
    medicine_2 = 'Pate pectoral',
    medicine_3 = 'Mitte(l|ls) gegen',
    medicine_4 = 'Mitte(l|ls) wider',
    medicine_5 = 'Mitte(l|ls) f\u00fcr',
    medicine_6 = 'bew(\u00e4|e)hr(tes|ts|trs) Mitte(l|ls)',
    medicine_7 = 'dicke H\u00e4lse',
    medicine_8 = 'Zahn Pulver',
    medicine_9 = 'gegen (Schnupfen|Kattarh|Engbr\u00fcstigkeit|Heiserkeit)',
    medicine_10 = 'unfehlbar(es|e) Mittel',
    medicine_11 = 'vortrefflich(en|es|e) Mittel',
    bandage = 'Bandage|Bruchb(a|\u00e4)nd',
    soap = 'S(e|a)i(f|ff)e',
    washing = 'Flecken Kug(el|le)',
    hygiene_1 = 'Waschschw(a|\u00e4)mm|Wasch-Schw(a|\u00e4)mm|Handwasch|Waschwasser',
    hygiene_2 = 'englische Erde',
    hair = 'B\u00fcrste|(Chignon|Haar|Frauenzimmer|Locken|Fris(ie|i)r)k(\u00e4|a)mm|Haarnadel|(Chignon|Haar|Frauenzimmer|Locken|Fris(ie|i)r)k(\u00e4|a)mm|
    Haarpuder|Haar-Puder',
    cosmetic_1 = 'Po(mm|m)ade|Puderlade|Maca(ss|s)ara|(K|C)osmeti(k|c)|Quaste',
    cosmetic_2 = 'kosmetische Mittel',
    shaving = 'Rasiermesser',
    perfume_1 = 'Parf(\u00fc|u)m|Parf(\u00fc|u)merie|Fla(c|k)on|Rosen\u00f6l',
    perfume_2 = 'eau de',
    perfume_3 = '(K|C)(o|\u00f6)(l|ll)ni(sch|sches) Wasser',
    chemical_1 = 'Ameisengeist|A(mm|m)oni(ak|um)|(Ch|C)onchil|Salpeter|Bittererde|
    Brocke(l|li)-A(mm|m|ml|mml)ung|Brocke(l|li)a(mm|m|ml|mml)ung|Far(be|b)kraut',
    chemical_2 = 'Chemische Pr(\u00e4|e)parate',
    chemical_3 = 'Chemisches Pr(\u00e4|e)parat',
    polish = 'Schmiere|Wichse|Schella(ck|k)'
  )
  dict$neg <- list(
    noobject = 'Gegend\\\\b|Mittelsorte', # no objects
    adjectives = 'mittellos', # adjectives
    food = 'Cacao|Kaffee', # food in form of powder
    object = 'Mittelst\u00fcck|Pulverh(o|\u00f6)rn|Pulverflaschen', # other objects
    book = 'Abhandlung|Jahrb(u|\u00fc)ch', # excludes book titles
    mill = 'Pulverm\u00fchl|Pulver-M\u00fchl', # mill for grinding things into powder
    other = 'vermittel|mittelst|Medizinalbeh\u00f6rde|Medizinal-Kollegien|mittelschwer', # words containing 'mittel' (no objects)
    textile = 'Gel(d|t)beute-Quaste|Quastenspitz', # decoration for bags
    profession = 'B\u00fcrstenbinder', # profession
    place = 'Seifensiederey', # place for making sopa
    eau_1 = 'bateau de', # stick
    eau_2 = 'B(u|\u00fc)reau', # cabinet
    eau_3 = 'Eau de Noyeaux', # liquor, v1
    eau_4 = 'Eau deNoyeaux', # liquor, v2
    eau_5 = 'Eau de Vie', #liquor, v3
    eau_4 = 'Tableau de' # picture of something
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_things
#' @export
tagfilter_weapon <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    general = 'Waffe|Ordonnanz-G',
    crossbow = 'Armbrust',
    bayonet = 'Bajo(nn|n)et',
    sword = 'Degen|S(a|\u00e4)bel|(K|C)arabiner',
    gun = 'Flin(t|th)e|Gewehr|Pis(t|th)(o|oh)l|B\u00fcchse|Stutzer',
    cartridge = 'Patron|Gibern|Kuge(l|ln)model|Kuge(l|ln)-Model',
    sheath = 'Scheide'
  )
  dict$neg <- list(
    adjective = 'responsabel', # contains 'sabel
    storage = '(Zucker|Ble(i|y)|Pfund)b\u00fcchs', # storage (B\u00fcchse)
    tobacco = 'T(o|a)ba(ck|k|kk|c)', # tobacco stored in boxes (B\u00fcchse)
    other = 'Markscheide|Abscheide|unterscheiden|bescheiden|Scheideweg', # no objects (containing 'scheide')
    profession = 'Scheidenmacher' # profession
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_shopequip <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    storage = '(C|K)omptoir-K(a|\u00e4)sten|(C|K)omptoirk(a|\u00e4)sten|W(aa|a)renk(\u00e4|a)st|W(aa|a)ren-K(\u00e4|a)st',
    display = 'Gl(a|\u00e4)sk(a|\u00e4)st|Gl(\u00e4|a)ser-K(a|\u00e4)st|Glasglocke',
    desk = 'Ladent((c|k)orpus|tisch)',
    cash = 'Gel(d|t)((k|c)ass|trog|st(o|\u00f6)ck)|Gel(d|t)-((K|C)ass|Trog|St(o|\u00f6)ck)|Cassa',
    scale = '(Fuhr|Gold|Magazin|Schnell|W(a|aa)ren|Fl(a|\u00e4)schen)w(\u00e4|aa|a)g|(Fuhr|Gold|Magazin|Schnell|W(a|aa)ren|Fl(a|\u00e4)schen)-W(\u00e4|aa|a)g|
    (Einsatz|Eisen)gewicht|(Einsatz|Eisen)-Gewicht|lbstein|Messinggewicht|Waage|W(\u00e4|aa|a)g(kengel|bal(ck|k)e)|W(\u00e4|aa|a)g-(Kengel|Bal(ck|k)e)',
    scale_phrase = 'Einsatz Gewicht',
    counting = 'W(aa|a|\u00e4)renz\u00e4hl',
    general = 'Handelsutensil|Ladenger\u00e4(th|t)scha(f|ff)t'
  )
  dict$neg <- list(
    noobject = '(Waisen|Gesellschafts)-Cassa|Cassa-Stifter', # no objects
    finance = 'Capitalien', # finance ads
    storage = 'Cassette|Cassettlein' # other storage objects
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_tool <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    fire = 'Bl(a|\u00e4)(s|se|\u00df|ss)balg',
    woodworking = 'Bohrer|Brenneisen|Drehstuhl|H(o|\u00f6)bel|Hackb(a|\u00e4)nk|Hack-B(a|\u00e4)nk|H(a|\u00e4)mmer|
    S\u00e4ge|Schneideisen|Schraubst(o|\u00f6)ck|Schreinerwerckzeug|Schneidmaschine|F(u|\u00fc)gbl(o|\u00f6)ch|Anhau',
    screw = 'Leimschraube',
    knife = 'Barbiermesser',
    metalworking = 'Ambo(s|\u00df|ss)|Feldschmidt|Reibstein',
    stoneworking = 'Schleifstein|Steinschleife',
    sharpening = 'Streichriemen|Abziehleder',
    textileworking = 'Strump(f|ff)w(e|\u00e4)ber-Stuhl|Strump(f|ff)w(e|\u00e4)berstuhl|Tuch-Pre(ss|\u00df)|Tuchpre(ss|\u00df)|Zwirnmaschine|Fadenz\u00e4hler|
    Schwefelk(a|\u00e4)st|se(i|y)denrad|Leinenwe(ber|b)stuhl|P(a|o)ssamen(ter|t)stuhl|P(a|o)ssamen(ter|t)-Stuhl|Zwirnm\u00fchl|Zwirn-M\u00fchl|Se(i|y)denm\u00fchl|
    Se(i|y)den-M\u00fchl|W(e|\u00e4)bst(u|\u00fc)l',
    textiles_1 = 'eisernes R\u00e4dlein',
    mill = 'Kammr(\u00e4|a)d|Wasserr(a|\u00e4)d|se(i|y)denwaage|se(i|y)den-Waage|se(i|y)denwindmaschine|se(i|y)denwind-Maschine|Schwungrad|Schwung-Rad|M(\u00fc|a)hlstein|
    Wendelbaum',
    paper = 'Siegelpre(ss|\u00df)',
    printing = 'Kupferpre(ss|\u00df)|Kupferdruckerpre(ss|\u00df)|Drucktisch',
    straw = 'Strohschneid',
    distilling = 'Brenn(hafen|kessel|h\u00e4u(s|\u00df)lein)',
    other = 'Stemmeisen|Zange|Wasserwaag|Zirkel|Zollstab',
    general = 'Wer(k|c|ck)zeug|Quincaillerie'
  )
  dict$neg <- list(
    name = 'Rothenhammer', # family name
    noobjects = 'Proportional-Zirkel|Zirkels\\\\b', # no objects
    adjective = 'zirkelrund', # adjectives
    place = 'blaue(n|r) Hammer', # house name
    immo = 'Hammerschmidte', # immo ads
    music = 'Hammer(c|k)laver', # muscial instrument
    strawberry = 'Frambose', # strawberries and liquors (french)
    adjective = 'ungehobelt' # description of wood
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_stationary <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    pen = 'Bleistift|Griffel|Schreib(feder|zeug)',
    drawing = 'Zeichnungs(kreide|papier)|Pinsel|Far(ben|b)k(\u00e4|a)st|Far(ben|b)-K(\u00e4|a)st',
    ink = '(T|D)inte',
    notebook = 'Carnet|(Notiz|Schreib)(b(u|\u00fc)ch|mappe)',
    paper = 'Briefpre(ss|\u00df)|(Musik|Kreu(z|zlein)|(C|K)on(c|z)ept|Stab|Schreib|Druck|Post|Karten|Marmor|Noten|Pack|se(i|y)den|Brief|Stempel|Flie(\u00df|ss|s))pa(p|pp)ier|Pa(p|pp)ierrest|Pappier|
    (Musik|Kreu(z|zlein)|(C|K)on(c|z)ept|Stab|Schreib|Druck|Post|Karten|Marmor|Noten|Pack|se(i|y)den|Brief|Stempel|Flie(\u00df|ss|s))-Pa(p|pp)ier|Schreibkarte|Schreibrolle|Ma(k|c)ulatur',
    paper_2 = 'B\u00f6gen Papier',
    paper_3 = 'Resten Papier',
    paper_4 = 'neu(e|en|es) Papier',
    cardboard = 'Karton',
    slate = 'Schieferta(f|v)el',
    seal = 'P(e|i)(t|tt)scha(f|ff)t|Siegellack',
    other = 'Lineal|Schreibunterlage|Federmesser|Sch(ee|e)re',
    general = 'Schreibzeug'
  )
  dict$neg <- list(
    ocr = 'h\u00f6chstinter|s\u00e4tintert', # ocr mistakes
    book = 'Gesangbuch', # filters out book ads
    light = 'Lichtscheer', # scissors to cut candle wicks
    tobacco = 'T(o|a)ba(k|kk|c|ck)', # tobacco objects (can be from paper)
    noobject = 'Dintenfleck|gepinselt|W(a|\u00e4)schere(i|y)|dinter(e|\u00e9)t', # no objects
    profession = 'Tuchscheer|Scheerer', # professions
    garden = '(Haag|Baum)scheere|(Haag|Baum)-Scheere' # for cutting plants
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_jewellery <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    flowers = '(Ball|Kunst)blume|(Braut|To(dt|t)en)kr(a|\u00e4)nz',
    necklace = 'Hals(kette|band)', # ad 'Kette' when completing dictionary (a lot of negatives)
    earring = 'Oh(ren|r)(beh(a|\u00e4)ng|ring)',
    bracelet = 'Bracelet',
    pin = 'Vorstecknadel',
    general = 'Bijouteri'
  )
  dict$neg <- list(
    dog = 'Hund' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_wood <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    firewood = 'Brennhol(tz|z)',
    coal_1 = 'tannene Kohlen',
    coal_2 = 'Par(th|t)e(y|i) Kohlen',
    material = '(Buchen|Tannen|Ahorn|Linden)(hol(tz|z)|st(a|\u00e4)mm)|(Buchen|Tannen|Ahorn|Linden)-(St(a|\u00e4)mm|Hol(tz|z))|Weichselrohr|Weichsel-Rohr',
    form = '(Boden|Scheit|(T|D)augen)hol(tz|z)|Fleckling|F\u00fchrling|Hol(tz|z)-Raspel|Hol(tz|z)bock|Pf(a|\u00e4)hl|(Pr\u00fcgel|Knebel)hol(tz|z)|Lohst(o|\u00f6)ck|
    S(a|\u00e4)gsp(\u00e4|a|\u00e4h|ah)n|St\u00e4mme|Stammhol(tz|z)|Wellen|Fa(ss|\u00df)(d|t)auge|Pl\u00fctschi|S\u00e4gb(a|\u00e4)um|Drehsp(\u00e4h|\u00e4)n',
    building = 'Bauhol(tz|z)|Latten|Diele',
    unknown = 'Kammhol(tz|z)'
# maybe include 'hol(tz|z)' and 'Klafter', but negatives are tricky
  )
  dict$neg <- list(
    name = 'Hr Pf\u00e4hler', # family name
    official = 'Begehren', # official ads containing wood (z.B. Bauholz-Begehren)
    measure = 'f\u00fchrlin(g|gs)wei(s|\u00df)', # measurement
    copper = 'Kup(ff|f)er(b|p)latte', # copper prints
    description = 'unter Platten', # description of furniture
    adjective = 'glatte', # adjective
    stone = 'Marmor-Platte|Marmorplatte|Sandsteinplatte|Sandstein-Platte', # type of stones
    textiles = 'Glatten|Bastplatten|Garn', # type of textiles
    ocr = 'dieLehre', # ocr mistakes
    verb = '(zu|versch)wellen', # verbs
    place_1 = 'Schwar(z|tz)enpfahl|Schwar(z|tz)pfahl|Schwar(z|tz)en-Pfahl|Gr\u00fcnpfahlg(a|\u00e4)|Pfahlg(a|\u00e4)', # placenames
    place_2 = 'Schwar(z|tz)en Pfahl', # placenames
    immo = 'Brennhol(tz|z)-Magazin|Behausung|Losament|Kammer|Stube' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_barrel <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bottle = 'Bouteille(n|s)|Pi(\u00e8|e|\u00e9)ces|Gutter|Selterserwasser-Kr(\u00fc|u)ge',
    barrel_1 = 'Fa(ss|\u00df)\\\\b|F\u00e4(ss|\u00df)er\\\\b|(\u00d6|Oe)hlst(u|\u00fc)cklein|F\u00e4(ss|\u00df)(chen|lein)',
    barrel_2 = 'St(\u00fc|u)cklein Fa(s|\u00df|ss)'
    )
  dict$neg <- list(
    textile = 'Canafa\u00df', # specific kind of textile
    measure_1 = '\u00e0 piece', # measure, v1
    measure_2 ='vom Fa(s|\u00df)', # measure, v2
    immo_1 = 'Losament|Stube', # excludes immo ads, v1 (Keller doesn't work as a negative, bc often Keller and F\u00e4sser are sold together)
    immo_2 = 'Platz f\u00fcr', # excludes immo ads, v2
    wood = '(D|T)augenhol(tz|z)', # wood for barrels
    carneval = 'Fa(ss|\u00df)nacht', # carneval
    wine = '\\\\bin (Bouteillen|Pi(\u00e8|e|\u00e9)ces|Gutter|Fa(ss|\u00df)|F\u00e4(ss|\u00df)er|St\u00fccklein)' # full barrels and bottles
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_tobaccoobjects <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    pipe = 'Pfeife|Taba(k|ck|ks|cks|c|cs)pfeife|Pfeifen(kopf|raumer|rohr)',
    storage = '((C|Z)iga(rr|r)en|Taba(k|ck|ks|cks|c|cs))-(B(\u00fc|u)chs|Bux|Etuis|Dose|Beutel)|
((C|Z)iga(rr|r)en|Taba(k|ck|ks|cks|c|cs))(beutel|b(\u00fc|u)chs|bux|etuis|dose)|
    Taba(k|ck|ks|cks|c|cs)kasten|Taba(r|k|ck|ks|cks|c|cs)ier|Tabattier|
    Taba(r|k|ck|ks|cks|c|cs)ti(e|\u00e8)r'
  )
  dict$neg <- list(
    unknwon = 'Pfeifenerde', # unknown object (keep it in?)
    birds_1 = 'zum Paaren', # ads for birds with descriptions (Pfeifen), v1
    birds_2 = 'sch\u00f6n pfeifen', # ads for birds with descriptions (Pfeifen), v2
    birds_3 = 'pfeifend', #ads for birds with descriptions (Pfeifen), v3
    immo = 'Losament|Kuchin|K\u00fcche|Stube|M\u00fchle' # filters out immo ads
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_hay <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    hay = 'H(e|\u00f6)(u|\u00fc|w)\\\\b|\\\\bEm(d|bd|db)\\\\b|H(e|\u00f6)(u|\u00fc|w)gra(ss|\u00df|s)',
    straw = 'Stroh\\\\b',
    pasture_1 = 'Klee|Herbstweid',
    pasture_2 = 'Gra(s|ss|\u00df) auf'
  )
  dict$neg <- list(
    trolley = 'Heu-W(\u00e4|a)ge', # trolley for hay/straw
    seeds = 'Kleesaamen|Klee-Saamen', # seeds
    hat = 'Stroh-H(\u00fc|u)(t|th)', # straw hats
    textile = 'Stroh-Decke', # textiles
    profession = 'Stroh-Arbeiten|Stroh-Schneidstuhl', # professions using straw
    noobject = 'Kleebau', # no objects
    name = 'Kleemann', # family name
    objects = 'Stroh-(Sessel|Sack)|Stroh(sack|sessel)|Rockstroh|Stroh-Bord', # objects out of straw
    ocr = '\u00f6ffentlicheu|franz\u00f6sischeu|K\u00fccheu', # ocr mistakes
    place = 'auf dem Heu' # placename (Heuberg, finds those not in one word)
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_woodobject <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    woodturning = 'Drechslerw(aa|a)r|Drechsler-W(aa|a)r',
    carving = 'Schnitzw(aa|a)r|Schnitz-W(aa|a)r',
    general = 'H(o|\u00f6)l(tz|z|zen)(decke|werk|w(a|aa)r)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_dung <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    dung_1 = '(K(\u00fc|u)h|Pfer(d|de)|Schwei(n|ne)|Tauben|Ziegen|Sch(a|aa)f)(bau|mist)', # possibly add '\\\\bBau\\\\b', but a lot of negatives at this point
    dung_2 = 'verw\u00e4hrter Bau'
  )
  dict$neg <- list(
    other = 'Wasser-Kunst', # 'Bau- und Wasserkunst'
    work = 'Maschinen-Bau', # work
    authorities = 'Bau-Colleg' # insitution
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_plant <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    tree = 'B(a|\u00e4)um|Obstb(\u00e4|a)um',
    bush_1 = 'Gar(th|t)enbu(chs|x)|Oleander',
    bush_2 = 'gr\u00fcner Bu(chs|x)',
    flower = 'Blumenzwiebel|Blumen-Zwiebel|N\u00e4gelin',
    flower_2 = 'Blumen Zwiebel',
    seed = 'S(a|aa)men\\\\b',
    seedling = 'Setzling',
    root = '(Dahlien|Sparglen)-Wur(tz|z)(el|e)n',
    general = 'Pflan(z|tz)e|Gew\u00e4chs'

  )
  dict$neg <- list(
    measure = 'baumweis', # measurement of wood
    noobject = 'Baumbach(t|n)|Pflanzen-System',
    food = 'Baum\u00f6(hl|l)', # food
    activity = '(Baum|Be)schneiden', # cutting trees
    name_1 = 'Wittwe N\u00e4gelin', # family name 1
    name_2 = 'Jakob N\u00e4gelin', # family name 2
    name_3 = 'Adam N\u00e4gelin', # family name 3
    fair = 'Bude|Messe', # descriptions of fair stalls by trees
    wine = 'Wein', # sometimes described as 'bestes Gew\u00e4chs'
    book = 'Tafeln|Meisterst\u00fcck|Schriften|Prospekt|Buchbinder', # excludes book illustrations
    adjective = 'arbeitsam|heisam|heilsam|gemeinsam|grausam|zusamen|ehrsam|erwerbsam', # adjectives with 'samen'
    verb = 'samen halten', # verb
    name = 'Baumgartner|Baumann|Baumeister', # family names
    textile = 'Baumwoll', # cotton
    other_1 = 'Baum-Trotte|Baumtrotte|Weinb(a|\u00e4)um|Baumleiter|Baum-Messer|Baummesser|Baumaterial|
    Brosamen|Pflanzenschleim|Wendelbaum|Wendel-Baum|Brennbaum|Schlagb(a|\u00e4)um|Baum-Garten', # other object, v1
    other_2 = 'Baum Trotte', # other objects, v2
    place_1 = 'Maulbaum|Bauma\\\\b', # place names, v1
    place_2 = '(dem|am|ans|vom) B(\u00e4|a)umlein', # place names, v2
    immo = 'Matten|Feld|Baumgarten|Liegenscha(f|ff)t|Losamen|Wohnh(a|\u00e4)us|Wohnung|Wirtshaus|Scheuer|Juchart', # excludes immo ads with plants
    description = 'nu(ss|\u00df)baumnen|kirschbaumholzen|nu(\u00df|ss)baumesteller|(birn|nu(\u00df|ss)|kirsch)b(\u00e4|a)um(ern|en)|baumrund|(Nu(ss|\u00df)|Buchs)baum(hol(tz|z)|geh\u00e4us)' # description of furniture and clothes
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_glasses <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    glasses = 'Brille|(Augen|Lese)gl(a|\u00e4)s',
    lens = 'Luppe|Augenspiegel',
    opera = 'Lorgnette|Perspektiv|Lorgnon',
    telescope = 'Telescop|Fernr(o|\u00f6)hr',
    microscope = 'Micros(c|k)op',
    laterna = 'Laterna magica',
    optical_1 = 'optische Instrumente',
    optical_2 = 'Optik\\\\b',
    other = 'Landscha(f|ff)tsspiegel'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_soil <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    soil = '(Garten|Matten)grund|(Garten|Matten)-Grund',
    gravel = 'Birsgrien|Kieselstein|Asphalt',
    lime = 'Kal(k|ch)',
    red = 'ro(t|th)e W(aa|a)r',
    plaster_1 = 'gebr(an|au)nter G(y|i)ps',
    ash = '(gute|buchene) Asch(e|en)\\\\b'
  )
  dict$neg <- list(
    name = 'Kalkbrenner', # family name
    pharmacy = 'Chlorkalk', # pharmacy object
    immo_2 = 'Wohnung|Kammer|Losament|Stube|K\u00fcche|Laden|beziehen|Fronfast|Keller|Zimmer' # excludes immo ads
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_agriculturalobjects <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    tool = 'Dreschflegel|(Heu|Lad|Lade)gabel|Pfl(u|\u00fc)g|Sense|Rechen|Mattenmesser|Matten-Messer',
    trolley = 'G\u00fcllenkarren|Heuwagen',
    storage = 'Obsthurte|Obstk(o|\u00f6)rb',
    wine = 'Rebsteck',
    animal = 'Mastb\u00fctte|Bienenst(o|\u00f6)ck'
  )
  dict$neg <- list(
    french = 'senseignent', # french containing 'sense'
    trolley = 'Heuwagen', # trolley for hay
    name = 'Pflugg(\u00e4|a)', # place name
    ocr = 'senseit|wachsense|sorechen', # ocr mistakes (contain 'sense')
    name = 'Rechenmacher', # family name
    other = 'Rechenscha(f|ff)t|Rechen(r\u00e4th|kun(s|f)t|meister|schul|rat)|Rechen-R\u00e4th', # no objects
    verb = 'erfrechen|sprechen|brechen|(ent|gut)sprechen', # berbs containing 'rechen'
    maths = 'Rechen(kunst|tafel|buch)|Rechen-(Tafel|Kunst|Buch)' # mathematics
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_riding <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    saddle = 'S(a|\u00e4)ttel|Steigb\u00fcgel',
    bridle = '(Pfer(de|d)|Reit)((b|ge)iss|geschir)|(Pfer(de|d)|Reit)-((B|Ge)iss|Geschir)',
    whip = 'Peitsche',
    spur = 'Sporren',
    blanket = 'Pfer(ded|dd)ecke'
  )
  dict$neg <- list(
    toy = 'Schwungpferd' # hobby horse
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_well <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    well = '(Br(u|\u00fc)n(n|nen)|Wasser)stein|(Brun(n|nen)|Wasser)-Stein|Br(u|\u00fc)n(n|nen)trog|Br(u|\u00fc)n(n|nen)-Stein|Br(u|\u00fc)n(n|nen)-Trog|
    Br(u|\u00fc)n(n|nen)stiefel|Br(u|\u00fc)n(n|nen)-Stiefel',
    pump = 'Wasserpumpe|Wasser-Pumpe',
    parts = 'Zu(ge|be)h\u00f6rde zu einem (Ziehbrunn|Br(u|\u00fc)n(n|nen)|Wasser)'
  )
  dict$neg <- list(
    immo = 'Wohnung|Kammer|Losament|Stube|K\u00fcche|Laden|beziehen|Fronfast|Keller|Zimmer|Hof|H\u00f6flein|Haus|H(a\u00fc|\u00e4u|au)slein' # excludes immo ads
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_things
#' @export
tagfilter_naturalia <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    naturalia = 'Muscheln|Schmetterling', # 'Muscheln' can also be a material, e.g. describing buttons
    minerals = 'Versteinerung|Mineralien'
  )
  dict$neg <- list(
    books = 'Anweisung', # book titles containing naturalia
    buttons = 'Kn(o|\u00f6)pf' # buttons from naturalia material
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_container <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    container = 'Z(u|\u00fc)ber|Eimer|Trog|B(\u00f6|o|u)(ck|k|ckt|kt)(lin|re)'
  )
  dict$neg <- list(
    measure = 'z(u|\u00fc)berweis', # measuring something in 'Z\u00fcber'
    ocr = 'beimerian', # ocr mistake
    name_1 = 'Fr. B\u00f6cklin', # family name, v1
    name_2 = 'Fr.B\u00f6cklin', # family name, v2
    name_3 = 'Seimer', # family name, v3
    place = 'Rirheim|Blotzheim|Regisheim|Hochheim|M(\u00fc|\u00fch)(l|ll)heim|Pforzheim|Arlesheimer|T\u00fcrckheimer|H(e|\u00e4)genheim|Ma(n|nn)heim', # place names
    cash = 'Gel(d|t)trog', # object for holding cash
    fire = 'Feuereimer|Feuer-Eimer', # fire prevention
    well = 'Brunntrog', # well
    other = 'Geheimerat|Zubereitung', # no objects
    trolley = 'Trogkarren', # trolley
    verbs = 'berichten|betrogen|zubereite|hinzuberuf' # verbs
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_firestart <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    lighter = 'Feuer(stahl|zeug|schw(a|\u00e4)mm)|Z\u00fcndmaschine',
    match = '(Schwefel|Z\u00fcnd)h(o|\u00f6)l(z|tz)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_extinguisher <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    extinguisher = 'Feu(er|r)spritze|Feu(er|r)-(S|s)pritze',
    bucket_1 = '(Feuer|L\u00f6sch)(eimer|trog|z(u|\u00fc)ber)|(Feuer|L\u00f6sch)-(eimer|trog|z(u|\u00fc)ber)',
    bucket_2 = 'Feuer (Eimer|Trog|Z(u|\u00fc)ber)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_firework <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    firework = 'Feu(er|r)werk'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_antique <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    antique_1 = 'Antique-K(o|\u00f6)pf|Antiquek(o|\u00f6)pf|Statue',
    antique_2 = 'r\u00f6misch(e|en) Figur',
    antique_3 = 'Antique K(\u00f6|o)pf'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_key <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    home = 'Hausschl\u00fcssel',
    furniture = 'Schrankschl\u00fcssel|Uhrenschl\u00fcssel'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_cane <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    cane_1 = 'Spazierst(o|\u00f6)ck',
    cane_2 = 'spanisc(h|he|hes|her) R(o|\u00f6)hr'
  )
  dict$neg <- list(
    other = 'Blasrohr' # other objects
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_wineobject <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    production = 'Weintrott|Z(a|\u00e4)pfen',
    storage = '(Wein|Herbst)b(\u00fc|u|o)(k|ck)te|B(\u00fc|u|o)(k|ck)t(e\\\\b|en\\\\b)',
    consumption = 'Wei(nhah|nh\u00e4h|nha|nh\u00e4)nen|Wein-H(ah|a)nen|Wein-H(\u00e4h|\u00e4)nen|Wein(krause|schild)'
  )
  dict$neg <- list(
    verb = 'abz(a|\u00e4)pfen' # verbs
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_rope <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    rope = '(Spann|Zug|(B|P)lunder)(strick|seil)|(Spann|Zug|(B|P)lunder)-(Strick|Seil)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_things
#' @export
tagfilter_tavernobject <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    sign = 'Wir(th|t)sschild',
    general = 'Wir(t|th)scha(f|ff)tsger\u00e4t'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_things
#' @export
tagfilter_feed <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    potatoes = 'Schwei(n|ns)erd\u00e4pfel|Schwei(n|ns)-Erd\u00e4pfel',
    feed_1 = 'F\u00fctterung der',
    feed_2 = 'M\u00e4stung der',
    feed_3 = 'zum verf\u00fcttern',
    ocr = 'zumverf\u00fcttern'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_things
#' @export
tagfilter_miscobject <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    misc = 'Gegenst\u00e4nde|Waarenlager',
    magnets = 'Magnet'
  )
  dict$neg <- list(
    other = 'Gedanken|B(u|\u00fc)ch|Brief|Berathungsgegenst\u00e4nd|magnetisch|Magnetismus|Obligation' # no objects
  )
  create_filter_output(dict)
}
