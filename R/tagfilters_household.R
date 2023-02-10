#' Tagfilter Household
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for household and furniture related ads and 
#' finds categories like beds, chairs and cabinets as well as smaller items like
#' mirrors, toys and kitchen utensils.
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_household
#' @seealso tagfilters
NULL




#' @rdname tagfilter_household
#' @export
tagfilter_bed <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    bed = "Be(th|tt)",
    cradle = "Wiege"
  )
  dict$neg <- list(
    date = "Bettag", # holiday
    book = "Schriften|Schreiben|Bibel", # filters out book ads
    measure = "wiegend", # measurement
    immo = "Zimmer|Küche|Losament|Kammer|Wohnung|Stüblein|Stube", # filters out immo ads (some actual beds are lost by excluding these)
    garden = "Frühbett", # garden object
    household = "Be(th|tt)pfanne|Be(tt|th)eingu(ß|s)", # household objects
    alphabet = "Alphabeth", # alphabet
    birth = "Kindbe(t|th)\\b|Kind-Be(t|th\\b)", # filters out ads for work around birth
    verb_1 = "gebe(tt|th)en|bettend", # verbs containing "bett/beth"
    verb_2 = "zu wiegen", # weighing v1
    verb_3 = "Centner wiegen", # weighing v2
    name = "(Eli|Li)sabet|Babe(tt|th)|E(l|i)(ß|l|z|s)beth|Bethune", # names of people and places including "bett/beth"
    placename = "Betti(g|ng)en|Bettwil|Baselgebeth", # placenames containing "Bett"
    textiles = "Deckbet|Be(tt|th)(barchent|teppich|wer(k|kh|ck)|zeug|sack|tril|deck)|Be(tt|th)-(barchent|teppich|wer(k|kh)|zeug|sack|tril|deck)", # these are household textiles
    feathers = "Be(tt|th)fede|Be(tt|th)-f(e|eh)de", # bed feathers
    misc ="(sch|ch)wiegen|bettel|wiegewohnt|bethi\\b|Bethaus|\\bGebett" # words containing "bett" or "wiege" not referring to objects
  )
  create_filter_output(dict)
}



#' @rdname tagfilter_household
#' @export
tagfilter_bedding <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    bedding = "Hauszeug|Ma(t|d)ra(t|z)|Be(tt|th)(deck|wer|zeug|sack|eingu(s|ß))|Be(tt|th)-(deck|wer|zeug|sack|eingu(s|ß))|Federbe(th|tt)|
    Decke|(Stro(ch|h)|Nacht)s(a|ä)ck|Kissen|(Unter|Deck)be(tt|th)"
  )
  dict$neg <- list(
    carriage = "Schlitten|(Pack|Rei(s|se))kissen|Wägelein|Kütschlein|Cabriolet|Kummeter", # descriptions of carriages
    cover = "zu decken", # verb
    book = "Goldschnitt", # excludes descriptions of books
    service = "ausbessern|flicken", # services concerning household textiles
    furniture = "Canapee|Fortepiano", # excludes furniture
    discover = "entdecke", # meaning discovery of smth
    name = "Neudecker|Decker", # family name
    horse = "Pfer(de|d)decke|bedecken", # horse covers, will be included in category for riding
    other = "Deckenstock", # unsure of meaning, but no household textile
    wood = "Holzdecke", # pieces of wood
    lid = "Deckel", # lid of a container
    ocr = "zugleichzudecke|Enideckend" # ocr mistake
    )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_tablelinen <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    tablelinen = "(Tisch|Tafel)(zeug|deck|t(u|ü)ch|(p|b)lunder)",
    napkin = "Serviette|Handt(ü|u)ch"

  )
  dict$neg <- list(
    napkinring = "Serviett(e|en)-(Ring|Schieber)" # placeholder
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_carpet <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
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

#' @rdname tagfilter_household
#' @export
tagfilter_chair <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    chair = "St(u|ü)hl|Sitz",
    bank = "B(a|ä)nk"
    # maybe exclude Sitz later, as it only finds two ads also containing Sessel (which are upholstery)
  )
  dict$neg <- list(
    bowel = "Stuhlgang", # bowel movement
    immo = "Landsitz|Dachst(u|ü)hl|Baurenhauß|Losament", # filters out immo ads
    verb_1 = "sitzen können", # description of seating
    verb_2 = "sitzt\\b|übersitzen", # verbs
    measure = "Stühle voll", # kind of measurement
    tool = "(Maschinen|Mühle|Dreh|Schneid)-St(u|ü)hl|Maschinen|(Mühle|Dreh|Schneid)st(u|ü)hl|Strump(f|ff)w(e|ä)ber-St(u|ü)hl|Strump(ff|f)w(ä|e)berstuhl|Strump(f|ff)-w(e|ä)ber-St(u|ü)hl|Hobelb(a|ä)nk", # tools for certain professions
    loom = "(W(ä|e)(b|ber)|B(a|ä)n(d|del)|P(o|a)(s|ss|ß)amen(t|ter))st(u|ü)hl|(W(ä|e)(b|ber)|B(a|ä)n(d|del)|P(o|a)(s|ss|ß)amen(t|ter))-Stuhl|Posamentier", # looms for weaving
    profession = "Stuhlschreiner|Strump(ff|f)w(ä|e)ber", # professions including "stuhl"
    possession = "Besitz", # words meaning possession including "sitz"
    meeting = "Sitzung|Be(y|i)sitzer", # word for meetings
    churchchairs = "(Frauen|Weibe(r|n)|M(a|ä)n(nen|n|ner)|Kirc(hen|h))sitz|(Frauen|Weibe(r|n)|M(a|ä)n(nen|n|ner)|Kirc(hen|h))-sitz|Weibe(r|n)vorsitz|Weibe(r|n)-Vorsitz|
    (Frauen|Weibe(r|n)|M(a|ä)n(nen|n|ner)|Kirc(hen|h)|Gold)st(u|ü)hl|(Frauen|Weibe(r|n)|M(a|ä)n(nen|n|ner)|Kirc(hen|h)|Gold)-st(u|ü)hl|Kanzel", # describing churchchairs (other category)
    church_1 = "St. Leonhardt", # filters out remaining churchchairs, v1
    church_2 = "Münsterkirch", # filters out remaining churchchairs, v2
    description = "sitz(ig|end)", # description of other objects (how many seats)
    noobject = "Besitz|unnsitz", # no objects
    domicile = "Wohnsitz", # word for domicile
    carriage = "Bernersitz|Kutsche|W(a|ä)g(en|ägel)|ban(c|k)|sitzig" # pointing to carriages or carriage parts (seats for carriages)
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_cabinet <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    cupboard = "(B|b)(u|ü)ff(e|er)t",
    dresser = "(K|C)o(mm|m)ode|(T|t)rumeau",
    cabinet = "(K|k)(a|ä)sten|(C|K|c|k)orpus|(S|s)chrank|(K|k)ästlein|(K|k)ensterlin",
    book = "(B|b)ücherkäst|(B|b)üchersch"
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
    clock = "B(ü|u)ffet-Uhr", # timepiece
    lottery = "Kasten-Ampts-Lotterie|Kastenam(b|d)ts-Lotterie|Kastenamt", # filters out lottery ads
    immo = "Kammer|Keller|Bodenzin(ß|s|ss)", # filters out immo ads
    toy = "Baukästen", # box for building bricks
    dollbox = "Polichinell", # finds pop up doll boxes
    paintbox = "Farbk(ä|a)sten", # describes paintboxes
    key = "(Schrank|Kasten)schlüssel" # means key to a cabinet
  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_stove <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    stoves = "(O|Ö|Oe)fen|(K|C)amin|(Oe|Ö|O)fel",
    grates = "Feu(er|r)h(u|ü)nd|K(u|ü)nstblech",
    bedpan = "Be(th|tt)pfanne"
  )
  dict$neg <- list(
    domestic = "Glät(t|te|ti)(o|ö|oe)fe|Glät(t|te|ti)-(o|ö|oe)fe", # domestic utensils
    kitchen = "Bratenöfelein", # kitchen utensil
    weapon = "Oefension", # description of use of weapons
    factory = "Schmöltzöfen", # factory equipment
    book = "Stadtbibliothek", # filters out book ads
    work = "Kaminfeger", # filters out work ads
    immo_1 = "Güter|Kaminküche|(K|C)amin-Kammer|Losament|Gasth(ö|o)fe", # filters out immo ads, v1
    immo_2 = "ohne (K|C)amin", # filters out immo ads, v2
    immo_3 = "mit (K|C)amin", # filters out immo ads, v3
    oil = "Tropfen", # fragrant oil for pouring on ovens
    iron = "Glätteöfelein", # ironing
    placename = "Oberhofen|Byrofeld", # placenames containing "ofen"
    alecove = "Ale(k|c)ofen|Alickofen|Alckhofen", # alecoves if written with an "f"
    familyname = "Bachofen", # family names containing "ofen",
    ocr = "kanovenst" # ocr mistakes
  )

  create_filter_output(dict)
}

#' @rdname tagfilter_household
#' @export
tagfilter_mirror <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    mirror = "Spiegel"
  )
  dict$neg <- list(
    accessoire = "Sackspiegel|Sack-Spiegel", # small portable mirror
    carriage = "Kutsch|Berline", # filters out windows for carriages
    uni = "Vorlesung|Experiment", # lectures for physics etc.
    book = "(Tugend|Fürsten)spiegel|Taschenkalender|Schriften|Druckere(y|i)", # filters out book ads
    instrument = "Spiegel-Telescop", # measuring instrument
    quality = "Spiegelglanz", # describes a quality of something, not the object
    placename = "Spiegel(g(a|ä)(ss|ß)|h(oo|o)f)" # placenames
  )
  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    timepiece = "(U|Ü|Ue)hr|Pendul"
  )
  dict$neg <- list(
    ocr = "Jühr", # ocr mistakes
    book = "Schriften", # filters out book ads
    official = "Polizeigericht", # official notices (e.g. lotteries of timepieces)
    shoe = "Schuhrin", # clasp for shoes
    verb = "zuehrend|spuhrt|führen|ausgeführ|herrühr|verspühr|derührt|gerührt|geführ|berühr|spühr|rühr|ührt|ühret", # verbs containing "uhr/ühr"
    other = "willk(ü|u)hr|Bauhr|Füuhr|Spuhr|Ge(b|d)(ü|u)hr|Aufführ|Führlohn|Wühren|ge(n|b)ührend|Anruhrung", # other words containing "uhr/ühr"
    key = "Uhrenschlüssel|Uhrschlüssel", # key for a clock, very often lost & found, maybe include later or in another category?
    chain_1 = "Uh(r|ren)(kette|hafte|b(a|ä)nd)|Schnüre|Uh(ren|r)-(kette|hafte|b(a|ä)nd)", # chain for pocketwatches, v1
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
    time_13 = "(el|l|eil)f Uhr", # specific time, number 11
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
    pocketwatch_1 = "(Taschen|Sack|Repetir|Repetitions|Frauenzimmer|Zylinder)(ue|u|ü)hr|(Taschen|Sack|Repetir|Repetitions|Frauenzimmer|Zylinder)-(ue|u|ü)hr", # pocketsize watches (accessoire), v1
    pocketwatch_2 = "Sack Uhr", # pocketsize watches (accessoire), v2
    pocketwatch_3 = "Uhr mit der Kette", # pocketsize watches (accessoire), v3
    profession = "(U|u)hrmach|Uhrenmach", # watchmakers, also excludes some relevant ads
    place = "\\bWuhr", # placename
    name = "Puhrmann", # family name
    driving = "F(u|ü)hr" # words associated with "driving" ("fuhr")
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    kitchentable = "Küch(e|en)-Tafel|Küch(e|en)tafel",
    table = "Tisch"
  )
  dict$neg <- list(
    place = "Gürtisch|Serintisch", # place name
    knife = "(Tisch|Tranchier)messer", # knives
    board = "Tischg(ä|a)ng", # board and lodging
    building = "Holzdecke", # pieces of wood
    ironing = "Glättetisch|Glätte-Tisch", # ironing board
    light = "Wachs(kerze|licht)|Tischlampe", # lighting for on tables
    plant = "Artischock", # plant
    key = "Schlü(ss|ß)el|Schlü(ß|ss)len", # keys for tables
    immo = "Keller", # filters out immo ads
    book = "Buch(drucker|binder)|Schriften", # filters out book ads
    adjective = "eritisch|(c|k)roatisch|mathematisch|juridisch|moscovitisch|pra(k|c|kk)tisch|sch(o|ö)ttisch|optisch|homeopatisch|helvetisch|
    zeutisch|tematisch|statistisch|(b|p)ritisch|betisch|(k|c)ritisch|passavantisch|musicatisch|romatisch|dramatisch|authentisch|brittisch|politisch|kosmetisch|rheumatisch|
    teristisch|systematisch|romantisch|schematisch|moscobitisch|elastisch|theoretisch|levantisch|alphabetisch|gichtisch|aromatisch|poetisch|zantisch|
    statisch|matisch|artistisch|patriotisch|neumotisch|mystisch|protestantisch|arithmetisch|orientalisch|elastisch|corbutisch|astatisch|juristisch|achromatisch|städtisch", # multiple adjectives containing "tisch"
    textile = "Tisch(zeug|tepp|t(u|ü)ch|deck|lach)|Tisch(zeug|tepp|t(u|ü)ch|deck|lach|plunder)|Unters(a|ä)tz", # table linen is in another category
    profession = "Tischler|Tischmacher", # professions containing "tisch"
    person = "Tischgenos|Arndtisch|Kantisch|Bahrdtisch|Sokratisch" # persons not objects
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_tableware <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    plate = "Teller",
    tableware = "Geschir|Biergl(ä|a)s|Tasse|Humpe|Becher",
    material = "Por(z|c)el|Steingut|Fayence|Kr(y|i)stallwaa|Kr(y|i)stall-Wa|Steingeschir|Stein-Geschir|Ma(j|i)oli(c|k)a|
    Zin(n|nen)geschirr|Zin(n|nen)-Geschirr|Zin(n|nen)w(aa|a)re|Zin(n|nen)-W(aa|a)re|Blechw(aa|a)re|Blech-W(aa|a)re",
    jug ="(K|C)anne|(K|C)arafe",
    beverage = "(C|K)aff(e|ee|é)ti(e|è)r|Cr(e|ê)mier"
  )
  dict$neg <- list(
    cupboard = "Geschirrkasten", # cupboard for cutlery
    wineobject = "Weingeschir|Herbstgeschir", # objects for wine production
    wine = "teller-Wein", # kind of wine
    cutlery = "(Silber|Kupfer)geschir", # cutlery
    toy = "Soldaten", # tin soldiers
    washing = "Bauc(hg|heg|hig|hyg)eschir|Bauc(h|he)-geschir|Waschgeschir|Wasch-geschir", # washing utensils
    bird = "Tauben", # pidgeaon "Geschirr"
    churchchair = "Cannel-Roost", # filters out churchchairs
    profession = "Kannengiesser|Schriftsteller", # profession
    tool = "(Schmie(de|d)|Schneid|We(b|ber))-Geschir|(Schmie(de|d)|Schneid|We(b|ber))geschir|Bierbrauere(y|i)", # tools
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


#' @rdname tagfilter_household
#' @export
tagfilter_bureau <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    bureau = "Se(k|c)ret(ä|ai)r", # one false positive for "Secretär" as a job
    workdesk ="P(u|ü)lt"
  )
  dict$neg <- list(
    other = "sepulta", # no objects
    verb = "pultzen", # verbs containing "pult"
    key = "Schlüssel", # keys for bureaus
    profession = "PolizeySekretär|Commission", # secretaries of certain commissions
    catapult = "Katapult|tapult" # catapults, sometimes the K is not correctly recognized
  )

  create_filter_output(dict)

}



#' @rdname tagfilter_household
#' @export
tagfilter_toy <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
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
    tin_3 = "Zin(n|nen)w(waa|a)ren zum Aufstellen",
    tin_4 = "Zin(n|nen)w(waa|a)ren zumAufstellen",
    tin_5 = "Zin(n|nen)w(waa|a)renzum Aufstellen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )
  create_filter_output(dict)
}

#' @rdname tagfilter_household
#' @export
tagfilter_game <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    billard = "Bi(l|ll|li|lli)ard",
    chess = "Schach(bret|spiel)",
    pocketgame = "Taschenspiel",
    domino = "Domino",
    lotto = "Lottospiel",
    cards = "Spielkarten|Whist|Trocquen-Kart|Spiel-Kart",
    dice = "Würfelspiel",
    bowling = "Kegel(spiel|ries)",
    general = "Spiel"
  )
  dict$neg <- list(
    playing = "zu spielen", # playing something (mostly music)
    marionette = "Margonetten-Spiel", # marionettes
    book = "Schriften", # filters out book ads
    competition = "ausgekegelt", # competition in certain games (bowling)
    authority = "Gesetz|Verordnung", # related official notices
    other = "Ausspiel|Beispiel", # other unrelated words containing "spiel"
    children = "ABC-Spiel", # toys and games for children
    work = "Spielkarten-Arbeit|Anstellung|kundig", # filters out related work ads
    fireworks = "Feuerwerkspiel", # fireworks
    music = "(Pianoforte|(K|C)lavier)spiel", # playing music, musicians
    thief = "Taschenspieler", # tricksters and thieves
    play = "spielend|gespielt|Spielens\\b", # meaning to play something
    name = "Nachspiel", # family names
    theater = "(Trau(r|er)|Lust|Schau)spiel" # ads for theater plays

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
# I excluded "Krug" because of a lot of false negatives (family name, place name) difficult to filter out
tagfilter_kitchen <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    distilling = "Brennhafen",
    washingup = "Wasserstein",
    preparation = "Stössel|M(ö|o)rser|Wahlh(ö|o)lz|Krauthobel|Messerwetz|Milchsecht|(Pfropfen|Zapfen)zieher|Beizebütte|Kaffeebrett|
    Ca(ff|f)ebrett|(T|Th)eema(sch|ch)ine|(T|Th)ee-Ma(sch|ch)ine",
    tea = "(T|Th)ee Ma(sch|ch)ine",
    bred = "Bro(d|t) Tr(ö|o)g",
    cooking = "Kochlöffel|Kochhafen|Schwenkkesse|Kochgeschir|Pfanne|Pfännlein|Kunsthafe",
    cooker = "Kaffeh(ee|e)rd|Sparh(ee|e)rd|Kochhafen|Kaffee-Dampfmaschine|Kaffeedampfmaschine|Braten(ö|o)fe|Dre(y|i)fu(ß|ss)",
    storage ="Salzfäss|Salzfass|Milchflasche|Fleischbütte|Sauerkrautstand|Sauerkraut-Stand|Zucker(dose|buchs)|Kühlstand|Krautstand|Fischtrog|
    Bro(d|t)korb|Bro(d|t)körb|T(hee|hée|ee)-Boit|Wasserzuber|Fischbeck(i|e)n|Bro(d|t)-Tr(ö|o)g",
    fish = "Brunn zu (Fisch|Krebs)",
    pan = "Casserolle|Bressière|Daubière|Bratenwender|Federbrä(t|th)er",
    baking = "Mödel|Waffleneisen|Wafflen-Eisen|Gugelhopfform|(Gugelhopf|Biscuit)-Form",
    press = "Pre(ss|ß)spindel|Pre(ss|ß)-Spindel|Pre(ss|ß)stang|Pre(ss|ß)-Stang|Handpre(ss|ß)|Stockpre(ss|ß)",
    mill = "(Oe|Ö)lmühl|Erdäpfelmühle|Erdäpfel-Mühle|(K|C)a(ff|f)emühle|(K|C)a(ff|f)e-Mühle|Kirschenmühle|Kirschen-Mühle|
    Mahlmühle|Mahl-Mühle"
  )
  dict$neg <- list(
    profession = "Pfannenschm(i|ie)d", # professions
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


#' @rdname tagfilter_household
#' @export
tagfilter_lighting <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    pocks = "Kindsblatern", # chicken pocks
    lighting = "Leuchter|Chandelier|Lampe|Latern|Nachtlicht|Lichtst(o|ö)ck|Lamep(en|e)gl(a|ä)s|
    Gingette",
    oil = "Lampe(nö|noe|n-Ö|n-Oe)(hl|l)",
    candle = "Wachs(kerzen|lichter)|Kerze",
    wick = "Lamendocht",
    other = "Lichtscheer"

  )
  dict$neg <- list(
    tool = "Kerzenmacher-Werckzeug|Kerzenmacherwerkzeug", # tools for candle making
    book = "Welttheater|Zauberlaterne", # filters out book ads
    fireworks = "Feuerwerk", # fireworks
    carriage = "einsp(ä|a)nnig|zweisp(ä|a)nnig|Kutsche", # excludes descriptions of carriages
    magica = "magica", # excludes laterna magica
    lecture = "Physik" # description of university lectures

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_instrument <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    tuning = "Stimmgabel",
    keyboard = "Flügel|Piano|(C|K)lavier|Spi(n|nn)et",
    drum = "Trommel", # maybe exclude here drums specifically for children (toys)?
    wind = "Flöte|Clarinet|Fagot|Trompet|Oboe|Posaune|Posth(o|ö)rn|Waldh(o|ö)rn",
    guitar = "G(ui|i)(t|tt)ar",
    string = "Violin|Contrebass|Cell(o|e)|Harfe",
    resin = "Colophonium|Geigenharz",
    strings = "Saiten",
    organ = "Aeoloti(k|c)on|Harmonika|Haus(ö|o)rgel|Clavicordium",
    misc = "Instrument"
  )
  dict$neg <- list(
    noobject = "Excellen(tz|z)|Pucelle", # no objects
    form = "wie eine Trompete", # formed like a trumpet
    invention = "erfunden", # technical instruments
    official = "Bewilligung|Ordnung", # official notices, prohibition of music etc.
    book = "Vorwort|Gedicht", # book ads
    food = "Vermicelle", # food
    adjective = "scharfen|excellen", # contains "harfe" or "celle"
    work = "Clavierschlagen", # excludes work ads
    divider = "Spanische Wand", # dividers with "Flügel"
    notes = "instrumental|Genre", # music notes
    concert_1 = "Conzert|Concert", # excludes announcements of concerts, v1
    concert_2 = "Freunde des", # excludes announcements of concerts, v2
    physics = "physikal|mechani|chirurgi|opti", # excludes optical, mechanical, chirurgical and physical instruments
    domestic = "Sticktrommel", # instrument for embroidery (see domestic appliances)
    immo = "Flügelgebäude|Fensterflügel|Fenster|Parcelle", # description of certain kind of building or building material
    food = "Geflügel", # chicken and oder poultry
    print = "abonniren|Magazin|Composition|Ges(a|ä)ng|Auszüg|Auszug|Heft|Begleitung", # excludes printed matter (musical notes), "Gesang" also excludes some relevant ads
    profession = "le(c|ç)on" # excludes work-related ads (music teaching), "Unterricht" other possibility, but excludes also some relevant ads

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_wallpaper <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    wallpaper = "Tape(t|z)"
  )
  dict$neg <- list(
    adjective = "tapetzier", # adjectives
    attraction = "vorstellen", # advertisments for attraction at the place of a "Tapezierer"
    service = "waschen", # services from "Tapezirer" unrelated to wallpaper
    furniture = "Fauteuil", # furniture with descriptions or from "Tapezirer"
    immo = "Behausung|Küche|austapezirt|tapez(i|ie)rt", # excludes immo ads with description of wallpaper
    sewing = "Nadeln" # sewing needles

  )

  create_filter_output(dict)

}



#' @rdname tagfilter_household
#' @export
tagfilter_cutlery <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    cutlery = "Lö(f|ff)el|Gabel|Messer",
    misc ="Silber(garnitur|geschir)|Besteck"
  )
  dict$neg <- list(
    profession = "Messerschmid|Saltzmesser", # excludes profession, maybe exclude if too many relevant ads cut
    measure = "Messerspi(ß|tz)", # measuring something
    shooting = "Schützen", # ads for "Schützenfest" with cutlery as prices
    death = "gewesen|hinterlassen", # filters out death notices
    ocr = "Zurückgabel", # ocr mistake (actually "Zurückgabe")
    saddle = "Löffel-Sattel", # certain kind of saddle
    measure = "Augenmesser|Löffelvoll", # tools for measuring
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
    noobject = "Messerlohn", # no objects
    razor = "Ras(ie|i)rmesser|Barb(ie|i)rmesser" # razors and

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_divider <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
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


#' @rdname tagfilter_household
#' @export
tagfilter_petobject <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    bird = "Käfig|Paarh(a|ä)(u|ü)(s|ß)|Paar-h(a|ä)(u|ü)(s|ß)|Anh(e|a|ä)n(g|ck|c|k)k(ö|ä|e)fi(g|ch)|Flugbret|
    Maisenschlag|Taubennest|Tauben-Nest|Taubenschlag|Vogel(schlag|käfig|käfich)",
    dog ="Hundesst(ä|a)ll|Hundest(ä|a)ll|Hundsst(ä|a)ll|(Hunde|Hunds)halsband",
    fish = "Goldfischglas"
  )
  dict$neg <- list(
    lost = "abhanden|verloren|verloffen|entloffen|zugeloffen|entlief|entflogen" # excludes lost and found animals with descriptions

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_upholstery <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    couch = "(C|K)an(a|e)(f|p|ps)(a|é|e)",
    armchair ="Sessel|F(au|ua)t(ei|eui)l"
  )
  dict$neg <- list(
    work = "Sesselfabrik", # occupation or place of manufacture for upholstery
    carpet = "Vorl(a|e)g(et|t)eppich" # carpets ment to be placed before a couch

  )

  create_filter_output(dict)

}



#' @rdname tagfilter_household
#' @export
tagfilter_domestic <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    embroidery = "Sticktrommel",
    sewing = "Nadeln|Nadlerwaaren|Nadler-Waaren|Nähk(ä|a)st|Steckgufen|Stecknadel|Fingerh(u|ü)t",
    knitting = "Strick(nadel|seckel|sstiefel)",
    iron = "Bügeleisen|Glät(t|te)(eisen|tisch|öfelein|ofen)|Kleidermange",
    washing ="(Wasch|Bauch)(kessel|bütte)|Wasch-(Kessel|Bütte)|Bauc(he|h)geschir|Bauc(he|h)-Geschir|Bauc(he|h)bütte|Bauc(he|h)-Bütte|
    Plunderstang",
    spinning = "Spinnr(a|ä)d|Spuhlrad|Schlumpstock"
  )
  dict$neg <- list(
    accessoire_1 = "(Vorsteck|Haar)nadel|Broche|Halstuch-Stecknadel|Halst(u|ü)c(h|her)nadel", # accessoire, v1
    accessoire_2 = "goldene Stecknadel" # accessoire, v2

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_garden <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    trellis = "Spalier",
    bench = "Gartenb(a|ä)nk|Bänklein",
    pot = "Gärtner-Cloches|Blumengestel",
    tool ="Baum-Sch(ee|e)re|Baumsch(ee|e)re|Haag-Sch(ee|e)re|Haagsch(ee|e)re|Baum-S(a|ä)ge|
    Baums(a|ä)ge|Gertel|Gartengeschirr"
  )
  dict$neg <- list(
    tool = "Hobelbänk", # tools
    immo = "Landg(u|ü)t|Juchart|Wohnung" # filters out immo ads

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_homedeco <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    vase = "Vase",
    general = "Zimmerzierrat"
  )
  dict$neg <- list(
    name = "Vaseria" # names

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_art <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    art = "Aquarel|Handzeichnung|(Oe|Ö)lgemäld|(Oe|Ö)lbild|Kupferstiche|Bilder|Gem(ä|äh)lde|M(a|ah)lere(y|i)en"
  )
  dict$neg <- list(
    food = "Frühbet|Früh-Bet", # vegetables early in the year
    embroidery = "Sticken", # description of embroidery
    collection = "Kabinet|Ausstellung|Sammlung|Vorstellung", # collections of art for visiting
    women = "Weibsbild", # meaning woman/women
    weapon = "Degen", # descriptions of weapons with decorations
    book_1 = "mit Kupferstich", # excludes prints in books
    book_2 = "Bilderb(u|ü)ch|Zeitschrift|Katalog|Schriften|Predigt|Fabel|Titel|Leseb(u|ü)ch|Erklärung|Erzählung|Beschreibung|B(i|ie)bel" # filters out book ads

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_bathobject <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    bath = "B(a|aa)d(bütt|kast|käst)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_household
#' @export
tagfilter_mischousehold <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "lend", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    misc_1 = "hausräthlich|hausrätlich",
    misc_2 = "Hausgerät"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}
