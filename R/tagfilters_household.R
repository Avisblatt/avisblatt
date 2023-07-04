#' Tagfilter Household
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for household and furniture related ads and 
#' finds categories like beds, chairs and cabinets as well as smaller items like
#' mirrors, toys and kitchen utensils.
#' 
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
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
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bed = 'Be(th|tt)',
    cradle = 'Wiege'
  )
  dict$neg <- list(
    date = 'Bettag', # holiday
    book = 'Schriften|Schreiben|Bibel', # filters out book ads
    measure = 'wiegend', # measurement
    immo = 'Zimmer|K\u00fcche|Losament|Kammer|Wohnung|St\u00fcblein|Stube', # filters out immo ads (some actual beds are lost by excluding these)
    garden = 'Fr\u00fchbett', # garden object
    household = 'Be(th|tt)pfanne|Be(tt|th)eingu(\u00df|s)', # household objects
    alphabet = 'Alphabeth', # alphabet
    birth = 'Kindbe(t|th)\\\\b|Kind-Be(t|th\\\\b)', # filters out ads for work around birth
    verb_1 = 'gebe(tt|th)en|bettend', # verbs containing 'bett/beth'
    verb_2 = 'zu wiegen', # weighing v1
    verb_3 = 'Centner wiegen', # weighing v2
    name = '(Eli|Li)sabet|Babe(tt|th)|E(l|i)(\u00df|l|z|s)beth|Bethune', # names of people and places including 'bett/beth'
    placename = 'Betti(g|ng)en|Bettwil|Baselgebeth', # placenames containing 'Bett'
    textiles = 'Deckbet|Be(tt|th)(barchent|teppich|wer(k|kh|ck)|zeug|sack|tril|deck)|Be(tt|th)-(barchent|teppich|wer(k|kh)|zeug|sack|tril|deck)', # these are household textiles
    feathers = 'Be(tt|th)fede|Be(tt|th)-f(e|eh)de', # bed feathers
    misc ='(sch|ch)wiegen|bettel|wiegewohnt|bethi\\\\b|Bethaus|\\\\bGebett' # words containing 'bett' or 'wiege' not referring to objects
  )
  create_filter_output(dict)
}



#' @rdname tagfilter_household
#' @export
tagfilter_bedding <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bedding = 'Hauszeug|Ma(t|d)ra(t|z)|Be(tt|th)(deck|wer|zeug|sack|eingu(s|\u00df))|Be(tt|th)-(deck|wer|zeug|sack|eingu(s|\u00df))|Federbe(th|tt)|
    Decke|(Stro(ch|h)|Nacht)s(a|\u00e4)ck|Kissen|(Unter|Deck)be(tt|th)'
  )
  dict$neg <- list(
    carriage = 'Schlitten|(Pack|Rei(s|se))kissen|W\u00e4gelein|K\u00fctschlein|Cabriolet|Kummeter', # descriptions of carriages
    cover = 'zu decken', # verb
    book = 'Goldschnitt', # excludes descriptions of books
    service = 'ausbessern|flicken', # services concerning household textiles
    furniture = 'Canapee|Fortepiano', # excludes furniture
    discover = 'entdecke', # meaning discovery of smth
    name = 'Neudecker|Decker', # family name
    horse = 'Pfer(de|d)decke|bedecken', # horse covers, will be included in category for riding
    other = 'Deckenstock', # unsure of meaning, but no household textile
    wood = 'Holzdecke', # pieces of wood
    lid = 'Deckel', # lid of a container
    ocr = 'zugleichzudecke|Enideckend' # ocr mistake
    )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_tablelinen <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    tablelinen = '(Tisch|Tafel)(zeug|deck|t(u|\u00fc)ch|(p|b)lunder)',
    napkin = 'Serviette|Handt(\u00fc|u)ch'

  )
  dict$neg <- list(
    napkinring = 'Serviett(e|en)-(Ring|Schieber)' # placeholder
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_carpet <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    carpet = 'T(e|a)(pp|p)i|Bodent(u|\u00fc)ch',
    curtain = 'Vorh(a|\u00e4)ng'
  )
  dict$neg <- list(
    food = 'Tapioca', # names for food products
    instrument = 'Fortepiano' # excludes musical instruments (containig 'tepi')
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_chair <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    chair = 'St(u|\u00fc)hl|Sitz',
    bank = 'B(a|\u00e4)nk'
    # maybe exclude Sitz later, as it only finds two ads also containing Sessel (which are upholstery)
  )
  dict$neg <- list(
    bowel = 'Stuhlgang', # bowel movement
    immo = 'Landsitz|Dachst(u|\u00fc)hl|Baurenhau\u00df|Losament', # filters out immo ads
    verb_1 = 'sitzen k\u00f6nnen', # description of seating
    verb_2 = 'sitzt\\\\b|\u00fcbersitzen', # verbs
    measure = 'St\u00fchle voll', # kind of measurement
    tool = '(Maschinen|M\u00fchle|Dreh|Schneid)-St(u|\u00fc)hl|Maschinen|(M\u00fchle|Dreh|Schneid)st(u|\u00fc)hl|Strump(f|ff)w(e|\u00e4)ber-St(u|\u00fc)hl|Strump(ff|f)w(\u00e4|e)berstuhl|Strump(f|ff)-w(e|\u00e4)ber-St(u|\u00fc)hl|Hobelb(a|\u00e4)nk', # tools for certain professions
    loom = '(W(\u00e4|e)(b|ber)|B(a|\u00e4)n(d|del)|P(o|a)(s|ss|\u00df)amen(t|ter))st(u|\u00fc)hl|(W(\u00e4|e)(b|ber)|B(a|\u00e4)n(d|del)|P(o|a)(s|ss|\u00df)amen(t|ter))-Stuhl|Posamentier', # looms for weaving
    profession = 'Stuhlschreiner|Strump(ff|f)w(\u00e4|e)ber', # professions including 'stuhl'
    possession = 'Besitz', # words meaning possession including 'sitz'
    meeting = 'Sitzung|Be(y|i)sitzer', # word for meetings
    churchchairs = '(Frauen|Weibe(r|n)|M(a|\u00e4)n(nen|n|ner)|Kirc(hen|h))sitz|(Frauen|Weibe(r|n)|M(a|\u00e4)n(nen|n|ner)|Kirc(hen|h))-sitz|Weibe(r|n)vorsitz|Weibe(r|n)-Vorsitz|
    (Frauen|Weibe(r|n)|M(a|\u00e4)n(nen|n|ner)|Kirc(hen|h)|Gold)st(u|\u00fc)hl|(Frauen|Weibe(r|n)|M(a|\u00e4)n(nen|n|ner)|Kirc(hen|h)|Gold)-st(u|\u00fc)hl|Kanzel', # describing churchchairs (other category)
    church_1 = 'St. Leonhardt', # filters out remaining churchchairs, v1
    church_2 = 'M\u00fcnsterkirch', # filters out remaining churchchairs, v2
    description = 'sitz(ig|end)', # description of other objects (how many seats)
    noobject = 'Besitz|unnsitz', # no objects
    domicile = 'Wohnsitz', # word for domicile
    carriage = 'Bernersitz|Kutsche|W(a|\u00e4)g(en|\u00e4gel)|ban(c|k)|sitzig' # pointing to carriages or carriage parts (seats for carriages)
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_cabinet <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    cupboard = '(B|b)(u|\u00fc)ff(e|er)t',
    dresser = '(K|C)o(mm|m)ode|(T|t)rumeau',
    cabinet = '(K|k)(a|\u00e4)sten|(C|K|c|k)orpus|(S|s)chrank|(K|k)\u00e4stlein|(K|k)ensterlin',
    book = '(B|b)\u00fccherk\u00e4st|(B|b)\u00fcchersch'
  )
  # 'Sch(a|\u00e4)fft' finds a lot of unrelated verbs (e.g. 'angeschafft') and no ads in 1734/1834
  # (therefore here excluded, maybe put it in later); exlusion of verbs through negative list has to be made
  # if Schafft is necessary for other time periods
  dict$neg <- list(
    building = 'Wasserkasten', # waterstorage
    textile = 'Wachsdecken|Wachst\u00fccher|Kommodendecke', # cover of dressers
    measure = 'auf den Kasten', # measuring of something in boxes
    tool = 'Schwefelkasten', # tool for working with textiles
    instrument = 'Piano', # can be described with a 'Kasten'
    authority = 'Kasten des Klingenthals', # salt depot
    book = 'Anleitung|Juris|Grammatik', # filters out titles of books
    storage = 'Haberk(a|\u00e4)st', # small storage for grains
    carriage = 'Chais(e|en)k(a|\u00e4)st|Chais(e|en)-K(a|\u00e4)st|Sch\u00e4sen|Kutschen-Kasten', # carriages
    bath = 'Baadk(a|\u00e4)st|Badk(a|\u00e4)st', # bath tub
    clock = 'B(\u00fc|u)ffet-Uhr', # timepiece
    lottery = 'Kasten-Ampts-Lotterie|Kastenam(b|d)ts-Lotterie|Kastenamt', # filters out lottery ads
    immo = 'Kammer|Keller|Bodenzin(\u00df|s|ss)', # filters out immo ads
    toy = 'Bauk\u00e4sten', # box for building bricks
    dollbox = 'Polichinell', # finds pop up doll boxes
    paintbox = 'Farbk(\u00e4|a)sten', # describes paintboxes
    key = '(Schrank|Kasten)schl\u00fcssel' # means key to a cabinet
  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_stove <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    stoves = '(O|\u00d6|Oe)fen|(K|C)amin|(Oe|\u00d6|O)fel',
    grates = 'Feu(er|r)h(u|\u00fc)nd|K(u|\u00fc)nstblech',
    bedpan = 'Be(th|tt)pfanne'
  )
  dict$neg <- list(
    domestic = 'Gl\u00e4t(t|te|ti)(o|\u00f6|oe)fe|Gl\u00e4t(t|te|ti)-(o|\u00f6|oe)fe', # domestic utensils
    kitchen = 'Braten\u00f6felein', # kitchen utensil
    weapon = 'Oefension', # description of use of weapons
    factory = 'Schm\u00f6ltz\u00f6fen', # factory equipment
    book = 'Stadtbibliothek', # filters out book ads
    work = 'Kaminfeger', # filters out work ads
    immo_1 = 'G\u00fcter|Kamink\u00fcche|(K|C)amin-Kammer|Losament|Gasth(\u00f6|o)fe', # filters out immo ads, v1
    immo_2 = 'ohne (K|C)amin', # filters out immo ads, v2
    immo_3 = 'mit (K|C)amin', # filters out immo ads, v3
    oil = 'Tropfen', # fragrant oil for pouring on ovens
    iron = 'Gl\u00e4tte\u00f6felein', # ironing
    placename = 'Oberhofen|Byrofeld', # placenames containing 'ofen'
    alecove = 'Ale(k|c)ofen|Alickofen|Alckhofen', # alecoves if written with an 'f'
    familyname = 'Bachofen', # family names containing 'ofen',
    ocr = 'kanovenst' # ocr mistakes
  )

  create_filter_output(dict)
}

#' @rdname tagfilter_household
#' @export
tagfilter_mirror <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    mirror = 'Spiegel'
  )
  dict$neg <- list(
    accessoire = 'Sackspiegel|Sack-Spiegel', # small portable mirror
    carriage = 'Kutsch|Berline', # filters out windows for carriages
    uni = 'Vorlesung|Experiment', # lectures for physics etc.
    book = '(Tugend|F\u00fcrsten)spiegel|Taschenkalender|Schriften|Druckere(y|i)', # filters out book ads
    instrument = 'Spiegel-Telescop', # measuring instrument
    quality = 'Spiegelglanz', # describes a quality of something, not the object
    placename = 'Spiegel(g(a|\u00e4)(ss|\u00df)|h(oo|o)f)' # placenames
  )
  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    timepiece = '(U|\u00dc|Ue)hr|Pendul'
  )
  dict$neg <- list(
    ocr = 'J\u00fchr', # ocr mistakes
    book = 'Schriften', # filters out book ads
    official = 'Polizeigericht', # official notices (e.g. lotteries of timepieces)
    shoe = 'Schuhrin', # clasp for shoes
    verb = 'zuehrend|spuhrt|f\u00fchren|ausgef\u00fchr|herr\u00fchr|versp\u00fchr|der\u00fchrt|ger\u00fchrt|gef\u00fchr|ber\u00fchr|sp\u00fchr|r\u00fchr|\u00fchrt|\u00fchret', # verbs containing 'uhr/\u00fchr'
    other = 'willk(\u00fc|u)hr|Bauhr|F\u00fcuhr|Spuhr|Ge(b|d)(\u00fc|u)hr|Auff\u00fchr|F\u00fchrlohn|W\u00fchren|ge(n|b)\u00fchrend|Anruhrung', # other words containing 'uhr/\u00fchr'
    key = 'Uhrenschl\u00fcssel|Uhrschl\u00fcssel', # key for a clock, very often lost & found, maybe include later or in another category?
    chain_1 = 'Uh(r|ren)(kette|hafte|b(a|\u00e4)nd)|Schn\u00fcre|Uh(ren|r)-(kette|hafte|b(a|\u00e4)nd)', # chain for pocketwatches, v1
    chain_2 = 'Uhren Kette', # chain for pocketwatches, v2
    lost = 'verloren|gefunden|Finder', # filters out lost and found ads (pocketwatches)
    book = 'Uhrfeder', # booktitle
    time_1 = 'Nachts Uhr', # nighttime, not object
    time_2 = 'um Uhr', # specific time (if number not recognised)
    time_3 = 'ein Uhr', # specific time, number 1
    time_4 = 'zwe(i|y) Uhr', # specific time, number 2
    time_5 = 'dre(i|y) Uhr', # specific time, number 3
    time_6 = 'vier Uhr', # specific time, number 4
    time_7 = 'f\u00fcn(f|ff) Uhr', # specific time, number 5
    time_8 = 'sechs Uhr', # specific time, number 6
    time_9 = 'sieben Uhr', # specific time, number 7
    time_10 = 'acht Uhr', # specific time, number 8
    time_11 = 'neun Uhr', # specific time, number 9
    time_12 = 'zehn Uhr', # specific time, number 10
    time_13 = '(el|l|eil)f Uhr', # specific time, number 11
    time_14 = 'zw\u00f6lf Uhr', # specific time, number 12
    time_15 = 'zwey Uhr', # specific time, number 13
    time_16 = '\\\\d\\\\sUhr', # specific times with numbers, mostly meaning a time and not an object, version 1
    #### this regex does not seem to always work - what is the problem? I think the problem is the whitespace... It should catch e.g. '5 Uhr' or '12 Uhr'
    time_17 = '\\\\dUhr', # specific times with numbers, mostly meaning a time and not an object, version 2
    time_18 = 'tags Uhr', # specific time
    time_19 = 'fr\u00fch Uhr', # specific time
    time_20 = 'Uhrzeit', # specific time
    time_21 = 'Uhr vormittag', # specific time
    time_22 = 'Uhr Abend', # specific time
    time_23 = '\\\\d.\\\\sUhr', # specific time
    time_24 = 'Versteigerung|Versammlung|Tanz-Anzeige', # auctions and meetings with information about time
    pocketwatch_1 = '(Taschen|Sack|Repetir|Repetitions|Frauenzimmer|Zylinder)(ue|u|\u00fc)hr|(Taschen|Sack|Repetir|Repetitions|Frauenzimmer|Zylinder)-(ue|u|\u00fc)hr', # pocketsize watches (accessoire), v1
    pocketwatch_2 = 'Sack Uhr', # pocketsize watches (accessoire), v2
    pocketwatch_3 = 'Uhr mit der Kette', # pocketsize watches (accessoire), v3
    profession = '(U|u)hrmach|Uhrenmach', # watchmakers, also excludes some relevant ads
    place = '\\\\bWuhr', # placename
    name = 'Puhrmann', # family name
    driving = 'F(u|\u00fc)hr' # words associated with 'driving' ('fuhr')
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    kitchentable = 'K\u00fcch(e|en)-Tafel|K\u00fcch(e|en)tafel',
    table = 'Tisch'
  )
  dict$neg <- list(
    place = 'G\u00fcrtisch|Serintisch', # place name
    knife = '(Tisch|Tranchier)messer', # knives
    board = 'Tischg(\u00e4|a)ng', # board and lodging
    building = 'Holzdecke', # pieces of wood
    ironing = 'Gl\u00e4ttetisch|Gl\u00e4tte-Tisch', # ironing board
    light = 'Wachs(kerze|licht)|Tischlampe', # lighting for on tables
    plant = 'Artischock', # plant
    key = 'Schl\u00fc(ss|\u00df)el|Schl\u00fc(\u00df|ss)len', # keys for tables
    immo = 'Keller', # filters out immo ads
    book = 'Buch(drucker|binder)|Schriften', # filters out book ads
    adjective = 'eritisch|(c|k)roatisch|mathematisch|juridisch|moscovitisch|pra(k|c|kk)tisch|sch(o|\u00f6)ttisch|optisch|homeopatisch|helvetisch|
    zeutisch|tematisch|statistisch|(b|p)ritisch|betisch|(k|c)ritisch|passavantisch|musicatisch|romatisch|dramatisch|authentisch|brittisch|politisch|kosmetisch|rheumatisch|
    teristisch|systematisch|romantisch|schematisch|moscobitisch|elastisch|theoretisch|levantisch|alphabetisch|gichtisch|aromatisch|poetisch|zantisch|
    statisch|matisch|artistisch|patriotisch|neumotisch|mystisch|protestantisch|arithmetisch|orientalisch|elastisch|corbutisch|astatisch|juristisch|achromatisch|st\u00e4dtisch', # multiple adjectives containing 'tisch'
    textile = 'Tisch(zeug|tepp|t(u|\u00fc)ch|deck|lach)|Tisch(zeug|tepp|t(u|\u00fc)ch|deck|lach|plunder)|Unters(a|\u00e4)tz', # table linen is in another category
    profession = 'Tischler|Tischmacher', # professions containing 'tisch'
    person = 'Tischgenos|Arndtisch|Kantisch|Bahrdtisch|Sokratisch' # persons not objects
  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_tableware <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    plate = 'Teller',
    tableware = 'Geschir|Biergl(\u00e4|a)s|Tasse|Humpe|Becher',
    material = 'Por(z|c)el|Steingut|Fayence|Kr(y|i)stallwaa|Kr(y|i)stall-Wa|Steingeschir|Stein-Geschir|Ma(j|i)oli(c|k)a|
    Zin(n|nen)geschirr|Zin(n|nen)-Geschirr|Zin(n|nen)w(aa|a)re|Zin(n|nen)-W(aa|a)re|Blechw(aa|a)re|Blech-W(aa|a)re',
    jug ='(K|C)anne|(K|C)arafe',
    beverage = '(C|K)aff(e|ee|\u00e9)ti(e|\u00e8)r|Cr(e|\u00ea)mier'
  )
  dict$neg <- list(
    cupboard = 'Geschirrkasten', # cupboard for cutlery
    wineobject = 'Weingeschir|Herbstgeschir', # objects for wine production
    wine = 'teller-Wein', # kind of wine
    cutlery = '(Silber|Kupfer)geschir', # cutlery
    toy = 'Soldaten', # tin soldiers
    washing = 'Bauc(hg|heg|hig|hyg)eschir|Bauc(h|he)-geschir|Waschgeschir|Wasch-geschir', # washing utensils
    bird = 'Tauben', # pidgeaon 'Geschirr'
    churchchair = 'Cannel-Roost', # filters out churchchairs
    profession = 'Kannengiesser|Schriftsteller', # profession
    tool = '(Schmie(de|d)|Schneid|We(b|ber))-Geschir|(Schmie(de|d)|Schneid|We(b|ber))geschir|Bierbrauere(y|i)', # tools
    polish = 'Wichse|Schmiere', # polish for harnesses and cutlery
    immo = 'Acker|Reben|Zimmer|Losament|Landg(u|\u00fc)t', # filters out immo ads
    wine = 'Herbsgeschir|Trettgeschir|Kellergeschir', # tools for wine making
    french = 'couteller', # cutlery in French
    measure_1 = 'Becherweise', # measuring of something in cups, v1
    measure_2 = 'be(y|i)m Becher', # measuring of something in cups, v2
    measure_3 = 'zwe(y|i) Becher', # measuring of something in cups, v3
    measure_4 = 'Becher \u00e0', # measuring of something in cups, v4
    wine = 'Muscateller', # special kind of wine
    letter = 'Briefsteller', # writer of a letter
    order = 'Besteller', # ordering
    pipe = 'Pfeifenkopf|Pfeifenk\u00f6pf', # parts of tobacco pipes from porcelaine
    harness = 'Pferdgeschir|Pferdegeschir|Fuhrgeschir|Kutschengeschir|Kutschen-Geschir|
    Chaisegeschir|Chaise-Geschir|Kummetgeschir|Kumetgeschir|Pferd|Geschell|Hindergeschir|
    Fuhrwerkgeschir|Sillengeschir|Brustplatt-Geschir|Reitgeschir|Traggeschir', # different words containing 'geschir' in the meaning of harness
    tavern_1 = 'Wirtshaus zur Kanne', # an inn in Basel is called 'Zur Kanne'
    tavern_2 = 'Zur Kanne' # an inn in Basel is called 'Zur Kanne'
  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_bureau <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bureau = 'Se(k|c)ret(\u00e4|ai)r', # one false positive for 'Secret\u00e4r' as a job
    workdesk ='P(u|\u00fc)lt'
  )
  dict$neg <- list(
    other = 'sepulta', # no objects
    verb = 'pultzen', # verbs containing 'pult'
    key = 'Schl\u00fcssel', # keys for bureaus
    profession = 'PolizeySekret\u00e4r|Commission', # secretaries of certain commissions
    catapult = 'Katapult|tapult' # catapults, sometimes the K is not correctly recognized
  )

  create_filter_output(dict)

}



#' @rdname tagfilter_household
#' @export
tagfilter_toy <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    learning = 'ABC-Spiel',
    doll = 'Puppe|Puppen-Korbw(a|\u00e4)g|Puppenkorbw(a|\u00e4)g',
    bricks = 'Bauh\u00f6lz(chen|lein)|Baukasten',
    castle = 'Felsenburg',
    boy = 'Spiel-Boit|Spielboit|Aufstellschachtel',
    horse = '(Schwung|Steck|Stecken)pferd',
    general = 'Spiel(waa|a)re|Kinderspiel',
    tin_1 = 'zinn(erne|ern|erner|ernes) Soldat',
    tin_2 = 'Zin(n|nen)soldat|Zin(n|nen)-Soldat',
    tin_3 = 'Zin(n|nen)w(waa|a)ren zum Aufstellen',
    tin_4 = 'Zin(n|nen)w(waa|a)ren zumAufstellen',
    tin_5 = 'Zin(n|nen)w(waa|a)renzum Aufstellen'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder

  )
  create_filter_output(dict)
}

#' @rdname tagfilter_household
#' @export
tagfilter_game <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    billard = 'Bi(l|ll|li|lli)ard',
    chess = 'Schach(bret|spiel)',
    pocketgame = 'Taschenspiel',
    domino = 'Domino',
    lotto = 'Lottospiel',
    cards = 'Spielkarten|Whist|Trocquen-Kart|Spiel-Kart',
    dice = 'W\u00fcrfelspiel',
    bowling = 'Kegel(spiel|ries)',
    general = 'Spiel'
  )
  dict$neg <- list(
    playing = 'zu spielen', # playing something (mostly music)
    marionette = 'Margonetten-Spiel', # marionettes
    book = 'Schriften', # filters out book ads
    competition = 'ausgekegelt', # competition in certain games (bowling)
    authority = 'Gesetz|Verordnung', # related official notices
    other = 'Ausspiel|Beispiel', # other unrelated words containing 'spiel'
    children = 'ABC-Spiel', # toys and games for children
    work = 'Spielkarten-Arbeit|Anstellung|kundig', # filters out related work ads
    fireworks = 'Feuerwerkspiel', # fireworks
    music = '(Pianoforte|(K|C)lavier)spiel', # playing music, musicians
    thief = 'Taschenspieler', # tricksters and thieves
    play = 'spielend|gespielt|Spielens\\\\b', # meaning to play something
    name = 'Nachspiel', # family names
    theater = '(Trau(r|er)|Lust|Schau)spiel' # ads for theater plays

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
# I excluded 'Krug' because of a lot of false negatives (family name, place name) difficult to filter out
tagfilter_kitchen <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    distilling = 'Brennhafen',
    washingup = 'Wasserstein',
    preparation = 'St\u00f6ssel|M(\u00f6|o)rser|Wahlh(\u00f6|o)lz|Krauthobel|Messerwetz|Milchsecht|(Pfropfen|Zapfen)zieher|Beizeb\u00fctte|Kaffeebrett|
    Ca(ff|f)ebrett|(T|Th)eema(sch|ch)ine|(T|Th)ee-Ma(sch|ch)ine',
    tea = '(T|Th)ee Ma(sch|ch)ine',
    bred = 'Bro(d|t) Tr(\u00f6|o)g',
    cooking = 'Kochl\u00f6ffel|Kochhafen|Schwenkkesse|Kochgeschir|Pfanne|Pf\u00e4nnlein|Kunsthafe',
    cooker = 'Kaffeh(ee|e)rd|Sparh(ee|e)rd|Kochhafen|Kaffee-Dampfmaschine|Kaffeedampfmaschine|Braten(\u00f6|o)fe|Dre(y|i)fu(\u00df|ss)',
    storage ='Salzf\u00e4ss|Salzfass|Milchflasche|Fleischb\u00fctte|Sauerkrautstand|Sauerkraut-Stand|Zucker(dose|buchs)|K\u00fchlstand|Krautstand|Fischtrog|
    Bro(d|t)korb|Bro(d|t)k\u00f6rb|T(hee|h\u00e9e|ee)-Boit|Wasserzuber|Fischbeck(i|e)n|Bro(d|t)-Tr(\u00f6|o)g',
    fish = 'Brunn zu (Fisch|Krebs)',
    pan = 'Casserolle|Bressi\u00e8re|Daubi\u00e8re|Bratenwender|Federbr\u00e4(t|th)er',
    baking = 'M\u00f6del|Waffleneisen|Wafflen-Eisen|Gugelhopfform|(Gugelhopf|Biscuit)-Form',
    press = 'Pre(ss|\u00df)spindel|Pre(ss|\u00df)-Spindel|Pre(ss|\u00df)stang|Pre(ss|\u00df)-Stang|Handpre(ss|\u00df)|Stockpre(ss|\u00df)',
    mill = '(Oe|\u00d6)lm\u00fchl|Erd\u00e4pfelm\u00fchle|Erd\u00e4pfel-M\u00fchle|(K|C)a(ff|f)em\u00fchle|(K|C)a(ff|f)e-M\u00fchle|Kirschenm\u00fchle|Kirschen-M\u00fchle|
    Mahlm\u00fchle|Mahl-M\u00fchle'
  )
  dict$neg <- list(
    profession = 'Pfannenschm(i|ie)d', # professions
    death = 'von Basel', # excludes death notices
    measure_1 = '\\\\d.\\\\sZ(\u00fc|u)ber', # measurement of something, v1
    measure_2 = 'in Kr\u00fcgen', # measurement of something, v2
    measure_3 = 'Z(\u00fc|u)ber Wasser', # measurement of water, v3
    tool = 'F(a|\u00e4)rbkessel|F(a|\u00e4)rb-Kessel', # tools
    name = 'Her(rn|r) Kr(\u00fc|u)g', # family name
    textile = 'gem\u00f6delt', # certain kind of decoration of a textile
    mineralwater = 'Selterser|Selter', # mostly stored mineral water (bevarage), very few instances used for bottles only (these will get lost with this negative)
    immo = 'ablaufen|Kammer|H(a|\u00e4)u(s|\u00df)', # filters out immo ads with kitchen and a 'ablaufendem Wasserstein', 'Kammer' filters out the rest of the immo ads
    weapon = 'Messing-M\u00f6rser' # description of a weapon

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_lighting <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    pocks = 'Kindsblatern', # chicken pocks
    lighting = 'Leuchter|Chandelier|Lampe|Latern|Nachtlicht|Lichtst(o|\u00f6)ck|Lamep(en|e)gl(a|\u00e4)s|
    Gingette',
    oil = 'Lampe(n\u00f6|noe|n-\u00d6|n-Oe)(hl|l)',
    candle = 'Wachs(kerzen|lichter)|Kerze',
    wick = 'Lamendocht',
    other = 'Lichtscheer'

  )
  dict$neg <- list(
    tool = 'Kerzenmacher-Werckzeug|Kerzenmacherwerkzeug', # tools for candle making
    book = 'Welttheater|Zauberlaterne', # filters out book ads
    fireworks = 'Feuerwerk', # fireworks
    carriage = 'einsp(\u00e4|a)nnig|zweisp(\u00e4|a)nnig|Kutsche', # excludes descriptions of carriages
    magica = 'magica', # excludes laterna magica
    lecture = 'Physik' # description of university lectures

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_instrument <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    tuning = 'Stimmgabel',
    keyboard = 'Fl\u00fcgel|Piano|(C|K)lavier|Spi(n|nn)et',
    drum = 'Trommel', # maybe exclude here drums specifically for children (toys)?
    wind = 'Fl\u00f6te|Clarinet|Fagot|Trompet|Oboe|Posaune|Posth(o|\u00f6)rn|Waldh(o|\u00f6)rn',
    guitar = 'G(ui|i)(t|tt)ar',
    string = 'Violin|Contrebass|Cell(o|e)|Harfe',
    resin = 'Colophonium|Geigenharz',
    strings = 'Saiten',
    organ = 'Aeoloti(k|c)on|Harmonika|Haus(\u00f6|o)rgel|Clavicordium',
    misc = 'Instrument'
  )
  dict$neg <- list(
    noobject = 'Excellen(tz|z)|Pucelle', # no objects
    form = 'wie eine Trompete', # formed like a trumpet
    invention = 'erfunden', # technical instruments
    official = 'Bewilligung|Ordnung', # official notices, prohibition of music etc.
    book = 'Vorwort|Gedicht', # book ads
    food = 'Vermicelle', # food
    adjective = 'scharfen|excellen', # contains 'harfe' or 'celle'
    work = 'Clavierschlagen', # excludes work ads
    divider = 'Spanische Wand', # dividers with 'Fl\u00fcgel'
    notes = 'instrumental|Genre', # music notes
    concert_1 = 'Conzert|Concert', # excludes announcements of concerts, v1
    concert_2 = 'Freunde des', # excludes announcements of concerts, v2
    physics = 'physikal|mechani|chirurgi|opti', # excludes optical, mechanical, chirurgical and physical instruments
    domestic = 'Sticktrommel', # instrument for embroidery (see domestic appliances)
    immo = 'Fl\u00fcgelgeb\u00e4ude|Fensterfl\u00fcgel|Fenster|Parcelle', # description of certain kind of building or building material
    food = 'Gefl\u00fcgel', # chicken and oder poultry
    print = 'abonniren|Magazin|Composition|Ges(a|\u00e4)ng|Ausz\u00fcg|Auszug|Heft|Begleitung', # excludes printed matter (musical notes), 'Gesang' also excludes some relevant ads
    profession = 'le(c|\u00e7)on' # excludes work-related ads (music teaching), 'Unterricht' other possibility, but excludes also some relevant ads

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_wallpaper <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    wallpaper = 'Tape(t|z)'
  )
  dict$neg <- list(
    adjective = 'tapetzier', # adjectives
    attraction = 'vorstellen', # advertisments for attraction at the place of a 'Tapezierer'
    service = 'waschen', # services from 'Tapezirer' unrelated to wallpaper
    furniture = 'Fauteuil', # furniture with descriptions or from 'Tapezirer'
    immo = 'Behausung|K\u00fcche|austapezirt|tapez(i|ie)rt', # excludes immo ads with description of wallpaper
    sewing = 'Nadeln' # sewing needles

  )

  create_filter_output(dict)

}



#' @rdname tagfilter_household
#' @export
tagfilter_cutlery <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    cutlery = 'L\u00f6(f|ff)el|Gabel|Messer',
    misc ='Silber(garnitur|geschir)|Besteck'
  )
  dict$neg <- list(
    profession = 'Messerschmid|Saltzmesser', # excludes profession, maybe exclude if too many relevant ads cut
    measure = 'Messerspi(\u00df|tz)', # measuring something
    shooting = 'Sch\u00fctzen', # ads for 'Sch\u00fctzenfest' with cutlery as prices
    death = 'gewesen|hinterlassen', # filters out death notices
    ocr = 'Zur\u00fcckgabel', # ocr mistake (actually 'Zur\u00fcckgabe')
    saddle = 'L\u00f6ffel-Sattel', # certain kind of saddle
    measure = 'Augenmesser|L\u00f6ffelvoll', # tools for measuring
    kitchen = 'Transchiermesser|Kochl\u00f6ffel|Messerwetzer', # kitchen tools
    music = 'Stimmgabel', # tunig fork
    pocket = 'Sackmesser', # pocketknife
    agriculture = 'Heugabel|Ladgabel|Obstmesser|Mattenmesser|Matten-Messer',
    work = 'Kornmesser|messerey|Mehlmesser', # profession
    stove = 'Kachel\u00f6fel', # small stove containing 'l\u00f6fel'
    hunting = 'Waidmesser', # hunting knife
    diameter = 'Durchmesser', # diameter
    stationary = 'Federmesser|Falzmesser|Rastermesser', # tools for writing and for use of paper in general
    name = 'Langmesser', # family name
    noobject = 'Messerlohn', # no objects
    razor = 'Ras(ie|i)rmesser|Barb(ie|i)rmesser' # razors and

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_divider <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    spanish_1 = 'spa(n|nn)ische W(a|\u00e4)nd',
    spanish_2 = 'spa(n|nn)isch W(a|\u00e4)nd',
    divider = 'Scheidwand|Kunstwand'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_petobject <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bird = 'K\u00e4fig|Paarh(a|\u00e4)(u|\u00fc)(s|\u00df)|Paar-h(a|\u00e4)(u|\u00fc)(s|\u00df)|Anh(e|a|\u00e4)n(g|ck|c|k)k(\u00f6|\u00e4|e)fi(g|ch)|Flugbret|
    Maisenschlag|Taubennest|Tauben-Nest|Taubenschlag|Vogel(schlag|k\u00e4fig|k\u00e4fich)',
    dog ='Hundesst(\u00e4|a)ll|Hundest(\u00e4|a)ll|Hundsst(\u00e4|a)ll|(Hunde|Hunds)halsband',
    fish = 'Goldfischglas'
  )
  dict$neg <- list(
    lost = 'abhanden|verloren|verloffen|entloffen|zugeloffen|entlief|entflogen' # excludes lost and found animals with descriptions

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_upholstery <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    couch = '(C|K)an(a|e)(f|p|ps)(a|\u00e9|e)',
    armchair ='Sessel|F(au|ua)t(ei|eui)l'
  )
  dict$neg <- list(
    work = 'Sesselfabrik', # occupation or place of manufacture for upholstery
    carpet = 'Vorl(a|e)g(et|t)eppich' # carpets ment to be placed before a couch

  )

  create_filter_output(dict)

}



#' @rdname tagfilter_household
#' @export
tagfilter_domestic <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    embroidery = 'Sticktrommel',
    sewing = 'Nadeln|Nadlerwaaren|Nadler-Waaren|N\u00e4hk(\u00e4|a)st|Steckgufen|Stecknadel|Fingerh(u|\u00fc)t',
    knitting = 'Strick(nadel|seckel|sstiefel)',
    iron = 'B\u00fcgeleisen|Gl\u00e4t(t|te)(eisen|tisch|\u00f6felein|ofen)|Kleidermange',
    washing ='(Wasch|Bauch)(kessel|b\u00fctte)|Wasch-(Kessel|B\u00fctte)|Bauc(he|h)geschir|Bauc(he|h)-Geschir|Bauc(he|h)b\u00fctte|Bauc(he|h)-B\u00fctte|
    Plunderstang',
    spinning = 'Spinnr(a|\u00e4)d|Spuhlrad|Schlumpstock'
  )
  dict$neg <- list(
    accessoire_1 = '(Vorsteck|Haar)nadel|Broche|Halstuch-Stecknadel|Halst(u|\u00fc)c(h|her)nadel', # accessoire, v1
    accessoire_2 = 'goldene Stecknadel' # accessoire, v2

  )

  create_filter_output(dict)

}


#' @rdname tagfilter_household
#' @export
tagfilter_garden <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    trellis = 'Spalier',
    bench = 'Gartenb(a|\u00e4)nk|B\u00e4nklein',
    pot = 'G\u00e4rtner-Cloches|Blumengestel',
    tool ='Baum-Sch(ee|e)re|Baumsch(ee|e)re|Haag-Sch(ee|e)re|Haagsch(ee|e)re|Baum-S(a|\u00e4)ge|
    Baums(a|\u00e4)ge|Gertel|Gartengeschirr'
  )
  dict$neg <- list(
    tool = 'Hobelb\u00e4nk', # tools
    immo = 'Landg(u|\u00fc)t|Juchart|Wohnung' # filters out immo ads

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_homedeco <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    vase = 'Vase',
    general = 'Zimmerzierrat'
  )
  dict$neg <- list(
    name = 'Vaseria' # names

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_art <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    art = 'Aquarel|Handzeichnung|(Oe|\u00d6)lgem\u00e4ld|(Oe|\u00d6)lbild|Kupferstiche|Bilder|Gem(\u00e4|\u00e4h)lde|M(a|ah)lere(y|i)en'
  )
  dict$neg <- list(
    food = 'Fr\u00fchbet|Fr\u00fch-Bet', # vegetables early in the year
    embroidery = 'Sticken', # description of embroidery
    collection = 'Kabinet|Ausstellung|Sammlung|Vorstellung', # collections of art for visiting
    women = 'Weibsbild', # meaning woman/women
    weapon = 'Degen', # descriptions of weapons with decorations
    book_1 = 'mit Kupferstich', # excludes prints in books
    book_2 = 'Bilderb(u|\u00fc)ch|Zeitschrift|Katalog|Schriften|Predigt|Fabel|Titel|Leseb(u|\u00fc)ch|Erkl\u00e4rung|Erz\u00e4hlung|Beschreibung|B(i|ie)bel' # filters out book ads

  )

  create_filter_output(dict)

}

#' @rdname tagfilter_household
#' @export
tagfilter_bathobject <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    bath = 'B(a|aa)d(b\u00fctt|kast|k\u00e4st)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_household
#' @export
tagfilter_mischousehold <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    misc_1 = 'hausr\u00e4thlich|hausr\u00e4tlich',
    misc_2 = 'Hausger\u00e4t'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder

  )

  create_filter_output(dict)

}
