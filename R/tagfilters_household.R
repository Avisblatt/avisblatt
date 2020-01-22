#' Dictionary Bed
#' @export
tagfilter_bed <- function(){
  dict <- list()
  dict$pos <- list(
    bed = "[B|b]ett|[B|b]eth|[K|k]orbwag|[W|w]iege"
  )
  dict$neg <- list(
    name = "Elisabet|Babett", # names of people and places including "bett/beth"
    placename = "Bettigen|Bettingen", # placenames containing "Bett"
    textiles = "Bettdeck|Deckbet|Bettwerk|Bettwerkh|Bettzeug|Bethzeug|Bettsack|Bethsack|Bett-Tril|Betttril|Bethtril", # these are household textiles
    feathers = "Bettfede|Bethfede",
    misc ="[V|v]erschwiegen|bettel" # words containing "bett" or "wiege" not referring to objects
  )
  create_filter_output(dict)
}


#' Dictionary Household Textiles
#' @export
tagfilter_household_textile <- function(){
  dict <- list()
  dict$pos <- list(
    table_linen = "Tafeltuch|Tischtuch|Tischzeug|Tischdeck|Tischtüch",
    bedding = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|
    Bethzeug|Bettsack|Bethsack|Decke|Strochsack|Strohsäck|Kissen|Unterbett",
    carpet = "Teppi|Tepi|Tapi|Tappi|Bodentuch|Bodentüch",
    curtain = "Vorhang|Vorhäng|Jalousie",
    misc = "Schaubdeck"
  )
  dict$neg <- list(
    carriage = "Chaise|Schäse", # excludes carriages with drapes or other textiles
    name = "Neudecker", # family name
    horse = "Pferdedecke|Pferddecke", # horse covers, will be included in category for riding
    other = "Deckenstock", # unsure of meaning, but no household textile
    wood = "Holzdecke", # pieces of wood
    lid = "Deckel" # lid of a container
    )

  create_filter_output(dict)

}

#' Dictionary Chair
#' @export
tagfilter_chair <- function(){
  dict <- list()
  dict$pos <- list(
    chair = "[S|s]t[u|ü]hl|[S|s]itz"
    # maybe exclude Sitz later, as it only finds two ads also containing Sessel (which are upholstery)
  )
  dict$neg <- list(
    loom = "Webstuhl|Bandstuhl|Posamentstuhl|[W|w]eberstuhl", # looms for weaving
    churchchair = "Frauenstuhl|Weiberstuhl", # churchchairs are in another category
    profession = "Stuhlschreiner", # professions including "stuhl"
    possession = "besitz|Besitz", # words meaning possession including "sitz"
    meeting = "Sitzung", # word for meeting
    churchchairs = "Frauensitz|Weibersitz|Mannensitz|[K|k]irchensitz|Mannssitz|Männersitz", # describing churchchairs (other category)
    description = "sitzig|sitzend", # description of other objects (how many seats)
    domicile = "Wohnsitz", # word for domicile
    carriage = "[K|k]utsche|[W|w]agen|[W|w]ägelein|[W|w]ägelin|banc|bank" # pointing to carriages or carriage parts (seats for carriages)
  )

  create_filter_output(dict)

}

#' Dictionary Cupboards and Cabinets
#' @export
tagfilter_cabinet <- function(){
  dict <- list()
  dict$pos <- list(
    cupboard = "[B|b]uffet|[B|b]üffet|[B|b]üffert",
    dresser = "[K|C]ommode|Trumeau",
    cabinet = "[K|k]asten|[C|c]orpus|[S|s]chrank|[K|k]ästen|Kästlein|[K|k]orpus",
    book = "[B|b]ücherkäst"
  )
  # "[K|C]ommode" is problematic, since it can also be an adjective for other objects,
  # therefore here only included if upper case
  # "[S|s]ch[a|ä]fft" finds a lot of unrelated verbs (e.g. "angeschafft") and no ads in this sample
  # (therefore here excluded, maybe put it in later); exlusion of verbs through negative list has to be made
  # if Schafft is necessary for other time periods
  dict$neg <- list(
    toy = "Baukästen", # box for building bricks
    dollbox = "Polichinell", # finds pop up doll boxes
    paintbox = "Farbkästen|Farbkasten", # describes paintboxes
    key = "Schrankschlüssel" # means key to a cabinet
  )

  create_filter_output(dict)

}


#' Dictionary Stoves and Related Objects
#' @export
tagfilter_stove <- function(){
  dict <- list()
  dict$pos <- list(
    stoves = "[O|o|ö|Ö]fen|[O|o]efen|[K|k|C|c]amin|[O|o]efel|Öefel|öfel",
    grates = "[F|f]euerh[u|ü]nd|Kunstblech"
  )
  dict$neg <- list(
    iron = "Glätteöfelein", # ironing
    placename = "Oberhofen", # placenames containing "ofen"
    alecove = "Alekofen", # alecoves if written with an "f"
    familyname = "Bachofen" # family names containing "ofen"
  )

  # returns some ads for immo with an "ofen", not for the object itself
  # excluding these through combination with immo-tagfilter?

  create_filter_output(dict)

}

#' Dictionary Mirrors
#' @export
tagfilter_mirror <- function(){
  dict <- list()
  dict$pos <- list(
    mirror = "[S|s]piegel"
    # no negative list necessary so far
  )
  dict$neg <- list(
    quality = "[S|s]piegelglanz", # describes a quality of something, not the object
    placename = "Spiegelgasse|Spiegelgässl|Spiegelgäßl|Spiegelgaß" # a street in Basel
  )
  create_filter_output(dict)

}

#' Dictionary Timepieces
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$pos <- list(
    timepiece = "[U|u]hr|[P|p]endul"
  )
  dict$neg <- list(
    key = "Uhrenschlüssel|Uhrschlüssel", # key for a clock, very often lost & found, maybe include later or in another category?
    chain = "Uhrkette|Uhrenkette|Uhrhafte|Uhrenhafte|Uhrenband|Uhrband|Uhrenbänd|Uhrbänd", # chain for pocketwatches
    lost = "verloren|gefunden|Finder", # filters out lost and found ads (pocketwatches)
    book = "Uhrfeder", # booktitle
    time_1 = "Nachts Uhr", # nighttime, not object
    time_2 = "um Uhr", # specific time (if number not recognised)
    time_3 = "ein Uhr", # specific time, number 1
    time_4 = "zwei Uhr", # specific time, number 2
    time_5 = "drei Uhr", # specific time, number 3
    time_6 = "vier Uhr", # specific time, number 4
    time_7 = "fünf Uhr", # specific time, number 5
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
    pocketwatch = "Taschenuhr|Sackuhr|Repetiruhr|Frauenzimmeruhr|Zylinderuhr|Repetir-uhr|Frauenzimmer-uhr|Zylinder-uhr", # pocketsize watches (accessoire)
    profession = "[U|u]hrmach|Uhrenmach", # watchmakers, also excludes some relevant ads
    place = "\\bWuhr", # placename
    name = "Puhrmann", # family name
    driving = "[F|f]uhr" # words associated with "driving" ("fuhr")
  )

  create_filter_output(dict)

}

#' Dictionary Tables
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$pos <- list(
    kitchentable = "Küche-Tafel|Küchetafel|Küchentafel|Küchen-Tafel",
    table = "[T|t]isch"
  )
  dict$neg <- list(
    # negative list has to be extended
    adjective = "praktisch|schottisch|optisch|homeopatisch|helvetisch|
    romatisch|dramatisch|authentisch|brittisch|politisch|kosmetisch
    elastisch|theoretisch", # multiple adjectives containing "tisch"
    textile = "[T|t]ischzeug|[T|t]ischtepp|[T|t]ischtuch|[T|t]ischtüch", # table linen is in another category
    profession = "[T|t]ischler", # professions containing "tisch"
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
    tableware = "[G|g]eschir|[B|b]iergläs|[B|b]ierglas|Tasse|Humpe|Becher",
    material = "[P|p]orzel|[P|p]orcel|[S|s]teingut|[F|f]ayence|Krystallwaa|Krystall-Waa",
    jug ="[K|k|C|c]anne|[K|k|C|c]arafe"
  )
  dict$neg <- list(
    french = "couteller", # cutlery in French
    measure = "Becherweise", # measuring of something in cups
    wine = "Muscateller", # special kind of wine
    letter = "Briefsteller", # writer of a letter
    order = "Besteller", # ordering
    pipe = "Pfeifenkopf|Pfeifenköpf", # parts of tobacco pipes from porcelaine
    harness = "Pferdgeschir|Pferdegeschir|Fuhrgeschir|
    Chaisegeschir|Chaise-Geschir|Kummetgeschir|Kumetgeschir|
    Fuhrwerkgeschir|Sillengeschir", # different words containing "geschir" in the meaning of harness
    tavern = "Wirtshaus zur [K|k]anne" # an inn in Basel is called "Zur Kanne"
  )

  create_filter_output(dict)

}


#' Dictionary Bureaus and Work Desks
#' @export
tagfilter_bureau <- function(){
  dict <- list()
  dict$pos <- list(
    bureau = "[S|s]ekretär|[S|s]ecretär|[S|s]ecretair|[S|s]ekretair", # one false positive for "Secretär" as a job
    workdesk ="[P|p]ult"
  )
  dict$neg <- list(
    profession = "PolizeySekretär", # secretary of police
    catapult = "Katapult|tapult" # catapults, sometimes the K is not correctly recognized
  )

  create_filter_output(dict)

}

#' Dictionary Small Storage
#' @export
tagfilter_storage <- function(){
  dict <- list()
  dict$pos <- list(
    basket = "[K|k]orb|[K|k]örb",
    box ="[K|k]iste|[K|k]istch|[K|k]ästchen",
    bag = "[S|s]ack|[S|s]äcke|[S|s]äckch"
  )
  dict$neg <- list(
    carriage = "Chaise|Schäse", # excludes carriages with baskets or boxes
    stroller = "Kinderwagen|Kinderwäg", # excludes strollers with baskets
    pocketknife = "Sackmesser", # pocketknife
    travel= "Fussack|Fußsack|Reissack|Reisesack|Reis-Sack|Reise-Sack", # foot rest for carriages and bags for travel
    bed = "Strohsack|Bettsack|Nachtsack", # bag full of straw, used as bedding
    textile = "Sacktuch", # textile
    work = "lesen|schreiben|rechnen", # filtering out work ads
    grain = "Kernen", # filters out ads selling grain in bags
    food = "Erdäpfel|Grundbirne|Setzerdäpfel", # filters out ads selling different kind of food in bags
    measure_1 = "[K|k]orbweis|[K|k]istchenweis", # measuring something by baskets or boxes, variant 1
    measure_2 = "Kistchen zu", # measuring something by boxes, variant 2
    meausure_3 = "Kistchen von", # measuring something by boxes, variant 3
    measure_4 = "//d//sKistchen", # measuring something by boxes, variant 4
    meausure_5 = "Kistchen à", # measuring something by boxes, variant 5
    meausure_6 = "Kistchen v.", # measuring something by boxes, variant 6
    meausure_7 = "pr. Kistchen", # measuring something by boxes, variant 7
    measure_8 = "der Sack zu",  # measuring something by bags, variant 8
    measure_9 = "\\d\\sSack", # number of bags of something
    measure_9 = "\\d\\sSäck", # number of bags of something
    beehive = "Bienenkorb|Bienenkörb", # bee hives
    stroller = "[K|k]orbwage|[K|k]orbwäge", # strollers and prams (own category)
    profession = "Korbmacher|Korbhändler|Korbladen|KorbLaden", # professions and shops
    cabinet = "Bücherkäst", # book cabinets
    watch = "Sackuhr", # pocketwatch
    pistol = "Sack-Pistol|Sackpistol", # pocket pistols
    telescope = "Sack-Perspek|Sackpersp", # pocket telescope
    light = "Sackfeuer|Sacklatern", # pocket lighter and lantern
    graveyard = "Gottesacker", # graveyard
    textile = "[S|s]acktuch|[S|s]acktüch" # name for specific kind of cloth
  )
  # maybe exclude Bettsack and add it to bed dictionary?

  create_filter_output(dict)

}

#' Dictionary Toys for Children
#' @export
tagfilter_toy <- function(){
  dict <- list()
  dict$pos <- list(
    doll = "[P|p]uppe",
    misc = "Felsenburg|[S|s]pielwaare"
    # has to be extended with other toys
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
    billard = "[B|b]illard|[B|b]illiard|[B|b]ilard|[B|b]iliard",
    chess = "[S|s]chachbret|[S|s]chachspiel",
    pocketgame = "Taschenspiel"
    # has to be extended with other games
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Kitchen Utensils
#' @export
tagfilter_kitchen <- function(){
  dict <- list()
  dict$pos <- list(
    distilling = "Brennhafen",
    washingup = "Wasserstein",
    preparation = "Mörser|Wahlholz|Erdäpfel-Mühle|Kaffeemühle|Wahlhölz|Krauthobel|Messerwetz|Milchsecht",
    cooking = "Kochlöffel",
    cooker = "Kaffeheerd|Sparheerd|Kochhafen|KaffeeDampfmaschine",
    storage ="Zuber|Züber|Krüg|Kessel|Salzfäss|Salzfass|Milchflasche|Fleischbütte|Sauerkrautstand", # I excluded "Krug" because of a lot of false negatives (family name, place name) difficult to filter out
    # maybe a new dictionary for washing utensils? Bauchkessel, Waschkessel
    pan = "Casserolle|Bressière|Daubière|Bratenwender",
    baking = "Mödel|Waffleneisen"
  )
  dict$neg <- list(
    textile = "gemödelt", # certain kind of decoration of a textile
    preparation = "zubereit|Zubereit", # meaning preparation of something (containing "zuber")
    mineralwater = "Selterser|Selter", # mostly stored mineral water (bevarage), very few instances used for bottles only (these will get lost with this negative)
    immo = "ablaufen|Kammer", # filters out immo ads with kitchen and a "ablaufendem Wasserstein", "Kammer" filters out the rest of the immo ads
    weapon = "Messing-Mörser" # description of a weapon

  )

  create_filter_output(dict)

}

#' Dictionary Lighting
#' @export
tagfilter_lighting <- function(){
  dict <- list()
  dict$pos <- list(
    lighting = "Leuchter|Chandelier|Lampe|Laterne|Nachtlicht|Lichtstock|Lichtstöck"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder: so far no negatives necessary

  )

  create_filter_output(dict)

}

#' Dictionary Musical Instruments
#' @export
tagfilter_instrument <- function(){
  dict <- list()
  dict$pos <- list(
    tuning = "Stimmgabel",
    keyboard = "Flügel|Piano|Clavier|Klavier|Spinet",
    drum = "Trommel", # maybe exclude here drums specifically for children (toys)?
    wind = "Flöte|Clarinet|Fagot|Trompet",
    guitar = "Guitar",
    strings = "Violin|Contrebass|Cello",
    other = "Aeolotikon|Aeoloticon", # one ad gets lost with negative "Ges[a|ä]ng" - worth it?
    misc = "Instrument"
  )
  dict$neg <- list(
    concert = "Conzert|Concert", # excludes announcements of concerts
    physics = "physikal|mechani|chirurgi|opti", # excludes optical, mechanical, chirurgical and physical instruments
    domestic = "Sticktrommel", # instrument for embroidery (see domestic appliances)
    immo = "Flügelgebäude|Fensterflügel|Fenster", # description of certain kind of building or building material
    food = "Geflügel", # chicken and oder poultry
    print = "abonniren|Magazin|Composition|Ges[a|ä]ng|Auszüg|Auszug|Heft|Begleitung", # excludes printed matter (musical notes)
    profession = "Unterricht|lecon" # excludes work-related ads (music teaching)

  )

  create_filter_output(dict)

}

#' Dictionary Building Components
#' @export
tagfilter_building <- function(){
  dict <- list()
  dict$pos <- list(
    door = "Thüre", # possibly too many immo ads with "Thüre", negatives are tricky
    window = "Fenster|Kreuzstock|Kreuzstöck",
    misc = "Bodenplättl|Bodenplatt"
  )
  dict$neg <- list(
    divider = "Scheidwand", # excludes doors as part of room dividers
    cabinet = "Kasten", # excludes doors as a part of cabinets
    immo = "Ladenthüre|beschlüssig|beschlossen|Estrich" # words indicating immo-ads

  )

  create_filter_output(dict)

}

#' Dictionary Wallpaper
#' @export
tagfilter_wallpaper <- function(){
  dict <- list()
  dict$pos <- list(
    wallpaper = "Tapet"
  )
  dict$neg <- list(
    sewing = "Nadeln" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Suitcases
#' @export
tagfilter_suitcase <- function(){
  dict <- list()
  dict$pos <- list(
    suitcase = "Koffer|Coffre|Coffer",
    travelbag = "Reissack|Reisesack|Reissäck|Reisesäck|Reis-Sack|Reis-Säck|Reise-Sack|Reise-Säck"
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
    ocr = "Zurückgabel", # ocr mistake (actually "Zurückgabe")
    saddle = "Löffel-Sattel", # certain kind of saddle
    measure = "Augenmesser", # tools for measuring
    kitchen = "Transchiermesser|Kochlöffel|Messerwetzer", # kitchen tools
    music = "Stimmgabel", # tunig fork
    pocket = "Sackmesser", # pocketknife
    agriculture = "Heugabel|Ladgabel|Obstmesser|Mattenmesser|Matten-Messer",
    work = "Messerschmied|Messerschmid", # profession
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
    spanish_1 = "spanische Wand",
    spanish_2 = "spanische Wänd",
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
    bird = "Paarhäus|Käfig|Paarhaus",
    dog ="Hundesställ|Hundestall"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Upholstery
#' @export
tagfilter_upholstery <- function(){
  dict <- list()
  dict$pos <- list(
    couch = "Canapé|Kanapse|Kanape|Canape|Kanefa",
    armchair ="[S|s]essel|[F|f]auteil|[F|f]uateuil|[F|f]auteuil"
  )
  dict$neg <- list(
    work = "Sesselfabrik", # occupation or place of manufacture for upholstery
    carpet = "Vorlageteppich" # carpets ment to be placed before a couch

  )

  create_filter_output(dict)

}


#' Dictionary Houseplants and Related Objects
#' @export
tagfilter_plantobject <- function(){
  dict <- list()
  dict$pos <- list(
    tableware = "Blumengestel"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Domestic Appliances
#' @export
tagfilter_domestic <- function(){
  dict <- list()
  dict$pos <- list(
    embroidery = "Sticktrommel",
    iron = "Bügeleisen|Glätteisen|Glättetisch|Glätteöfelein",
    washing ="Waschkessel|Bauchegeschir|Bauchgeschir|Bauchebütte|Plunderstang",
    spinning = "Spinnrad|Spinnräd|Spuhlrad"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Garden Furniture and Objects
#' @export
tagfilter_garden <- function(){
  dict <- list()
  dict$pos <- list(
    trellis = "Spalier",
    bench = "Gartenbank|Bänklein|Gartenbänk",
    pot = "Gärtner-Cloches",
    tool ="Baum-Scheere"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Misc Household Goods (Unspecified)
#' @export
tagfilter_mischoushold <- function(){
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
