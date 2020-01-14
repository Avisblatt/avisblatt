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
    table_linen = "Tafeltuch|Tischtuch|Tischzeug|Tischdeck",
    bedding = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|Bethzeug|Bettsack|Bethsack",
    carpet = "Teppi|Tepi|Tapi|Tappi|Bodentuch",
    curtain = "Vorhang|Vorhäng|Jalousie",
    misc = "Schaubdeck"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder as a negative
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
    cabinet = "[K|k]asten|[B|b]uffet|[C|c]orpus|[K|C]ommode|[S|s]chrank|[K|k]ästen|[B|b]üffet|[B|b]üffert|[B|b]ücherkäst"
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
    key = "Schrankschlüssel", # means key to a cabinet
  )

  create_filter_output(dict)

}


#' Dictionary Stoves and Related Objects
#' @export
tagfilter_stove <- function(){
  dict <- list()
  dict$pos <- list(
    stoves = "[O|o|ö|Ö]fen|[O|o]efen|[K|k|C|c]amin|[O|o]efel",
    grates = "[F|f]euerh[u|ü]nd"
  )
  dict$neg <- list(
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
    profession = "[U|u]hrmach", # watchmakers
    driving = "[F|f]uhr", # words associated with "driving" ("fuhr")
    time = "\\d\\sUhr" # mostly meaning a time and not an object
    #### this regex does not seem to work - is it false or is there another problem? It should catch e.g. "5 Uhr" or "12 Uhr"
  )

  create_filter_output(dict)

}

#' Dictionary Tables
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$pos <- list(
    table = "[T|t]isch|Küche-Tafel|Küchetafel|Küchentafel|Küchen-Tafel"
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
    tableware = "[G|g]eschir|[B|b]iergläs|[B|b]ierglas",
    material = "[P|p]orzel|[P|p]orcel|[S|s]teingut|[F|f]ayence|Krystallwaa|Krystall-Waa",
    jug ="[K|k|C|c]anne|[K|k|C|c]arafe"
  )
  dict$neg <- list(
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
    measure_1 = "[K|k]orbweis|[K|k]istchenweis", # measuring something by baskets or boxes, variant 1
    measure_2 = "Kistchen zu", # measuring something by boxes, variant 2
    meausure_3 = "Kistchen von", # measuring something by boxes, variant 3
    measure_4 = "//d//sKistchen", # measuring something by boxes, variant 4
    meausure_5 = "Kistchen à", # measuring something by boxes, variant 5
    meausure_6 = "Kistchen v.", # measuring something by boxes, variant 6
    meausure_7 = "pr. Kistchen", # measuring something by boxes, variant 7
    beehive = "Bienenkorb|Bienenkörb", # bee hives
    stroller = "[K|k]orbwage|[K|k]orbwäge", # strollers and prams (own category)
    profession = "Korbmacher|Korbhändler|Korbladen|KorbLaden", # professions and shops
    cabinet = "Bücherkäst", # book cabinets
    watch = "Sackuhr", # pocketwatch
    pistol = "Sack-Pistol|Sackpistol", # pocket pistols
    telescope = "Sack-Perspek|Sackpersp", # pocket telescope
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

#' Dictionary Kitchen Utensils NEU
#' @export
tagfilter_kitchen <- function(){
  dict <- list()
  dict$pos <- list(
    washingup = "Wasserstein",
    preparation = "Mörser|Wahlholz|Erdäpfel-Mühle|Kaffeemühle|Wahlhölz",
    cooker = "Kaffeeheerd|Sparheerd|Kochhafen|KaffeeDampfmaschine",
    storage ="Zuber|Krug|Krüg|Kessel||Krauthobel",
    pan = "Casserolle|Bressière|Daubière|Bratenwender",
    baking = "Mödel|Waffleneisen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Lighting NEU
#' @export
tagfilter_lighting <- function(){
  dict <- list()
  dict$pos <- list(
    lighting = "Leuchter|Chandelier|Lampe|Laterne|Nachtlicht"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Musical Instruments NEU
#' @export
tagfilter_instrument <- function(){
  dict <- list()
  dict$pos <- list(
    piano = "Flügel||Piano|Clavier|Klavier",
    drum = "Trommel",
    spinet = "Spinet",
    guitar = "Guitar",
    other = "Aeolotikon",
    misc = "Instrument"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Building Components NEU
#' @export
tagfilter_building <- function(){
  dict <- list()
  dict$pos <- list(
    door = "Thüre",
    window = "Fenster|Kreuzstock|Kreuzstöck",
    trellis = "Spalier",
    misc = "Trumeau" # is this really a building component?
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Wallpaper NEU
#' @export
tagfilter_wallpaper <- function(){
  dict <- list()
  dict$pos <- list(
    wallpaper = "Tapet|Tapisse"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Suitcases NEU
#' @export
tagfilter_suitcase <- function(){
  dict <- list()
  dict$pos <- list(
    tableware = "Koffer|Coffre|Coffer"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Cutlery NEU
#' @export
tagfilter_cutlery <- function(){
  dict <- list()
  dict$pos <- list(
    cutlery = "Löfel|Löffel|Gabel|Messer",
    misc ="Silbergarnitur"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Measuring Instruments NEU
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


#' Dictionary Room Dividers NEU
#' @export
tagfilter_divider <- function(){
  dict <- list()
  dict$pos <- list(
    spanish_1 = "spanische Wand",
    spanish_2 = "spanische Wänd",
    misc ="Kunstwand"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Objects for Pets NEU
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


#' Dictionary Upholstery NEU
#' @export
tagfilter_upholstery <- function(){
  dict <- list()
  dict$pos <- list(
    couch = "Canapé|Kanapse|Kanape|Canape",
    armchair ="[S|s]essel|[F|f]auteil|[F|f]uateuil|[F|f]auteuil"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Houseplants and Related Objects NEU
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


#' Dictionary Domestic Appliances NEU
#' @export
tagfilter_domestic <- function(){
  dict <- list()
  dict$pos <- list(
    iron = "Bügeleisen|Glätteisen",
    washing ="Waschkessel|Bauchegeschir|Bauchgeschir",
    spinning = "Spinnrad|Spinnräd"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Garden Furniture and Objects NEU
#' @export
tagfilter_gardenobject <- function(){
  dict <- list()
  dict$pos <- list(
    tableware = "Gartenbank|Bänklein",
    jug ="Gärtner-Cloches"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Paintings NEU
#' @export
tagfilter_painting <- function(){
  dict <- list()
  dict$pos <- list(
    tableware = "Portrait"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}


#' Dictionary Misc Household Goods NEU
#' @export
tagfilter_mischoushol <- function(){
  dict <- list()
  dict$pos <- list(
    misc_1 = "hausräthl. Sachen",
    misc_2 ="hausräthliche Effekte",
    misc_3 = "hausräthliche Sachen",
    misc_4 = "Hausgerätschaft"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}
