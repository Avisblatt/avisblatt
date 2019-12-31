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
#' NEU: Bodentuch, Vorhang, Vorhäng, Jalousie
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
#' NEU: Sitz
#' @export
tagfilter_chair <- function(){
  dict <- list()
  dict$pos <- list(
    chair = "[S|s]t[u|ü]hl|Sitz"
  )
  dict$neg <- list(
    loom = "Webstuhl|Bandstuhl|Posamentstuhl|[W|w]eberstuhl", # looms for weaving
    churchchair = "Frauenstuhl|Weiberstuhl", # churchchairs are in another category
    profession = "Stuhlschreiner" # professions including "stuhl"
  )

  create_filter_output(dict)

}

#' Dictionary Cupboards and Cabinets
#' NEU: Schrank, Kästen, Buffet, Büffert
#' @export
tagfilter_cabinet <- function(){
  dict <- list()
  dict$pos <- list(
    cabinet = "[K|k]asten|[B|b]uffet|[C|c]orpus|[K|C]ommode|[S|s]ch[a|ä]fft|[S|s]chrank|Kästen|Buffet|Büffert"
  )
  # "[K|C]ommode" is problematic, since it can also be an adjective for other objects,
  # therefore here only included if upper case
  # "[S|s]ch[a|ä]fft" finds a lot of unrelated verbs (e.g. "angeschafft")
  # exlusion of these through negative list has to be made
  # when studying the ads in detail
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder as a negative
  )

  create_filter_output(dict)

}


#' Dictionary Stoves and Related Objects
#' NEU: [O|o]efel
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
#' NEU: Pendule
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$pos <- list(
    timepiece = "[U|u]hr|Pendul"
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
#' NEU: [T|t]afel
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$pos <- list(
    table = "[T|t]isch|[T|t]afel"
  )
  dict$neg <- list(
    # negative list has to be extended
    adjective = "praktisch|schottisch|optisch|homeopatisch|helvetisch|
    romatisch|dramatisch|authentisch|brittisch|politisch|kosmetisch
    elastisch|theoretisch", # multiple adjectives containing "tisch"
    textile = "[T|t]ischzeug|[T|t]ischtepp", # table linen is in another category
    profession = "[T|t]ischler", # professions containing "tisch"
    person = "Tischgenos" # persons not objects
  )

  create_filter_output(dict)

}

#' Dictionary Tableware
#' NEU: [P|p]orzel|[P|p]orcel, Steingut|Fayence, Biergläs
#' @export
tagfilter_tableware <- function(){
  dict <- list()
  dict$pos <- list(
    tableware = "[G|g]eschir|[P|p]orzel|[P|p]orcel|Steingut|Fayence|Biergläs",
    jug ="[K|k|C|c]anne"
  )
  dict$neg <- list(
    harness = "Pferdgeschir|Pferdegeschir|Fuhrgeschir|
    Chaisegeschir|Chaise-Geschir|Kummetgeschir|Kumetgeschir|
    Fuhrwerkgeschir|Sillengeschir", # different words containing "geschir" in the meaning of harness
    tavern = "Wirtshaus zur [K|k]anne" # an inn in Basel is called "Zur Kanne"
  )

  create_filter_output(dict)

}


#' Dictionary Bureaus and Work Desks NEU
#' @export
tagfilter_bureau <- function(){
  dict <- list()
  dict$pos <- list(
    bureau = "[S|s]ekretär|[S|s]ecretär|[S|s]ecretair|[S|s]ekretair",
    workdesk ="[P|p]ult"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )

  create_filter_output(dict)

}

#' Dictionary Small Storage NEU
#' @export
tagfilter_storage <- function(){
  dict <- list()
  dict$pos <- list(
    box = "[K|k]orb|[K|k]örb",
    basket ="[K|k]iste|[K|k]istch|[K|k]ästchen",
    bag = "[S|s]ack|[S|s]äcke|[S|s]äckch"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )

  create_filter_output(dict)

}

#' Dictionary Toys for Children NEU
#' @export
tagfilter_toy <- function(){
  dict <- list()
  dict$pos <- list(
    doll = "[P|p]uppe",
    misc = "Felsenburg"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder

  )

  create_filter_output(dict)

}

#' Dictionary Games NEU
#' @export
tagfilter_game <- function(){
  dict <- list()
  dict$pos <- list(
    billard = "[B|b]illard",
    chess = "[S|s]chach"
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
