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
    table_linen = "[T|t]afeltuch|[T|t]ischtuch|[T|t]ischzeug|[T|t]ischdeck",
    bedding = "[D|d]eckbet|[H|h]auszeug|[M|m]a[t|d]rat|[B|b]ettdeck|[B|b]e[tt|th]wer|Bettzeug|Bethzeug|Bettsack|Bethsack",
    carpet = "[T|t]eppi|[T|t][e|a]pi",
    misc = "[S|s]chaubdeck"
  )
  dict$neg <- list(
    # no exclusion necessary so far
  )

  create_filter_output(dict)

}

#' Dictionary Seating Furniture
#' @export
tagfilter_seat <- function(){
  dict <- list()
  dict$pos <- list(
    upholstery = "[S|s]essel|[F|f][au|ua]teil",
    chair = "[S|s]t[u|ü]hl"
  )
  dict$neg <- list(
    loom = "Webstuhl|Bandstuhl|Posamentstuhl|[W|w]eberstuhl", # looms for weaving
    churchchair = "Frauenstuhl|Weiberstuhl", # churchchairs are in another category
    profession = "Stuhlschreiner" # professions including "stuhl"
  )

  create_filter_output(dict)

}

#' Dictionary Cupboards, Cabinets and Storage
#' @export
tagfilter_cabinet <- function(){
  dict <- list()
  dict$pos <- list(
    cabinet = "[K|k]asten|[B|b]uffet|[C|c]orpus|[K|C]ommode|[S|s]ch[a|ä]fft"
  )
  # "[K|C]ommode" is problematic, since it can also be an adjective for other objects,
  # therefore here only included if upper case
  # "[S|s]ch[a|ä]fft" finds a lot of unrelated verbs (e.g. "angeschafft")
  # exlusion of these through negative list has to be made
  # when studying the ads in detail
  dict$neg <- list(
    # has to be extended with verbs
  )

  create_filter_output(dict)

}


#' Dictionary Stoves and Related Objects
#' @export
tagfilter_stove <- function(){
  dict <- list()
  dict$pos <- list(
    stoves = "[O|o|ö|Ö]fen|[O|o]efen|[K|k|C|c]amin",
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
    placename = "Spiegelgasse|Spiegelgässl" # a street in Basel
  )
  create_filter_output(dict)

}

#' Dictionary Timepieces
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$pos <- list(
    timepiece = "[U|u]hr"
  )
  dict$neg <- list(
    profession = "[U|u]hrmach", # watchmakers
    driving = "[F|f]uhr", # words associated with "driving" ("fuhr")
    time = "\d\sUhr" # mostly meaning a time and not an object
    #### this regex does not seem to work - is it false or is there another problem? It should catch e.g. "5 Uhr" or "12 Uhr"
  )

  create_filter_output(dict)

}

#' Dictionary Tables
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$pos <- list(
    table = "[T|t]isch"
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
#' @export
tagfilter_tableware <- function(){
  dict <- list()
  dict$pos <- list(
    tableware = "[G|g]eschir",
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



