#' Dictionary Bed
#' @export
tagfilter_bed <- function(){
  dict <- list()
  dict$pos <- list(
    bed = "[B|b]ett|[B|b]eth|[K|k]orbwag|[W|w]iege"
  )
  dict$neg <- list(
    misc = "Elisabet|[V|v]erschwiegen|bettel|Babett"
  )
  create_filter_output(dict)
}


#' Dictionary Household Textiles
#' @export
tagfilter_household_textile <- function(){
  dict <- list()
  dict$pos <- list(
    table_linen = "[T|t]afeltuch|[T|t]ischtuch|[T|t]ischzeug|[T|t]ischdeck",
    bedding = "[D|d]eckbet|[H|h]auszeug|[M|m]a[t|d]rat",
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
    non_seats = "Webstuhl|Bandstuhl|Frauenstuhl|Weiberstuhl|Stuhlschreiner"
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
    placenames = "Oberhofen|Bachofen",
    alecoves = "Aleko[v|f]en"
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
    mirror = "[S|s]piegel",
  )
  dict$neg <- list(
    # no exclusion necessary so far
  )

  create_filter_output(dict)

}

#' Dictionary Timepieces
#' @export
tagfilter_timepiece <- function(){
  dict <- list()
  dict$pos <- list(
    timepiece = "[U|u]hr",
  )
  dict$neg <- list(
    profession = "[U|u]hrmach",
    driving = "[F|f]uhr"
  )

  create_filter_output(dict)

}

#' Dictionary Tables
#' @export
tagfilter_table <- function(){
  dict <- list()
  dict$pos <- list(
    table = "[T|t]isch",
  )
  dict$neg <- list(
    # negative list has to be extended
    adjective = "praktisch|schottisch|optisch|homeopatisch",
    textile = "[T|t]ischzeug]",
    profession = "[T|t]ischler"
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
    Chaisegeschir|Chaise-Geschir|Kummetgeschir|Kumetgeschir",
    tavern = "Wirtshaus zur [K|k]anne"
  )

  create_filter_output(dict)

}



