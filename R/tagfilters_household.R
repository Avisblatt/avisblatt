#' Dictionary Bed
#' @export
tagfilter_bed <- function(){
  dict <- list()
  dict$pos <- list(
    bed = "[B|b]ett|[B|b]eth|[K|k]orbwag|[W|w]iege"
  )
  dict$neg <- list(
    misc = "Elisabeth|Verschwiegenheit|bettel"
  )
  create_filter_output(dict)
}


#' Dictionary
#' @export
tagfilter_seats <- function(){
  dict <- list()
  dict$pos <- list(
    basics = "[S|s]essel|[S|s]t[u|ü]hl|[F|f][au|ua]teil"
  )
  dict$neg <- list(
    non_seats = "Webstuhl|Bandstuhl|Frauenstuhl|Weiberstuhl|Stuhlschreiner"
  )

  create_filter_output(dict)

}

