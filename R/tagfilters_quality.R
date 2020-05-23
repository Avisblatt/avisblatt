#' @export
tagfilter_secondhand <- function(){
  # secondhand items can often be identified by specific phrases and wordings
  # but absence of these phrases does not mean absence of secondhand items
  dict <- list()
  dict$pos <- list(
    used_phrase_1 = "halb neu",
    used_phrase_2 = "so viel als neu",
    used_phrase_3 = "noch neu",
    used_phrase_4 = "fast gantz neu",
    used_phrase_5 = "fast neu",
    used_phrase_6 = "nicht gar neu",
    used_phrase_7 = "nicht neu",
    used_phrase_8 = "noch schier neu",
    used_phrase_9 = "schier gantz neu",
    used_phrase_10 = "schier neu",
    used_phrase_11 = "meistens neu",
    used_phrase_12 = "sauber conditioniert",
    used_phrase_13 = "fast durchgehend neu",
    used_phrase_14 = "fast durchgehends neu",
    used_phrase_15 = "beinahe gantz neu",
    used_phrase_16 = "fast ganz neu",
    used_phrase_17 = "schier ganz neu",
    used_phrase_18 = "beinahe ganz neu",
    used_phrase_19 = "so viel wie neu",
    used_phrase_20 = "so viel als",
    used_phrase_21 = "halbalt",
    used_phrase_22 = "in bestem Stand",
    used_phrase_23 = "gut (c|k)ondition(ie|i)rt",
    used_phrase_24 = "(c|k)ondition(ie|i)rt",
    used_phrase_25 = "gut unterhalten",
    used_phrase_26 = "in gutem Stand",
    used_phrase_27 = "schon gebraucht",
    used_phrase_28 = "brauchbar",
    used_phrase_29 = "halbneu",
    used_phrase_30 = "halb alt",
    used_phrase_31 = "noch brauchbar",
    used_phrase_32 = "fast gan(tz|z) neu"
  )

  dict$neg <- list(
    # up until now no negatives necessary
  test = "testestest")

  create_filter_output(dict)
}

