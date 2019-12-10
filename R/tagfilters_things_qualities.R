tagfilter_secondhand <- function(){
  # secondhand items can often be identified by specific phrases and wordings
  # but absence of these phrases does not mean absence of secondhand items
  dict <- list()
  dict$pos <- list(
    used_phrase_1 = "halb neu",
    used_phrase_2 = "so viel als neu",
    used_phrase_3 = "noch neu",
    used_phrase_4 = "fast gan[tz|z] neu",
    used_phrase_5 = "fast neu",
    used_phrase_6 = "nicht gar neu",
    used_phrase_7 = "nicht neu",
    used_phrase_8 = "noch schier neu",
    used_phrase_9 = "schier gan[tz|z] neu",
    used_phrase_10 = "schier neu",
    used_phrase_11 = "meistens neu",
    used_phrase_12 = "sauber conditioniert",
    used_phrase_13 = "fast durchgehend neu",
    used_phrase_14 = "fast durchgehends neu",
    used_phrase_14 = "beinahe gant[tz|z] neu"
  )

  dict$neg <- list(
    # up until now no negatives necessary
  )

  create_filter_output(dict)
}
