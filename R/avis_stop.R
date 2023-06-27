#' Avisblatt Specific Stopword Dictionary
#'
#' Collection of stopwords.
#'
#' @param s character vector of stopwords
#' @param keep_standard boolean should default stopwords be kept when new stopwords are provided? Defaulst to TRUE.
#' @return list containing vectors of stopwords.
#' @importFrom stringi stri_unescape_unicode
#' @export
avis_stop <- function(s = NULL, keep_standard = TRUE){
  default_stopwords <- c("der", "die", "das", "dem", "den", "des", "dessen", "deren",
                         "denen", "ein", "einer", "einen", "eine", "eines", "einem", "diesen",
                         "dieses", "dieser", "diese", "dies", "welch", "welche", "welches",
                         "welchem", "welcher", "welchen", "solch", "solche", "solches",
                         "solchem", "solcher", "solchen", "einige", "einigen", "einigem",
                         "einiger", "einiges", "all", "alle", "aller", "allen", "allem",
                         "alles", "allda", "und", "oder", "aber", "weil", "wegen", "wenn",
                         "da\\u00df", "hingegen", "nicht", "so", "wie", "als", "auch",
                         "nur", "mit", "samt", "nebst", "dazu", "ferner", "ohne", "noch",
                         "etwas", "etwa", "circa", "extra", "mehr", "sehr", "ganz", "viel",
                         "dito", "item", "hiermit", "hiemit", "an", "am", "im", "in",
                         "ins", "hier", "dort", "beym", "bey", "bei", "beim", "neben",
                         "von", "vom", "davon", "auf", "aus", "\\u00fcber", "unter", "durch",
                         "hinter", "vor", "nach", "gleich", "seit", "bis", "nun", "davor",
                         "eben", "schon", "um", "zu", "zum", "zur", "f\\u00fcr", "gegen",
                         "et", "\\u00e0", "a", "no", "la", "le", "les", "pour", "par",
                         "dans", "ich", "mir", "wir", "uns", "unser", "er", "sie", "es",
                         "wir", "sie", "ihr", "ihm", "ihme", "sein", "seine", "seinen",
                         "seinem", "seiner", "ihre", "ihren", "ihrem", "ihrer", "wer",
                         "was", "wie", "man", "sich", "ist", "sind", "seyn", "wird", "werden",
                         "worden", "w\\u00fcrde", "w\\u00fcrden", "hat", "hatten", "habe",
                         "haben", "kann", "kan", "k\\u00f6nnen", "fr", "rthlr", "btz",
                         "fl", "kr", "xr")
  if(is.null(s)){
    avis_stopwords <- default_stopwords
  } else {
    if(keep_standard){
      avis_stopwords <- c(s, default_stopwords)
    } else {
      avis_stopwords <- s
    }
  }
  stri_unescape_unicode(avis_stopwords)
}
