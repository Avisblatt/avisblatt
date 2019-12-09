#' Filter Quanteda Corpus: Labor
#' @export
tagfilter_labor <- function(){
  # Note how phrases are used inside a dictionary.
  # Dict elements may be converted to quanteda dictionaries
  # and because those are not bare regexp, we need
  # to hold expressions containing whitespace
  # separately, see also ?kwic Note on patterns.
  dict <- list()
  dict$pos <- list(
    work = "Arbeit$|arbeiten$|Arbeiter$",
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld",
    position_phrase_1 = "Platz als",
    position_phrase_2 = "Platz zu erhalten",
    position ="Anstellung|^Dienst$|^Dienste|einzutreten|unterzukommen|^Lohn|Verdienst"
  )

  dict$neg <- list(
    misc = "Ornement"
  )

  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Real Estate
#' @export
tagfilter_real_estate <- function(){
  dict <- list()
  dict$pos <- list(
    apartment = "losament|^Zimmer|Wohnung|Kammer|Keller"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus
#' @export
tagfilter_lotto <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_misc <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus
#' @export
tagfilter_transport <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus
#' @export
tagfilter_caritas <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus
#' @export
tagfilter_position <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus
#' @export
tagfilter_food <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus
#' @export
tagfilter_animal <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}




#' Filter Quanteda Corpus
#' @export
tagfilter_info <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_bling <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus
#' @export
tagfilter_textile <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_grocery <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_finance <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_print <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_household_goods <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_things <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus
#' @export
tagfilter_ <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}









