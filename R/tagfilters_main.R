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
    work = "Arbeit$|arbeiten|Arbeiter$",
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld",
    position_phrase_1 = "Platz als",
    position_phrase_2 = "Platz zu erhalten",
    position ="Anstellung|^Dienst$|^Dienste|einzutreten|unterzukommen|^Lohn|Verdienst"
  )
  dict$neg <- list(
    misc = "Ornement|Kundmachung"
    # some of the ads recognized by the filter are public announcements, introduced as "Kundmachung"
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


#' Filter Quanteda Corpus: Lottery
#' @export
tagfilter_lotto <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Other
#' @export
tagfilter_other <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Transport
#' @export
tagfilter_transport <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Charity
#' @export
tagfilter_charity <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Placement
#' @export
tagfilter_placement <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Board
#' @export
tagfilter_board <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Animal
#' @export
tagfilter_animal <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}




#' Filter Quanteda Corpus: Information
#' @export
tagfilter_info <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Jewellery
#' @export
tagfilter_bling <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Church Seat
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Filter Quanteda Corpus: Textile
#' @export
tagfilter_textile <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Grocery
#' @export
tagfilter_grocery <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Finance
#' @export
tagfilter_finance <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Print
#' @export
tagfilter_print <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Household
#' @export
tagfilter_household_goods <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Filter Quanteda Corpus: Miscellaneous Things
#' @export
tagfilter_things <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}









