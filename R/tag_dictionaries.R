# Tag Dictionary blueprint
# might want to add import and export
# operators for
# declarative json definitions of dicts.
# #' Dictionary
# #' @export
# tag_dict_ <- function(){
#   dict <- list()
#   dict$pos <- list(
#
#   )
#   dict$neg <- list(
#
#   )
#   dict
# }


#' Dictionary Labor
#' @export
tag_dict_work <- function(){
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
  dict
}


#' Dictionary Real Estate
#' @export
tag_dict_real_estate <- function(){
  dict <- list()
  dict$pos <- list(
    apartment = "losament|^Zimmer|Wohnung|Kammer|Keller"
  )
  # dict$neg <- list(
  #
  # )
  dict
}




merge_dicts <- function(...){
  l <- list(...)
  dict <- list()
  dict$pos <- unlist(sapply(l, "[[", "pos"), recursive = F)
  dict$neg <- unlist(sapply(l, "[[", "neg"), recursive = F)
  dict
}

m <- merge_dicts(tag_dict_bed(),tag_dict_work())
m$neg





#' Dictionary
#' @export
tag_dict_lotto <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_misc <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Dictionary
#' @export
tag_dict_transport <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Dictionary
#' @export
tag_dict_caritas <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Dictionary
#' @export
tag_dict_position <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Dictionary
#' @export
tag_dict_food <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Dictionary
#' @export
tag_dict_animal <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}




#' Dictionary
#' @export
tag_dict_info <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_bling <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_churchseat <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}



#' Dictionary
#' @export
tag_dict_textile <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_grocery <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_finance <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_print <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_household_goods <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_things <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}


#' Dictionary
#' @export
tag_dict_ <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  dict
}








#' Filter by Dictionary, Return Document IDs
#' @export
filter_by_dict <- function(corp, dict, identifier = "id",
                           ignore.case = TRUE){
  re_pos <- paste(unlist(dict$pos), collapse = "|")

  tf_pos <- grepl(re_pos, corp$documents$texts,
                  ignore.case = ignore.case)

  if(!is.null(dict$neg)){
    re_neg <- paste(unlist(dict$neg), collapse = "|")
    tf_neg <- !grepl(re_neg, corp$documents$texts,
                    ignore.case = ignore.case)
    sel <- tf_pos == tf_neg
  } else{
    sel <- tf_pos
  }

  ids <- docvars(corp, identifier)[sel]
  ids

}

