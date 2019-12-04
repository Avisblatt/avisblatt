#' Regular Expressions to Identify Work Related Ads
#'
#' Dictionary for manual adcontent
#' tag "arbeitsstelle".
#'
#' @export
dict_work <- function(){
  dictionary(list(
    work = "Arbeit$|arbeiten$|Arbeiter$",
    position ="Platz als|Anstellung|^Dienst$|^Dienste|einzutreten|unterzukommen|^Lohn|Verdienst",
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld")
  )
}


#' Regular Expressions to Identify Real Estate Related Ads
#'
#' Dictionary for manual adcontent tag "immo".
#'
#' @export
dict_real_estate <- function(){
  dictionary(list(
    apartment = "losament|^Zimmer|Wohnung|Kammer|Keller"
  ))
}

#' Get Document Ids by Dictionary
#' @export
corpus_id_by_dict <- function(corp, dict,
                               identifier = "id"){
  docnames(corp) <- docvars(corp, identifier)
  kwic_res <- kwic(corp,
                   pattern = dict,
                   valuetype = "regex")
  kwic_res$docname
}

