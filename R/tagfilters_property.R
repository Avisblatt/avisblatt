#' Filter Quanteda Corpus: Housing within "labourinfo" section
#' @export
tagfilter_housing1 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo")
  dict$pos <- list(
    general = "Behausung|Wohnung|Losament|(G|g)ebäud|(H|h)äu(s|ß)lein", # not "Haus" as this often used in contact references in all sorts of ads
    spaces = "Rebacker|Garten|Gärtlein|Höflein|Matten|Juchart",
    buildings = "Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer",
    rooms = "\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\b|(G|g)emächer\b|Küchelein|Al(ek|k)oven",
    storage = "Magazin|Keller|Dachboden|Estri[ch|g]",
    amenities = "Abtritt|Abwasser|Altan|Bauchkessel|(B|b)runnen|Wasserleitung|Wasserstein",
    amenities_phrase = "Platz zu Holz"
  )
  dict$neg <- list(
    #exclude job ads for people attending to rooms
    othercat_job = "[m|M]agd|[k|K]necht|Mädchen"
  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Housing outside "labourinfo" section
#' @export
tagfilter_housing2 <- function(){
  dict <- list()
  dict$inapplicable <- list("labourinfo")
  dict$pos <- list(
    general = "Behausung|Wohnung|Losament|(G|g)ebäud|(H|h)äu(s|ß)lein", # not "Haus" as this often used in contact references in all sorts of ads
    spaces = "Rebacker|Garten|Gärtlein|Höflein|Matten|Juchart",
    buildings = "Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer",
    rooms = "\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\b|(G|g)emächer\b|Küchelein|Al(ek|k)oven",
    storage = "Magazin|Keller|Dachboden|Estri[ch|g]",
    amenities = "Abtritt|Abwasser|Altan|Bauchkessel|(B|b)runnen|Wasserleitung|Wasserstein",
    amenities_phrase = "Platz zu Holz"
  )
  dict$neg <- list(
    #exclude job ads for people attending to rooms
    othercat_job = "[m|M]agd|[k|K]necht|Mädchen"
  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Church Seat
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$pos <- list(
    seat = "[K|k]irchenstuhl|[K|k]irchensitz|[K|k]irchenstühl",
    folding_seat = "Anhencker",
    female = "Weibersitz|Weiber-Sitz|Weiberstuhl|Weiberstühle|Frauensitz|Frauenstuhl|Frauenstühle",
    male = "Mannsitz|Mannensitz|Mannstuhl|Mannstühle|Mannssitz|Mannsstuhl|Mannsstühle|Männersitz|Männerstuhl|Männerstühle"
  )
  create_filter_output(dict)
}
