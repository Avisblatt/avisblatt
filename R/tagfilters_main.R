#' Provide list of umbrella terms
#' @export
umbrella_terms <- function(){
  umbrella_stand <- list(
    ut_consumables_drink = "alcohol|chocolate|coffee|milk|mineralwater|tea|syrup",
    ut_consumables_food = "butter|cheese|driedfruit|eggs|broth|fish|fruit|grain|honey|legumes|meat|mushrooms|nuts|\\boil|pasta|pastry|poultry|preserves|spices|sugar|tropicalfruit|vegetable", #\\boil otherwise will include tagfilter soil
    ut_consumables_drugs = "health|tobaccoproducts|mineralwater",
    ut_consumables_other = "firework",
    ut_household = "bed|bedding|carpet|tablelinen|cabinet|chair|stove|mirror|table|timepiece|bureau|tableware|toy|game|kitchen|lighting|instrument|wallpaper|cutlery|divider|petobject|upholstery|domestic|garden|homedeco|bathobject|art|antique|mischousehold",
    ut_clothing = "clothing|costume|uniform|outerwear|sleepwear|underwear|\\bhand|shoes|headdress|neck",
    ut_textiles = "bedding|carpet|clothing|costume|uniform|outerwear|sleepwear|underwear|\\bhand|handkerchief|shoes|headdress|neck|texmaterial|cloth|yarn",
    ut_things_accessories = "bag|mercery|jewellery|glasses|key|cane|umbrella|weapon|riding|tobaccoobjects",
    ut_things_storeNmove = "carriage|pushchair|suitcase|trolley|storage|container|barrel",
    ut_things_devicesNcomponents = "buildingcomponents|woodobjekt|shopequip|tavernobject|rope|well|agriculturalobjects|measure|stationary|tool|firestart|extinguisher|wineobject|miscobject",
    ut_things_materials = "animalraw|plantraw|plant|naturalia|wood|feed|hay|dung|soil",
    ut_animals = "dogs|birds|cats|otherpets|livestock" #what to do with animalobjects ?
  )
  umbrella_stand
}


#' Filter Quanteda Corpus: Lottery
#' @export
tagfilter_lottery <- function(){
  dict <- list()
  dict$pos <- list(
    lottery = "Lotter[i|y]|verlo[o|h]s",
    lot = "\\bLoos\\b",
    numbers_phrase = "folgende Nummern"
  )
  dict$neg <- list(
    childrensgame = "Lotterie-Spiel"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Lostandfound inside lostandfound section
#' @export
tagfilter_lostandfound1 <- function(){
  dict <- list()
  dict$applicable <- list("lostandfoundheader")
  dict$pos <- list(
    all = "." #
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Lostandfound outside lostandfound section
#' @export
tagfilter_lostandfound2 <- function(){
  dict <- list()
  dict$pos <- list(
    lost = "(V|v)erl(o|oh)r(ne|en)",
    found = "gefund(en|ne)",
    stolen = "gesto(hl|l)en"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}
