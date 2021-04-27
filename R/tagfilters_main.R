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



#' Filter Quanteda Corpus: Print inside bookstore section
#' @export
tagfilter_print1 <- function(){
  dict <- list()
  dict$applicable <- list("bookstore")
  dict$pos <- list(
    all = "." #
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Print outside bookstore section
#' @export
tagfilter_print2 <- function(){
  dict <- list()
  dict$pos <- list(
    book = "^Buch$|^Bücher[n]$|^Bucher$",
    edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
    material = "^gedruckt$|^Pergament$",
    person = "Buchhändler|^Buchdrucker$|^Buchbinder$",
    place = "Buchhand|Buchdruckere[y|i]|Buchladen|Leihbibl|Leseanstalt",
    format_1 = "\\bin Fol.$",
    format_2 = "in 4to.",
    format_3 = "4°|8°|tom.$|[O|o]ctavo|^Bogen$|^[1-9] Bögen$|Halbfranzband|^[ein|un]gebunden$|^brosch[.|iert]$",
    format_4 = "in [1-9] Bänden",
    format_5 = "^gedruckte[n] Fortsetzung$",
    ausstattung = "^Kupf[f]er$|Holzschnitt|Stahlstich",
    catalog = "Catalogus|Katalog",
    participant = "Mithalte*|Pr[ae|ä]numerant[en]|Abonnent",
    types = "Wörterbuch|Zeitung|Zeitschrift",
    type = "Neueste Schriften",
    title_1 = "Rauracher|Rau-racher|Raura-cher",
    title_2 = "Allgemeine[n] Zeitung",
    title_3 = "Christliche[r|n] Volksbote*",
    title_5 = "Annalen",
    title_6 = "Missions-Magazin",
    title_7 = "Basler-Zeitung|Basler Zeitung",
    title_8 = "Wochenblatt"
  )
  dict$neg <- list(

    lost_prayerbooks = "Psalmbuch|[g|G]esangbuch",
    region = "Entlibuch|Schönenbuch",
    bible = "Buch Mose",
    work = "Platz als",
    other = "Haushaltungsbuch|[Z|z]uber|Fischbeckin|Oefelin|Rohre|Schuffe|chuffe|Geschir|Meldung|Tabacks-Buchs|Tabacksbuchs|Anfangsbuchstabe[n]|Buchstabe[n]|Buchführung|Buchhaltung|Buchsbaum|Buchenholz|Pergamenter|Foulard|Näharbeit"
  )
  create_filter_output(dict)
}
