#' Dictionary Carriages and Related Objects
#' @export
tagfilter_carriage <- function(){
  dict <- list()
  dict$pos <- list(
    whip = "Chaisepeitsche|Schäsepeitsche|Chaise-Peitsche|Schäse-Peitsche",
    harness = "K[u|ü]mmet-Geschir|K[u|ü]mmetgeschir|Chaisegeschir|Chaise-Geschir|Schäsegeschir|Schäse-Geschir",
    coach = "W[a|aa]gen|Wägen",
    sleigh = "Familienschlitten",
    family = "Familie-W[a|aa]gen|Familienw[a|aa]gen",
    travel = "Reisew[a|aa]gen",
    riding = "Reitw[a|aa]gen|Reitwägelein",
    charabanc = "Char-à-banc|Char-a-banc",
    carriage = "Malaben|Malborough|Berline//b|[K|C]abriolet|Coupe|Kutsche//b|Kütschlein|[T|D]roscheke|[T|D]roschke|Trosque|Pascule|Pout//b",
    chaise = "Chaise|Chaiselein|Chaislein|Schäse|Schäslein|Schäselein",
    characteristic = "einspännig|zwe[y|i]spännig|4rädrig|1spännig|zwe[i|y]rädrig"
  )
  dict$neg <- list(
    trolley = "Leiterwagen|Bauernwagen|Bauern-Wagen|Dielenwagen|Dielen-Wagen|Bauernwägel", # trolleys, see other category
    verb = "wagen wir", # meaning of to dare something
    burial = "Todtenwagen", # transportation of corpses
    immo = "Wagenschopf|W[a|aa]genremise|W[a|aa]gen-Remise", # shelter for a carriage
    mail = "Post[a|aa|ä]gen|Postkutsche|Post-Anzeige|Postanzeige|Post-Chaise|Postillon", # mail coach
    measure_1 = "W[a|aa|ä]gen voll", # measurement of something in coaches, v1
    measure_2 = "Grube|w[a|aa|ä]genwei[s|ß]|Heu|Emd|verwährt", # carloads and objects measured in carloads
    scale = "Schnell-W[a|aa|ä]g|Schnellw[a|aa|ä]g|Waagbalcken|Romaine", # scales
    tool = "W[a|aa|ä]genwinde|W[a|aa|ä]gen-Winde|Winde", # tool for lift heavy loads
    lost_1 = "ab einem W[a|aa]gen", # losing something from a carriage, v1
    lost_2 = "ab seinem W[a|aa]gen", # losing something from a carriage, v2
    lost_3 = "verloren|verlohren|verlohrne", # losing something from a carriage, v3
    lost_4 = "fallen lassen", # losing something from a carriage, v4
    stroller = "Kinderwagen|Korbwagen", # stroller for children or as toys
    metal = "Berlinereisen|Berliner-Eisen", # specific kind of metal
    worker = "Kutscher|Lohnkutscher|F[ü|u]hrmann", # coachmen
    service_1 = "erbietet|Tanz-Anzeige", # services with a carriage, v1
    service_2 = "bereit stehen", # services with a carriage, v2
    service_3 = "hin zu führen", # services with a carriage, v3
    books = "Buchhandlung", # books containing instructions for carriage-making
    wallpaper = "Tapete|tapezieren", # wallpaper decoration for carriages
    travel_1 = "Retour-Chaise|verreisen|Retour-èferd", # travel by carriage v1
    travel_2 = "Platz haben", # travel by carriage, v2
    travel_3 = "zu fahren", # travel by carriage, v3
    travel_4 = "verlangt Platz", # travel by carriage, v4
    travel_5 = "diese Woche", # travel by carriage, v5
    travel_6 = "gewidmet|Einkehr", # travel by carriage, v6
    travel_7 = "dahin gehend", # travel by carriage, v7
    travelcompanion = "Reisegesellschaft|Comagnie|Gesellschafft|Gelegenheit|Compagnie", # searching and offering of company
    other = "Pantzer|Flaschenkette", # other small objects
    animal = "Chaise-Pferd|Reisepferd|Chaisepferd" # horses for drawing carriages
  )
  create_filter_output(dict)
}

#' Dictionary Trolleys (Carriages for Transportation of Objects, not People)
#' @export
tagfilter_trolley <- function(){
  dict <- list()
  dict$pos <- list(
    harness = "Sillen-Geschir|Sillengeschir"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Placeholder
#' @export
tagfilter_placeholder <- function(){
  dict <- list()
  dict$pos <- list(
    placeholder = "bibedibabediboo"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}
