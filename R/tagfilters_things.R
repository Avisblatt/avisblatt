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
    mail = "Post[a|aa|ä]gen|Postkutsche|Post-Anzeige|Postanzeige", # mail coach
    measure = "W[a|aa|ä]gen voll", # measurement of something in coaches
    stroller = "Kinderwagen|Korbwagen", # stroller for children or as toys
    metal = "Berlinereisen|Berliner-Eisen", # specific kind of metal
    worker = "Kutscher|Lohnkutscher", # coachmen
    service_1 = "erbietet", # services with a carriage, 1
    service_2 = "bereit stehen", # services with a carriage, 2
    service_3 = "hin zu führen|Tanz-Anzeige", # services with a carriage, 3
    books = "Buchhandlung", # books containing instructions for carriage-making
    wallpaper = "Tapete|tapezieren", # wallpaper decoration for carriages
    travelcompanion = "Reisegesellschaft", # searching and offering of company
    animal = "Chaise-Pferd|Reisepferd|Chaisepferd" # horses for drawing carriages
  )
  create_filter_output(dict)
}

#' Dictionary Trolleys (Carriages for Transportation of Objects, not People)
#' @export
tagfilter_carriage <- function(){
  dict <- list()
  dict$pos <- list(
    harness = "Sillen-Geschir|Sillengeschir",
    sleigh = "Familienschlitten",
    family = "Familie-W[a|aa]gen|Familienw[a|aa]gen",
    travel = "Reisew[a|aa]gen",
    riding = "Reitw[a|aa]gen|Reitwägelein",
    carriage = "Malaben|Malborough|Berline//b|[K|C]abriolet|Coupe|Kutsche//b|Kütschlein|[T|D]roscheke|[T|D]roschke|Pascule|Pout//b",
    chaise = "Chaise|Chaiselein|Chaislein|Schäse|Schäslein|Schäselein",
    characteristic = "einspännig|zwe[y|i]spännig|4rädrig|1spännig|zwe[i|y]rädrig"
  )
  dict$neg <- list(
    burial = "Todtenwagen", # transportation of corpses
    immo = "Wagenschopf|W[a|aa]genremise|W[a|aa]gen-Remise", # shelter for a carriage
    mail = "Post[a|aa|ä]gen|Postkutsche|Post-Anzeige|Postanzeige", # mail coach
    measure = "W[a|aa|ä]gen voll", # measurement of something in coaches
    stroller = "Kinderwagen|Korbwagen", # stroller for children or as toys
    metal = "Berlinereisen|Berliner-Eisen", # specific kind of metal
    worker = "Kutscher|Lohnkutscher", # coachmen
    service_1 = "erbietet", # services with a carriage, 1
    service_2 = "bereit stehen", # services with a carriage, 2
    service_3 = "hin zu führen|Tanz-Anzeige", # services with a carriage, 3
    books = "Buchhandlung", # books containing instructions for carriage-making
    wallpaper = "Tapete|tapezieren", # wallpaper decoration for carriages
    travelcompanion = "Reisegesellschaft", # searching and offering of company
    animal = "Chaise-Pferd|Reisepferd|Chaisepferd" # horses for drawing carriages
  )
  create_filter_output(dict)
}
