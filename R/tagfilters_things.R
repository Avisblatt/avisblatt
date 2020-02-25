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
    handcart = "Handwägel|Leiterwägel|Leiterwagen|Sto[ss|ß]karren|Trogkarre",
    trolley = "Bauer[nwa|nwä|n-Wa|n-Wä]ge|Diele[nw|n-W]agen",
    harness = "Sillen-Geschir|Sillengeschir",
    objects = "Wagenkette|[Leit|Zug]seil|Fuhrwerk"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Health, Cosmetics and Drugstore Products
#' @export
tagfilter_health <- function(){
  dict <- list()
  dict$pos <- list(
    medicine_1 = "Bal[s|ss]am|Heilmittel|Hustentäfel|Salbe|Tin[k|c]tur|Gichtpapier",
    medicine_2 = "[P|D]ate pectoral",
    medicine_3 = "Isländisch Moos",
    medicine_4 = "Mittel gegen",
    medicine_5 = "Mittel wider",
    medicine_6 = "Mittel für",
    bandage = "Bandage|Bruchb[a|ä]nd|Watte",
    soap = "Seife",
    hair = "Bürste|Kämme|Kamm|Kämmche|Haarnadel",
    cosmetic_1 = "Po[mm|m]ade|Puderlade|Maca[ss|s]ara|[K|C]osmetik|Kosmeti[k|c]|Quaste",
    cosmetic_2 = "kosmetische Mittel",
    shaving = "Rasiermesser",
    perfume_1 = "Parf[ü|u]m|Parf[ü|u]merie|Fla[c|k]on|Rosenöl",
    perfume_2 = "eau de",
    perfume_3 = "Kolni[sch|sches] Wasser",
    chemical_1 = "Ameisengeist|A[mm|m]oniak|A[mm|m]onium|[Ch|C]onchil|Salpeter|Bittererde|Brockel-A[mm|m]ung|Brockela[mm|m]ung|Far[be|b]kraut",
    chemical_2 = "Chemische Präparate",
    chemical_3 = "Chemisches Präparat",
    polish = "Schmiere|Wichse|Schella[ck|k]"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}


#' Dictionary Weapons and Related Objects
#' @export
tagfilter_weapon <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Waffe",
    crossbow = "Armbrust",
    bayonet = "Bajo[nn|n]et",
    sword = "Degen|S[a|ä]bel",
    gun = "Flinte|Gewehr|Pistol|Büchse",
    cartridge = "Patron|Gibern",
    sheath = "Scheide"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Shop Equipment
#' @export
tagfilter_shopequip <- function(){
  dict <- list()
  dict$pos <- list(
    storage = "[C|K]omptoir-Kasten|[C|K]omptoirkasten|[C|K]omptoir-Kästen|[C|K]omptoirkästen|W[aa|a]renkäst|
    W[aa|a]renkast|W[aa|a]ren-Käst|W[aa|a]ren-Kast",
    display = "Glask[a|ä]st|Gläser-K[a|ä]st|Glaser-K[a|ä]st|Glasglocke",
    desk = "Ladent[c|k]orpus|Ladentisch",
    cash = "Geld[k|c]ass|Geld-[K|C]ass|Geldtrog|[K|C]asse|[K|C]assa",
    scale = "Fuhrw[ä|aa|a]ge|Einsat[zg|z-g]ewicht|Goldw[ä|aa|a]ge|lbstein|Magazin-W[ä|aa|a]ge|
    Schnellw[ä|aa|a]ge|Waaren-W[ä|aa|a]ge|Waarenw[ä|aa|a]ge|Waren-W[ä|aa|a]ge|Warenw[ä|aa|a]ge|Messinggewicht",
    counting = "W[aa|a]renzähl",
    general = "Handelsutensil"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Tools and Instruments
#' @export
tagfilter_tool <- function(){
  dict <- list()
  dict$pos <- list(
    fire = "Bla[s|se]balg",
    woodworking = "Bohrer|Brenneisen|Drehstuhl|H[o|ö]bel|Hackb[a|ä]nk|Hack-B[a|ä]nk|H[a|ä]mmer|Säge|Schneideisen|Schrabust[o|ö]ck|
    Schreinerwerckzeug|Schneidmaschine|Fugbl[o|ö]ch",
    screw = "Schraube",
    knife = "Barbiermesser",
    metalworking = "Ambos|Feldschmidt",
    stoneworking = "Schleifstein|Steinschleife",
    sharpening = "Streichriemen|Abziehleder",
    textileworking = "Strumpfw[e|ä]ber-Stuhl|Strumpfw[e|ä]berstuhl|Tuch-Pre[ss|ß]|Tuchpre[ss|ß]|Zwirnmaschine|Fadenzähler|
    Schwefelk[a|ä]st|Seidenrad",
    mill = "Kammr[ä|a]d|Wasserr[a|ä]d|Seide[nw|n-w]aage|Seidenwin[dm|d-m]aschine|Schwun[gr|g-r]ad",
    paper = "Siegelpre[ss|ß]",
    printing = "Kupferpre[ss|ß]",
    straw = "Strohschneid",
    distilling = "Brennhafen|Brennkessel",
    other = "Stemmeisen|Zange|[K|C]arabiner|Feile|Wasserwaag|Zirkel|Zollstab",
    general = "Werkzeug"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Stationary and Paperware
#' @export
tagfilter_stationary <- function(){
  dict <- list()
  dict$pos <- list(
    pen = "Bleistift|Griffel|Schreibfeder|Schreibzeug",
    drawing = "Zeichnungs[kreide|papier]|Pinsel|Far[ben|b|ben-|b-]käst|Far[ben|b|ben-|b-]kast",
    ink = "[T|D]inte",
    notebook = "Carnet|Notizb[u|ü]ch|Schreibb[u|ü]ch|Schreibmappe",
    paper = "Briefpapier|Briefpre[ss|ß]|Papier|Postpapier|Schreibkarte|Romain|Schreibrolle|Makulatur",
    cardboard = "Karton",
    slate = "Schieferta[f|v]el",
    seal = "Petschaft|Siegellack",
    other = "Lineal|Schreibunterlage|Federmesser|Scheere",
    general = "Schreibzeug"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Jewellery
#' @export
tagfilter_jewellery <- function(){
  dict <- list()
  dict$pos <- list(
    flowers = "Ballblume|Brautkr[a|ä]nz|Todtenkr[a|ä]nz|Totenkr[a|ä]nz|Kunstblume",
    necklace = "Halskette",
    earring = "Ohrenbeh[a|ä]ng|Ohrenring|Ohrbeh[a|ä]ng|Ohrring",
    bracelet = "Bracelet",
    pin = "Vorstecknadel",
    general = "Bijouteri"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Wood
#' @export
tagfilter_wood <- function(){
  dict <- list()
  dict$pos <- list(
    firewood = "Brennholz",
    material = "Buchenholz|Tannenholz|Ahorn-St[a|ä]mm|Weichse[lr|l-R]ohr",
    form = "Bodenholz|Fleckling|Führling|Holz-Raspel|Holzbock|Pf[a|ä]hl|[Prügel|Knebel]holz|
    S[a|ä]gspähn|Scheitholz|Stämme|Stammholz|Wellen|Daugenholz|Fa[ss|ß]dauge|Plütschi|Reisten|Sägb[a|ä]um",
    unknown = "Kammholz",
    general = "Holz|Klafter"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Barrels and Bottles
#' @export
tagfilter_barrel <- function(){
  dict <- list()
  dict$pos <- list(
    bottle = "Bouteillen|Pi[è|e|é]ces|Gutter|Selterserwasser-Kr[ü|u]ge",
    barrel = "Fa[ss|ß]|Fä[ss|ß]er|Stücklein"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Tobacco and Related Objects
#' @export
tagfilter_tobacco <- function(){
  dict <- list()
  dict$pos <- list(
    tobacco = "Taba[k|ck]|Rauchtaba[k|ck]|Schnupftaba[k|ck]",
    pipe = "Pfeife|Taba[k|ck|ks|cks]pfeife|Pfeifenkopf|Pfeifenraumer|Pfeifenrohr",
    storage = "[C|Z]igarren-Büchse|[C|Z]igarren-Etuis|[C|Z]igarrenbüchse|[C|Z]igarrenetuis|Taba[k|ck|ks|cks]dose|Taba[k|ck|ks|cks]beutel"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Hay and Straw
#' @export
tagfilter_hay <- function(){
  dict <- list()
  dict$pos <- list(
    hay = "Heu|Emd|Heugra[ss|ß]",
    straw = "Stroh",
    pasture = "Klee|Herbstweid"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Unspecified Wooden Objects
#' @export
tagfilter_woodobject <- function(){
  dict <- list()
  dict$pos <- list(
    woodturning = "Drechlse[rw|r-w]aar",
    carving = "Schnit[zw|z-w]aar",
    general = "Holdecke|H[ö|o]lzenwerk|H[ö|o]lzwerk"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder
  )
  create_filter_output(dict)
}

#' Dictionary Dung
#' @export
tagfilter_dung <- function(){
  dict <- list()
  dict$pos <- list(
    dung = "Bau\\b|K[ü|u]hbau|Pfer[d|de]bau|Schwei[n|ne]bau|Taubenmist|Ziegenbau"
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


