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
    work = "Beruf|arbeiten|Beschäfti|beschäfti|Besorgung|kochen|Kochen|nähen|Waschen|glätten|Glätten",
    work_phrase_1 = "zu waschen",
    #removed \\bArbeiter\\b, \\bArbeit\\b and rechtschaffen for now, as it produced too many false positives
    qualification = "\\bZeugnisse|\\erfahrene|versteht|geübte",
    position = "[m|M]agd|[k|K]necht|Köchin|Seidenbandweber|Seidenweber|Seidenwinder|Zettler",
    apprentice = "Lehrling|Lehrjung|in die Lehr|Lehrgeld",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDienst\\b|\\bDienste\\b|einzutreten|eintreten\\b|unterzukommen|\\bLohn\\b|Verdienst"
  )
  dict$neg <- list(
    #"darin zu / zum kochen": describes cookware, not people
    misc = "Ornement",
    warnig = "warne|Warnung",
    misc_phrase1 = "zum kochen",
    misc_phrase2 = "darin zu kochen",
    misc_phrase3 = "Dienst zu erweisen",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "[B|b]eerdigt|verstorben|dito|Dito|bendaselbst|unrichtig|genöthigt",
    othercat_info_phrase1 = "meinem Namen",
    othercat_realestate = "Losament|Kammer|Stübchen|Remise",
    othercat_boarding = "Kosthaus",
    othercat_boarding_phrase1 = "//bdie Kost//b",
    #othercategory: excluding lost&found, auction, funeral news,
    # some real estate and boarding  - which is (almost)
    # never combined with job offers/requests
    #"dito" and "ebendaselbst" is used in funeral ads, but never labor ads (just 1 exception)
    #"unrichtig" and "in meinem Namen" found in clarification ads
    other_transactions = "//bTausch//b|ubscri|übergeben|abzugeben|überlassen|vermieten|verlehen|usleihe|kaufen|Preis|Artikel|versteiger|Versteiger|vergant|//bGant//b",
    #transactions that are not associtaed with the job market (ubscri -> Subscription, subscribieren)
    proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    proclamation_phrase_1 = "Publikation in Betreff",
    proclamation_phrase_2 = "Basel, den",
    proclamation_phrase_3 = "Kanzle[i|y] der Stadt Basel"
    #proclamation: some of the ads recognized by the filter are public announcements"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Advertising/promoting business
#' @export
tagfilter_bizpromo <- function(){
  # as far as services is concerned,
  # this was tagged "labor" (otherwise "things")
  # but might be wise to draw a line between this & job market
  dict <- list()
  dict$pos <- list(
    advertising = "Anzeige|anzeigen|anzuzeigen|angezeigt|benachrichtigen",
    advertising_phrase1 = "zeigt an",
    recommending = "rekommandi",
    recommending_phrase1 = "empfiehlt sich",
    customertrust = "Zutrauen|Zuspruch",
    serving = "bedienen"
  )
  dict$neg <- list(
    # excluding some jobmarket ads and public proclamations
    employment ="Anstellung|angestellt|\\bDienst\\b|einzutreten|eintreten\\b|unterzukommen|\\bLohn\\b|Verdienst",
    proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    proclamation_phrase_1 = "Publikation in Betreff",
    proclamation_phrase_2 = "Basel, den",
    proclamation_phrase_3 = "Kanzle[i|y] der Stadt Basel",
    report_to_registry_office_phrase1 = "Anzeige im Berichthaus",
    report_to_registry_office_phrase2 = "gefälligst Anzeige"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Real Estate
#' @export
tagfilter_real_estate <- function(){
  dict <- list()
  dict$pos <- list(
    apartment = "losament|Wohnung|^Zimmer|Stube|Kammer|Keller|Remise"
  )
  dict$neg <- list(
    #exclude job ads for people attending to rooms
    othercat_job = "[m|M]agd|[k|K]necht|Mädchen"

  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Board
#' @export
tagfilter_board <- function(){
  dict <- list()
  dict$pos <- list(
    boarding = "\\bKost\\b"
  )
  dict$neg <- list(
    misc = "Zeugnisse",
    misc_phrase1 = "Kost kochen",
    misc_phrase2 = "Kost erfahren"

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Lottery
#' @export
tagfilter_lotto <- function(){
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


#' Filter Quanteda Corpus: Other
#' @export
tagfilter_other <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Transport
#' @export
tagfilter_transport <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Charity
#' @export
tagfilter_charity <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Placement
#' @export
tagfilter_placement <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Animal
#' @export
tagfilter_animal <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}




#' Filter Quanteda Corpus: Information
#' @export
tagfilter_info <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Jewellery
#' @export
tagfilter_bling <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Church Seat
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$pos <- list(
    seat = "[K|k]irchenstuhl|[K|k]irchensitz|[K|k]irchenstühl",
    female = "Weibersitz|Weiberstuhl|Weiberstühle|Frauensitz|Frauenstuhl|Frauenstühle",
    male = "Mannsitz|Mannstuhl|Mannstühle|Mannssitz|Mannsstuhl|Mannsstühle|Männersitz|Männerstuhl|Männerstühle"
  )
  create_filter_output(dict)
}



#' Filter Quanteda Corpus: Textile
#' @export
tagfilter_textile <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Grocery
#' @export
tagfilter_grocery <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Finance
#' @export
tagfilter_finance <- function(){
  dict <- list()
  dict$pos <- list(
    capital = "[C|K]apital|Vogtsgeld",
    capitalphrase_1 = "[f|F][r|l]. [0-9]+ auf",
    capitalphrase_2 = "[0-9]+ [f|F][r|l]. auf",
    capitalphrase_1 = "[f|F][r|l]. [0-9]+ nach",
    capitalphrase_2 = "[0-9]+ [f|F][r|l]. nach",
    interest = "Zinsentrichtung|Dividende|Quartal-Beitrag",
    loan = "[H|h]ypothek|[a|A]nle[i|]h[n|e]",
    security = "[P|p]f[a|ä]nd",
    securityphrase_1 = "auf erste Versicherung",
    securityphrase_2 = "auf zwe[i|y]te Versicherung",
    securityphrase_3 = "auf gute Versicherung",
    securityphrase_4 = "auf hinlängliche Versicherung",
    paper = "[O|b]ligation|Staatsanlehn|Staatsanleih|A[c|k]tien|Gültbrief|\\bAnleihe",
    action = "Anforderung|Zahlung",
    insurance= "Lebensversicherung|Assekuranz|-[K|C]ass[e|a]|Pensions[c|k]ass|Wittwen[k|c]ass"
  )
  dict$neg <- list(

    meta = "Hypothekenwesen",
    othercat_things = "Geld-[K|C]ass[a|e]|Geld[k|c]ass[a|e]",
    othercat_lostandfound = "verloren|gefunden",
    othercat_info = "beerdigt|verstorben|bendaselbst|unrichtig|genöthigt",
    othercat_infophrase1 = "ungültig ansehet",
    othercat_realestate = "Losament|Stübchen|Zimmer|Remise",
    other_transactions = "//bTausch//b|ubscri|übergeben|abzugeben|überlassen|Artikel|versteiger|Versteiger|vergant|//bGant//b"  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Print
#' @export
tagfilter_print <- function(){
  dict <- list()
  dict$pos <- list(
    book = "Buch|Bücher[n]|Bucher",
    edition = "Auflage|Ausgabe|Prachtausgabe|Bdchen",
    material = "gedruckt|Pergament",
    person = "Buchhändler|Buchdrucker|Buchbinder",
    place = "Buchhand*|Buchdruckere[y|i]|Buchladen|Leihbibl*|Lesegesellschaft|Leseanstalt",
    format_1 = "in Fol.",
    format_2 = "in 4to.",
    format_3 = "4°|8°|tom.$|[O|o]ctavo|Bogen|Bögen|Halbfranzband|[ein|un]gebunden|brosch[.|iert]",
    format_4 = "in [1-9] Bänden",
    format_5 = "gedruckte[n] Fortsetzung",
    ausstattung = "Kupf[f]er|Holzschnitt*|Stahlstich",
    catalog = "Catalogus|Katalog",
    participant = "Mithalte*|Pr[ae|ä]numerant[en]|Abonnent*",
    types = "Wörterbuch|Zeitung|Zeitschrift",
    type = "Neueste Schriften",
    title_1 = "Rauracher|Rau-racher|Raura-cher",
    title_2 = "Allgemeine[n] Zeitung",
    title_3 = "Christliche[r|n] Volksbote*",
    title_4 = "Kantonsblatt|Kantons-blatt",
    title_5 = "Annalen",
    title_6 = "Missions-Magazin",
    title_7 = "Basler-Zeitung|Basler Zeitung",
    title_8 = "Wochenblatt"
  )
  dict$neg <- list(

    lost_prayerbooks = "Psalmbuch|[g|G]esangbuch",
    region = "Entlibuch|Schönenbuch",
    bible = "Buch Mose",
    other = "Haushaltungsbuch|Buchstabe|Buchhaltung|Buchsbaum"
  )
  create_filter_output(dict)

}


#' Filter Quanteda Corpus: Household
#' @export
tagfilter_household_goods <- function(){
  merge_filters(tagfilter_bed(),
                tagfilter_household_textile(),
                tagfilter_seat(),
                tagfilter_cabinet(),
                tagfilter_stove(),
                tagfilter_mirror(),
                tagfilter_timepiece(),
                tagfilter_table(),
                tagfilter_tableware())
}



#' Filter Quanteda Corpus: Miscellaneous Things
#' @export
tagfilter_things <- function(){
  dict <- list()
  dict$pos <- list(

  )
  dict$neg <- list(

  )
  create_filter_output(dict)
}
