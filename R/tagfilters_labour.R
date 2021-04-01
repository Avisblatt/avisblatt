#' Filter Quanteda Corpus: Labour
#' @export
tagfilter_employment <- function(){
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
