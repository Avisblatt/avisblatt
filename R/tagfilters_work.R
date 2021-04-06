#' Filter Quanteda Corpus: Employment in labourinfo section
#' @export
tagfilter_employment1 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo")
  dict$pos <- list(
    work = "Beruf|arbeiten|(B|b)eschäfti|Besorgung",
    qualification = "\\bZeugnisse|\\erfahrene|versteht|geübte",
    young = "\\bJun(g|ge|gen)|\\bKna(b|be|ben)|jung(e|er|en) ([a-zäöüß]+ )*(Mann|Frau|Pers(o|oh)n)|\\bMensch",
    women = "Weibs(\\-P|p)ers(o|oh)n",
    apprentice = "Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)",
    assistant = "Gesel(l|le|len)\\b|Bediente|(m|M)(ä|a)gd\\b|(k|K)necht",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDiens(t|te)\\b|ein(zut|t)reten|unterzukommen|\\bLohn\\b|Verdienst|Sala(ir|r)"
  )
  dict$neg <- list(
    placement = "\\bplac(i|ie)r|\\b(T|t)ausch",
    warning = "warne|Warnung|meinem Namen",
    misc_phrase1 = "Dienst zu erweisen",
    #proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    #proclamation_phrase_1 = "Publikation in Betreff",
    #proclamation_phrase_2 = "Basel, den",
    #proclamation_phrase_3 = "Kanzle[i|y] der Stadt Basel",
    other_transactions = "//bTausch//b|ubscri|übergeben|abzugeben|überlassen|vermieten|(verl|entl|aus|l)(e|ei|ey)hen|kau(f|ff)en|Preis|Artikel|\\bKund(i|e)n\\b|(V|v)ersteiger|vergant|//bGant//b" ##transactions that are not associtaed with the job market (ubscri -> Subscription, subscribieren)
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Employment in pertinent sections outside labourinfo
#' @export
tagfilter_employment2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews", "demand", "offer")
  dict$pos <- list(
    work = "Beruf|arbeiten|(B|b)eschäfti|Besorgung",
    qualification = "\\bZeugnisse|\\erfahrene|versteht|geübte",
    young = "\\bJun(g|ge|gen)|\\bKna(b|be|ben)|jung(e|er|en) ([a-zäöüß]+ )*(Mann|Frau|Pers(o|oh)n)|\\bMensch",
    women = "Weibs(\\-P|p)ers(o|oh)n",
    apprentice = "Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)",
    assistant = "Gesel(l|le|len)\\b|Bediente|(m|M)(ä|a)gd\\b|(k|K)necht",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDiens(t|te)\\b|ein(zut|t)reten|unterzukommen|\\bLohn\\b|Verdienst|Sala(ir|r)"
  )
  dict$neg <- list(
    placement = "\\bplac(i|ie)r|\\b(T|t)ausch",
    warning = "warne|Warnung|meinem Namen",
    misc_phrase1 = "Dienst zu erweisen",
    #proclamation = "Kundmachung|Polizey-Anzeige|Bekanntmachung|Erinnerung",
    #proclamation_phrase_1 = "Publikation in Betreff",
    #proclamation_phrase_2 = "Basel, den",
    #proclamation_phrase_3 = "Kanzle[i|y] der Stadt Basel",
    other_transactions = "//bTausch//b|ubscri|übergeben|abzugeben|überlassen|vermieten|(verl|entl|aus|l)(e|ei|ey)hen|kau(f|ff)en|Preis|Artikel|\\bKund(i|e)n\\b|(V|v)ersteiger|vergant|//bGant//b" ##transactions that are not associtaed with the job market (ubscri -> Subscription, subscribieren)
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Advertising/promoting business in section labourinfo
#' @export
tagfilter_bizpromo1 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo")
  dict$pos <- list(
    advertising = "Anzeige|anzeigen|anzuzeigen|angezeigt|benachrichtigen",
    advertising_phrase1 = "zeigt an",
    recommending = "rekomm(a|e)ndi|empfiehlt sich|sich empfehlen",
    customertrust = "Zutrauen|Zuspruch",
    serving = "bedienen"
  )
  dict$neg <- list(
    apprentice = "Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)",
    assistant = "Gesel(l|le|len)\\b|Bediente|(m|M)agd\\b|(k|K)necht",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDiens(t|te)\\b|ein(zut|t)reten|unterzukommen|\\bLohn\\b|Verdienst|Sala(ir|r)|Zeugnis",
    report_to_registry_office_phrase1 = "Anzeige im Berichthaus",
    report_to_registry_office_phrase2 = "gefälligst Anzeige"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Advertising/promoting business outside section labourinfo
#' @export
tagfilter_bizpromo2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews", "demand", "offer")
  dict$pos <- list(
    advertising = "Anzeige|anzeigen|anzuzeigen|angezeigt|benachrichtigen",
    advertising_phrase1 = "zeigt an",
    recommending = "rekommandi",
    recommending_phrase1 = "empfiehlt sich",
    customertrust = "Zutrauen|Zuspruch",
    serving = "bedienen"
  )
  dict$neg <- list(
    apprentice = "Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)",
    assistant = "Gesel(l|le|len)\\b|Bediente|(m|M)agd\\b|(k|K)necht",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDiens(t|te)\\b|ein(zut|t)reten|unterzukommen|\\bLohn\\b|Verdienst|Sala(ir|r)|Zeugnis",
    report_to_registry_office_phrase1 = "Anzeige im Berichthaus",
    report_to_registry_office_phrase2 = "gefälligst Anzeige"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Boarding
#' @export
tagfilter_boarding <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo", "othernews", "demand", "offer")
  dict$pos <- list(
    boarding = "\\bKos(t|ga|gä)\\b"
  )
  dict$neg <- list(
    misc_phrase1 = "Kost kochen",
    misc_phrase2 = "Kost erfahren"
  )
  create_filter_output(dict)
}
