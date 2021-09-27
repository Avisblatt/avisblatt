#' Filter Quanteda Corpus: Employment offer section
#' @export
tagfilter_employmentoffer <- function(){
  dict <- list()
  dict$applicable <- list("joboffer")
  dict$pos <- list(
    all = "."
  )
  dict$neg <- list(
    placeholder = "yyyyy"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Employment seeking section
#' @export
tagfilter_employmentseeking <- function(){
  dict <- list()
  dict$applicable <- list("jobseek")
  dict$pos <- list(
    all = "."
  )
  dict$neg <- list(
    placeholder = "yyyyy"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Employment in labourinfo section
#' @export
tagfilter_employment1 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo")
  dict$pos <- list(
    work = "Beruf|arbeiten|(B|b)eschäfti|Besorgung|A(auf|b)wart|(u|U)nterricht",
    qualification = "\\bZeugnisse|Aufführung|\\erfahrene|versteht|geübte",
    young = "\\bJun(g|ge|gen)|\\bKna(b|be|ben)|jung(e|er|en) ([a-zäöüß]+ )*(Mann|Frau|Pers(o|oh)n)|\\bMensch",
    women = "Weibs(\\-P|p)ers(o|oh)n",
    apprentice = "Lehr(ling|(J|j)ung|(K|k)nab|(T|t)ocht)|in die Lehr|Lehrgel(d|t|dt)",
    assistant = "(G|g)esel(l|le|len)\\b|Commis|Geh(i|ü)lfe",
    servant = "(b|B)ediente|(D|d)iener\\b|Laquai|(m|M)(ä|a)g(d|de)\\b|(k|K)necht",
    positions = "(A|S(a|ä)ug)amme|Vorgänger|Näher|Schneider|Zettler|Lehrer|Gärtner|Kutscher|Lehenmann|Arbeiter",
    workplace = "(H|h)andlung|Handelshaus|Co(m|mp)toir|Schreinstub|(F|f)abrik|Haushaltung|Kundenh(a|ä)us",
    conditions = "\\bLohn\\b|Verdienst|Sala(ir|r)|Bed(i|ü|u)ngn(i|ü|u)s",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDiens(t|te)\\b|ein(zut|t)reten|unterzukommen",
    competence = "Buchhaltung|Kochkunst|häuslich(e|en) Geschäft"
  )
  dict$neg <- list(
    warning = "warne|Warnung|meinem Namen",
    misc_phrase1 = "Dienst zu erweisen",
    other_transactions = "//b(t|T)ausch|vermieten|(verl|entl|aus|l)(e|ei|ey)hen|kau(f|ff)en" ##transactions that are not associtaed with the job market
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Employment in pertinent sections outside labourinfo
#' @export
tagfilter_employment2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews", "demanding", "offering")
  dict$pos <- list(
    work = "Beruf|arbeiten|(B|b)eschäfti|Besorgung|Abwart", # leaving out |(u|U)nterricht here
    qualification = "\\bZeugnisse|\\erfahrene|versteht|geübte",
    apprentice = "Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)",
    conditions = "\\bLohn\\b|Verdienst|Sala(ir|r)|Bedingnis",
    employment_phrase_1 = "einen Platz",
    employment_phrase_2 = "ein Platz als",
    employment ="Anstellung|angestellt|\\bDiens(t|te)\\b|ein(zut|t)reten|unterzukommen"
  )
  dict$neg <- list(
    warning = "warne|Warnung|meinem Namen",
    misc_phrase1 = "Dienst zu erweisen",
    other_transactions = "//b(t|T)ausch|ubscri|übergeben|abzugeben|überlassen|vermieten|(verl|entl|aus|l)(e|ei|ey)hen|kau(f|ff)en|Preis|Artikel|\\bKund(i|e)n\\b|(V|v)ersteiger|vergant|//bGant//b" ##transactions that are not associtaed with the job market (ubscri -> Subscription, subscribieren)
  )
  dict$include <- employment_include()
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
  dict$exclude <- bizpromo_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Advertising/promoting business outside section labourinfo
#' @export
tagfilter_bizpromo2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews", "demanding", "offering")
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
  dict$include <- bizpromo_include()
  dict$exclude <- bizpromo_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Boarding section
#' @export
tagfilter_boarding1 <- function(){
  dict <- list()
  dict$applicable <- list("boarding")
  dict$pos <- list(
    all = "."
  )
  dict$neg <- list(
    placeholder = "yyyyy"
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Boarding in other sections
#' @export
tagfilter_boarding2 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo", "othernews", "demanding", "offering")
  dict$pos <- list(
    boarding = "\\bKos(t|ga|gä)|Tischgäng"
  )
  dict$neg <- list(
    misc_phrase1 = "Kost kochen",
    misc_phrase2 = "Kost erfahren"
  )
  create_filter_output(dict)
}
