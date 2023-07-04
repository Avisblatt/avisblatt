#' Tagfilter work
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads concerning the job market (especially 
#' servants, clerks, and apprentices), promoting services, and boarding 
#' arrangements.
#'
#' Tagfilters can only predict if an ad is pertinent to a given topic. 
#' Depending on the complexity of the topic and the development stage of a 
#' tagfilter, there can be a considerable number of false positives and false 
#' negatives. 
#' 
#' The precision and sensitivity of some (families of) tagfilters can be 
#' measured by comparison to a manual classification for four sample years 
#' (1734, 1754, 1774 and 1834) from an early stage of the Avisblatt project.
#' Since the manual classification does often only roughly match the scope of 
#' the tagfilters, their true precision and sensitivity are underestimated.
#' 
#' Calculated that way, the family of tagfilters concerning employment 
#' shows a precision >92% and a sensitivity >78%.
#' 
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_work
#' @seealso tagfilters
NULL


#' Filter Quanteda Corpus: Employment offer section
#' @name tagfilter_work
#' @export
tagfilter_employmentoffer <- function(){
  dict <- list()
  dict$applicable <- list('joboffer')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    placeholder = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Employment seeking section
#' @name tagfilter_work
#' @export
tagfilter_employmentseeking <- function(){
  dict <- list()
  dict$applicable <- list('jobseek')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    placeholder = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Employment in labourinfo section
#' @name tagfilter_work
#' @export
tagfilter_employment1 <- function(){
  dict <- list()
  dict$applicable <- list('labourinfo')
  dict$pos <- list(
    work = 'Beruf|arbeiten|(B|b)esch\u00e4fti|Besorgung|A(auf|b)wart|(u|U)nterricht',
    qualification = '\\\\bZeugnisse|Auff\u00fchrung|\\\\erfahrene|versteht|ge\u00fcbte',
    young = '\\\\bJun(g|ge|gen)|\\\\bKna(b|be|ben)|jung(e|er|en) ([a-z\u00e4\u00f6\u00fc\u00df]+ )*(Mann|Frau|Pers(o|oh)n)|\\\\bMensch',
    women = 'Weibs(\\\\-P|p)ers(o|oh)n',
    apprentice = 'Lehr(ling|(J|j)ung|(K|k)nab|(T|t)ocht)|in die Lehr|Lehrgel(d|t|dt)',
    assistant = '(G|g)esel(l|le|len)\\\\b|Commis|Geh(i|\u00fc)lfe',
    servant = '(b|B)ediente|(D|d)iener\\\\b|Laquai|(m|M)(\u00e4|a)g(d|de)\\\\b|(k|K)necht',
    positions = '(A|S(a|\u00e4)ug)amme|Vorg\u00e4nger|N\u00e4her|Schneider|Zettler|Lehrer|G\u00e4rtner|Kutscher|Lehenmann|Arbeiter',
    workplace = '(H|h)andlung|Handelshaus|Co(m|mp)toir|Schreinstub|(F|f)abrik|Haushaltung|Kundenh(a|\u00e4)us',
    conditions = '\\\\bLohn\\\\b|Verdienst|Sala(ir|r)|Bed(i|\u00fc|u)ngn(i|\u00fc|u)s',
    employment_phrase_1 = 'einen Platz',
    employment_phrase_2 = 'ein Platz als',
    employment ='Anstellung|angestellt|\\\\bDiens(t|te)\\\\b|ein(zut|t)reten|unterzukommen',
    competence = 'Buchhaltung|Kochkunst|h\u00e4uslich(e|en) Gesch\u00e4ft'
  )
  dict$neg <- list(
    warning = 'warne|Warnung|meinem Namen',
    misc_phrase1 = 'Dienst zu erweisen',
    other_transactions = '//b(t|T)ausch|vermieten|(verl|entl|aus|l)(e|ei|ey)hen|kau(f|ff)en' ##transactions that are not associtaed with the job market
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Employment in pertinent sections outside labourinfo
#' @name tagfilter_work
#' @export
tagfilter_employment2 <- function(){
  dict <- list()
  dict$applicable <- list('othernews', 'demanding', 'offering')
  dict$pos <- list(
    work = 'Beruf|arbeiten|(B|b)esch\u00e4fti|Besorgung|Abwart', # leaving out |(u|U)nterricht here
    qualification = '\\\\bZeugnisse|\\\\erfahrene|versteht|ge\u00fcbte',
    apprentice = 'Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)',
    conditions = '\\\\bLohn\\\\b|Verdienst|Sala(ir|r)|Bedingnis',
    employment_phrase_1 = 'einen Platz',
    employment_phrase_2 = 'ein Platz als',
    employment ='Anstellung|angestellt|\\\\bDiens(t|te)\\\\b|ein(zut|t)reten|unterzukommen'
  )
  dict$neg <- list(
    warning = 'warne|Warnung|meinem Namen',
    misc_phrase1 = 'Dienst zu erweisen',
    other_transactions = '//b(t|T)ausch|ubscri|\u00fcbergeben|abzugeben|\u00fcberlassen|vermieten|(verl|entl|aus|l)(e|ei|ey)hen|kau(f|ff)en|Preis|Artikel|\\\\bKund(i|e)n\\\\b|(V|v)ersteiger|vergant|//bGant//b' ##transactions that are not associtaed with the job market (ubscri -> Subscription, subscribieren)
  )
  dict$include <- employment_include()
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Advertising/promoting business in section labourinfo
#' @name tagfilter_work
#' @export
tagfilter_bizpromo1 <- function(){
  dict <- list()
  dict$applicable <- list('labourinfo')
  dict$pos <- list(
    advertising = 'Anzeige|anzeigen|anzuzeigen|angezeigt|benachrichtigen',
    advertising_phrase1 = 'zeigt an',
    recommending = 'rekomm(a|e)ndi|empfiehlt sich|sich empfehlen',
    customertrust = 'Zutrauen|Zuspruch',
    serving = 'bedienen'
  )
  dict$neg <- list(
    apprentice = 'Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)',
    assistant = 'Gesel(l|le|len)\\\\b|Bediente|(m|M)agd\\\\b|(k|K)necht',
    employment_phrase_1 = 'einen Platz',
    employment_phrase_2 = 'ein Platz als',
    employment ='Anstellung|angestellt|\\\\bDiens(t|te)\\\\b|ein(zut|t)reten|unterzukommen|\\\\bLohn\\\\b|Verdienst|Sala(ir|r)|Zeugnis',
    report_to_registry_office_phrase1 = 'Anzeige im Berichthaus',
    report_to_registry_office_phrase2 = 'gef\u00e4lligst Anzeige'
  )
  dict$exclude <- bizpromo_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Advertising/promoting business outside section labourinfo
#' @name tagfilter_work
#' @export
tagfilter_bizpromo2 <- function(){
  dict <- list()
  dict$applicable <- list('othernews', 'demanding', 'offering')
  dict$pos <- list(
    advertising = 'Anzeige|anzeigen|anzuzeigen|angezeigt|benachrichtigen',
    advertising_phrase1 = 'zeigt an',
    recommending = 'rekommandi',
    recommending_phrase1 = 'empfiehlt sich',
    customertrust = 'Zutrauen|Zuspruch',
    serving = 'bedienen'
  )
  dict$neg <- list(
    apprentice = 'Lehr(ling|jung|knab)|in die Lehr|Lehrgel(d|t|dt)',
    assistant = 'Gesel(l|le|len)\\\\b|Bediente|(m|M)agd\\\\b|(k|K)necht',
    employment_phrase_1 = 'einen Platz',
    employment_phrase_2 = 'ein Platz als',
    employment ='Anstellung|angestellt|\\\\bDiens(t|te)\\\\b|ein(zut|t)reten|unterzukommen|\\\\bLohn\\\\b|Verdienst|Sala(ir|r)|Zeugnis',
    report_to_registry_office_phrase1 = 'Anzeige im Berichthaus',
    report_to_registry_office_phrase2 = 'gef\u00e4lligst Anzeige'
  )
  dict$include <- bizpromo_include()
  dict$exclude <- bizpromo_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Boarding section
#' @name tagfilter_work
#' @export
tagfilter_boarding1 <- function(){
  dict <- list()
  dict$applicable <- list('boarding')
  dict$pos <- list(
    all = '.'
  )
  dict$neg <- list(
    placeholder = 'yyyyy'
  )
  create_filter_output(dict)
}

#' Filter Quanteda Corpus: Boarding in other sections
#' @name tagfilter_work
#' @export
tagfilter_boarding2 <- function(){
  dict <- list()
  dict$applicable <- list('labourinfo', 'othernews', 'demanding', 'offering')
  dict$pos <- list(
    boarding = '\\\\bKos(t|ga|g\u00e4)|Tischg\u00e4ng'
  )
  dict$neg <- list(
    misc_phrase1 = 'Kost kochen',
    misc_phrase2 = 'Kost erfahren'
  )
  create_filter_output(dict)
}
