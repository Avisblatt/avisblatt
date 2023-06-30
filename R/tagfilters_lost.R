#' Tagfilter Lost and Found
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads announcing lost or found, and potentially 
#' stolen items.
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
#' Calculated that way, the family of tagfilters concerning lost and found items
#' shows a precision >88% and a sensitivity >98%.
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_lostandfound
#' @seealso tagfilters
NULL


#' Filter Quanteda Corpus: Lostandfound inside lostandfound section
#' @rdname tagfilter_lostandfound
#' @export
tagfilter_lostandfound1 <- function(){
  dict <- list()
  dict$applicable <- list('lostandfoundheader')
  dict$pos <- list(
    all = '.' #
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Lostandfound outside lostandfound section
#' @rdname tagfilter_lostandfound
#' @export
tagfilter_lostandfound2 <- function(){
  dict <- list()
  dict$applicable <- list('othernews')
  dict$pos <- list(
    loss='verloren|Verloren|verlohren|Verlohren|entfallen',
    loss_1='(E|e)ntwen|zur(u|\u00fc)(k|ck)\\\\sgelassen',
    loss_2='(W|w)egge(nommen|gekommen)',
    loss_3='(H|h)inweg(gekommen|genommen|getragen|practicirt)',
    loss_4='(H|h)inweg\\\\s(gekommen|genommen|getragen|practicirt)',
    loss_5='(A|a)b(H|h)anden',
    loss_6='(liegen|stehen)\\\\sgeblieben|(liegen|stehen)\\\\slassen',
    found_1= 'gefunden',
    claim='(A|a)ns(pr|r)(a|u)ch',
    offer='(gefunden\\\\sworden|aufbehalten|zustellen|(Z|z)ur(u|\u00fc)ckgabe)',
    owner='(R|r)rechtm(\u00e4|a)(\u00df|ss|t|s)ig(e|en|er)\\\\s((B|b)esitzer|(V|v)erlier|Proprietar)',
    owner_1='zust(a|\u00e4)ndig\\\\sgewesen',
    description='beschreiben',
    accidentally='Verge(ss|\u00df)|Unversehenheit|Ubersehen|Unkenntni(ss|\u00df)|vergessen|(V|v)erwechsl|Unachtsamkeit|Irrthum',
    reward='(E|e)rk(e|a)nntlich|(T|t)rin(k|ck)|Belohnung|Recompen',
    theft_1='(D|d)ieb|Taschenspieler|Th\u00e4ter|Entwender|entwendet|gestohlen|erbrochen|eingebrochen|wegge(n|k)om|weg\\\\sge(n|k)om|practicir|bestehlen',
    theft_2='entwendet|gestohlen|erbrochen|eingebrochen|wegge(n|k)om|weg\\\\sge(n|k)om|practicir',
    violence='gewalt|(U|u)nrech|entf(u|\u00fc)hrt',
    discoverer='(F|f)inder|(W|w)(i|ie)derbringer|Ueberbringer',
    discretion='(V|v)erschw(ei|ie)g(ung|en|genheit)',
    precaution='(K|k)ur(tz|z)weil|(in|inne)behalten|aufbehalten|(V|v)erd(a|\u00e4)cht',
    return='zum\\\\sVorschein',
    return_2='an\\\\sBeh\u00f6rdte|an\\\\sseine\\\\sBeh\u00f6rdte|(R|N)(\u00fc|u)ckgabe',
    return_3='wieder\\\\s(zugestellt|zugekommen|angebo(tt|t)en)|(zugestellt|abgegeben|abgenommen|geliehen|vertauscht|aufgehoben)\\\\sworden',
    miss='(V|v)ermiss|misset|\\\\smisst|vermi\u00dft|mi\u00dfet|gemi\u00dft|gemisst',
    fair_reward='rechtm(\u00e4|a)(\u00df|ss|t)ig(e|en|er)\\\\s(Belohnung|R\u00fcckgabe)',
    escaped_bird='zugeflogen|weggeflogen|verflogen|mitfliegt|verirrt',
    spotted_animal='zugeflogen|verlo(ff|f)en|zugelo(ff|f)en|(zu|nach)gel(au|o)(ff|f)en|(auf|ein)gefangen',
    chase='(F|f)ortjagen|(W|w)egjagen|wegtreiben|fortzuweisen|entloffen|angezeigt\\\\sund\\\\sentkommen',
    vigilant='wachsam',
    borrow='gelehnet|geliehen',
    confused='verwechsel'
  )
  dict$neg <- list(
    tableware='(T|t)rin(k|ck)(G|g)l(\u00e4|a)s|(T|t)rin(k|ck)(G|g)eschir|((T|t)rin(k|ck)(B|b)echer)|(T|t)rin(k|ck)(en|et|wasser|quelle|bar|empf)',
    advertising='(V|v)erehrlichen|(raisonnablen|billi(ch|g)en)\\\\s(Preis|Ertrag)|befriedigen|Bedienung|Wohlwollen',
    purchase='nach\\\\sBelieben|Liebhaber|Zuspruch|Kaufbedingni(\u00df|ss)|Bestellungen|bestellt|r(e|ec)o(mm|m)endiren|anzuve(x|r)trauen',
    purchase_1='kau(ff|f)en\\\\sbegehrt|nach\\\\sBelieb|offerieret|begehr(t|et)|Zutrauen|(E|e)mpf(eh|ie|ieh|o|oh)l|einpfiehlt',
    purchase_3='zu\\\\s(haben|verschiessen|verkau(ff|f)en)|herzustellen|preis',
    immo_offer='((E|e)i(ne|n)|(D|d)ie|(D|d)er)\\\\s(Behausung|Wohnung|Losament|Gar(th|t)en)',
    immo='Rebacker|Jucharten|(B|b)equem|meubl(i|ie)rt,(B|b)odenzi,Ko(mm|m)lich',
    auction='(V|v)ergant|(V|v)ersteige',
    demand='w\u00fcnscht\\\\s',
    obituaries='begraben|hinterlassen|(u|\u00fc)berlassen|Legat|Verstorbenen',
    labour='Zeugnisse|Empfehle|Einladung|Diensttreue|Verpflichtungen|Kenntnisse|Anstellung|\\\\bw\u00fcnschen|Diligence|\\\\slernen',
    transport='zu\\\\s(reisen|verreisen)',
    header='(A|a)llerhand\\\\sNachrichten',
    lottery='Lotterie|Ausl(oo|o)sung',
    Professions='Comptoirist|Han(s|ds)-Arbeit|Le(r|kt)ionen|(E|e)rwerb|Ragion|kitten',
    servants='S(a\u00fc|eu|au)gamme',
    accomodation='unterzukommen|unterkommen|Logis|Speisen|Einquartier|Verpflegung|log(i|ie)rt',
    public='Publi(c|k)um|Verordnung|Geschrey|Anweisung',
    print='Fortgang|(P|p)r\u00e4numerant|Erneuerung|mithalten|Porto',
    announcement='\\\\b(N|n)\u00e4chsten\\\\b|n\u00e4chstk\u00fcnftig|Ansprachen|bevorstehen',
    Recreation='Er\u00f6ffnung|Bewilligung|Patienten|aufzuwarten|Lust|Geheimni\u00df|Vorstellung|Unterhaltung',
    denunciation='(v|V)erl(e|\u00e4|a)um|(P|B|p|b)a(s|ss)(k|ku|q|qu|g|gu)i',
    doctor='Operator|Augendoktor|Zahn|\\\\scuriren',
    charity='ungl(\u00fc|u)ck|mild|H(\u00fc|i)l(f|ff)e|Geschenk|Vertheilung|Liebesgabe',
    finance='Steuer|Obligat|Subs(c|e)|Schuld|Liquid|Be(i|y)tr(a|\u00e4)g|Konkurs|(B|b)ettel|Rehabilitation|Schatzungs|Einzug|Stappelgeld',
    other='Taubstummen-Anstalt|Mili(z|tz)|Zisternen|Einsa\u00dfen'
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Crime with focus on property delinquency
#' @rdname tagfilter_lostandfound
#' @export
tagfilter_crime <- function(){
  dict <- list()
  dict$applicable <- list('othernews','lostandfound')
  dict$pos <- list(
    thief='(D|d)ieb|Taschenspieler|Th\u00e4ter|Entwender',
    theft='entwendet|gest(oh|o)len|erbrochen|eingebrochen|wegge(n|k)om|weg\\\\sge(n|k)om|practicir|Vorsatz|gerau(be|b)t',
    sinner='fr(e|a)(v|y)(le|el)|(f|F)rech',
    suspicious='(V|v)erd(a|\u00e4)cht',
    violence='gewalt',
    fencing='verse(tz|z),ausgelegt',
    unjust='unrecht',
    abduct='entf(u|\u00fc)hr',
    fun='Scherz|Schertz|Spa(\u00df|ss)',
    night_crime='schw\u00e4rmer|Nachtw(\u00e4|a)ch',
    warning='Warnung'
  )

  dict$neg <- list(
    fire_brigade = 'Feuerspritze|Probierung|Mannschaft',
    sale='Zuspruch',
    print='Verzeichni|Liebhaber',
    legacy='Erbschaft',
    borrow='Verleihen',
    news='Gr\u00f6nland|Labrador'
  )
  create_filter_output(dict)
}

