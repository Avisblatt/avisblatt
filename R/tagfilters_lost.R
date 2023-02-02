#' Tagfilter Lost and Found
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads announcing lost or found, and potentially 
#' stolen items.
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
#' @rdname tagfilter_lostandfound
#' @export
tagfilter_lostandfound2 <- function(){
  dict <- list()
  dict$applicable <- list("othernews")
  dict$pos <- list(
    loss="verloren|Verloren|verlohren|Verlohren|entfallen",
    loss_1="(E|e)ntwen|zur(u|ü)(k|ck)\\sgelassen",
    loss_2="(W|w)egge(nommen|gekommen)",
    loss_3="(H|h)inweg(gekommen|genommen|getragen|practicirt)",
    loss_4="(H|h)inweg\\s(gekommen|genommen|getragen|practicirt)",
    loss_5="(A|a)b(H|h)anden",
    loss_6="(liegen|stehen)\\sgeblieben|(liegen|stehen)\\slassen",
    found_1= "gefunden",
    claim="(A|a)ns(pr|r)(a|u)ch",
    offer="(gefunden\\sworden|aufbehalten|zustellen|(Z|z)ur(u|ü)ckgabe)",
    owner="(R|r)rechtm(ä|a)(ß|ss|t|s)ig(e|en|er)\\s((B|b)esitzer|(V|v)erlier|Proprietar)",
    owner_1="zust(a|ä)ndig\\sgewesen",
    description="beschreiben",
    accidentally="Verge(ss|ß)|Unversehenheit|Ubersehen|Unkenntni(ss|ß)|vergessen|(V|v)erwechsl|Unachtsamkeit|Irrthum",
    reward="(E|e)rk(e|a)nntlich|(T|t)rin(k|ck)|Belohnung|Recompen",
    theft_1="(D|d)ieb|Taschenspieler|Thäter|Entwender|entwendet|gestohlen|erbrochen|eingebrochen|wegge(n|k)om|weg\\sge(n|k)om|practicir|bestehlen",
    theft_2="entwendet|gestohlen|erbrochen|eingebrochen|wegge(n|k)om|weg\\sge(n|k)om|practicir",
    violence="gewalt|(U|u)nrech|entf(u|ü)hrt",
    discoverer="(F|f)inder|(W|w)(i|ie)derbringer|Ueberbringer",
    discretion="(V|v)erschw(ei|ie)g(ung|en|genheit)",
    precaution="(K|k)ur(tz|z)weil|(in|inne)behalten|aufbehalten|(V|v)erd(a|ä)cht",
    return="zum\\sVorschein",
    return_2="an\\sBehördte|an\\sseine\\sBehördte|(R|N)(ü|u)ckgabe",
    return_3="wieder\\s(zugestellt|zugekommen|angebo(tt|t)en)|(zugestellt|abgegeben|abgenommen|geliehen|vertauscht|aufgehoben)\\sworden",
    miss="(V|v)ermiss|misset|\\smisst|vermißt|mißet|gemißt|gemisst",
    fair_reward="rechtm(ä|a)(ß|ss|t)ig(e|en|er)\\s(Belohnung|Rückgabe)",
    escaped_bird="zugeflogen|weggeflogen|verflogen|mitfliegt|verirrt",
    spotted_animal="zugeflogen|verlo(ff|f)en|zugelo(ff|f)en|(zu|nach)gel(au|o)(ff|f)en|(auf|ein)gefangen",
    chase="(F|f)ortjagen|(W|w)egjagen|wegtreiben|fortzuweisen|entloffen|angezeigt\\sund\\sentkommen",
    vigilant="wachsam",
    borrow="gelehnet|geliehen",
    confused="verwechsel"
  )
  dict$neg <- list(
    tableware="(T|t)rin(k|ck)(G|g)l(ä|a)s|(T|t)rin(k|ck)(G|g)eschir|((T|t)rin(k|ck)(B|b)echer)|(T|t)rin(k|ck)(en|et|wasser|quelle|bar|empf)",
    advertising="(V|v)erehrlichen|(raisonnablen|billi(ch|g)en)\\s(Preis|Ertrag)|befriedigen|Bedienung|Wohlwollen",
    purchase="nach\\sBelieben|Liebhaber|Zuspruch|Kaufbedingni(ß|ss)|Bestellungen|bestellt|r(e|ec)o(mm|m)endiren|anzuve(x|r)trauen",
    purchase_1="kau(ff|f)en\\sbegehrt|nach\\sBelieb|offerieret|begehr(t|et)|Zutrauen|(E|e)mpf(eh|ie|ieh|o|oh)l|einpfiehlt",
    purchase_3="zu\\s(haben|verschiessen|verkau(ff|f)en)|herzustellen|preis",
    immo_offer="((E|e)i(ne|n)|(D|d)ie|(D|d)er)\\s(Behausung|Wohnung|Losament|Gar(th|t)en)",
    immo="Rebacker|Jucharten|(B|b)equem|meubl(i|ie)rt,(B|b)odenzi,Ko(mm|m)lich",
    auction="(V|v)ergant|(V|v)ersteige",
    demand="wünscht\\s",
    obituaries="begraben|hinterlassen|(u|ü)berlassen|Legat|Verstorbenen",
    labour="Zeugnisse|Empfehle|Einladung|Diensttreue|Verpflichtungen|Kenntnisse|Anstellung|\\bwünschen|Diligence|\\slernen",
    transport="zu\\s(reisen|verreisen)",
    header="(A|a)llerhand\\sNachrichten",
    lottery="Lotterie|Ausl(oo|o)sung",
    Professions="Comptoirist|Han(s|ds)-Arbeit|Le(r|kt)ionen|(E|e)rwerb|Ragion|kitten",
    servants="S(aü|eu|au)gamme",
    accomodation="unterzukommen|unterkommen|Logis|Speisen|Einquartier|Verpflegung|log(i|ie)rt",
    public="Publi(c|k)um|Verordnung|Geschrey|Anweisung",
    print="Fortgang|(P|p)ränumerant|Erneuerung|mithalten|Porto",
    announcement="\\b(N|n)ächsten\\b|nächstkünftig|Ansprachen|bevorstehen",
    Recreation="Eröffnung|Bewilligung|Patienten|aufzuwarten|Lust|Geheimniß|Vorstellung|Unterhaltung",
    denunciation="(v|V)erl(e|ä|a)um|(P|B|p|b)a(s|ss)(k|ku|q|qu|g|gu)i",
    doctor="Operator|Augendoktor|Zahn|\\scuriren",
    charity="ungl(ü|u)ck|mild|H(ü|i)l(f|ff)e|Geschenk|Vertheilung|Liebesgabe",
    finance="Steuer|Obligat|Subs(c|e)|Schuld|Liquid|Be(i|y)tr(a|ä)g|Konkurs|(B|b)ettel|Rehabilitation|Schatzungs|Einzug|Stappelgeld",
    other="Taubstummen-Anstalt|Mili(z|tz)|Zisternen|Einsaßen"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Crime with focus on property delinquency
#' @rdname tagfilter_lostandfound
#' @export
tagfilter_crime <- function(){
  dict <- list()
  dict$applicable <- list("othernews","lostandfound")
  dict$pos <- list(
    thief="(D|d)ieb|Taschenspieler|Thäter|Entwender",
    theft="entwendet|gest(oh|o)len|erbrochen|eingebrochen|wegge(n|k)om|weg\\sge(n|k)om|practicir|Vorsatz|gerau(be|b)t",
    sinner="fr(e|a)(v|y)(le|el)|(f|F)rech",
    suspicious="(V|v)erd(a|ä)cht",
    violence="gewalt",
    fencing="verse(tz|z),ausgelegt",
    unjust="unrecht",
    abduct="entf(u|ü)hr",
    fun="Scherz|Schertz|Spa(ß|ss)",
    night_crime="schwärmer|Nachtw(ä|a)ch",
    warning="Warnung"
  )

  dict$neg <- list(
    fire_brigade = "Feuerspritze|Probierung|Mannschaft",
    sale="Zuspruch",
    print="Verzeichni|Liebhaber",
    legacy="Erbschaft",
    borrow="Verleihen",
    news="Grönland|Labrador"
  )
  create_filter_output(dict)
}

