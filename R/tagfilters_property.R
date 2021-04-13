#' Filter Quanteda Corpus: Housing for rent in renting section
#' @export
tagfilter_housing_rent1 <- function(){
  dict <- list()
  dict$applicable <- list("lendoffer", "lenddemand")
  dict$pos <- list(
    general = "(B|b)ehausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein", # not "Haus" as this often used in contact references in all sorts of ads
    spaces = "Rebacker|Garten|Gärtlein|Höfl(e|ei)n|Matten|Juchart",
    buildings = "Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Laden\\b|\\Stand\\b|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer",
    rooms = "\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Al(ek|k)oven",
    storage = "Magazin|Keller\\b|Dachboden|Estri[ch|g]",
    amenities = "Abtritt|Abwasser|Altan|Bauchkessel|(B|b)runnen|Wasserleitung|Wasserstein",
    amenities_phrase = "Platz zu Holz"
  )
  dict$neg <- list(
    living_at = "wohnha(f|fft)",
    position_phrase1 = "(vor|für|au(f|ff)|in) (der|die|eine) Behausung",
    position_phrase2 = "(vor|für|au(f|ff)|in) ein Haus\\b",
    position_phrase3 = "be(i|y) Haus\\b",
    othercat_job = "[m|M]agd|[k|K]necht|Mädchen|Junge",
    booksale = "Buchhandlung|\\B(u|ü)ch(er|lein)|Haus(-\\B|b)(u|ü)chTra(c|k)t(a|ä)t",
    sled = "Haus-Schlitten"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for rent in other sections
#' @export
tagfilter_housing_rent2 <- function(){
  dict <- list()
  dict$applicable <- list("offer", "demand", "exchange", "othernews")
  dict$pos <- list(
    general_1   = "((B|b)ehausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)",
    spaces_1    = "(Rebacker|Garten|Gärtlein|Höfl[e|ei]n|Matten|Juchart).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)",
    buildings_1 = "(Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Laden\\b|\\Stand\\b|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)",
    rooms_1     = "(\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Magazin|Keller\\b|Dachboden|Estri[ch|g]).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)",
    general_2   = "(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n).*(?=Behausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein)",
    spaces_2    = "(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n).*(?=Rebacker|Garten|Gärtlein|Höfl[e|ei]n|Matten|Juchart)",
    buildings_2 = "(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n).*(?=Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)",
    rooms_2     = "(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n).*(?=\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Magazin|Keller\\b|Dachboden|Estri[ch|g])"
  )
  dict$neg <- list(
    booksale = "Buchhandlung|\\B(u|ü)ch(er|lein)|Haus(-\\B|b)(u|ü)chTra(c|k)t(a|ä)t"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing in conjunction with boarding
#' @export
tagfilter_housing_rent3 <- function(){
  dict <- list()
  dict$applicable <- list("labourinfo")
  dict$pos <- list(
    rooms_1     = "(\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b).*(?=\\bKost\\b)",
    rooms_2     = "\\bKost\\b.*(?=\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b)"
  )
  dict$neg <- list(
    not_job = "\\bDiens(t|te|ten)\\b|Lehr(\\-G|g)eld|\\bLohn\\b|Entlohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen|Aufträge|Arbeiten|Anstellung|Zeugnis|verdingen"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in sale and auction sections, unambiguous search terms
#' @export
tagfilter_housing_sale1 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "saledemand", "auctions")
  dict$pos <- list(
    object = "(B|b)ehausung|Bewohnung|Losament|Liegenschaft|Rebacker",
    object_no = "Das ([a-zäöüß\\-]+ )*(Haus|Gebäude) No\\.",
    object_pub = "^([0-9]+. )*(Das|Ein) (W(i|ü)r(t|th)s|Gast)(\\-H|h)au(s|ß)",
    object_mill = "^([0-9]+. )*(Die|Eine) ([PSM][a-zäöü\\-])*(m|M)(u|ü)h(l|le)", #PS = Papier or Säge or Mahl-Mühle
    object_cellar = "^([0-9]+. )*(Der|Ein) (Flaschen\\-)Keller",
    object_misc = "^([0-9]+. )*(Der|Die|Das|Ei(n|ne)) ([A-ZÄÖÜ][a-zäöü])*((\\-]G|g)arten|(\\-]S|s)che(u|ü)(er|ren))",
    object_part = "St(u|ü)ck (Gu(t|th)\\b|Reben|Feld|Matten|Land)",
    sale = "Haus(\\-V|v)er(kauf|steigerung)|Hausgant",
    new_house = "neu( |ge|er)baute ([a-zäöüß\\-]+ )*Haus",
    measure = "Juchart|\\bTauen Matt|Matten"
  )
  dict$neg <- list(
    location_auction1 = "in ((des)*(Hr|Herr|Mst)|(der)*(Fr|Wi))[a-zäöüß\\.]* ([A-Za-zäöü][a-zäöüß\\'\\-\\.\\,]+ )*(Wohnb|B)ehausung",
    location_auction2 = "in der ([A-Za-zäöü][a-zäöüß\\'\\-]+ )*(Wohnb|B)ehausung",
    hay = "\\bEm(d|bd|dt)\\b|\\bGra(s|ß)\\b!(\\-H(ä|a)u(ß|s)|\\-Garten)|\\bH(eu|eü|ew|öw)\\b!(\\-H(ä|a)u(ß|s))|Heugras|H(eu|eü|ew|öw)\\-Gra(s|ß)",
    dung = "Kühbau|verwährter"
    )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in sale and auction sections, more ambiguous search terms
#' @export
tagfilter_housing_sale2 <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "saledemand", "auctions")
  dict$pos <- list(
    object = "\\bGuth\\b|(Land|Lust)(\\-G|g)(ü|u)(t|th)|\\bHäu(s|ß)lein", # not "Haus" as this often used in contact references in all sorts of ads
    object_phrase1 = "\\b(E|e)i(n|ne) ([a-zäöüß\\-]+ )*(Wohnung\\b|Logis|Kammer\\b|Stub(e|en)\\b|Scheuer\\b|Scheuren\\b|Stallung\\b|Spe(i|y)cher|(Sommerh|Bauchh|Waschh|Wohn|H)au(s|ß)\\b|Logis\\b|Garten\\b|Zimmer\\b|Kelle(r|rn)\\b|Stall\\b|\\bSpe(y|i)cher\\b)",
    including1 = "\\bStuben\\b.+(\\bKammern\\b|\\bKeller)|(\\bKammern\\b|\\bKeller).+\\bStuben\\b",
    including2 = "(mit|von|sam(b.+|t)|nebst) ([0-9]+. |[a-zäöüß\\-]+ )*(Wohnung|Stuben|Kammern|Keller|(Oe|E)st(er|r)ich|\\bGarten\\b|Scheuer|Stall|\\bHof\\b|Ho(f|ff)statt)",
    including3 = "(und|gro(ss|ß)en) Matten"
  )
  dict$neg <- list(
    living_at = "wohnha(f|fft)",
    location_auction1 = "in ((des)*(Hr|Herr|Mst)|(der)*(Fr|Wi))[a-zäöüß\\.]* ([A-Za-zäöü][a-zäöüß\\'\\-\\.\\,]+ )*(Wohnb|B)ehausung",
    location_auction2 = "in der ([A-Za-zäöü][a-zäöüß\\'\\-]+ )*(Wohnb|B)ehausung",
    preposition_phrase1 = "((V|v)o(r|n)|(F|f)ür|(A|a)u(f|ff|s)|(a|A)(b|n)|(I|i|U|u)(n|m)|(Z|z)u|(B|b)e(y|i)) (die|das|de(r|n|m|s)|ei(n|ne|nem|ner)) (Hrn\\. |Mstr\\. |[A-Z]\\. |[a-zäöü\\-]+ )*(Wohnung\\b|Scheuer\\b|Scheuren\\b|Stallung\\b|(Sommerh|Bauchh|Waschh|H)au(s|ß)\\b|Garten\\b|Garten\\-H(ä|a)u|Kammer\\b|Zimmer\\b|Stub(e|en)\\b|Kelle(r|rn)\\b|Stall\\b|\\bSpe(y|i)cher\\bRebacker|\\bWerkstatt)",
    preposition_phrase2 = "((V|v)o(r|n)|(F|f)ür|(A|a)u(f|ff|s)|(a|A)(b|n)|(I|i|U|u)(n|m)|(Z|z)u|(B|b)e(y|i)) (Fr(\\.|au) |(Hrn\\.|Her(r|rn|ren)) |(Mstr\\.|Meister) |([A-Z]\\. )+|([A-Za-zäöü][a-zäöüß\'\\-])+ )*(\\bGu(t|th)\\b|Landgut|Lustgut|Fabrik|Blaiche|Mühle)",
    preposition_phrase3 = "be(i|y) Haus\\b",
    to_apply_to1 = "(für|in)* ei(n|ne) ([a-zäöüß]+ )*(Zimmer|Stube|Kammer|\\wäuslein|\\w*(H|h)aus|Garten) (zu |dienlich )*(!ver(mi|kau|le))[a-zäöüß]+en",
    to_apply_to2 = "damit zu heizen |ein Zimmer damit|Zimmer ka(n|nn)\\b|Zimmer einzufassen",
    not_job = "\\bDiens(t|te|ten)\\b|\\bLohn\\b|Entlohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen|Aufträge|Arbeiten|Zeugnis",
    compounds = "Hau(s|ß)\\-|Scheuren\\-|Garten\\-|Stall\\-|Keller\\-|Zimmer\\-|Stuben\\-|Kammer\\-",
    not_a_real_house_phrase1 = "Haus von Karton",
    not_a_real_house = "Fliegen(\\-H|h)(a|ä)us|Paar(\\-H|h)(a|ä)u(s|ß)",
    pet1 = "Eichh(o|ö)rn|(V|v)(o|ö)gel",
    sled = "Hau(s|ß) Schlitten",
    other_goods = "Stein\\-Gut",
    booksale = "Buchhandlung|\\B(u|ü)ch(er|lein)|Haus(-\\B|b)(u|ü)chTra(c|k)t(a|ä)t",
    hay = "\\bEm(d|bd|dt)\\b|\\bGra(s|ß)\\b!(\\-H(ä|a)u(ß|s)|\\-Garten)|\\bH(eu|eü|ew|öw)\\b!(\\-H(ä|a)u(ß|s))|Heugras|H(eu|eü|ew|öw)\\-Gra(s|ß)",
    dung = "Kühbau|verwährter",
    registry_offers = "Berichthaus ist zu haben"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in propertysaleoffer section (section is easily overlooked, so adding all ads to housing tags as well)
#' @export
tagfilter_housing_sale3 <- function(){
  dict <- list()
  dict$applicable <- list("propertysaleoffer")
  dict$pos <- list(
    all = "." #
  )
  dict$neg <- list(
    yyy = "yyyyy"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in other sections
#' @export
tagfilter_housing_sale4 <- function(){
  dict <- list()
  dict$applicable <- list("offer", "demand", "exchange", "othernews")
  dict$pos <- list(
    general_1   = "((B|b)ehausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein).*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b)",
    spaces_1    = "(Rebacker|Garten|Gärtlein|Höfl(e|ei)n|Matten|Juchart).*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b)",
    buildings_1 = "(Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer).*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b)",
    rooms_1     = "(\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Magazin|Keller\\b|Dachboden|Estri[ch|g]).*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b)",
    general_2   = "((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b).*(?=(B|b)ehausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein)",
    spaces_2    = "((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b).*(?=Rebacker|Garten|Gärtlein|Höfl(e|ei)n|Matten|Juchart)",
    buildings_2 = "((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b).*(?=Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)",
    rooms_2     = "((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b).*(?=\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Magazin|Keller\\b|Dachboden|Estri[ch|g])"
  )
  dict$neg <- list(
    booksale = "Buchhandlung|\\B(u|ü)ch(er|lein)|Haus(-\\B|b)(u|ü)chTra(c|k)t(a|ä)t"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing offered or requested, unclear if for sale or rent
#' @export
tagfilter_housing_other1 <- function(){
  dict <- list()
  dict$applicable <- list("offer", "demand", "exchange")
  dict$pos <- list(
    object = "(B|b)ehausung|Bewohnung|Losament|Liegenschaft|Rebacker",
    object_no = "Das (Haus|Gebäude) No\\.",
    object_pub = "^([0-9]+. )*(Das|Ein) (W(i|ü)r(t|th)s|Gast)(\\-H|h)au(s|ß)",
    object_mill = "^([0-9]+. )*(Die|Eine) ([PSM][a-zäöü\\-])*(m|M)(u|ü)h(l|le)", #PS = Papier or Säge or Mahl-Mühle
    object_cellar = "^([0-9]+. )*(Der|Ein) (Flaschen\\-)Keller",
    object_misc = "^([0-9]+. )*(Der|Die|Das|Ei(n|ne)) ([A-ZÄÖÜ][a-zäöü])*((\\-]G|g)arten|(\\-]S|s)che(u|ü)(er|ren))",
    object_part = "St(u|ü)ck (Gu(t|th)\\b|Reben|Feld|Matten|Land)",
    sale = "Haus(\\-V|v)er(kauf|steigerung)|Hausgant",
    new_house = "neu( |ge|er)baute ([a-zäöüß\\-]+ )*Haus",
    measure = "Juchart|\\bTauen Matt|Matten"
  )
  dict$neg <- list(
    living_at = "wohnha(f|ff)t|Wohnungs(verä|\\-Verä|ä|\\-Ä)nder",
    living_at_phrase1 = "Wohnung No\\.",
    living_at_phrase2 = "(Wohnung|Behausung|Logis) (ver|ge)änd",
    living_at_phrase3 = "(ver|ge)änderte (Wohnung|Behausung)",
    location_1 = "((I|i)n|(B|b)e(y|i)) (((des )*(Hr|Herr|Mst)|(der )*(Fr|Wi|Jg|Ju))[a-zäöüß\\.] )*([A-Za-zäöü][a-zäöüß\\'\\-\\.\\,]+ )*(Wohnb|B)ehausung|Hau(s|ß)",
    location_2 = "((I|i)n|(B|b)e(y|i)) (seine|ihre)(m|r)* (Behausung|Wohnung|Losament|\\bLogis\\b|Hau(s|ß)\\b|Kammer\\b|Zimmer\\b|Stub(e|en)\\b|Kelle(r|rn)\\b|Stall\\b|\\bWerkstatt)",
    not_job = "\\bDiens(t|te|ten)\\b|Lehr(\\-G|g)eld|\\bLohn\\b|Entlohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen|Aufträge|Arbeiten|Anstellung|Zeugnis|verdingen",
    not_sale = "(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b",
    not_rent = "mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n",
    not_a_real_house = "Fliegen(\\-H|h)(a|ä)us|Paar(\\-H|h)(a|ä)u(s|ß)",
    compounds = "Hau(s|ß)\\-|Scheuren\\-|Garten\\-|Stall\\-|Keller\\-|Zimmer\\-|Stuben\\-|Kammer\\-",
    hay = "\\bEm(d|bd|dt)\\b|\\bGra(s|ß)\\b!(\\-H(ä|a)u(ß|s)|\\-Garten)|\\bH(eu|eü|ew|öw)\\b!(\\-H(ä|a)u(ß|s))|Heugras|H(eu|eü|ew|öw)\\-Gra(s|ß)",
    booksale = "Buchhandlung|\\B(u|ü)ch(er|lein)|Haus(-\\B|b)(u|ü)chTra(c|k)t(a|ä)t"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing, unclear if for sale or rent
#' @export
tagfilter_housing_other2 <- function(){
  dict <- list()
  dict$applicable <- list("offer", "demand", "exchange", "othernews")
  dict$pos <- list(
    general_1   = "((B|b)ehausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein).*(?=\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden)",
    spaces_1    = "(Rebacker|Garten|Gärtlein|Höfl(e|ei)n|Matten|Juchart).*(?=\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden)",
    buildings_1 = "(Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer).*(?=\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden)",
    rooms_1     = "(\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Magazin|Keller\\b|Dachboden|Estri[ch|g]).*(?=\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden)",
    general_2   = "(\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden).*(?=(B|b)ehausung|Wohnung|Losament|\\bLogis\\b|(G|g)ebäud|(H|h)äu(s|ß)lein)",
    spaces_2    = "(\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden).*(?=Rebacker|Garten|Gärtlein|Höfl(e|ei)n|Matten|Juchart)",
    buildings_2 = "(\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden).*(?=Comptoir|Hofstatt|\bLokal\b|Mühle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst(a|ä)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)",
    rooms_2     = "(\\b(begehr|wünsch)(t|en)|offer(i|ie)r(t|en)|zu haben|überlassen|tauschen|zu beziehen|bezogen werden).*(?=\\bZimmer|(S|s)tube|(S|s)tüb(lei|ch)|Kammer|(G|g)emach\\b|(G|g)emächer\\b|Küchelein|Magazin|Keller\\b|Dachboden|Estri[ch|g])"
  )
  dict$neg <- list(
    living_at = "wohnha(f|ff)t|Wohnungs(verä|\\-Verä|ä|\\-Ä)nder",
    living_at_phrase1 = "Wohnung No\\.",
    living_at_phrase2 = "(Wohnung|Behausung|Logis) (ver|ge)änd",
    living_at_phrase3 = "(ver|ge)änderte (Wohnung|Behausung)",
    location_1 = "((I|i)n|(B|b)e(y|i)) (((des )*(Hr|Herr|Mst)|(der )*(Fr|Wi|Jg|Ju))[a-zäöüß\\.] )*([A-Za-zäöü][a-zäöüß\\'\\-\\.\\,]+ )*(Wohnb|B)ehausung|Hau(s|ß)",
    location_2 = "((I|i)n|(B|b)e(y|i)) (seine|ihre)(m|r)* (Behausung|Wohnung|Losament|\\bLogis\\b|Hau(s|ß)\\b|Kammer\\b|Zimmer\\b|Stub(e|en)\\b|Kelle(r|rn)\\b|Stall\\b|\\bWerkstatt)",
    not_job = "\\bDiens(t|te|ten)\\b|Lehr(\\-G|g)eld|\\bLohn\\b|Entlohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen|Aufträge|Arbeiten|Anstellung|Zeugnis|verdingen",
    not_sale = "(K|k)au(f|ff)en|(V|v)ver(ä|a)u(ß|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\b",
    not_rent = "mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n",
    not_a_real_house = "Fliegen(\\-H|h)(a|ä)us|Paar(\\-H|h)(a|ä)u(s|ß)",
    compounds = "Hau(s|ß)\\-|Scheuren\\-|Garten\\-|Stall\\-|Keller\\-|Zimmer\\-|Stuben\\-|Kammer\\-",
    hay = "\\bEm(d|bd|dt)\\b|\\bGra(s|ß)\\b!(\\-H(ä|a)u(ß|s)|\\-Garten)|\\bH(eu|eü|ew|öw)\\b!(\\-H(ä|a)u(ß|s))|Heugras|H(eu|eü|ew|öw)\\-Gra(s|ß)",
    booksale = "Buchhandlung|\\B(u|ü)ch(er|lein)|Haus(-\\B|b)(u|ü)chTra(c|k)t(a|ä)t"
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Church Seat
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$pos <- list(
    seat = "(K|k)irchen((s|\\-S)itz|(s|\\-S)t(ü|u)hl)",
    folding_seat = "Anhencker",
    female = "(Weiber|Frau(en|n))((s|\\-S)itz|(s|\\-S)t(ü|u)hl)",
    male    = "M(a|ä)n(n|nen|ner)((s|\\-S)itz|(s|\\-S)t(ü|u)hl)"
  )
  create_filter_output(dict)
}
