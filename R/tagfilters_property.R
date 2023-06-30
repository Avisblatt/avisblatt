#' Tagfilter Property
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for ads concerning the housing and property market,
#' and the (tradable) entitlement to fixed seats in churches.
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
#' Calculated that way, the family of tagfilters concerning housing and property
#' shows a precision >94% and a sensitivity >90%. The churchseat tagfilter shows
#' a precision >96% and a sensitivity >96%.
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_property
#' @seealso tagfilters
NULL


#' Filter Quanteda Corpus: Housing for rent in renting section
#'
#' Tagfilters are used internally in the creation of yearly collections
#' of Avisblatt ads, to attribute tags to different ads.
#'
#' @name tagfilter_property
#' @export
tagfilter_housing_rent1 <- function(){
  dict <- list()
  dict$applicable <- list('lendoffer', 'lenddemand', 'lend')
  dict$pos <- list(
    # not 'Haus' as this often used in contact references in all sorts of ads
    general = '(B|b)ehausung|(W|w)ohnung|Losament|\\\\bL(o|a)gi(s|\\\\s)\\\\b|
    (G|g)eb\u00e4ud|(H|h)\u00e4u(s|\u00df)lein|(Land|Wohn)haus|Landg(\u00fc|u)t|G(u|\u00fc)tlein',
    a_house = '(\\\\b E|\\\\s e)in Haus',
    house_to_rent = 'Haus( | zu )(verm|m)iet',
    date = '(F|f)roh?nfasten',
    spaces = 'Rebacker|Garten|G\u00e4rtlein|H\u00f6fl(e|ei)n|Matten|Juchart',
    buildings = paste0('Comptoir|Hofstatt|\\\\bLokal\\\\b|M\u00fchle|Remise|Scheuer|',
    'Schmiede|Schreinerei|B(e|\u00e4|a)ckere(y|i)|W(\u00e4|a)schere(y|i)|Wirthschaft|',
    'Stall|Werkst(a|\u00e4)tt|Laden\\\\b|\\\\Stand\\\\b|Lustgut|Laube|(Sommer|Bauch|Wasch)haus|Weichekammer'),
    floor = '(D|d)er( ganze | )(1st|2te|erste|zwe(i|y)te|dritte|untere|mittlere|obere) Stock',
    rooms = paste0('\\\\bchambr(e|es)\\\\b|\\\\bZimmer|(S|s)tube|(S|s)t(\u00fc|u)b(lei|ch)',
                   '|Kammer|(G|g)emach\\\\b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein|Al(ek|k)oven|Kostbett'),
    storage = 'Magazin|Keller\\\\b|Dachboden|Estri[ch|g]',
    amenities = paste0('m(\u00f6|eu)blie?r|Abtritt|Abwasser|Altan|Bauchkessel|
                       (B|b)runnen|Wasserleitung|Wasserstein'),
    location1 = '(gut|best)(e|er) Lage',
    location2 = '(gut|zentral|nah) gelegen'
  )
  dict$neg <- list(
    position_phrase1 = '(vor|f\u00fcr|au(f|ff)|in) (der|die|eine) Behausung',
    position_phrase2 = '(vor|f\u00fcr|au(f|ff)|in) ein Haus\\\\b',
    position_phrase3 = 'be(i|y) Haus\\\\b',
    booksale = 'Buchhandlung|\\\\B(u|\u00fc)ch(er|lein)|Haus(-\\\\B|b)(u|\u00fc)chTra(c|k)t(a|\u00e4)t',
    sled = 'Haus-Schlitten'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for rent in other sections
#' @name tagfilter_property
#' @export
tagfilter_housing_rent2 <- function(){
  dict <- list()
  dict$applicable <- list('offering', 'demanding', 'exchange', 'othernews')
  dict$pos <- list(
    general_1   = paste0('((B|b)ehausung|Wohnung|Losament|\\\\bLogis\\\\b|Landgut|',
                         '(G|g)eb\u00e4ud|(H|h)\u00e4u(s|\u00df)lein).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)'),
    spaces_1    = paste0('(Rebacker|Garten|G\u00e4rtlein|H\u00f6fl[e|ei]n|Matten|Mattland|',
                         'Juchart).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)'),
    buildings_1 = paste0('(Comptoir|Hofstatt|\\\\bLokal\\\\b|M\u00fchle|Remise|Scheuer|',
                         'Schmiede|Schreinerei|Stall|Werkst(a|\u00e4)tt|Laden\\\\b',
                         '|\\\\Stand\\\\b|Lustgut|Laube|Sommerhaus|Bauchhaus|',
                         'Waschhaus|Weichekammer).*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)'),
    rooms_1     = paste0('(\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|Kammer|',
    '(G|g)emach\\\\b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein|Magazin|Keller\\\\b|Dachboden|Estri[ch|g])',
    '.*(?=mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)'),
    general_2   = paste0('(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)',
                         '.*(?=Behausung|Wohnung|Losament|\\\\bLogis\\\\b|(G|g)eb\u00e4ud|(H|h)\u00e4u(s|\u00df)lein)'),
    spaces_2    = paste0('(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)',
    '.*(?=Rebacker|Garten|G\u00e4rtlein|H\u00f6fl[e|ei]n|Matten|Juchart)'),
    buildings_2 = paste0('(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)',
                         '.*(?=Comptoir|Hofstatt|\\bLokal\\b|M\u00fchle|Remise|Scheuer|',
                         'Schmiede|Schreinerei|Stall|Werkst(a|\u00e4)tt|Lustgut|Laube|',
                         'Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)'),
    rooms_2     = paste0('(mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n)',
                         '.*(?=\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|Kammer|',
                         '(G|g)emach\\\\b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein|Magazin|',
                         'Keller\\\\b|Dachboden|Estri[ch|g])')
  )
  dict$neg <- list(
    booksale = 'Buchhandlung|\\\\B(u|\u00fc)ch(er|lein)|Haus(-\\\\B|b)(u|\u00fc)chTra(c|k)t(a|\u00e4)t'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing in conjunction with boarding
#' @export
tagfilter_housing_rent3 <- function(){
  dict <- list()
  dict$applicable <- list('labourinfo')
  dict$pos <- list(
    rooms_1     = paste0('(\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|Kammer|',
                         '(G|g)emach\\\\b|(G|g)em\u00e4cher\\\\b).*(?=\\\\bKost\\\\b)'),
    rooms_2     = paste0('\\\\bKost\\\\b.*(?=\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|',
                         'Kammer|(G|g)emach\\\\b|(G|g)em\u00e4cher\\\\b)')
  )
  dict$neg <- list(
    not_job = paste0('\\\\bDiens(t|te|ten)\\\\b|Lehr(\\\\-G|g)eld|\\\\bLohn\\\\b|',
                     'Entlohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen|',
                     'Auftr\u00e4ge|Arbeiten|Anstellung|Zeugnis|verdingen')
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in sale and auction sections, unambiguous search terms
#' @name tagfilter_property
#' @export
tagfilter_housing_sale1 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'auctions')
  dict$pos <- list(
    object = '(B|b)ehausung|Bewohnung|Losament|Liegenschaft|Rebacker',
    object_no = 'Das ([a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*(Haus|Geb\u00e4ude) No\\\\.',
    object_pub = '^([0-9]+. )*(Das|Ein) (W(i|\u00fc)r(t|th)s|Gast)(\\\\-H|h)au(s|\u00df)',
    object_mill = '^([0-9]+. )*(Die|Eine) ([PSM][a-z\u00e4\u00f6\u00fc\\\\-])*(m|M)(u|\u00fc)h(l|le)', #PS = Papier or S\u00e4ge or Mahl-M\u00fchle
    object_cellar = '^([0-9]+. )*(Der|Ein) (Flaschen\\\\-)Keller',
    object_misc = paste0('^([0-9]+. )*(Der|Die|Das|Ei(n|ne)) ([A-Z\u00c4\u00d6\u00dc][a-z\u00e4\u00f6\u00fc])',
                         '*((\\\\-]G|g)arten|(\\\\-]S|s)che(u|\u00fc)(er|ren))'),
    object_part = 'St(u|\u00fc)ck (Gu(t|th)\\\\b|Reben|Feld|Matten|Land)',
    furniture = 'm(\u00f6|eu)blie?r',
    sale = 'Haus(\\\\-V|v)er(kauf|steigerung)|Hausgant',
    new_house = 'neu( |ge|er)baute ([a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*Haus',
    measure = 'Juchart|\\\\bTauen Matt|Matten'
  )
  dict$neg <- list(
    location_auction1 = paste0('in ((des)*(Hr|Herr|Mst)|(der)*(Fr|Wi))[a-z\u00e4\u00f6\u00fc\u00df\\\\.]*',
                               '([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df\\\\-\\\\.\\\\,]+ )*(Wohnb|B)ehausung'),
    location_auction2 = 'in der ([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*(Wohnb|B)ehausung',
    hay = paste0('\\\\bEm(d|bd|dt)\\\\b|\\\\bGra(s|\u00df)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s)|\\\\-Garten)|',
                 '\\\\bH(eu|e\u00fc|ew|\u00f6w)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s))|Heugras|H(eu|e\u00fc|ew|\u00f6w)\\\\-Gra(s|\u00df)'),
    dung = 'K\u00fchbau|verw\u00e4hrter'
    )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in sale and auction sections, more ambiguous search terms
#' @name tagfilter_property
#' @export
tagfilter_housing_sale2 <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'auctions')
  dict$pos <- list(
    object = '\\\\bGuth\\\\b|(Land|Lust).?(G|g)(\u00fc|u)t|\\\\bH\u00e4u(s|\u00df)lein', # not 'Haus' as this often used in contact references in all sorts of ads
    object_phrase1 = '\\\\b(E|e)i(n|ne) ([a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*(Wohnung\\\\b|Logis|Kammer\\\\b|Stub(e|en)\\\\b|Scheuer\\\\b|Scheuren\\\\b|Stallung\\\\b|Spe(i|y)cher|(Sommerh|Bauchh|Waschh|Wohn|H)au(s|\u00df)\\\\b|Logis\\\\b|Garten\\\\b|Zimmer\\\\b|Kelle(r|rn)\\\\b|Stall\\\\b|\\\\bSpe(y|i)cher\\\\b)',
    including1 = '\\\\bStuben\\\\b.+(\\\\bKammern\\\\b|\\\\bKeller)|(\\\\bKammern\\\\b|\\\\bKeller).+\\\\bStuben\\\\b',
    including2 = '(mit|von|sam(b.+|t)|nebst) ([0-9]+. |[a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*(Wohnung|Stuben|Kammern|Keller|(Oe|E)st(er|r)ich|\\\\bGarten\\\\b|Scheuer|Stall|\\\\bHof\\\\b|Ho(f|ff)statt)',
    including3 = '(und|gro(ss|\u00df)en) Matten'
  )
  dict$neg <- list(
    living_at = 'wohnha(f|fft)',
    location_auction1 = paste0('in ((des)*(Hr|Herr|Mst)|(der)*(Fr|Wi))[a-z\u00e4\u00f6\u00fc\u00df\\\\.]',
    '* ([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df\\\\-\\\\.\\\\,]+ )*(Wohnb|B)ehausung'),
    location_auction2 = 'in der ([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*(Wohnb|B)ehausung',
    preposition_phrase1 = paste0('((V|v)o(r|n)|(F|f)\u00fcr|(A|a)u(f|ff|s)|(a|A)(b|n)|',
                                 '(I|i|U|u)(n|m)|(Z|z)u|(B|b)e(y|i))',
                                 '(die|das|de(r|n|m|s)|ei(n|ne|nem|ner))',
                                 '(Hrn\\\\. |Mstr\\\\. |[A-Z]\\\\. |[a-z\u00e4\u00f6\u00fc\\\\-]+ )*',
                                 '(Wohnung\\\\b|Scheuer\\\\b|Scheuren\\\\b|Stallung\\\\b|',
                                 '(Sommerh|Bauchh|Waschh|H)au(s|\u00df)\\\\b|Garten\\\\b|',
                                 'Garten\\\\-H(\u00e4|a)u|Kammer\\\\b|Zimmer\\\\b|Stub(e|en)',
                                 '\\\\b|Kelle(r|rn)\\\\b|Stall\\\\b|',
                                 '\\\\bSpe(y|i)cher\\\\bRebacker|\\\\bWerkstatt)'),
    preposition_phrase2 = paste0('((V|v)o(r|n)|(F|f)\u00fcr|(A|a)u(f|ff|s)|(a|A)(b|n)',
                                 '|(I|i|U|u)(n|m)|(Z|z)u|(B|b)e(y|i)) (Fr(\\\\.|au)',
                                 '|(Hrn\\\\.|Her(r|rn|ren)) |(Mstr\\\\.|Meister) |',
                                 '([A-Z]\\\\. )+|([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df\\\\\\-])+ )',
                                 '*(\\\\bGu(t|th)\\\\b|Landgut|Lustgut|Fabrik|Blaiche|M\u00fchle)'),
    preposition_phrase3 = 'be(i|y) Haus\\\\b',
    to_apply_to1 = paste0('(f\u00fcr|in)* ei(n|ne) ([a-z\u00e4\u00f6\u00fc\u00df]+ )*(Zimmer|Stube|Kammer',
                          '|\\\\w\u00e4uslein|\\\\w*(H|h)aus|Garten) (zu |dienlich )*',
                          '(!ver(mi|kau|le))[a-z\u00e4\u00f6\u00fc\u00df]+en'),
    to_apply_to2 = 'damit zu heizen |ein Zimmer damit|Zimmer ka(n|nn)\\\\b|Zimmer einzufassen',
    not_job = paste0('\\\\bDiens(t|te|ten)\\\\b|\\\\bLohn\\\\b|Entlohnung|recomm(a|e)',
                     'nd(i|ie)r(en|t)|empfiehlt|empfehlen|Auftr\u00e4ge|Arbeiten|Zeugnis'),
    compounds = 'Hau(s|\u00df)\\\\-|Scheuren\\\\-|Garten\\\\-|Stall\\\\-|Keller\\\\-|Zimmer\\\\-|Stuben\\\\-|Kammer\\\\-',
    not_a_real_house_phrase1 = 'Haus von Karton',
    not_a_real_house = 'Fliegen(\\\\-H|h)(a|\u00e4)us|Paar(\\\\-H|h)(a|\u00e4)u(s|\u00df)',
    pet1 = 'Eichh(o|\u00f6)rn|(V|v)(o|\u00f6)gel',
    sled = 'Hau(s|\u00df) Schlitten',
    other_goods = 'Stein\\\\-Gut',
    booksale = 'Buchhandlung|\\\\B(u|\u00fc)ch(er|lein)|Haus(-\\\\B|b)(u|\u00fc)chTra(c|k)t(a|\u00e4)t',
    hay = paste0('\\\\bEm(d|bd|dt)\\\\b|\\\\bGra(s|\u00df)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s)|\\\\-Garten)|',
                 '\\\\bH(eu|e\u00fc|ew|\u00f6w)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s))|Heugras|H(eu|e\u00fc|ew|\u00f6w)\\\\-Gra(s|\u00df)'),
    dung = 'K\u00fchbau|verw\u00e4hrter',
    registry_offers = 'Berichthaus ist zu haben'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for auction sale in the house itself
#' @name tagfilter_property
#' @export
tagfilter_housing_sale3 <- function(){
  dict <- list()
  dict$applicable <- list('auctions')
  dict$pos <- list(
    object = '(?<!in der|schen|sel\\\\.) (B|Wohnb)ehausung'
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in propertysaleoffer section (section is easily overlooked, so adding all ads to housing tags as well)
#' @name tagfilter_property
#' @export
tagfilter_housing_sale4 <- function(){
  dict <- list()
  dict$applicable <- list('propertysaleoffer')
  dict$pos <- list(
    all = '.' #
  )
  dict$neg <- list(
    yyy = 'yyyyy'
  )
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing for sale in other sections
#' @name tagfilter_property
#' @export
tagfilter_housing_sale5 <- function(){
  dict <- list()
  dict$applicable <- list('offering', 'demanding', 'exchange', 'othernews')
  dict$pos <- list(
    general_1   = paste0('((B|b)ehausung|Wohnung|Losament|\\\\bLogis\\\\b|(Land|',
                         'Lehen)gut|(G|g)eb\u00e4ud|(H|h)\u00e4u(s|\u00df)lein).*(?=(V|v)erstei',
                         'ger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)u(\u00df|ss)er|au(f|',
                         'ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) ',
                         '(Verk|K)au(f|ff)\\\\b)'),
    spaces_1    = paste0('(Rebacker|Garten|G\u00e4rtlein|H\u00f6fl(e|ei)n|Matten|Mattland|',
                         'Juchart).*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)',
                         'ver(\u00e4|a)u(\u00df|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|',
                         'z(u|um) (Verk|K)au(f|ff)\\\\b)'),
    buildings_1 = paste0('(Comptoir|Hofstatt|\\bLokal\\b|M\u00fchle|Remise|Scheuer|',
                         'Schmiede|Schreinerei|Stall|Werkst(a|\u00e4)tt|Lustgut|Laube|',
                         'Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)',
                         '.*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)',
                         'ver(\u00e4|a)u(\u00df|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|',
                         'z(u|um) (Verk|K)au(f|ff)\\\\b)'),
    rooms_1     = paste0('(\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|Kammer|(G|g)emach',
                         '\\\\b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein|Magazin|Keller\\\\b|Dachboden',
                         '|Estri[ch|g]).*(?=(V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|',
                         '(V|v)ver(\u00e4|a)u(\u00df|ss)er|au(f|ff)(ger|r)u',
                         '(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\\\b)'),
    general_2   = paste0('((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)',
                         'u(\u00df|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) ',
                         '(Verk|K)au(f|ff)\\\\b).*(?=(B|b)ehausung|Wohnung|Losament|',
                         '\\\\bLogis\\\\b|(G|g)eb\u00e4ud|(H|h)\u00e4u(s|\u00df)lein)'),
    spaces_2    = paste0('((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)u',
                         '(\u00df|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) ',
                         '(Verk|K)au(f|ff)\\\\b).*(?=Rebacker|Garten|G\u00e4rtlein|H\u00f6fl',
                         '(e|ei)n|Matten|Juchart)'),
    buildings_2 = paste0('((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)u',
                         '(\u00df|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) ',
                         '(Verk|K)au(f|ff)\\\\b).*(?=Comptoir|Hofstatt|\\bLokal\\b|',
                         'M\u00fchle|Remise|Scheuer|Schmiede|Schreinerei|Stall|Werkst',
                         '(a|\u00e4)tt|Lustgut|Laube|Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)'),
    rooms_2     = paste0('((V|v)ersteiger(t|ung)|(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)u',
                         '(\u00df|ss)er|au(f|ff)(ger|r)u(f|ff)en|ergan(t|th)|z(u|um) ',
                         '(Verk|K)au(f|ff)\\\\b).*(?=\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb',
                         '(lei|ch)|Kammer|(G|g)emach\\\\b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein',
                         '|Magazin|Keller\\\\b|Dachboden|Estri[ch|g])')
  )
  dict$neg <- list(
    booksale = 'Buchhandlung|\\\\B(u|\u00fc)ch(er|lein)|Haus(-\\\\B|b)(u|\u00fc)chTra(c|k)t(a|\u00e4)t'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing offered or requested, unclear if for sale or rent
#' @name tagfilter_property
#' @export
tagfilter_housing_other1 <- function(){
  dict <- list()
  dict$applicable <- list('offering', 'demanding', 'exchange')
  dict$pos <- list(
    object = '(B|b)ehausung|Bewohnung|Losament|Liegenschaft|Rebacker',
    object_no = 'Das (Haus|Geb\u00e4ude) No\\\\.',
    object_pub = '^([0-9]+. )*(Das|Ein) (W(i|\u00fc)r(t|th)s|Gast)(\\\\-H|h)au(s|\u00df)',
    object_mill = '^([0-9]+. )*(Die|Eine) ([PSM][a-z\u00e4\u00f6\u00fc\\\\-])*(m|M)(u|\u00fc)h(l|le)', #PS = Papier or S\u00e4ge or Mahl-M\u00fchle
    object_cellar = '^([0-9]+. )*(Der|Ein) (Flaschen\\\\-)Keller',
    object_misc = paste0('^([0-9]+. )*(Der|Die|Das|Ei(n|ne)) ([A-Z\u00c4\u00d6\u00dc][a-z\u00e4\u00f6\u00fc])',
                         '*((\\\\-]G|g)arten|(\\\\-]S|s)che(u|\u00fc)(er|ren))'),
    object_part = 'St(u|\u00fc)ck (Gu(t|th)\\\\b|Reben|Feld|Matten|Land)',
    sale = 'Haus(\\\\-V|v)er(kauf|steigerung)|Hausgant',
    new_house = 'neu( |ge|er)baute ([a-z\u00e4\u00f6\u00fc\u00df\\\\-]+ )*Haus',
    measure = 'Juchart|\\\\bTauen Matt|Matten'
  )
  dict$neg <- list(
    living_at = 'wohnha(f|ff)t|Wohnungs(ver\u00e4|\\\\-Ver\u00e4|\u00e4|\\\\-\u00c4)nder',
    living_at_phrase1 = 'Wohnung No\\\\.',
    living_at_phrase2 = '(Wohnung|Behausung|Logis) (ver|ge)\u00e4nd',
    living_at_phrase3 = '(ver|ge)\u00e4nderte (Wohnung|Behausung)',
    location_1 = paste0('((I|i)n|(B|b)e(y|i)) (((des )*(Hr|Herr|Mst)|(der )*',
                        '(Fr|Wi|Jg|Ju))[a-z\u00e4\u00f6\u00fc\u00df\\\\.] )*([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df',
                        '\\\\-\\\\.\\\\,]+ )*(Wohnb|B)ehausung|Hau(s|\u00df)'),
    location_2 = paste0('((I|i)n|(B|b)e(y|i)) (seine|ihre)(m|r)* ',
                        '(Behausung|Wohnung|Losament|\\\\bLogis\\\\b|Hau(s|\u00df)\\\\b|',
                        'Kammer\\\\b|Zimmer\\\\b|Stub(e|en)\\\\b|Kelle(r|rn)\\\\b|Stall',
                        '\\\\b|\\\\bWerkstatt)'),
    not_job = paste0('\\\\bDiens(t|te|ten)\\\\b|Lehr(\\\\-G|g)eld|\\\\bLohn\\\\b|',
                     'Entlohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen',
                     '|Auftr\u00e4ge|Arbeiten|Anstellung|Zeugnis|verdingen'),
    not_sale = paste0('(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)u(\u00df|ss)er|au(f|ff)(ger|r)',
                      'u(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\\\b'),
    not_rent = 'mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n',
    not_a_real_house = 'Fliegen(\\\\-H|h)(a|\u00e4)us|Paar(\\\\-H|h)(a|\u00e4)u(s|\u00df)',
    compounds = 'Hau(s|\u00df)\\\\-|Scheuren\\\\-|Garten\\\\-|Stall\\\\-|Keller\\\\-|Zimmer\\\\-|Stuben\\\\-|Kammer\\\\-',
    hay = paste0('\\\\bEm(d|bd|dt)\\\\b|\\\\bGra(s|\u00df)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s)|\\\\-Garten)|',
                 '\\\\bH(eu|e\u00fc|ew|\u00f6w)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s))|Heugras|H(eu|e\u00fc|ew|\u00f6w)\\\\-Gra(s|\u00df)'),
    booksale = 'Buchhandlung|\\\\B(u|\u00fc)ch(er|lein)|Haus(-\\\\B|b)(u|\u00fc)chTra(c|k)t(a|\u00e4)t'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Housing, unclear if for sale or rent
#' @name tagfilter_property
#' @export
tagfilter_housing_other2 <- function(){
  dict <- list()
  dict$applicable <- list('offering', 'demanding', 'exchange', 'othernews')
  dict$pos <- list(
    general_1   = paste0('((B|b)ehausung|Wohnung|Losament|\\\\bLogis\\\\b|(G|g)eb\u00e4ud',
                         '|(H|h)\u00e4u(s|\u00df)lein).*(?=\\\\b(begehr|w\u00fcnsch)(t|en)|offer',
                         '(i|ie)r(t|en)|zu haben|\u00fcberlassen|tauschen|zu beziehen|',
                         'bezogen werden)'),
    spaces_1    = paste0('(Rebacker|Garten|G\u00e4rtlein|H\u00f6fl(e|ei)n|Matten|Juchart)',
                         '.*(?=\\\\b(begehr|w\u00fcnsch)(t|en)|offer(i|ie)r(t|en)|zu',
                         'haben|\u00fcberlassen|tauschen|zu beziehen|bezogen werden)'),
    buildings_1 = paste0('(Hofstatt|\\bLokal\\b|M\u00fchle|Remise|Scheuer|Schmiede|',
                         'Schreinerei|Stall|Werkst(a|\u00e4)tt|Lustgut|Laube|Sommerhaus|',
                         'Bauchhaus|Waschhaus|Weichekammer).*(?=\\\\b(begehr|w\u00fcnsch)',
                         '(t|en)|offer(i|ie)r(t|en)|zu haben|\u00fcberlassen|tauschen|zu',
                         'beziehen|bezogen werden)'),
    rooms_1     = paste0('(\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|Kammer|(G|g)emach\\\\',
                         'b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein|Magazin|Keller\\\\b|Dachboden|',
                         'Estri[ch|g]).*(?=\\\\b(begehr|w\u00fcnsch)(t|en)|offer(i|ie)',
                         'r(t|en)|zu haben|\u00fcberlassen|tauschen|zu beziehen|bezogen werden)'),
    general_2   = paste0('(\\\\b(begehr|w\u00fcnsch)(t|en)|offer(i|ie)r(t|en)|zu haben|',
                         '\u00fcberlassen|tauschen|zu beziehen|bezogen werden).*(?=(',
                         'B|b)ehausung|Wohnung|Losament|\\\\bLogis\\\\b|(G|g)eb\u00e4ud|(H|h)\u00e4u(s|\u00df)lein)'),
    spaces_2    = paste0('(\\\\b(begehr|w\u00fcnsch)(t|en)|offer(i|ie)r(t|en)|zu haben',
                         '|\u00fcberlassen|tauschen|zu beziehen|bezogen werden).*',
                         '(?=Rebacker|Garten|G\u00e4rtlein|H\u00f6fl(e|ei)n|Matten|Juchart)'),
    buildings_2 = paste0('(\\\\b(begehr|w\u00fcnsch)(t|en)|offer(i|ie)r(t|en)|zu haben',
                         '|\u00fcberlassen|tauschen|zu beziehen|bezogen werden).*',
                         '(?=Comptoir|Hofstatt|\\bLokal\\b|M\u00fchle|Remise|Scheuer|',
                         'Schmiede|Schreinerei|Stall|Werkst(a|\u00e4)tt|Lustgut|Laube|',
                         'Sommerhaus|Bauchhaus|Waschhaus|Weichekammer)'),
    rooms_2     = paste0('(\\\\b(begehr|w\u00fcnsch)(t|en)|offer(i|ie)r(t|en)|zu haben|',
                         '\u00fcberlassen|tauschen|zu beziehen|bezogen werden).*',
                         '(?=\\\\bZimmer|(S|s)tube|(S|s)t\u00fcb(lei|ch)|Kammer|(G|g)',
                         'emach\\\\b|(G|g)em\u00e4cher\\\\b|K\u00fcchelein|Magazin|Keller\\\\b|',
                         'Dachboden|Estri[ch|g])')
  )
  dict$neg <- list(
    living_at = 'wohnha(f|ff)t|Wohnungs(ver\u00e4|\\\\-Ver\u00e4|\u00e4|\\\\-\u00c4)nder',
    living_at_phrase1 = 'Wohnung No\\\\.',
    living_at_phrase2 = '(Wohnung|Behausung|Logis) (ver|ge)\u00e4nd',
    living_at_phrase3 = '(ver|ge)\u00e4nderte (Wohnung|Behausung)',
    location_1 = paste0('((I|i)n|(B|b)e(y|i)) (((des )*(Hr|Herr|Mst)|(der )*',
                        '(Fr|Wi|Jg|Ju))[a-z\u00e4\u00f6\u00fc\u00df\\\\.] )*([A-Za-z\u00e4\u00f6\u00fc][a-z\u00e4\u00f6\u00fc\u00df',
                        '\\\\-\\\\.\\\\,]+ )*(Wohnb|B)ehausung|Hau(s|\u00df)'),
    location_2 = paste0('((I|i)n|(B|b)e(y|i)) (seine|ihre)(m|r)*',
                        '(Behausung|Wohnung|Losament|\\\\bLogis\\\\b|Hau(s|\u00df)\\\\b|',
                        'Kammer\\\\b|Zimmer\\\\b|Stub(e|en)\\\\b|Kelle(r|rn)\\\\b|Stall',
                        '\\\\b|\\\\bWerkstatt)'),
    not_job = paste0('\\\\bDiens(t|te|ten)\\\\b|Lehr(\\\\-G|g)eld|\\\\bLohn\\\\b|Entl',
                     'ohnung|recomm(a|e)nd(i|ie)r(en|t)|empfiehlt|empfehlen|Auftr',
                     '\u00e4ge|Arbeiten|Anstellung|Zeugnis|verdingen'),
    not_sale = paste0('(K|k)au(f|ff)en|(V|v)ver(\u00e4|a)u(\u00df|ss)er|au(f|ff)(ger|r)u',
                      '(f|ff)en|ergan(t|th)|z(u|um) (Verk|K)au(f|ff)\\\\b'),
    not_rent = 'mie(t|th)(e|u)n|le(ih|yh|hn)en|(P|p)acht(e|u)n',
    not_a_real_house = 'Fliegen(\\\\-H|h)(a|\u00e4)us|Paar(\\\\-H|h)(a|\u00e4)u(s|\u00df)',
    compounds = 'Hau(s|\u00df)\\\\-|Scheuren\\\\-|Garten\\\\-|Stall\\\\-|Keller\\\\-|Zimmer\\\\-|Stuben\\\\-|Kammer\\\\-',
    hay = paste0('\\\\bEm(d|bd|dt)\\\\b|\\\\bGra(s|\u00df)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s)|\\\\-Garten)|',
                 '\\\\bH(eu|e\u00fc|ew|\u00f6w)\\\\b!(\\\\-H(\u00e4|a)u(\u00df|s))|Heugras|H(eu|e\u00fc|ew|\u00f6w)\\\\-Gra(s|\u00df)'),
    booksale = 'Buchhandlung|\\\\B(u|\u00fc)ch(er|lein)|Haus(-\\\\B|b)(u|\u00fc)chTra(c|k)t(a|\u00e4)t'
  )
  dict$exclude <- housing_exclude()
  create_filter_output(dict)
}


#' Filter Quanteda Corpus: Church Seat
#' @name tagfilter_property
#' @export
tagfilter_churchseat <- function(){
  dict <- list()
  dict$applicable <- list('saledemand', 'saleoffer', 'lendoffer', 'lenddemand',
                          'lend', 'labourinfo', 'auctions', 'othernews',
                          'exchange', 'demanding', 'offering', 'ps')
  dict$pos <- list(
    seat = '(K|k)irchen(.?(s|S)i?t?z|.?(s|S)t(\u00fc|u)hl)',
    folding_seat = 'Anhencker',
    female = '(Weiber|Frau(en|n)(|zimmer))(|vor)(.?(s|S)i?t?z|.?(s|S)t(\u00fc|u)hl)',
    male    = '(M(a|\u00e4)n(n|nen|ner)|Herren)(|vor)(.?(s|S)i?t?z|.?(s|S)t(\u00fc|u)hl)'
  )
  create_filter_output(dict)
}
