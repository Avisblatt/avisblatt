#' Tagfilter Property
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for textile related ads and finds broader categories like
#' clothing and mentions of textile materials as well as smaller items like handkerchiefs,
#' special occasion wear (tag: costume) or muffs and gloves (tag: hand).
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
#' Calculated that way, the family of tagfilters concerning textiles shows
#' a precision >80% and a sensitivity >81%.
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_textiles
#' @seealso tagfilters
NULL


#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_clothing <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    clothing_general = '(?<!zu\\\\s|f\u00fcr\\\\s)(?<!Nacht|Unter|Toten)(?<!Schlaf|Todten)(Kle(i|y)d|\u00c4rmel|(?<!Sch)(?<!ge)Weste)(?!(\\\\-?(e|er|en|er|en)((k(a|\u00e4)st|schrank|mange|k\u00e4stlein))))(?!\\\\s(zu(m|)\\\\s|)(vertilgen|stricken|dienlich|w(a|\u00e4)schen|machen|gl\u00e4tten|n\u00e4hen|putzen|stricken|mangen))',
    clothing_coat_dress = '(?<!zu\\\\s|f\u00fcr\\\\s)(?<!Meyen|Nacht|Unter|Toten)(?<!Schlaf|Todten)(?<!Bolingb|Uniform)(?<!Offizier|Exerzier)(?<!Offiziers)R(o|\u00f6)ck\\\\w*?(?!(\\\\s(zu(m|)\\\\s|)(vertilgen|stricken|dienlich|w(a|\u00e4)schen|machen|gl\u00e4tten|n\u00e4hen|putzen|stricken|mangen))|stroh)',
    clothing_apron = '(?<!zu\\\\s|f\u00fcr\\\\s)Tscho(b|p|pp)en|F\u00fcrtuch\\\\w*?(?!\\\\s(zu(m|)\\\\s|)(vertilgen|stricken|dienlich|w(a|\u00e4)schen|machen|gl\u00e4tten|n\u00e4hen|putzen|stricken|mangen))',
    clothing_trousers = '(?<!zu\\\\s|f\u00fcr\\\\s)(?<!Bac|Buc|Alk|Alc)(?<!Kalc|Bauc|Alik|Alic|Alck|Ober|Frey)(?<!Alick)(?<!Waltig)Hose(?!nli(e|)(\u00df|s|ss)mer|n(tr\u00e4ger|kn(o|\u00f6)pf))(?!\\\\s(zu(m|)\\\\s|)(vertilgen|stricken|dienlich|w(a|\u00e4)schen|machen|gl\u00e4tten|n\u00e4hen|putzen|stricken|mangen))',
    clothing_skirt = '(?<!zu\\\\s|f\u00fcr\\\\s)Jun(t|dt)e|J(u|\u00fc)ppe\\\\w*?(?!(\\\\szu(m|)\\\\s|)(vertilgen|stricken|dienlich|w(a|\u00e4)schen|machen|gl\u00e4tten|n\u00e4hen|putzen|stricken|mangen))',
    clothing_shirt = '(?<!zu\\\\s|f\u00fcr\\\\s)((Herren|)Hem(d|bd)|Chemise|(C|K)amis(o|oh)l)(?!(|e|er)(tr\u00e4ger|kn(o|\u00f6)pf|gufe))(?!\\\\s(zu(m|)\\\\s|)(vertilgen|stricken|dienlich|w(a|\u00e4)schen|machen|gl\u00e4tten|n\u00e4hen|putzen|stricken|mangen))'
  )
  dict$neg <- list(
    learning = 'Unterricht|Sch\u00fclerin|Lehrerin|unterrichten|Lehrgeld|Lehre', # learning activities related to clothing
    animal = 'Federnhund', # animal related clothing
    cleaning = 'Rosen\u00f6l|Fleckenb\u00fcchlein|Motten|Schaben|Wanzen|Waschwasser|Flecken-Kugelen|Kleiderputz|Kleiderb\u00fcrst', # ads for cleaning clothing and cleaning products
    immo = 'Geb\u00e4ude|Behausung|Keller|Jucharten', # immo ads
    carneval = 'Verkleidung|Milchjoggi|(Polichinel|Masken|Fastnach(t|ts))(\\\\-|)kleid', # costumes for carneval, see dictionary 'costume'
    work = 'Hau(\u00df|s|ss)bedient|Zeugni(ss|\u00df|s)|Platz\\\\sals|Taufschein|Testimon|Religion', # work and placement ads containing clothing
    adjectives = 'gekleidet|schr\u00f6cklich|tr(o|\u00f6)ck(e|ne)', # description of other object
    other = 'Brockel' # describes appearance of different objects (small chunks)
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_sleepwear <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    sleepwear = '(Schlaf|Nach(t|ts))(\\\\-|\\\\s|)(r(o|\u00f6)(ck|k|c)|(\u00e4|a|ae|e)rmel)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_uniform <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    uniform_coat = '(Offi(z|c)(i|j)e(rs|r)|Un(i|j)form|Infanter(i|ie)|F(\u00fc|u)selier|Exerz(i|j)er|(K|C)a(n|nn)on(i|t)er|Ordonanz|Dragoner|Constahler|Frey(\\\\-|)compagn(i|j)e|Mili(z|c)|Grenadier(s|)|Chasseur|Artillerie|Voltigeur|Landwehr)(\\\\-|\\\\s|)(R(o|\u00f6)ck|Weste|Montur)',
    uniform_jacket = 'Tscha(k|kk|ck)(o|e)(?!(\\\\-|\\\\s)(deckel))',
    uniform_general = 'Un(i|j)form(?!(\\\\-|\\\\s)(kn(o|\u00f6)pf))(?!\\\\sdienlich)',
    uniform_epaulets = 'Ep(au|o)let',
    uniform_multiples = 'Milit\u00e4r(\\\\-|\\\\s|)effekt'
  )
  dict$neg <- list(
    books = 'Buchhandlung|Buchh\u00e4ndler', # books with uniform-word in titles
    carneval = 'Fastnacht', # uniforms as fancy dress for carneval
    militia = 'Mil(i|j)(z|tz)-Aufgebot|Aufgebot\\\\san\\\\sdie\\\\sMili(z|tz)' # muster for the militia
  )
  create_filter_output(dict)
}




#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_underwear <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    underwear_general = '(?<!zu\\\\s|f\u00fcr\\\\s)(Lingerie|\\\\-Leiber|Leibchen|Unter(kleid|r(o|\u00f6)ck|(\u00e4|ae|a|e)rmel|hose))',
    underwear_corset = '(?<!zu\\\\s|f\u00fcr\\\\s)(K|C)orset|(?<!Arm\\\\-|Arm\\\\s|Arm)Br(u|\u00fc)ste|Schn(u|\u00fc)rbr(u|\u00fc)st',
    underwear_socks_stocking = '(Frauen(zimmer|)|Weiber|M(a|\u00e4)nn(e(n|r)|s)|Halb|Kinder|Laid|Moden)(\\\\-|)str(u|\u00fc)mpf|Socke|Str(u|\u00fc)mpf(?!(f)?(\\\\-|\\\\s)?(karren|w(\u00e4|e)b|ausbreit|fach|Fabri|wolle|stuhl|pre(s|\u00df)|garn))'
  )
  dict$neg <- list(
    n_corsets = '(K|C)orset(?=(\\\\-|\\\\s)?fabri|machen|zeug|macher)', # compound nouns with corsets, not objects
    health = 'Krebs|Mittel|Pomade', # health ads containing underwear words, mostly 'Br\u00fcste' (breasts)
    immo = 'Losament|Behausung|Gelegenheit|Garten', # excludes related immo ads
    book = 'Heft|Kalender', # excludes related prints
    service = 'Kundenh(\u00e4|a)us', # excludes ads for related services
    board = 'Kost' # excludes ads for board with additional services
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_outerwear <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    outerwear_male = 'Frack|Talar',
    outerwear_general = '(?<!zu\\\\s|f\u00fcr\\\\s)M(a|\u00e4)ntel|Co(at|tte)|Mantille|Kittel|Pellerine'
  )
  dict$neg <- list(
    french = 'tricotte', # french for knitting, exclude if only looking at German ads
    fabric = 'Mantel(zeug|stoff)', # fabric for coats
    mercery = 'Mantelhaft', # clasps for coats
    music = 'Instrumentalartikel' # musical instruments (contains 'talar')
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_costume <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    costume_carneval = '(Milch|Bauern)(\\\\-|)jogg(el|i)|(Masken|Fastnach(t|ts)|Polichine(l|ll))(\\\\-|)kle(i|y)d|Milchbrentl|Ritter(\\\\-|)r\u00fcstung',
    costume_shroud = 'T(o|\u00f6\u00f6|\u00f6)(dt|t)enr(\u00f6|o)ck',
    costume_traditional = '\\\\bTr(o|a)cht\\\\b|Bauerntr(o|a)cht',
    costume_baptism = 'Tauf(zeug|kleid|hem(d|bd))'
  )
  dict$neg <- list(
    looking = '(B|b)etracht' # verbs and nouns meaning 'looking at' conatining 'tracht'
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_shoes <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    shoes_slippers = 'Panto(ff|f)el(?!n?\\\\-?(holz|bouteill|z(\u00e4|a)pf))|Chauffe-pied',
    shoes_general = '(?<!Hand\\\\s|Hand\\\\-|Hand|Gulden|Rad|halben)\\\\s?Sch(u|\u00fc)h(?!\\\\-?(wachs|schw\u00e4rze|kraft|knech|(m|em)acher|schnalle|enmeister|rin(ck|k)|n(\u00e4|a)gel))',
    shoes_boots = '(?<!Strick)St(ie|\u00fc)(f|ff)el(?!\\\\-?(macher|wachs|schw\u00e4rze|wichse|str(u|\u00fc)mpf|h(o|\u00f6)lz))',
    shoes_soles = 'S(o|\u00f6)hle'
  )
  dict$neg <- list(
    education = 'Sch(u|\u00fc)hl', # old spelling for 'Schule'
    immo = 'Liegenschaft', # filters out immo ads with measurements in 'Schuh'
    wood = '(Boden|Daugen)holz|Fa\u00dfdaugen|Dielen|Dach', # wood, measured in 'Schuh'
    fountain = 'Brunnstiefel|Ziehbrunn|Brunnen|Rohr', # fountains (one part is also called 'Schuh')
    # work = 'Schuhmacher|Schuster|Schuhster', # occupations concerned with making shoes
    # newly excluded, PROBLEM: sometimes filters out relevant ads - exclusion of work and immo ads probably better solution
    misc = 'Radschuh', # other objects and nouns containing 'schuh'
    measure_1 = 'Schu(h|he)\\\\s(lang|breit|dick|hoch|weit|L\u00e4nge|hohe|gro(s|\u00df))', # removes 'Schuh' as measurement, version 1
    measure_2 = 'Schu(he|h)\\\\s(lang|breit|dick|hoch|weit|L\u00e4nge|hohe|gro(s|\u00df))', # removes 'Schuh' as measurement, version 2
    measure_3 = 'Schu(he|h)\\\\s\\\\d', # removes 'Schuh' as measurement, version 3
    measure_4 = '\\\\d\\\\sSchu(he|h)', # removes 'Schuh' as measurement, version 4
    measure_5 = '\\\\bM(a|aa)(\u00df|s|ss)\\\\b', # removes 'Schuh' as measurement, version 5
    measure_6 = 'Schu(he|h)\\\\-?l\u00e4nge|sch\u00fchig|schu(h|he)(hoch|breit|lang|tief|dick|gro(s|\u00df))', # removes 'Schuh' as measurement, version 6
    measure_7 = 'Schu(he|h)\\\\s(Breite|L\u00e4nge|H\u00f6he)', # removes 'Schuh' as measurement, version 7
    measure_8 = 'L\u00e4nge\\\\s\\\\d', # removes 'Schuh' as measurement, version 8
    measure_9 = 'Breite\\\\s\\\\d' # removes 'Schuh' as measurement, version 9
  )
  # some 'schuh' as measurement remain, e.g. 'L\u00e4nge 3 1/ 2 Schuh', maybe smth like:
  #(Breite|L\u00e4nge|H\u00f6he)\\s(\\w*?\\/?\\s){1,4}Schuh
  #Schuh\\\\s(\\\\w*?\\\\s){1,4}(Breite|L\u00e4nge|H\u00f6he)
  create_filter_output(dict)

}



#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_handkerchief <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
      handkerchief_1 = '(?<!f\u00fcr\\\\s)\\\\w*?((Sch|ch)(n|u)(u|uu)p(f|ff)|Sack|Nas|Nass)t(u|\u00fc)ch\\\\w*?|(M|m)(o|\u00f6)uc(h|i)oir\\\\w*?',
      handkerchief_2 = '(?<!zu\\\\s)\\\\w*?((Sch|ch)(n|u)(u|uu)p(f|ff)|Sack|Nas|Nass)t(u|\u00fc)ch\\\\w*?|(M|m)(o|\u00f6)uc(h|i)oir\\\\w*?'
  )
  dict$neg <- list(
    textile = 'Strohsacktuch' # sort of textile
  )

  create_filter_output(dict)

}


#' @rdname tagfilter_textiles
#' @export
tagfilter_hand <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    hand_muff = '\\\\w*?(S|s)chl(u|\u00fc)p(f|ff)er|(?<!(An|Ge|Hi|gu|zu))(?<!((a|A)uf))(?<!(Chri))\\\\w*?((S|s)t(o|oo|\u00f6)(\u00df|s|ss)\\\\b|(M|m)(a|\u00e4)r(t|d)er)(?!\\\\-?((B|b)uch|(W|w)oche|(F|f)alle|n\\\\b|en\\\\b|(P|p)elz|(B|b)r\u00e4m))',
    hand_gloves = '\\\\w*?(H|h)andsch(u|\u00fc)(h|e)(?!(\\\\s|\\\\-)?((F|f)abri(c|k)|((M|m)acher)))'
  )
  dict$neg <- list(
    # profession = 'Handschuhmacher', # maybe use profession, but also excludes some relevant ads
    books = 'Pastor|Gemartert|Christos|Chronik', # book ads
    other = 'Schl\u00fcpfer-Wein' # other objects
  )
  create_filter_output(dict)
}



#' @rdname tagfilter_textiles
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_neck <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    neck_colar = '(?<!zu\\\\s|f\u00fcr\\\\s)(Palatine|Kr\u00e4gen|Cols)',
    neck_necktie = '(?<!zu\\\\s|f\u00fcr\\\\s)\\\\w*?(C|K|c|k)rav(e|a)(t|tt)e',
    neck_scarf = '(?<!zu\\\\s|f\u00fcr\\\\s)(Halstuch|F(oulard|ichu)|((Sch|Sh|Ch|Schw)(al|awl|aul|alw|avl)(s|))\\\\b)'
  )
  dict$neg <- list(
    leash = 'Hundshalsband', # dog leash
    food = 'Selteser|(Sch|Ch)alo(tt|t)e', # food and drink
    immo = 'Cha(ll|l)et', # name for a small house
    noobject = '\u00e9cols|Acols|Philantrophischal|Chaldaicum|Haushal|Schalten|(des|wes)halb|Spitchal|schalt|shaltung|aushalte|einscha(l|tt)t', # no object
    name = 'Archal|Francols|Rochal|Pa(ch|sch)al|Mar(e|\u00ea|\u00e9)(ch|sch)al|Michal|Nicol|Gottschalk|Schallbacher|Engelschall', # names
    person = 'Ma(rsch|rech)al', # military rank
    ocr = 'weshals|sichals|auchal|welchal|durchal|gleichal|Schaltem|sichall', # ocr mistakes (whitespace is missing)
    sound = 'Schalles|Schalle|Schalsconservirung|Trompeten-Schall', # sound of something
    place = 'Schaltenbrand|M\u00f6nchaltdorf|Schalbach', # placenames
    fabric = 'Cha(l|ll)(is|y|on)', # special kind of fabric; PROBLEM: sometimes description for fabric of a scarve...
    measure = 'Waagschale', # measurement containing 'schal
    bowls = 'Schale' # bowls
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_textiles
#' @export
tagfilter_headdress <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    head_wig = '(Perruck\\\\w*)(?!(macher|machen))|H(a|aa)r(-|)(T|t)ours',
    head_cap = 'Kappe\\\\w*(?!-?(M|m)acher)|Capotte|Bonnet|M\u00fctze',
    head_general_1 = '(?<!Sch\u00f6n|Walds)(?<!Schaub)(H|h)(u|\u00fc)t(?!-?((M|m)acher|(N|n)\u00e4he(r|n)|(G|g)a(s|\u00df)|(F|f)abri(c|k)|(V|v)erlag))',
    head_general_2 = '(?<!Sch\u00f6n|Walds)(?<!Schaub)(H|h)(u|\u00fc)th(?!-?((M|m)acher|(N|n)\u00e4he(r|n)|(G|g)a(s|\u00df)|(F|f)abri(c|k)|(V|v)erlag))',
    head_general_3 = '(?<!Sch\u00f6n|Walds)(?<!Schaub)(H|h)(u|\u00fc)tt(?!-?((M|m)acher|(N|n)\u00e4he(r|n)|(G|g)a(s|\u00df)|(F|f)abri(c|k)|(V|v)erlag))',
    head_straw = 'Strohh(u|\u00fc)t\\\\w*(?!-?((M|m)acher|(N|n)\u00e4he(r|n)|(G|g)a(s|\u00df)|(F|f)abri(c|k)|(V|v)erlag))',
    head_female = '(?<!zu\\\\s|f\u00fcr\\\\s)H(a|\u00e4)ub(e|chen)|Kopfputz'
  )
  dict$neg <- list(
    name = 'Kappeler', # family names
    water = 'Se(i|y)dsch\u00fctzer', # name of a special water
    animal = '(D|T)aube', # description of animal with 'kappe'
    mind = 'zu//sh\u00fcte', # minding something/someone (usally children)
    plant = 'Zuckerhut', # name of a plant
    dump = 'h\u00fctten', # verbs meaning dumping something
    beware = 'verh\u00fcte', # verb meaning beware
    name = 'Attrihut|Schaubhut|Hauber|Schaubelt', # family names
    religion = 'Herrenhut', # religious group
    unclear = 'Bonneterie|Kappellin', # exact meaning unsure, Bonneterie maybe place of making bonnets?
    place = 'Kappeln|Hutting|Sch\u00fctzen|Eisenhut|Schutzen|Brodthau\u00df', # placenames containing 'hut/h\u00fct'
    other = 'H\u00fcter|Verh\u00fctung', # other non-objects
    verb_1 = '\\\\sth(u|\u00fc)t', # verbs (doing)
    verb_2 = 'sch(u|\u00fc)tt(e|i)|Schut|sch\u00fctzen', # to throw smth
    hut = 'H\u00fctte', # small house, hut
    object = 'Fingerh(u|\u00fc)t|Str(u|\u00fc)mp(f|ff)woll|Str(u|\u00fc)mpf-Woll|Z\u00fcndh\u00fctchen|Schutt' # other objects including 'hut' or 'strumpf'
  )
  create_filter_output(dict)
}





#' @rdname tagfilter_textiles
#' @export
tagfilter_texmaterial <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    atlas = 'gestreifter Atlas',
    texmaterial_unclear = 'Nappes|Senpareille|Napolitain|Circassien|(Et|Str)amin|(K|C)amelot|Altan',
    fur = 'Pelz|Marder|Zobel|Steinmarder|Fehlin',
    marcelline = 'Marcelin',
    muslin = 'Mousselin',
    lace = 'Neiges|Spitz(lein|e)',
    canvas = 'Leinwa(nd|t)|Ba(t|tt)ist|Canevas|Halblein',
    gingham = 'G(ui|i)ngham',
    semi_silk = 'Halbseide',
    silk_origin_1 = 'Gros de', # does not seem to work- why? is the space between the problem?
    silk_origin_2 = 'Grosde',
    silk = 'Marceline|Levantine|Seide|Blonde|Flore(th|t)|Taff(ent|et)|Crepvan|Creptamin|Krapp\\\\b',
    cashemere = 'Ca(si|che)mir',
    bast = 'Bast|Sparterie',
    wicker = 'Wiener Rohr',
    cotton = 'Linon|Baumwoll',
    linnen_1 = 'leinenes Gewebe', # does not seem to work- why? is the space between the problem?
    linnen_2 = '(Futter|Steif)keinen|(Futter|Steif)-Leinen|leinen(es|er)|Leinen(waaren|plunder|b(a|\u00e4)nd|zeug)',
    printed = 'Indienne|(C|K)attun|Persienne',
    tulle = '(T|Th)ull|Bobinet|Gaze',
    oilcloth = 'Wachs(t(u|\u00fc)ch|taff|lappen)',
    merino = 'M(\u00e9|e)rino',
    flax = 'Flachs',
    wool = 'Wollenband|-Wolle|\\\\bWollenwaare|Thibet|Al\u00e9pin|wollene'
  )
  dict$neg <- list(
    book = 'B\u00fccher|Buch|Gespr\u00e4ch', # excludes ads for prints
    death = 'Ehefrau|beerdigt', # excludes death notices
    umbrella = 'Paraplu(i|y|v)|Regenschirm|Pareplu(i|y|v)|Sonnenschirm|Ombrelle|Parasol|Parresol', # excludes umbrellas, since they are not only textile objects
    immo = 'Losament|Wohnung|Ziehbrunn', # filters out immo ads
    stone = 'Alabast(e|a)r|Bernstein', # kind of stones
    bastard = 'Bastard', # child out of wedlock
    name = 'Sebastian|Sebast', # first name
    animal = 'Seidenkaninchen', # certain kind of
    work = 'unterzubringen|begehrt|gewesener|Lehrgelt|pla(c|tz|z)ieren|Handschrift|Zeugni(s|\u00df)', # excludes work ads and description of people's jobs
    service = 'waschen|flicken', # filters out ads for services related to textiles
    paper = 'Seidenpapier|Seindeppr|Papier', # kind of paper
    profession = 'Seiden(zwirnmeister|winder|handel)|Spitzenh\u00e4ndler', # profession
    workplace = 'Seiden-Zwirnerey', # workplace
    tool = 'Seidenwind-Maschine|Seidenwindmaschine|Seiden(r(a|\u00e4)d|waage|windmaschine)|Floreth-Stuhl', # tool for winding silk thread
    place = 'Gei\u00dfspitz|Dre(y|i)spitz|Seidenhof', # placename
    instrument = 'Mundspitze', # part of musical instrument
    medicine = 'Balsam', # medicine with instructions to put it on a kind of cloth
    ohter = 'Pelz(f\u00fcsse|s(\u00e4|a)cke)|Brennkessel' # other objects
  )

  create_filter_output(dict)

}


#' @rdname tagfilter_textiles
#' @export
tagfilter_cloth <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    cloth = 'Reste|Zeug|T(u|\u00fc)ch|Geflecht|Etoffe|Gewebe'
  )
  dict$neg <- list(
    witness = 'Zeugen', # witnesses
    print = 'Subscription|B(u|\u00fc)ch|Papier', # print and paper ads
    carriage = 'Chaise', # carriages with textile decorations
    animal = 'Spitzpommer|Hund', # excludes ads with animal descriptions
    death = 'begraben', # excludes death notices
    immo = 'Juchart', # excludes immo ads
    election = 'Wahl', # excludes election notices
    other = 'Ratte|Schriften|Bibliothek|Feuerzeug|Rosen\u00f6l', # excludes other unrelated objects
    bag = 'T(a|\u00e4)sch|Seckel|Beutel|S(\u00e4|a)ck|Ridicule', # excludes bags and purses
    umbrella = 'Par(a|e)plu(i|y|v)|(Regen|Sonnen)schirm|Ombrelle|Pa(r|rr)(a|e)sol', # excludes umbrellas, since they are not only textile objects
    horse = ' Pferdtzeug|R(ei|eu)tzeug', # objects for riding etc.
    ocr = 'aufhaltetofferirt', # ocr mistake (whitespace missing)
    work = 'Zeugn(i|\u00fc|u)(s|\u00df)|t\u00fcchtig|Leumundszeug|Lehrling|Bedingnis|Lehre|Ladendiener|Reisender|ledig|Lehrgel(d|dt)', # filters out work advertisements
    occupation = 'Zeugwar|Tuch(h(a|\u00e4)ndler|scherer)|putzen|Flecken|n\u00e4hen|stricken', # Occupation 'Zeugwart' containing 'zeug'
    verb = '\u00fcberzeug|restera', # verbs containing 'zeug' or 'rest'
    household = 'Bett(zeug|eingu(s|\u00df))|Tischzeug|Pre\u00df|Lampe|Tapete', # household textiles and objects
    noun = 'bezeug|erzeug', # nouns containing 'zeug'
    tool = 'Wer(k|ks|ck)zeug|W(e|\u00e4)bst(u|\u00fc)hl|Presse', # tool
    place = 'Zeughaus|Zeug-Hau(\u00df|s)', # name of a house in Basel
    paper = 'Papierrest|(Rei(\u00df|ss)|Schreib)zeug', # scrap paper, writing and painting tools
    date = 'Jahrestermin' # refers to a date
  )

  create_filter_output(dict)

}



#' @rdname tagfilter_textiles
#' @export
tagfilter_yarn <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'lendoffer', 'lenddemand', 'lend', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'auctions', 'ps', 'lostandfoundheader')
  dict$pos <- list(
    yarn_general = 'Garn|Faden|Cordon|Litze',
    yarn_embroidery = 'Stick(seide|wolle|baum)',
    yarn_knitting = 'Strick(seide|wolle|baum)'
  )
  dict$neg <- list(
    water = 'Sedlitz', # kind of water
    doves = 'Garneten', # name for specific doves
    hungary = 'Ungarn', # Hungary
    print = 'Buch|B\u00fccher', # excludes print ads
    hunt = 'Jagd|Jagds(\u00e4|a)ck|Fischgarn', # hunting with yarn
    tool = '(Faden|Waaren)z(\u00e4|a)hler|Oehren', # tool for counting threads, needles for yarn
    adjectives = 'garni(es|e|s)|garni\\\\b|garn(ie|i)(rt|tt)|fadene|garnire', # adjectives for decorated with
    decoration = 'Garni(tur|rung)', # decoration on objects
    deco_description = 'Faden durchwirkt', # decorated with yarn, not yarn itself
    military = 'Garnison' # military garrison
  )

  create_filter_output(dict)

}
