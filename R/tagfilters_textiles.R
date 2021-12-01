#' Dictionary Clothing and Garments (General)
#' @export
tagfilter_clothing <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    clothing_general = "(?<!zu\\s|für\\s)(?<!Nacht|Unter|Toten)(?<!Schlaf|Todten)(Kle(i|y)d|Ärmel|(?<!Sch)(?<!ge)Weste)(?!(\\-?(e|er|en|er|en)((k(a|ä)st|schrank|mange|kästlein))))(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    clothing_coat_dress = "(?<!zu\\s|für\\s)(?<!Meyen|Nacht|Unter|Toten)(?<!Schlaf|Todten)(?<!Bolingb|Uniform)(?<!Offizier|Exerzier)(?<!Offiziers)R(o|ö)ck\\w*?(?!(\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))|stroh)",
    clothing_apron = "(?<!zu\\s|für\\s)Tscho(b|p|pp)en|Fürtuch\\w*?(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    clothing_trousers = "(?<!zu\\s|für\\s)(?<!Bac|Buc|Alk|Alc)(?<!Kalc|Bauc|Alik|Alic|Alck|Ober|Frey)(?<!Alick)(?<!Waltig)Hose(?!nli(e|)(ß|s|ss)mer|n(träger|kn(o|ö)pf))(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    clothing_skirt = "(?<!zu\\s|für\\s)Jun(t|dt)e|J(u|ü)ppe\\w*?(?!(\\szu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    clothing_shirt = "(?<!zu\\s|für\\s)((Herren|)Hem(d|bd)|Chemise|(C|K)amis(o|oh)l)(?!(|e|er)(träger|kn(o|ö)pf|gufe))(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))"
  )
  dict$neg <- list(
    learning = "Unterricht|Schülerin|Lehrerin|unterrichten|Lehrgeld|Lehre", # learning activities related to clothing
    animal = "Federnhund", # animal related clothing
    cleaning = "Rosenöl|Fleckenbüchlein|Motten|Schaben|Wanzen|Waschwasser|Flecken-Kugelen|Kleiderputz|Kleiderbürst", # ads for cleaning clothing and cleaning products
    immo = "Gebäude|Behausung|Keller|Jucharten", # immo ads
    carneval = "Verkleidung|Milchjoggi|(Polichinel|Masken|Fastnach(t|ts))(\\-|)kleid", # costumes for carneval, see dictionary "costume"
    work = "Hau(ß|s|ss)bedient|Zeugni(ss|ß|s)|Platz\\sals|Taufschein|Testimon|Religion", # work and placement ads containing clothing
    adjectives = "gekleidet|schröcklich|tr(o|ö)ck(e|ne)", # description of other object
    other = "Brockel" # describes appearance of different objects (small chunks)
  )
  create_filter_output(dict)
}


#' Dictionary Sleepwear
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_sleepwear <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    sleepwear = "(Schlaf|Nach(t|ts))(\\-|\\s|)(r(o|ö)(ck|k|c)|(ä|a|ae|e)rmel)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)
}


#' Dictionary Military Clothing/ Uniforms
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_uniform <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    uniform_coat = "(Offi(z|c)(i|j)e(rs|r)|Un(i|j)form|Infanter(i|ie)|F(ü|u)selier|Exerz(i|j)er|(K|C)a(n|nn)on(i|t)er|Ordonanz|Dragoner|Constahler|Frey(\\-|)compagn(i|j)e|Mili(z|c)|Grenadier(s|)|Chasseur|Artillerie|Voltigeur|Landwehr)(\\-|\\s|)(R(o|ö)ck|Weste|Montur)",
    uniform_jacket = "Tscha(k|kk|ck)(o|e)(?!(\\-|\\s)(deckel))",
    uniform_general = "Un(i|j)form(?!(\\-|\\s)(kn(o|ö)pf))(?!\\sdienlich)",
    uniform_epaulets = "Ep(au|o)let",
    uniform_multiples = "Militär(\\-|\\s|)effekt"
  )
  dict$neg <- list(
    books = "Buchhandlung|Buchhändler", # books with uniform-word in titles
    carneval = "Fastnacht", # uniforms as fancy dress for carneval
    militia = "Mil(i|j)(z|tz)-Aufgebot|Aufgebot\\san\\sdie\\sMili(z|tz)" # muster for the militia
  )
  create_filter_output(dict)
}




#' Dictionary Underwear
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_underwear <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    underwear_general = "(?<!zu\\s|für\\s)(Lingerie|\\-Leiber|Leibchen|Unter(kleid|r(o|ö)ck|(ä|ae|a|e)rmel|hose))",
    underwear_corset = "(?<!zu\\s|für\\s)(K|C)orset|(?<!Arm\\-|Arm\\s|Arm)Br(u|ü)ste|Schn(u|ü)rbr(u|ü)st",
    underwear_socks_stocking = "(Frauen(zimmer|)|Weiber|M(a|ä)nn(e(n|r)|s)|Halb|Kinder|Laid|Moden)(\\-|)str(u|ü)mpf|Socke|Str(u|ü)mpf(?!(f)?(\\-|\\s)?(karren|w(ä|e)b|ausbreit|fach|Fabri|wolle|stuhl|pre(s|ß)|garn))"
  )
  dict$neg <- list(
    n_corsets = "(K|C)orset(?=(\\-|\\s)?fabri|machen|zeug|macher)", # compound nouns with corsets, not objects
    health = "Krebs|Mittel|Pomade", # health ads containing underwear words, mostly "Brüste" (breasts)
    immo = "Losament|Behausung|Gelegenheit|Garten", # excludes related immo ads
    book = "Heft|Kalender", # excludes related prints
    service = "Kundenh(ä|a)us", # excludes ads for related services
    board = "Kost" # excludes ads for board with additional services
  )
  create_filter_output(dict)
}


#' Dictionary Outerwear
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_outerwear <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    outerwear_male = "Frack|Talar",
    outerwear_general = "(?<!zu\\s|für\\s)M(a|ä)ntel|Co(at|tte)|Mantille|Kittel|Pellerine"
  )
  dict$neg <- list(
    french = "tricotte", # french for knitting, exclude if only looking at German ads
    fabric = "Mantel(zeug|stoff)", # fabric for coats
    mercery = "Mantelhaft", # clasps for coats
    music = "Instrumentalartikel" # musical instruments (contains "talar")
  )
  create_filter_output(dict)
}


#' Dictionary Garments for Special Occassions and Costumes
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_costume <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    costume_carneval = "(Milch|Bauern)(\\-|)jogg(el|i)|(Masken|Fastnach(t|ts)|Polichine(l|ll))(\\-|)kle(i|y)d|Milchbrentl|Ritter(\\-|)rüstung",
    costume_shroud = "T(o|öö|ö)(dt|t)enr(ö|o)ck",
    costume_traditional = "\\bTr(o|a)cht\\b|Bauerntr(o|a)cht",
    costume_baptism = "Tauf(zeug|kleid|hem(d|bd))"
  )
  dict$neg <- list(
    looking = "(B|b)etracht" # verbs and nouns meaning "looking at" conatining "tracht"
  )
  create_filter_output(dict)
}


#' Dictionary Shoes
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_shoes <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    shoes_slippers = "Panto(ff|f)el(?!n?\\-?(holz|bouteill|z(ä|a)pf))|Chauffe-pied",
    shoes_general = "(?<!Hand\\s|Hand\\-|Hand|Gulden|Rad|halben)\\s?Sch(u|ü)h(?!\\-?(wachs|schwärze|kraft|knech|(m|em)acher|schnalle|enmeister|rin(ck|k)|n(ä|a)gel))",
    shoes_boots = "(?<!Strick)St(ie|ü)(f|ff)el(?!\\-?(macher|wachs|schwärze|wichse|str(u|ü)mpf|h(o|ö)lz))",
    shoes_soles = "S(o|ö)hle"
  )
  dict$neg <- list(
    education = "Sch(u|ü)hl", # old spelling for "Schule"
    immo = "Liegenschaft", # filters out immo ads with measurements in "Schuh"
    wood = "(Boden|Daugen)holz|Faßdaugen|Dielen|Dach", # wood, measured in "Schuh"
    fountain = "Brunnstiefel|Ziehbrunn|Brunnen|Rohr", # fountains (one part is also called "Schuh")
    # work = "Schuhmacher|Schuster|Schuhster", # occupations concerned with making shoes
    # newly excluded, PROBLEM: sometimes filters out relevant ads - exclusion of work and immo ads probably better solution
    misc = "Radschuh", # other objects and nouns containing "schuh"
    measure_1 = "Schu(h|he)\\s(lang|breit|dick|hoch|weit|Länge|hohe|gro(s|ß))", # removes "Schuh" as measurement, version 1
    measure_2 = "Schu(he|h)\\s(lang|breit|dick|hoch|weit|Länge|hohe|gro(s|ß))", # removes "Schuh" as measurement, version 2
    measure_3 = "Schu(he|h)\\s\\d", # removes "Schuh" as measurement, version 3
    measure_4 = "\\d\\sSchu(he|h)", # removes "Schuh" as measurement, version 4
    measure_5 = "\\bM(a|aa)(ß|s|ss)\\b", # removes "Schuh" as measurement, version 5
    measure_6 = "Schu(he|h)\\-?länge|schühig|schu(h|he)(hoch|breit|lang|tief|dick|gro(s|ß))", # removes "Schuh" as measurement, version 6
    measure_7 = "Schu(he|h)\\s(Breite|Länge|Höhe)", # removes "Schuh" as measurement, version 7
    measure_8 = "Länge\\s\\d", # removes "Schuh" as measurement, version 8
    measure_9 = "Breite\\s\\d" # removes "Schuh" as measurement, version 9
  )
  # some "schuh" as measurement remain, e.g. "Länge 3 1/ 2 Schuh", maybe smth like:
  #(Breite|Länge|Höhe)\s(\w*?\/?\s){1,4}Schuh
  #Schuh\\s(\\w*?\\s){1,4}(Breite|Länge|Höhe)
  create_filter_output(dict)

}



#' Dictionary Handkerchiefs
#' @export
# dictionary created by Anna Reimann, ORCID 0000-0001-8225-7851
tagfilter_handkerchief <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
      handkerchief_1 = "(?<!für\\s)\\w*?((Sch|ch)(n|u)(u|uu)p(f|ff)|Sack|Nas|Nass)t(u|ü)ch\\w*?|(M|m)(o|ö)uc(h|i)oir\\w*?",
      handkerchief_2 = "(?<!zu\\s)\\w*?((Sch|ch)(n|u)(u|uu)p(f|ff)|Sack|Nas|Nass)t(u|ü)ch\\w*?|(M|m)(o|ö)uc(h|i)oir\\w*?"
  )
  dict$neg <- list(
    textile = "Strohsacktuch" # sort of textile
  )

  create_filter_output(dict)

}


#' Dictionary Gloves and Muffs
#' @export
tagfilter_hand <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    hand_muff = "\\w*?(S|s)chl(u|ü)p(f|ff)er|(?<!(An|Ge|Hi|gu|zu))(?<!((a|A)uf))(?<!(Chri))\\w*?((S|s)t(o|oo|ö)(ß|s|ss)\\b|(M|m)(a|ä)r(t|d)er)(?!\\-?((B|b)uch|(W|w)oche|(F|f)alle|n\\b|en\\b|(P|p)elz|(B|b)räm))",
    hand_gloves = "\\w*?(H|h)andsch(u|ü)(h|e)(?!(\\s|\\-)?((F|f)abri(c|k)|((M|m)acher)))"
  )
  dict$neg <- list(
    # profession = "Handschuhmacher", # maybe use profession, but also excludes some relevant ads
    books = "Pastor|Gemartert|Christos|Chronik", # book ads
    other = "Schlüpfer-Wein" # other objects
  )
  create_filter_output(dict)
}



#' Dictionary Scarves, Colars, and Neckties
#' @export
tagfilter_neck <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    neck_colar = "(?<!zu\\s|für\\s)(Palatine|Krägen|Cols)",
    neck_necktie = "(?<!zu\\s|für\\s)\\w*?(C|K|c|k)rav(e|a)(t|tt)e",
    neck_scarf = "(?<!zu\\s|für\\s)(Halstuch|F(oulard|ichu)|((Sch|Sh|Ch|Schw)(al|awl|aul|alw|avl)(s|))\\b)"
  )
  dict$neg <- list(
    leash = "Hundshalsband", # dog leash
    food = "Selteser|(Sch|Ch)alo(tt|t)e", # food and drink
    immo = "Cha(ll|l)et", # name for a small house
    noobject = "écols|Acols|Philantrophischal|Chaldaicum|Haushal|Schalten|(des|wes)halb|Spitchal|schalt|shaltung|aushalte|einscha(l|tt)t", # no object
    name = "Archal|Francols|Rochal|Pa(ch|sch)al|Mar(e|ê|é)(ch|sch)al|Michal|Nicol|Gottschalk|Schallbacher|Engelschall", # names
    person = "Ma(rsch|rech)al", # military rank
    ocr = "weshals|sichals|auchal|welchal|durchal|gleichal|Schaltem|sichall", # ocr mistakes (whitespace is missing)
    sound = "Schalles|Schalle|Schalsconservirung|Trompeten-Schall", # sound of something
    place = "Schaltenbrand|Mönchaltdorf|Schalbach", # placenames
    fabric = "Cha(l|ll)(is|y|on)", # special kind of fabric; PROBLEM: sometimes description for fabric of a scarve...
    measure = "Waagschale", # measurement containing "schal
    bowls = "Schale" # bowls
  )
  create_filter_output(dict)
}

#' Dictionary Headdresses and Wigs
#' @export
tagfilter_headdress <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    head_wig = "(Perruck\\w*)(?!(macher|machen))|H(a|aa)r(-|)(T|t)ours",
    head_cap = "Kappe\\w*(?!-?(M|m)acher)|Capotte|Bonnet|Mütze",
    head_general_1 = "(?<!Schön|Walds)(?<!Schaub)(H|h)(u|ü)t(?!-?((M|m)acher|(N|n)ähe(r|n)|(G|g)a(s|ß)|(F|f)abri(c|k)|(V|v)erlag))",
    head_general_2 = "(?<!Schön|Walds)(?<!Schaub)(H|h)(u|ü)th(?!-?((M|m)acher|(N|n)ähe(r|n)|(G|g)a(s|ß)|(F|f)abri(c|k)|(V|v)erlag))",
    head_general_3 = "(?<!Schön|Walds)(?<!Schaub)(H|h)(u|ü)tt(?!-?((M|m)acher|(N|n)ähe(r|n)|(G|g)a(s|ß)|(F|f)abri(c|k)|(V|v)erlag))",
    head_straw = "Strohh(u|ü)t\\w*(?!-?((M|m)acher|(N|n)ähe(r|n)|(G|g)a(s|ß)|(F|f)abri(c|k)|(V|v)erlag))",
    head_female = "(?<!zu\\s|für\\s)H(a|ä)ub(e|chen)|Kopfputz"
  )
  dict$neg <- list(
    name = "Kappeler", # family names
    water = "Se(i|y)dschützer", # name of a special water
    animal = "(D|T)aube", # description of animal with "kappe"
    mind = "zu//shüte", # minding something/someone (usally children)
    plant = "Zuckerhut", # name of a plant
    dump = "hütten", # verbs meaning dumping something
    beware = "verhüte", # verb meaning beware
    name = "Attrihut|Schaubhut|Hauber|Schaubelt", # family names
    religion = "Herrenhut", # religious group
    unclear = "Bonneterie|Kappellin", # exact meaning unsure, Bonneterie maybe place of making bonnets?
    place = "Kappeln|Hutting|Schützen|Eisenhut|Schutzen|Brodthauß", # placenames containing "hut/hüt"
    other = "Hüter|Verhütung", # other non-objects
    verb_1 = "\\sth(u|ü)t", # verbs (doing)
    verb_2 = "sch(u|ü)tt(e|i)|Schut|schützen", # to throw smth
    hut = "Hütte", # small house, hut
    object = "Fingerh(u|ü)t|Str(u|ü)mp(f|ff)woll|Str(u|ü)mpf-Woll|Zündhütchen|Schutt" # other objects including "hut" or "strumpf"
  )
  create_filter_output(dict)
}





#' Dictionary Certain Types of Fabric/ Textile Material
#' This Dictionary is ment to find textiles in ads where no other dictionary is useful,
#' but a lot of ads are of course already found through other dictionaries;
#' qualities of textiles will be explored in other dictionaries in more detail
#' @export
tagfilter_texmaterial <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    atlas = "gestreifter Atlas",
    texmaterial_unclear = "Nappes|Senpareille|Napolitain|Circassien|(Et|Str)amin|(K|C)amelot|Altan",
    fur = "Pelz|Marder|Zobel|Steinmarder|Fehlin",
    marcelline = "Marcelin",
    muslin = "Mousselin",
    lace = "Neiges|Spitz(lein|e)",
    canvas = "Leinwa(nd|t)|Ba(t|tt)ist|Canevas|Halblein",
    gingham = "G(ui|i)ngham",
    semi_silk = "Halbseide",
    silk_origin_1 = "Gros de", # does not seem to work- why? is the space between the problem?
    silk_origin_2 = "Grosde",
    silk = "Marceline|Levantine|Seide|Blonde|Flore(th|t)|Taff(ent|et)|Crepvan|Creptamin|Krapp\\b",
    cashemere = "Ca(si|che)mir",
    bast = "Bast|Sparterie",
    wicker = "Wiener Rohr",
    cotton = "Linon|Baumwoll",
    linnen_1 = "leinenes Gewebe", # does not seem to work- why? is the space between the problem?
    linnen_2 = "(Futter|Steif)keinen|(Futter|Steif)-Leinen|leinen(es|er)|Leinen(waaren|plunder|b(a|ä)nd|zeug)",
    printed = "Indienne|(C|K)attun|Persienne",
    tulle = "(T|Th)ull|Bobinet|Gaze",
    oilcloth = "Wachs(t(u|ü)ch|taff|lappen)",
    merino = "M(é|e)rino",
    flax = "Flachs",
    wool = "Wollenband|-Wolle|\\bWollenwaare|Thibet|Alépin|wollene"
  )
  dict$neg <- list(
    book = "Bücher|Buch|Gespräch", # excludes ads for prints
    death = "Ehefrau|beerdigt", # excludes death notices
    umbrella = "Paraplu(i|y|v)|Regenschirm|Pareplu(i|y|v)|Sonnenschirm|Ombrelle|Parasol|Parresol", # excludes umbrellas, since they are not only textile objects
    immo = "Losament|Wohnung|Ziehbrunn", # filters out immo ads
    stone = "Alabast(e|a)r|Bernstein", # kind of stones
    bastard = "Bastard", # child out of wedlock
    name = "Sebastian|Sebast", # first name
    animal = "Seidenkaninchen", # certain kind of
    work = "unterzubringen|begehrt|gewesener|Lehrgelt|pla(c|tz|z)ieren|Handschrift|Zeugni(s|ß)", # excludes work ads and description of people's jobs
    service = "waschen|flicken", # filters out ads for services related to textiles
    paper = "Seidenpapier|Seindeppr|Papier", # kind of paper
    profession = "Seiden(zwirnmeister|winder|handel)|Spitzenhändler", # profession
    workplace = "Seiden-Zwirnerey", # workplace
    tool = "Seidenwind-Maschine|Seidenwindmaschine|Seiden(r(a|ä)d|waage|windmaschine)|Floreth-Stuhl", # tool for winding silk thread
    place = "Geißspitz|Dre(y|i)spitz|Seidenhof", # placename
    instrument = "Mundspitze", # part of musical instrument
    medicine = "Balsam", # medicine with instructions to put it on a kind of cloth
    ohter = "Pelz(füsse|s(ä|a)cke)|Brennkessel" # other objects
  )

  create_filter_output(dict)

}


#' Dictionary Unspecified Cloth and Fabric
#' @export
tagfilter_cloth <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    cloth = "Reste|Zeug|T(u|ü)ch|Geflecht|Etoffe|Gewebe"
  )
  dict$neg <- list(
    witness = "Zeugen", # witnesses
    print = "Subscription|B(u|ü)ch|Papier", # print and paper ads
    carriage = "Chaise", # carriages with textile decorations
    animal = "Spitzpommer|Hund", # excludes ads with animal descriptions
    death = "begraben", # excludes death notices
    immo = "Juchart", # excludes immo ads
    election = "Wahl", # excludes election notices
    other = "Ratte|Schriften|Bibliothek|Feuerzeug|Rosenöl", # excludes other unrelated objects
    bag = "T(a|ä)sch|Seckel|Beutel|S(ä|a)ck|Ridicule", # excludes bags and purses
    umbrella = "Par(a|e)plu(i|y|v)|(Regen|Sonnen)schirm|Ombrelle|Pa(r|rr)(a|e)sol", # excludes umbrellas, since they are not only textile objects
    horse = " Pferdtzeug|R(ei|eu)tzeug", # objects for riding etc.
    ocr = "aufhaltetofferirt", # ocr mistake (whitespace missing)
    work = "Zeugn(i|ü|u)(s|ß)|tüchtig|Leumundszeug|Lehrling|Bedingnis|Lehre|Ladendiener|Reisender|ledig|Lehrgel(d|dt)", # filters out work advertisements
    occupation = "Zeugwar|Tuch(h(a|ä)ndler|scherer)|putzen|Flecken|nähen|stricken", # Occupation "Zeugwart" containing "zeug"
    verb = "überzeug|restera", # verbs containing "zeug" or "rest"
    household = "Bett(zeug|eingu(s|ß))|Tischzeug|Preß|Lampe|Tapete", # household textiles and objects
    noun = "bezeug|erzeug", # nouns containing "zeug"
    tool = "Wer(k|ks|ck)zeug|W(e|ä)bst(u|ü)hl|Presse", # tool
    place = "Zeughaus|Zeug-Hau(ß|s)", # name of a house in Basel
    paper = "Papierrest|(Rei(ß|ss)|Schreib)zeug", # scrap paper, writing and painting tools
    date = "Jahrestermin" # refers to a date
  )

  create_filter_output(dict)

}



#' Dictionary Yarn
#' @export
tagfilter_yarn <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    yarn_general = "Garn|Faden|Cordon|Litze",
    yarn_embroidery = "Stick(seide|wolle|baum)",
    yarn_knitting = "Strick(seide|wolle|baum)"
  )
  dict$neg <- list(
    water = "Sedlitz", # kind of water
    doves = "Garneten", # name for specific doves
    hungary = "Ungarn", # Hungary
    print = "Buch|Bücher", # excludes print ads
    hunt = "Jagd|Jagds(ä|a)ck|Fischgarn", # hunting with yarn
    tool = "(Faden|Waaren)z(ä|a)hler|Oehren", # tool for counting threads, needles for yarn
    adjectives = "garni(es|e|s)|garni\\b|garn(ie|i)(rt|tt)|fadene|garnire", # adjectives for decorated with
    decoration = "Garni(tur|rung)", # decoration on objects
    deco_description = "Faden durchwirkt", # decorated with yarn, not yarn itself
    military = "Garnison" # military garrison
  )

  create_filter_output(dict)

}
