#' Dictionary Clothing and Garments (General)
#' @export
tagfilter_clothing <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Kle(i|y)d|R(o|ö)ck|Ärmel|Weste",
    apron = "Tscho(b|p|pp)en|Fürtuch",
    trousers = "Hose",
    dress = "Jun(t|dt)e|J(u|ü)ppe",
    shirt = "Hem(d|db)|Chemise|(C|K)amis(o|oh)l"
  )
  dict$neg <- list(
    activity_1 = "Kleider machen", # activity with clothes, v1
    activity_2 = "Kleider w(a|ä)schen", # activity with clothes, v2
    activity_3 = "Kleider glätten", # activity with clothes, v3
    activity_4 = "Kleider nähen", # activity with clothes, v4
    activity_5 = "Kleider putzen", # activity with clothes, v5
    activity_6 = "Kleider zu machen", # activity with clothes, v6
    activity_7 = "Kleider zu w(a|ä)schen", # activity with clothes, v7
    activity_8 = "Kleider zu glätten", # activity with clothes, v8
    activity_9 = "Kleider zu nähen", # activity with clothes, v9
    activity_10 = "Kleider zu putzen", # activity with clothes, v10
    activity_11 = "zu stricken", # activity with clothes, v11
    activity_12 = "einen Platz", # activity with clothes, v12
    activity_13 = "zum Putzen", # activity with clothes, v13
    activity_14 = "zu vertilgen", # activity with clothes, v14
    work = "Hosenließmer", # profession knitting trousers, maybe has to be included if these post relevant objects
    animal = "Federnhund", # filters out animal related clothing
    book = "Fleckenbüchlein", # book with instructions for cleaning
    death = "gewesen", # filters out death notices
    cleaning = "Waschwasser|Flecken-Kugelen|Kleiderputz", # detergent for cleaning clothes
    lime = "Kalchosen", # limestone
    fragrance = "Rosenöl", # fragrance for laundry
    immo = "Bauchosen|Gebäude|A(lck|lick)hosen", # immo ads
    place = "(Ober|Frey|Waltig)hosen", # placename
    position = "einen Platz", # filters out labour ads for servants
    name = "B(u|a)chosen", # family names
    straw = "Roc(ken|k)stroh", # special kind of straw
    carneval = "Milchjoggi|Fastnach(t|ts)kleid|(Polichinel|Masken)-Kleid|(Polichinel|Masken)kleid", # costumes for carneval, see dictionary "costume"
    other = "Brockel", # describes appearance of different objects (small chunks)
    work = "Lehre|Unterricht|Haußbedient|Zeugni(ss|ß|s)", # cleaning of clothes
    name = "Bolingbrocke|Meyenrock", # family names
    military = "(Offiziers|Uniform|Exerzier)-Rock", # see dictionary "uniform"
    shroud = "To(dt|t)enr(o|ö)ck", # see dictionary "costume"
    underwear = "Leibchen|Unter(ärmel|kleid|r(o|ö)ck)", # see dictionary "underwear"
    sleapwear = "Schlafr(o|ö)ck|Schlaf-R(o|ö)ck|Nacht(ärmel|rock|röck)", # see dictionary "sleapware"
    adjective = "gekleidet|schröcklich", # description of other object (mostly dolls)
    sister = "Schwester", # contains "weste"
    dry = "tr(o|ö)ck(e|ne)", # adjectives or verbs meaning "dry" but containing "rock"
    furniture_1 = "Kleider(k(a|ä)st|schrank|mange|kästlein)|Kleider-(k(a|ä)st|schrank|mange|kästlein)|K(a|ä)sten|Blunderschafft", # furniture for keeping clothing, see dictionary cabinet, v1
    furniture_2 = "kleider dienlich", # filters out cabinets for clothes
    verb = "gewesten", # filters out verbs containing clothing-words
    accessoire = "knopf|knöpf|träger|Hemdengufe" # non textile accessoires (Hemdknopf, Hosenträger etc.)

  )
  create_filter_output(dict)
}


#' Dictionary Sleepwear
#' @export
tagfilter_sleepwear <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Schlafr(o|ö)ck|Schlaf-R(o|ö)ck|Nacht(ärmel|rock|röck)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )
  create_filter_output(dict)
}



#' Dictionary Military Clothing/ Uniforms
#' @export
tagfilter_uniform <- function(){
  dict <- list()
  dict$pos <- list(
    general = "(Offizie(rs|r)|Uniform)-R(o|ö)ck|Exerzier-Weste|(Uniform|Offizie(rs|r))r(o|ö)ck|Tschako",
    uniform = "Uniform",
    epaulets = "Epaulett",
    general = "Militär-Effekt|Militäreffekt"
  )
  dict$neg <- list(
    militia = "Miliz-Aufgebot" # muster for the militia
  )
  create_filter_output(dict)
}


#' Dictionary Underwear
#' @export
tagfilter_underwear <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Lingerie|Leib(chen|er)|Unter(kleid|rock|röck|ärmel)|Montour",
    corset = "(K|C)orset|Br(u|ü)ste",
    socks = "Socke|Str(u|ü)mpf"
  )
  dict$neg <- list(
    name = "Kleiber", # family names
    trolley = "Str(u|ü)mp(f|ff)-Karren|Str(u|ü)mp(f|ff)karren", # kind of trolley
    immo = "Losament|Behausung|Gelegenheit|Garten", # excludes related immo ads
    book = "Heft|Kalender", # excludes related prints
    service = "Kundenh(ä|a)us", # excludes ads for related services
    board = "Kost", # excludes ads for board with additional services
    profession = "Str(u|ü)mp(f|ff)w(ä|e)b|Str(u|ü)mp(ff|f)-W(ä|e)b|Str(u|ü)mp(f|ff)ausbreit|Str(u|ü)mpffach|Str(u|ü)mp(f|ff)-Fabri|
    Armbruster|Strumpfausbreiter", # profession of making stockings or other containing relevant words, also excludes tools for profession (e.g. Strumpfweber-Stuhl)
    raw = "Str(u|ü)mp(f|ff)wolle|Str(u|ü)mp(f|ff)-Wolle" # yarn for making socks
  )
  create_filter_output(dict)
}


#' Dictionary Outerwear
#' @export
tagfilter_outerwear <- function(){
  dict <- list()
  dict$pos <- list(
    male = "Frack|Talar",
    general = "M(a|ä)ntel|Co(at|tte)|Mantille|Kittelein|Pellerine"
  )
  dict$neg <- list(
    french = "tricotte", # french for knitting, exclude if only looking at German ads
    fabric = "Mantelzeug", # fabric for coats
    mercery = "Mantelhaft", # clasps for coats
    music = "Instrumentalartikel" # musical instruments (contains "talar")
  )
  create_filter_output(dict)
}


#' Dictionary Garments for Special Occassions and Costumes
#' @export
tagfilter_costume <- function(){
  dict <- list()
  dict$pos <- list(
    carneval = "(Milch|Bauern)jogg(el|i)|Fastnach(t|ts)kle(i|y)d|Polichinel-Kle(i|y)d|Polichinelkle(i|y)d|Maskenkle(i|y)d|
    (Bauern|Milch)-Jogg(el|i)|Milchbrentl|Ritter-Rüstung|Ritterrüstung|Tschakko",
    shroud = "To(dt|t)enr(ö|o)ck",
    costume = "Bauerntr(o|a)cht|-Tracht",
    baptism = "Taufzeug"
  )
  dict$neg <- list(
    looking = "b)etracht" # verbs and nouns meaning "looking at" conatining "tracht"
  )
  create_filter_output(dict)
}


#' Dictionary Shoes
#' @export
# some "schuh" as measurement remain, e.g. "Länge 3 1/ 2 Schuh"
# excluding these through regex of "Länge, Breite etc." in proximity of 3 words to "Schuh"?
tagfilter_shoes <- function(){
  dict <- list()
  dict$pos <- list(
    slippers = "Pantoffel|Chauffe-pied",
    shoes = "Sch(u|ü)h",
    boots = "St(ie|ü)fel",
    soles = "S(o|ö)hle"
  )
  dict$neg <- list(
    education = "Sch(u|ü)hl", # old spelling for "Schule"
    immo = "Liegenschaft", # filters out immo ads with measurements in "Schuh"
    wood = "(Boden|Daugen)holz|Faßdaugen|Dielen", # wood, measured in "Schuh"
    polish = "Wichse|Schuh(wachs|schwärze)", # shoe polish
    fountain = "Brunnstiefel|Ziehbrunn", # fountains (one part is also called "Schuh")
    name = "Guldenschuh", # family name
    accessoire = "Handsch(u|ü)h|Hand-sch(u|ü)h", # textile accessoires (gloves)
    # work = "Schuhmacher|Schuster|Schuhster", # occupations concerned with making shoes
    # newly excluded, PROBLEM: sometimes filters out relevant ads - exclusion of work and immo ads probably better solution
    misc = "Radschuh|Schuh(kraft|knech)", # other objects and nouns containing "schuh"
    measure_1a = "Schuh\\s(lang|breit|dick|hoch|weit|Länge|hohe)", # removes "Schuh" as measurement, version 1a
    measure_1b = "Schuhe\\s(lang|breit|dick|hoch|weit|Länge|hohe)", # removes "Schuh" as measurement, version 1b
    measure_2a = "Schuh\\s\\d", # removes "Schuh" as measurement, version 2a
    measure_2b = "\\d\\sSchuh", # removes "Schuh" as measurement, version 2b
    measure_3 = "Nürnberger Maß", # removes "Schuh" as measurement, version 3
    measure_4 = "franz. Maß", # removes "Schuh" as measurement, version 4
    measure_5 = "Schuhlänge|SchuhLänge|Schuh-Länge|schühig", # removes "Schuh" as measurement, version 5
    measure_6 = "Schuh Breite", # removes "Schuh" as measurement, version 6
    measure_7 = "Schuh Länge", # removes "Schuh" as measurement, version 7
    measure_8 = "Schuh Höhe", # removes "Schuh" as measurement, version 8
    measure_9 = "Länge\\s\\d", # removes "Schuh" as measurement, version 9
    measure_10 = "Breite\\s\\d" # removes "Schuh" as measurement, version 10
  )

  create_filter_output(dict)

}


#' Dictionary Handkerchiefs
#' @export
tagfilter_handkerchief <- function(){
  dict <- list()
  dict$pos <- list(
    handkerchief = "(Schnupf|Sack)t(u|ü)ch|Mouchoir"

  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # placeholder, no negatives necessary so far
  )

  create_filter_output(dict)

}



#' Dictionary Gloves and Muffs
#' @export
tagfilter_hand <- function(){
  dict <- list()
  dict$pos <- list(
    muff = "Schl(u|ü)p(f|ff)er",
    gloves = "Handsch(u|ü)h"
  )
  dict$neg <- list(
    # profession = "Handschuhmacher", # maybe use profession, but also excludes some relevant ads
    other = "Schlüpfer-Wein" # other objects
  )
  create_filter_output(dict)
}



#' Dictionary Scarves, Colars, and Neckties
#' @export
tagfilter_neck <- function(){
  dict <- list()
  dict$pos <- list(
    colar = "Palatine|Kr(a|ä)gen|Cols",
    necktie = "Crav(e|a)(t|tt)e",
    scarf = "Halstuch|F(oulard|ichu)|(Sch|Sh|Ch)(al|awl|aul)"
  )
  dict$neg <- list(
    leash = "Hundshalsband", # dog leash
    food = "(Sch|Ch)alo(tt|t)e", # shallots
    immo = "Cha(ll|l)et", # name for a small house
    noobject = "Chaldaicum|Haushal|Schalten|(des|wes)halb|Spitchal|schalt|shaltung|aushalte|einscha(l|tt)t", # no object
    name = "Nicol|Gottschalk", # names
    person = "Ma(rsch|rech)al", # military rank
    ocr = "sichals|auchal|welchal|durchal|gleichal|Schaltem|sichall", # ocr mistakes (whitespace is missing)
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
  dict$pos <- list(
    wig = "Pe(rr|r)(u|ü)ck|H(a|aa)r-Tours",
    cap = "Kappe|Capotte|Bonnet|Mütze",
    general = "H(u|ü)t",
    straw = "Strohh(u|ü)t",
    female = "Haube|Häubchen|Kopfputz"
  )
  dict$neg <- list(
    name = "Kappeler", # family names
    water = "Se(i|y)dschützer", # name of a special water
    animal = "(D|T)aube", # description of animal with "kappe"
    mind = "zu//shüte", # minding something/someone (usally children)
    plant = "Zuckerhut", # name of a plant
    dump = "hütten", # verbs meaning dumping something
    beware = "verhüte", # verb meaning beware
    name = "Schaubhut|Hauber|Schaubelt", # family names
    religion = "Herrenhut", # religious group
    unclear = "Bonneterie|Kappellin", # exact meaning unsure, Bonneterie maybe place of making bonnets?
    # profession = "Kappenmacher|Hutmacher|Strohhutnähen", # professions, PROBLEM: often in ad including actual hats but not always... - leave out?
    place = "Hutting|Schönhut|Waldshut|Hutgasse|Huthgasse|Hutgaß|Schützen|Eisenhut|Schutzen|Brodthauß", # placenames containing "hut/hüt"
    immo = "Losament|Behausung|Stube|Stüblein|Wohnung|Schütti", # filters out immo ads
    work = "Gesell|Profe(ß|ss|s)ion|Ka(m|mm)erdiener", # filters out work ads
    other = "Hüter|Verhütung", # other non-objects
    verb_1 = "\\sth(u|ü)t", # verbs (doing)
    verb_2 = "sch(u|ü)tt(e|i)n|schütt(e|i)n|Schut|schützen", # to throw smth
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
  dict$pos <- list(
    atlas = "gestreifter Atlas",
    unclear = "Nappes|Senpareille|Napolitain|Circassien|(Et|Str)amin|Kamelott|Altan",
    fur = "Pelz|Marder|Zobel|Steinmarder|Fehlin",
    marcelline = "Marcelin",
    muslin = "Mousselin",
    lace = "Neiges|Spitz(lein|e)",
    canvas = "Leinwa(nd|t)|Ba(t|tt)ist|Canevas|Halblein",
    gingham = "G(ui|i)ngham",
    semi_silk = "Halbseide",
    silk_origin_1 = "Gros de", # does not seem to work- why? is the space between the problem?
    silk_origin_2 = "Grosde",
    silk = "Marceline|Levantine|Seide|Blonde|Flore(th|t)|Taff(ent|et)|Crepvan|Creptamin",
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
  dict$pos <- list(
    general = "Reste|Zeug|T(u|ü)ch|Geflecht|Etoffe|Gewebe"
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
  dict$pos <- list(
    general = "Garn|Faden|Cordon|Litze",
    embroidery = "Stick(seide|wolle|baum)",
    knitting = "Strick(seide|wolle|baum)"
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
