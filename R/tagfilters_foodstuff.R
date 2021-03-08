#' Dictionary Spices
#' @export
tagfilter_spices <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Spezerey(waaren|artikel)|Spezerey-(waaren|artikel)",
    cinammon = "Zi(m|mm|me|mme|nm)(t|et)",
    safran = "Safran",
    cloves = "Nägelein",
    nutmeg = "Muscatn(u|ü)ss",
    mustard = "Senf|Mou(st|t)ard"
  )
  dict$neg <- list(
    colour = "senfarb", # contains "senf" (e.g. eisenfarbe)
    name = "Isenflam", # name contains "senf"
    river = "Wiesenf", # name of river, contains "senf,
    sedan = "Senfte", # contains "senf"
    meat = "Ochsenfl", # contains "senf"
    legumes = "Hulsenfr", # contains "senf"
    other = "Eisenf" # contains "senf"
  )
  create_filter_output(dict)
}

#' Dictionary Meat
#' @export
tagfilter_meat <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Fleisch",
    sucklingpig = "Spa(n|nn)fer(k|ck)",
    snail = "Schnecken",
    tongue = "Zunge",
    lard = "Schmalz",
    gelatine = "Gelatine",
    bacon = "Speck",
    sausage = "Würst|(C|s)ervelat",
    ham = "Schinken|Chargouterie",
    salami = "Salami",
    frog = "Fröschenschenkel",
    rabbit = "Hasen"
  )
  dict$neg <- list(
    teeth = "Zahnfleisch|Zähne", # includes "fleisch"
    names = "Ramspeck|Würsteisen|Menzunge|Specker|Schmalzried", # family names
    tongue = "Zungenwerck|Versatzung|sturtzung|setzung|Bestzung|sitzung|zungebund", # tongue not as food
    description = "Schnecken-Steg|Heuschneck|Schneckensteg|Meerschneck|Schneckentritt|Schneckenzug", # "schnecke" as description, not as food
    job = "Lehrgeld", # catches ocr mistake: "zungen/jungen"
    speck = "Prospeckt", # contains "speck" but no food
    house = "hinter(n|m)\\sHase|zum\\sHase", # house names containing "hase"
    kitchen = "bütte", # kitchen utensils related to meat (contains "fleisch")
    colour = "fleischfarb", # colour description
    instrument = "Clarinett" # "zunge" for instruments
  )
  create_filter_output(dict)
}

#' Dictionary Poultry
#' @export
tagfilter_poultry <- function(){
  dict <- list()
  dict$pos <- list(
    duck = "\\bEnten",
    capon = "(C|K)apaun",
    goose = "Gänse",
    chicken = "Hahnen|Hühner|(P|B)oularde|Schepfen",
    pheasant = "Fasan",
    other = "Welschehahn"
  )
  dict$neg <- list(
    name = "Hühners|Hühnerwadel", # family name
    dog = "Hühnerh(u|ü)nd|Hühner-h(u|ü)nd", # dog
    eggs = "Hühner-E(y|i)|Hühnere(i|y)", # eggs
    supplies = "Hühner(h(a|ä)us|stall|gitter|hof|kampf)|Hühner-(h(a|ä)us|stall|gitter|hof|kampf)", # supplies for keeping poultry
    objects = "Probhahn|Schlangenhahn|Weinhahn|Schlüssel-Hahn|Faßhahn|messing", # objects containing "hahn" and messingener "hahnen"
    health = "Hühneraug", # corns
    down = "Hühner-Feder", # down
    french = "entendre|entedu", # french words containing "ente"
    weapon = "Entenflinte" # weapons for shooting poultry
  )
  create_filter_output(dict)
}

#' Dictionary Alcohol
#' @export
tagfilter_alcohol <- function(){
  dict <- list()
  dict$pos <- list(
    wine = "Wein\\b|Vin\\b|\\bWeine\\b|\\bWeins\\b",
    origin = "(Mar(g|gg)r(ä|a)(f|fl)er|Bordeau(x|r)|E(l|i)s(ä|a)(ss|ß)er|Burgunder|Gebirgs|Champagner|Zehnt|Rhein)(\\-|)weine",
    type = "Muscateller|Madeira|Muscat|Strohwein|Tockayer",
    spicedwine = "H(y|i)po(k|c)ras",
    liquer = "Liquer|Malaga|Anisette",
    spirits = "(Zwetschg(e|en)|(N|R)uß|Bitter)wasser",
    rum = "Rhum",
    brandy = "Brann(t|ten|dt|dten)wein|Cognac",
    kirsch = "Kir(ß|sch)wasser",
    other = "Pfeffers Wasser",
    beer = "Bier\\b",
    champagne = "Champagner",
    absinth = "Abs(i|y)nth",
    punch = "Punsch"
  )
  dict$neg <- list(
    other = "S(e|ö)nebier|Colombier", # names etc contaiing "Bier"
    ocr = "dabier|allbier", # ocr mistakes containing "bier
    job = "Barbier", # job containing "Bier"
    objects = "Bier-Kr(u|ü)g|leere", # empty objects for drinking and storing alcohol, might also exclude some relevant ads
    colour = "cognackerten", # description of colours
    health = "rhumatis", # rheuma
    french = "expliquer", # french words containing "liquer"
    food = "Muscatn(u|ü)ss|Mee(x|r)traube"
  )
  create_filter_output(dict)
}

#' Dictionary Milk
#' @export
tagfilter_milk <- function(){
  dict <- list()
  dict$pos <- list(
    milk = "Milch"
  )
  dict$neg <- list(
    objects = "Milc(h|\\-)(pot|kr(u|ü)g|k(a|ä)nn|bro(d|t)|h(a|ä)f|karr|geschir|joggi|flasche|glas|kuh|brent|h(a|ä)us|secht|speis)",
    person = "Milchmädchen",
    other = "milchgeben", # no objects and no foodstuff
    colour = "milch(farb|wei(s|ß))"
  )
  create_filter_output(dict)
}

#' Dictionary Coffee
#' @export
tagfilter_coffee <- function(){
  dict <- list()
  dict$pos <- list(
    coffee = "(C|K)a(ff|f)(ee|e|é)\\b"
  )
  dict$neg <- list(
    colour = "caffenen", # coffee-coloured
    objects = "(C|K)a(ff|f)(ee|e|é)\\-" # excluding objects for the consumption of coffee
  )
  create_filter_output(dict)
}

#' Dictionary Fish and Seafood
#' @export
tagfilter_fish <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Fisch\\b",
    herring = "H(ä|ü)(r|rr)ing",
    cod = "Stoc(k|ki)fisch",
    kipper = "(Bü|Pi)cking",
    other = "Mol(i|l)fisch",
    anchovies = "Anchoix|Sardellen",
    tuna = "Tonfisch",
    trout = "Forelle",
    pike = "Hecht",
    crab = "Krebse",
    salmon = "Wintersalmen",
    eels = "\\bAale"
  )
  dict$neg <- list(
    objects = "Fisch\\-|Fisch\\skäst", # excludes objects related to fishs and fishing
    place = "auf\\sdem\\sFisch" # excludes "Fischmarkt" if written as "Fisch markt"
  )
  create_filter_output(dict)
}

#' Dictionary Fruits
#' @export
tagfilter_fruit <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Obst\\b",
    apple = "\\b(Ae|Ä|A|Aey)(p|)(ff|f)el",
    plums = "Mirabolanen|Re(nn|n)e(t|tt)en|Zwetschgen|Pfl(a|ä)um(e|le)",
    grapes = "Trauben",
    mulberries = "Maulbeeren",
    rosehip = "Buttenmost",
    cherries = "Weichsel(\\-|)Kirsche",
    quinces = "Quitten"
  )
  dict$neg <- list(
    alcohol = "Zwetschgen(\\-|)wasser", # spirit from plums
    place = "Strauben", # house name containing "trauben"
    exotic = "Mee(x|r)traube", # exotic fruits (seperate filter)
    dried = "(Ae|Ä|A|Aey)p(ff|f)el(schnitz|wein|most)", # dried fruit (seperate filter)
    syroup = "Renneten-Syroup", # syroup (seperate filter)
    objects = "(Obst|Maulbeeren)\\-|Maulbeerensaft" # exclude objects related to fruits and plants
  )
  create_filter_output(dict)
}

#' Dictionary Dried and Candied Fruit
#' @export
tagfilter_driedfruit <- function(){
  dict <- list()
  dict$pos <- list(
    pears = "((Ae|Ä|A|Aey)p(ff|f)el|Bi(rr|r)en)schnitz",
    raisins = "Rosin(lein|en)|Sultanin|Weinbeere",
    candied = "Citronat|Ora(u|n)geat"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Tropical Fruits
#' @export
tagfilter_tropicalfruit <- function(){
  dict <- list()
  dict$pos <- list(
    fig = "Feigen\\b",
    orange = "Po(m|mm)er(a|ä)n(z|ze|zen)\\b|(O|D)rang(e|en)\\b",
    lemon = "(Z|C)itronen",
    seagrapes = "Me(er|x|xr|rx)traub",
    dates = "Da(t|tt)(le|el)n",
    melon = "Melone"
  )
  dict$neg <- list(
    juice = "Zitronensa(f|ff)t", # juice (seperate dictionary)
    objects = "(Po(m|mm)er(a|ä)n(z|ze|zen)|(C|Z)itronen|Melonen)-" # objects and plants related to tropical fruits
  )
  create_filter_output(dict)
}

#' Dictionary Nuts
#' @export
tagfilter_nuts <- function(){
  dict <- list()
  dict$pos <- list(
    chestnut = "Kastanien|Ma(rr|r)onen",
    hazelnut = "Haselnu(ß|s)\\b",
    almond = "Mand(le|el)n",
    oak = "Eich(el|le|ei)n"
  )
  dict$neg <- list(
    pastry = "Haselnu(ß|s|ss)-", # pastry with nuts
    coffee = "(Kastanien|Eich(el|le|ei)n)-(K|C)affe" # chestnut coffee
  )
  create_filter_output(dict)
}

#' Dictionary Honey
#' @export
tagfilter_honey <- function(){
  dict <- list()
  dict$pos <- list(
    honey = "Honig"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Vegetables
#' @export
tagfilter_vegetable <- function(){
  dict <- list()
  dict$pos <- list(
    esparagus = "Sparg(le|el)n",
    turnip = "R(ü|u)ben\\b",
    potatoes = "Erd(Ae|Ä|A|Ae)(pff|pf|y)el|Grundbirne"
  )
  dict$neg <- list(
    object = "Erd(Ae|Ä|A|Ae)(pff|pf|y)el-", # objects related to potatoes
    verb = "gruben", # contains "ruben"
    adjective = "trüben" # contains "rüben"
  )
  create_filter_output(dict)
}


#' Dictionary Mineral Water
#' @export
tagfilter_mineralwater <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Mineralwasser",
    names = "Sel(tes|tz|z|ters)er|(S(u|au)l(tz|z)|Schwall)bacher|Bussanger|(Sau(r|lz)|Sel(tz|z|t)er|Spaa)wasser\\b"
  )
  dict$neg <- list(
    objects = "-Kr(u|ü)g|wasserkr(u|ü)g" # jugs and bottles for mineral water
  )
  create_filter_output(dict)
}


#' Dictionary Preserves
#' @export
tagfilter_preserves <- function(){
  dict <- list()
  dict$pos <- list(
    sauerkraut = "Sau(er|r)kraut",
    pickles = "Cornichons|Essiggurken|Capris",
    olives = "Oliven\\b"
  )
  dict$neg <- list(
    objects = "Oliven\\-" # objects for consuming olives or olive oil
  )
  create_filter_output(dict)
}


#' Dictionary Chocolate
#' @export
tagfilter_chocolate <- function(){
  dict <- list()
  dict$pos <- list(
    chocolate = "(Ch|Scho)o(c|k|ck)ola(t|d)",
    cocoa = "(C|K)(u|a)(c|cc|k)ao"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}


#' Dictionary Grain and Flour
#' @export
tagfilter_grain <- function(){
  dict <- list()
  dict$pos <- list(
    flour = "Mehl\\b",
    grain = "Korn\\b|Kernen",
    oats = "\\bHabe(r|rn)\\b|Flocken",
    barley ="Gerste",
    grits = "Gries\\b",
    rye = "Roggen",
    wheat = "W(ä|a)i(gz|z)en",
    rice = "Reismehl|Caroli(n|na)-Rei(s|ß)|CarolinerRei(s|ß)"
  )
  dict$neg <- list(
    mustard = "Senfmehl", # mustard (in seperate dictionary)
    fruit = "ohne\\Kernen", # description of fruit
    objects = "(Habe(r|rn)|Roggen)-|Roggenschäub", # objects for storing grain
    ocr = "zu\\shaber", # ocr-mistake (haben)
    straw = "Stroh|Heugabel" # straw and utensils
  )
  create_filter_output(dict)
}


#' Dictionary Cheese
#' @export
tagfilter_cheese <- function(){
  dict <- list()
  dict$pos <- list(
    general = "K(ä|äi)(s|se|ß)\\b",
    emmenthaler = "Emm(en|e)thaler",
    gruyere = "(C|G)ruy(é|è|e)re"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Mushrooms
#' @export
tagfilter_mushrooms <- function(){
  dict <- list()
  dict$pos <- list(
    morel = "Morch(e|le)n",
    truffel = "Trüffel"
  )
  dict$neg <- list(
    dog = "Trüffelhund" # dog
  )
  create_filter_output(dict)
}

#' Dictionary Pastries
#' @export
tagfilter_pastry <- function(){
  dict <- list()
  dict$pos <- list(
    general = "(Zucker|Back)(waaren|werk|sachen)",
    zwieback = "Zwieback",
    gingerbread = "Lebk(ü|u)ch",
    cookies = "Leckerl(y|i)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Syrup and Juice
#' @export
tagfilter_syrup <- function(){
  dict <- list()
  dict$pos <- list(
    syrup = "S(y|i)r(o|ou|u)p",
    juice = "Saft|(L|C)imonad"
  )
  dict$neg <- list(
    ocr = "Saftan" # ocr mistake (Caftan or Safran?)
  )
  create_filter_output(dict)
}

#' Dictionary Sugar - also contains a lot of chocolate ads with sugar as ingredient
#' @export
tagfilter_sugar <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Zucker\\b|Zuckermehl|M(e|é)lis",
    treacle = "Zuckers(i|y)r(o|u)p|Zucker-S(i|y)r(o|u)p"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Tea
#' @export
tagfilter_tea <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Thee\\b|Tee\\b"
  )
  dict$neg <- list(
    french = "augmentee", # french containing "tee"
    ocr = "Leihbibliothee", # ocr mistake
    objects = "Thee-"
  )
  create_filter_output(dict)
}

#' Dictionary Oil and Vinegar
#' @export
tagfilter_oil <- function(){
  dict <- list()
  dict$pos <- list(
    oil = "(Öh|Ö|Oe|Oeh)(l|le)\\b",
    vinegar = "Essig\\b|Vinagire"
  )
  dict$neg <- list(
    objects = "Essig-",
    cabbage = "Köhl", # cabbage (Kohl)
    non_food = "Seife|(Brenn|Rosen|Holz|Haar|Lampen)(-|)(oe|oeh|öh|ö)l|Vitriol|(Öh|Ö|Oe|Oeh)(l|le)-" # products containing oil not for consumption
  )
  create_filter_output(dict)
}

#' Dictionary Pasta
#' @export
tagfilter_pasta <- function(){
  dict <- list()
  dict$pos <- list(
    noodles = "Nud(le|le)n",
    general = "Teigwaaren",
    macaroni = "Ma(c|cc)aroni",
    vermicelli = "Vermicelli",
    lasagna = "Lasagnette"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}


#' Dictionary Legumes
#' @export
tagfilter_legumes <- function(){
  dict <- list()
  dict$pos <- list(
    general = "\\bMue(ß|s)\\b",
    peas = "Erbsen",
    lentil = "Linsen",
    beans = "Bohnen"
  )
  dict$neg <- list(
    cocoa = "Cacao" # cocoa beans
  )
  create_filter_output(dict)
}


#' Dictionary Tobacco Products
#' @export
tagfilter_tobaccoproducts <- function(){
  dict <- list()
  dict$pos <- list(
    tobacco = "Taba(k|ck|c)(e|)\\b|(Rauch|Schnupf)taba(k|ck|c)\\b",
    cigar = "(C|Z)iga(rr|r)en|Cabanas",
    form = "Canaster",
    origin = "Maryland|Havanna|Portorico"
  )
  dict$neg <- list(
    immo = "Taba(k|ck|c)(e|)\\sK(a|ä)mmer", # immo for tobacco rooms
    objects = "\\bTaba(k|ck|c)(e|)-", # other objects for consuming tobacco or people and places
    storage = "-(B(ü|u)chs|Bux|Etuis|Dose|Beutel)|
    ((C|Z)iga(rr|r)en)|(Taba(k|ck|ks|cks|c|cs))(beutel|b(ü|u)chs|bux|etuis|dose)|
    Taba(k|ck|ks|cks|c|cs)kasten|Taba(r|k|ck|ks|cks|c|cs)ier|Taba(r|k|ck|ks|cks|c|cs|t)ti(e|è)r", # tobacco storage (seperate category)
    pipe = "Pfeife|Taba(k|ck|ks|cks|c|cs)pfeife|Pfeifen(kopf|raumer|rohr)" # tobacco pipes (seperate category)
  )
  create_filter_output(dict)
}


#' Dictionary Butter
#' @export
tagfilter_butter <- function(){
  dict <- list()
  dict$pos <- list(
    butter = "Butter|\\bAnken"
  )
  dict$neg <- list(
    pear = "Butter-Birnen", # pears
    object = "Ankenhäfen", # objects for storing butter
    cocoa = "Cacao-Butter" # cocoa butter
  )
  create_filter_output(dict)
}

#' Dictionary Eggs
#' @export
tagfilter_eggs <- function(){
  dict <- list()
  dict$pos <- list(
    eggs = "\\bE(y|i)er"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Broth
#' @export
tagfilter_broth <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Brühe",
    snails = "Schneckensaft"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}
