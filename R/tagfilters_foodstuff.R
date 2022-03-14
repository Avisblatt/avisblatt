#' Dictionary Spices
#' @export
tagfilter_spices <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "Spe(z|c)ere(y|i|j)\\-?\\s?(waaren|artikel)",
    cinammon = "Zi(m|mm|me|mme|nm)(t|et)(?!(farb|braun|wasser))",
    safran = "(?<!(zum|be(i|y|j))\\s)(?<!bem(i|y|j)\\s)(?<!zu\\s)Safran",
    cloves = "Nägelein(?!(farb|braun))",
    nutmeg = "Muscatn(u|ü)ss",
    salt = "\\bSalz(es|e|)\\b",
    mustard = "Senf(?!(farb|braun))|Mou(st|t)ard"
  )
  dict$neg <- list(
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
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "Fleisch",
    sucklingpig = "Spa(n|nn)fer(k|ck)",
    snail = "Schnecken",
    tongue = "Zunge",
    game = "Wildpret",
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
    manure = "dung\\b|bau\\b", # manure from certain animals
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
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    duck = "\\bEnten",
    capon = "(C|K)apaun",
    goose = "Gänse",
    chicken = "Hahnen|Hühner(?!\\-?\\s?(h(u|ü)nd|e(i|y)|h(a|ä)us|stall|gitter|hof|kampf|aug|feder))|(P|B)oularde|Schepfen",
    pheasant = "Fasan",
    other = "Welschehahn"
  )
  dict$neg <- list(
    name = "Hühners|Hühnerwadel", # family name
    objects = "Probhahn|Schlangenhahn|Weinhahn|Schlüssel-Hahn|Faßhahn|messing", # objects containing "hahn" and messingener "hahnen"
    french = "entendre|entedu", # french words containing "ente"
    weapon = "Entenflinte" # weapons for shooting poultry
  )
  create_filter_output(dict)
}

#' Dictionary Alcohol
#' @export
tagfilter_alcohol <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    wine = "\\b\\-?Wein(e|s)?\\b|Vin\\b",
    origin = "(Mar(g|gg)r(ä|a)(f|fl)er|Bordeau(x|r)|E(l|i)s(ä|a)(ss|ß)er|Burgunder|Gebirgs|Champagner|Zehnt|Rhein)\\-?\\s?(W|V)",
    type = "Muscat(eller|)(?!n(u|ü)ss)|Madeira|Strohwein|Tockayer",
    spicedwine = "H(y|i)po(k|c)ras",
    liquer = "Liq(ue|eu)r|Malaga|Anisette",
    spirits = "(Zwetschg(e|en)|(N|R)uß|Bitter)wasser",
    rum = "Rhum",
    brandy = "Brann(t|ten|dt|dten)wein|C(o|a)gnac",
    kirsch = "Kir(ß|sch)wasser",
    other = "Pfeffers Wasser",
    beer = "Bier(?!\\-)\\b",
    champagne = "Champagner",
    absinth = "Abs(i|y)nth",
    punch = "Punsch"
  )
  dict$neg <- list(
    immo = "Keller|Landgu(t|th)|Stallung|Jucharten|(Boden|Mie(t|th))zin(s|ß)|Ackerfeld|Losament|Stube|Matten", # excludes immo ads with space for wine or to plant smth
    other = "S(e|ö)nebier|Colombier", # names etc contaiing "Bier"
    ocr = "dabier|allbier", # ocr mistakes containing "bier
    job = "Barbier", # job containing "Bier"
    objects = "leere|Trotte", # empty objects for drinking and storing alcohol, might also exclude some relevant ads
    colour = "cognackerten", # description of colours
    health = "rhumatis", # rheuma
    french = "expliquer", # french words containing "liquer"
    food = "Mee(x|r)traube" # other food
  )
  create_filter_output(dict)
}

#' Dictionary Milk
#' @export
tagfilter_milk <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    milk = "Milch(?!\\-?\\s?pot|kr(u|ü)g|k(a|ä)nn|bro(d|t)|h(a|ä)f|karr|geschir|joggi|flasche|glas|kuh|brent|h(a|ä)us|secht|speis|mädchen|geben|farb|wei(s|ß))"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Coffee
#' @export
tagfilter_coffee <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    coffee = "(C|K)a(ff|f)(ee|e|é)(?!\\-)\\b",
    surrogate = "(C|K)a(ff|f)(ee|e|é)(\\-|\\s|)(Extra(c|k)t|Essenz|Su(rr|r)ogat|Pulver)",
    origin = "(M|N)o(kk|k)a|Java|Le(b|v)antisch|Domini(q|g)ue"
  )
  dict$neg <- list(
    colour = "caffenen" # coffee-coloured
  )
  dict$include <- list(
    "temp-1836-303-007" # Kaffe-EEssenz
  )
  create_filter_output(dict)
}

#' Dictionary Fish and Seafood
#' @export
tagfilter_fish <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "(?<!auf\\sdem\\s)Fisch(?!\\-?\\s?k(ä|a)st)",
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
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Fruits
#' @export
tagfilter_fruit <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "Obst\\b",
    apple = "\\b(Ae|Ä|A|Aey)(p|)(ff|f)el(?!\\-?\\s?schnitz|wein|most)",
    plums = "(Mirabolanen|Re(nn|n)e(t|tt)en|Zwetschgen|Pfl(a|ä)um(e|le))(?!\\-?\\s?wasser|s(i|y)r(u|ou)p|saft)",
    grapes = "(?<!Mee(x|r))Trauben",
    mulberries = "Maulbeeren(?!\\-?\\s?wasser|s(i|y)r(u|ou)p|saft)",
    rosehip = "Buttenmost",
    cherries = "Weichsel(\\-|)Kirsche",
    quinces = "Quitten"
  )
  dict$neg <- list(
    place = "Strauben" # house name containing "trauben"
  )
  create_filter_output(dict)
}

#' Dictionary Dried and Candied Fruit
#' @export
tagfilter_driedfruit <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
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
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    fig = "Feigen\\b",
    orange = "(Po(m|mm)er(a|ä)n(z|ze|zen)|(O|D)rang(e|en))(?!\\-)\\b",
    lemon = "(Z|C)itronen(?!\\-?\\s?gelb|Sa(f|ff)t)",
    seagrapes = "Me(er|x|xr|rx)traub",
    dates = "Da(t|tt)(le|el)n",
    melon = "Melone"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # objects and plants related to tropical fruits
  )
  create_filter_output(dict)
}

#' Dictionary Nuts
#' @export
tagfilter_nuts <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    chestnut = "(Kastanien|Ma(rr|r)onen)(?!\\-)\\b",
    hazelnut = "Haselnu(ß|s)(?!\\-)\\b",
    almond = "Mand(le|el)n",
    oak = "Eich(el|le|ei)n(?!\\-)\\b"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo" # chestnut coffee
  )
  create_filter_output(dict)
}

#' Dictionary Honey
#' @export
tagfilter_honey <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    honey = "Honig|Miel\\sde"
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
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    esparagus = "Sparg(le|el)n",
    turnip = "R(ü|u)ben\\b",
    potatoes = "(Erd(Ae|Ä|A|Ae)(pff|pf|y)el|Grundbirne)(?!\\-)"
  )
  dict$neg <- list(
    verb = "gruben", # contains "ruben"
    adjective = "trüben" # contains "rüben"
  )
  create_filter_output(dict)
}


#' Dictionary Mineral Water
#' @export
tagfilter_mineralwater <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "(Mineral|Sau(e|)r)(\\-)?(\\s)?wasser(?!(\\-)?(\\s)?Kr(u|ü)g|wasserkr(u|ü)g)",
    names = "(Sel(tes|tz|z|ters)(s|)er|(S(u|au)l(tz|z)|Fas?chinger|Pyrmonter|Schwa(ll|l))bacher|Bussanger|(Sau(r|lz)|Sel(tz|z|t)er|Spaa)(\\s|\\-)?wasser\\b|Eau\\sde\\sVals)(?!(\\-)?(\\s)?(wasser=?(\\-)?(\\s)?kr(u|ü)g))"
  )
  dict$neg <- list(
    objects = "bibedibabediboo" 
  )
  create_filter_output(dict)
}


#' Dictionary Preserves
#' @export
tagfilter_preserves <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    sauerkraut = "Sau(er|r)kraut(?!\\-?\\s?stand)",
    pickles = "Cornichons|Essiggurken|Capris",
    olives = "Oliven(?!\\-?)\\b"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}


#' Dictionary Chocolate
#' @export
tagfilter_chocolate <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    chocolate = "(Ch|Sch)o(c|k|ck)ola(t|d)",
    cocoa = "(C|K)(u|a)(c|cc|k)(a|n)o"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  dict$exclude <- list(
    "1d4803a9-d996-5a31-9897-12096cf4e791/t3" # Schokoladenstein (Marmor)
  )
  create_filter_output(dict)
}


#' Dictionary Grain and Flour
#' @export
tagfilter_grain <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    flour = "(?<!Senf)Mehl\\b",
    grain = "Korn\\b|(?<!ohne\\s)Kernen",
    oats = "\\bHabe(r|rn)(?!\\-?)\\b|Flocken",
    barley ="Gerste",
    grits = "Gries\\b",
    rye = "Roggen(?!\\-?)\\b",
    wheat = "W(ä|a)i(gz|z)en",
    rice = "Reismehl|Caroli(n|na)-Rei(s|ß)|CarolinerRei(s|ß)"
  )
  dict$neg <- list(
    ocr = "zu\\shaber", # ocr-mistake (haben)
    straw = "Stroh|Heugabel" # straw and utensils
  )
  create_filter_output(dict)
}


#' Dictionary Cheese
#' @export
tagfilter_cheese <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "K(ä|äi)(s|se|ß)\\b",
    emmenthaler = "E(mm|m)(en|e)thaler",
    gruyere = "(C|G)ruy(é|è|e)r"
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
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    morel = "Morch(e|le)n",
    truffel = "Trüffel(?!\\-?\\s?hund)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Pastries
#' @export
tagfilter_pastry <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "(Zucker|Back)(waaren|werk|sachen)",
    zwieback = "Zwieback",
    gingerbread = "Lebk(ü|u)ch",
    cookies = "Leckerl(y|i|e)|Offleten|Hüppen",
    pie = "Pasteten(?!b(e|a|ä)(c|k))"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  dict$include <- list(
    "986007a6-719c-5032-92ec-4c5950d19195/a7" # Zuckerbek [...] Waaren
  )
  create_filter_output(dict)
}

#' Dictionary Syrup and Juice
#' @export
tagfilter_syrup <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    syrup = "(?<!Zucker)\\-?\\s?S(y|i)r(o|ou|u)p",
    juice = "Saft|(L|C)imonad"
  )
  dict$neg <- list(
    ocr = "Saftan" # ocr mistake (Caftan or Safran?)
  )
  create_filter_output(dict)
}

#' Dictionary Sugar
#' @export
tagfilter_sugar <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "ps", "auctions")
  dict$pos <- list(
    general = "(?<!mit\\s)(?<!(ohne|avec|sans)\\s)Zucker\\b",
    form = "Zuckermehl|M(e|é)lis(?!sen)|(Mehl|Farine?|Brosam(en|)|Stücklein|Kropf)zucker",
    treacle = "Zuckers(i|y)r(o|u)p|Zucker-S(i|y)r(o|u)p"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  dict$exclude <- list(
    "925ab12b-377b-5573-b3ac-1483fc2bfb23/t2" # Bleyzucker-Fabrik
  )
  create_filter_output(dict)
}

#' Dictionary Tea ########### STAND HIER (macht Probleme)
#' @export
tagfilter_tea <- function(){
  dict <- list()
  dict$pos <- list(
    general = "\\bT(ee|he|hé)\\b(?!\\-)",
    multiple = "\\bT(ee|he|hé)(\\-)?(\\s?(Gattung|Sorte|(C|K)rummeten))",
    herbal = "(Kräuter|Glarner|Schwei(tz|z)er)(\\-)?(\\s)?t(h|)(ee|e|é)|T(h|)(ee|e|é)(\\-|\\s|\\-\\s)?Essen(z|\\;)",
    origin = "Haysan|Soatschon|Thé\\simpérial|Bo(é|e|ey)\\b|Pec(c|k)o|Chion|T(h|)(ee|e|é)\\sCongo"
  )
  dict$neg <- list(
    french = "augmentee", # french containing "tee"
    books = "Bibliothek", # contains "the
    ocr = "Leihbibliothee|Feld-Apolthee" # ocr mistakes
  )
  dict$include <- list(
    "56eeaaac-af5e-54d7-9c00-185229da55d5/t1", # Thee-Es. sentz (ocr-mistake, herbal)
    "57a30888-b244-5500-83eb-b7e1d46c39c9/t6" # Thee- (ocr-mistake)
    )
  create_filter_output(dict)
}

#' Dictionary Oil and Vinegar
#' @export
tagfilter_oil <- function(){
  dict <- list()
  dict$pos <- list(
    oil = "(Öh|Ö|Oe|Oeh)(l|le)(?!\\-)\\b",
    vinegar = "E(ss|ß)ig(?!\\-)\\b|Vinai?gi?re"
  )
  dict$neg <- list(
    cabbage = "Köhl", # cabbage (Kohl)
    non_food = "Seife|(Brenn|Rosen|Holz|Haar|Lampen)\\-?(oe|oeh|öh|ö)l|Vitriol" # products containing oil not for consumption
  )
  create_filter_output(dict)
}

#' Dictionary Pasta
#' @export
tagfilter_pasta <- function(){
  dict <- list()
  dict$pos <- list(
    noodles = "Nud(le|le)n",
    general = "Teigw(aa|a)ren",
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
    optical = "Brillen|Telescop|Fernrohr|Aparate", # optical instruments with "Linsen"
    cocoa = "Cacao" # cocoa beans
  )
  create_filter_output(dict)
}


#' Dictionary Tobacco Products
#' @export
tagfilter_tobaccoproducts <- function(){
  dict <- list()
  dict$pos <- list(
    tobacco = "(T(a|o)ba(k|ck|ks|cks|c|cs)e?\\b(?!\\-))(?!(\\-?\\s?)(beutel|b(ü|u)(chs|x)|(e|é)tuis|dose|kasten|kammer|pfei(f|ff)e|fabri(k|c)))",
    smokesniff = "Rauch|Schnupf)\\-?t(a|o)ba(k|ck|ks|cks|c|cs)e?\\b(?!\\-))(?!(\\-?\\s?)(beutel|b(ü|u)(chs|x)|(e|é)tuis|dose|kasten|kammer|pfei(f|ff)e|fabri(k|c)))",
    selection = "(T(a|o)ba(k|ck|ks|cks|c|cs)(e|)|(C|Z)iga(rr|r)e(n|s))(\\-|\\s|)(Lager|Verlag|Sorten|Anzeige|Blattern)",
    cigar = "(C|Z)iga(rr|r)e(n|s)|Cabanas",
    form = "(C|K)a?naster",
    origin = "Maryland|Havanna|(Porto(\\-|\\s|)ri(c|cc)o)|Domingo|Marocco|Marino|Macouba"
  )
  dict$neg <- list(
    storage = "Taba(r|k|ck|ks|cks|c|cs)t?i(e|é|è)r", # tobacco storage (seperate category)
    pipe = "Pfeife|Pfeifen(kopf|raumer|rohr)" # tobacco pipes (seperate category)
  )
  dict$include <- list(
    "fa5ecbf0-4982-5a3c-8e10-a4a990769be1/a6", # Portoricco-Nauchtabak
    "716d29bc-bf38-5f58-8026-e021ccfa07e1/t16", # Canasler-Tabak
    "63aad19e-c613-5423-bb39-e6914ec251c7/t12", # Schnupf- und Rauch- Tabuc
    "fc950439-01b3-51eb-b83f-57de4fd354bd/t6" # Rauch-Tabat-Lager
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
    frog = "Fr(ö|o)sch(en|)kur",
    snails = "Schneckensaft"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}
