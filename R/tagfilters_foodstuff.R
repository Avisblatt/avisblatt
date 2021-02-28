#' Dictionary Spices
#' @export
tagfilter_spices <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Spezerey",
    vanilla = "Vanille",
    cinammon = "Zi(m|mm|me|mme)t",
    mustard = "Senf|Moustard",
    anis = "(Ä|A)nis",
    pepper = "Pfeffer"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Meat
#' @export
tagfilter_meat <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Fleisch",
    sucklingpig = "Spanfer(k|ck)el",
    snail = "Schnecken",
    tongue = "Zunge",
    lard = "Schmalz",
    bacon = "Speck",
    sausage = "Würst|(C|s)ervelat",
    ham = "Schinken",
    salami = "Salami"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Poultry
#' @export
tagfilter_poultry <- function(){
  dict <- list()
  dict$pos <- list(
    duck = "Enten",
    capon = "Capaun"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Alcohol
#' @export
tagfilter_alcohol <- function(){
  dict <- list()
  dict$pos <- list(
    wine = "Wein|Vin",
    spicedwine = "H(y|i)pokras",
    liquer = "Liquer|Malaga",
    rum = "Rhum",
    brandy = "Brann(t|ten|dt|dten)wein|Cognac",
    kirsch = "Kirsch|Kirßwasser",
    beer = "Bier",
    champagne = "Champagner"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
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
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Coffee
#' @export
tagfilter_coffee <- function(){
  dict <- list()
  dict$pos <- list(
    coffee = "(C|K)a(ff|f)(ee|e|é)"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Fish
#' @export
tagfilter_fish <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Fisch",
    herring = "H(ä|ü)(r|rr)ing",
    cod = "Stoc(k|ki)fisch",
    kipper = "(Bü|Pi)cking",
    other = "Molifisch",
    anchovies = "Anchoix|Sardellen",
    tuna = "Tonfisch"
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
  dict$pos <- list(
    general = "Obst",
    apple = "(Ae|Ä|A|Aey)p(ff|f)el",
    plums = "Mirabolanen|Renneten",
    grapes = "Trauben",
    mulberries = "Maulbeeren",
    rosehip = "Buttenmost"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Dried and Candied Fruit
#' @export
tagfilter_driedfruit <- function(){
  dict <- list()
  dict$pos <- list(
    pears = "Birrenschnitz",
    raisins = "Rosinlein|Sultanin",
    candied = "Citronat|Oraugeat"
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
    fig = "Feigen",
    orange = "Po(m|mm)e(r(a|ä)nz|abe)|Orange",
    lemon = "(Z|C)itronen",
    seagrapes = "Me(er|x)trauben",
    dates = "Da(t|tt)(le|el)n",
    quinces = "Quitten"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Nuts
#' @export
tagfilter_nuts <- function(){
  dict <- list()
  dict$pos <- list(
    chestnut = "Kastanien|Ma(rr|r)onen",
    hazelnut = "Haselnu(ß|s)",
    almond = "Mandlen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
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
    turnip = "R(ü|u)ben",
    potatoes = "Erdäpfel|Erd\\Aeyfel"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Mineral Water
#' @export
tagfilter_mineralwater <- function(){
  dict <- list()
  dict$pos <- list(
    names = "Sel(tes|tz|z|ters)er|(S(u|au)l(tz|z)|Schwall)bacher|Bussanger|(Sau(r|lz)|Sel(tz|z)er|Spaa)wasser"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
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
    olives = "Oliven"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Salep
#' @export
tagfilter_salep <- function(){
  dict <- list()
  dict$pos <- list(
    salep = "Salep"
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
  dict$pos <- list(
    chocolate = "Cho(c|k)ola(t|d)",
    cocoa = "Cucao"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Grain
#' @export
tagfilter_grain <- function(){
  dict <- list()
  dict$pos <- list(
    grain = "Korn",
    oats = "Haber",
    barley ="Gerste",
    rye = "Roggen",
    wheat = "Wäigzen",
    rice = "Reis"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Flour
#' @export
tagfilter_flour <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Mehl",
    semolina = "Gries"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Cheese
#' @export
tagfilter_cheese <- function(){
  dict <- list()
  dict$pos <- list(
    general = "K(ä|äi)s",
    emmenthaler = "Emmenthaler"
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
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Pastries
#' @export
tagfilter_pastry <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Zuckerwaaren",
    gingerbread = "Lebk(ü|u)ch",
    cookies = "Leckerly"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Syrup
#' @export
tagfilter_syrup <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Syr(o|ou)p"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Sugar
#' @export
tagfilter_sugar <- function(){
  dict <- list()
  dict$pos <- list(
    general = "Zucker"
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
    general = "Thee"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Oil and Vinegar
#' @export
tagfilter_oil <- function(){
  dict <- list()
  dict$pos <- list(
    oil = "Öhl|Oel",
    vinegar = "Essig"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
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
    macaroni = "Macaroni"
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
    general = "Mueß",
    peas = "Erbsen",
    lentil = "Linsen"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Tobacco Products
#' @export
tagfilter_tobaccoproducts <- function(){
  dict <- list()
  dict$pos <- list(
    cigar = "Cigarren"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Butter
#' @export
tagfilter_butter <- function(){
  dict <- list()
  dict$pos <- list(
    butter = "Butter|Anken"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary Eggs
#' @export
tagfilter_eggs <- function(){
  dict <- list()
  dict$pos <- list(
    eggs = "Eyer"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}

#' Dictionary XXX
#' @export
tagfilter_XXX <- function(){
  dict <- list()
  dict$pos <- list(
    placeholder = "bibedibabediboo"
  )
  dict$neg <- list(
    placeholder = "bibedibabediboo"
  )
  create_filter_output(dict)
}
