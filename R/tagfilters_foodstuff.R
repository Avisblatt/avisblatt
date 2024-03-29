#' Tagfilter Food
#'
#' Tagfilters are regular expression based filters designed to tag ads in order
#' to classify ads based on their content. The avisblatt R package comes with
#' curated filters to search for food and drink related ads and finds ad content from spices,
#' chocolate and nuts to alcohol, milk and meat.
#'
#' Tagfilters can only predict if an ad is pertinent to a given topic.
#' Depending on the complexity of the topic and the development stage of a
#' tagfilter, there can be a considerable number of false positives and false
#' negatives.
#'
#' The tagfilters help site provides you with a list of available tagfilters
#' families.
#'
#' @name tagfilter_food
#' @seealso tagfilters
NULL


#' @rdname tagfilter_food
#' @export
tagfilter_spices <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    spices_general = 'Spe(z|c)ere(y|i|j)\\\\\\\\-?\\\\\\\\s?(waa?ren|arti(k|c)k?el)',
    spices_cinammon = 'Zin?mm?e?t(?!(farb|braun|wasser))',
    spices_saffron = '(?<!(zum|be(i|y|j))\\\\\\\\s)(?<!bem(i|y|j)\\\\\\\\s)(?<!zu\\\\\\\\s)Saff?ran',
    spices_cloves = 'N\\\\u00e4gelein(?!(farb|braun))',
    spices_nutmeg = 'Mus(c|k)atn(u|\\\\u00fc)ss',
    spices_salt = '\\\\\\\\bSalz(es|e|)\\\\\\\\b(?![\\\\u00fc\\\\u00e9\\\\u00f6\\\\u00e4\\\\u00e0\\\\u00e8])(?!\\\\\\\\-)',
    spices_mustard = 'Senft?\\\\\\\\-?(k(o|\\\\u00f6)rn|mehl)?|Mous?tard'
  )
  dict$neg <- list(
    name = '(I|Ei)senflam',
    river = 'Wiesenf',
    sedan = 'Senfte',
    meat = 'Ochsenfl',
    colour = 'senf(farb|braun)',
    legumes = 'H(u|\\\\u00fc)lsenfr'
  )
  dict$include <- c(
    '35710b39-9caf-50c7-ac30-080ceb2cf53d/t4' # Seuf (ocr mistake)
  )
  dict$exclude <- c(
    '8cc6411a-52a3-50c1-a846-162cd056e6df/t16', # 'n\u00e4geleinbrannes' (brown colour)
    'c2a797b3-a9d0-55cd-b2c6-36086d8ecf38/t4' # '\u00fcberlassenf' (ocr mistake)
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_meat <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange',
                          'othernews', 'ps', 'auctions')
  dict$pos <- list(
    meat_general = 'Fleisch',
    meat_sucklingpig = 'Spa(n|nn)fer(k|ck)',
    meat_snail = 'Schnecken',
    meat_tongue = 'Zunge',
    meat_game = 'Wildpret',
    meat_lard = 'Schmalz',
    meat_gelatine = 'Gelatine',
    meat_bacon = 'Speck',
    meat_sausage = '(W|w)(\\\\u00fc|u)rst|(C|s)ervelat',
    meat_ham = 'Schinken|Chargouterie',
    meat_salami = 'Salami',
    meat_frog = 'Fr(\\\\u00f6|o)schenschenkel',
    meat_rabbit = 'Hasen'
  )
  dict$neg <- list(
    manure = 'dung\\\\\\\\b|bau\\\\\\\\b(?![\\\\u00fc\\\\u00e9\\\\u00f6\\\\u00e4\\\\u00e0\\\\u00e8])',
    teeth = 'Zahnfleisch|Z\\\\u00e4hne',
    names = 'Ramspeck|W\\\\u00fcrsteisen|Menzunge|Specker|Schmalzried',
    tongue = 'Zungenwerck|Versatzung|sturtzung|setzung|Bestzung|sitzung|zungebund',
    description = 'Schnecken-Steg|Heuschneck|Schneckensteg|Meerschneck|Schneckentritt|Schneckenzug',
    job = 'Lehrgeld',
    speck = 'Prospeckt',
    house = 'hinter(n|m)\\\\\\\\sHase|zum\\\\\\\\sHase',
    kitchen = 'b\\\\u00fctte',
    colour = 'fleischfarb',
    instrument = 'Clarinett'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_poultry <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer',
                          'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    poultry_duck = '\\\\\\\\bEnten',
    poultry_capon = '(C|K)apaun',
    poultry_goose = 'G\\\\u00e4nse',
    poultry_chicken = 'Hahnen|H\\\\u00fchner(?!\\\\\\\\-?\\\\\\\\s?(h(u|\\\\u00fc)nd|e(i|y)|h(a|\\\\u00e4)us|stall|gitter|hof|kampf|aug|feder))|(P|B)oularde|Schepfen',
    poultry_pheasant = 'Fasan',
    poultry_other = 'Welschehahn'
  )
  dict$neg <- list(
    name = 'H\\\\u00fchners|H\\\\u00fchnerwadel',
    objects = 'Probhahn|Schlangenhahn|Weinhahn|Schl\\\\u00fcssel-Hahn|Fa\\\\u00dfhahn|messing',
    french = 'entendre|entedu',
    weapon = 'Entenflinte'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_alcohol <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    alcohohl_wine = '(?<!Sch)(?<!Wildsch)Wein(e|s)?\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|Vin\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    alcohohl_wineorigin = '(Mar(g|gg)r(\u00e4|a)(f|fl)er|Bordeau(x|r)|E(l|i)s(\u00e4|a)(ss|\u00df)er|Burgunder|Gebirgs|Champagner|Zehnt|Rhein)\\\\-?\\\\s?(W|V)',
    alcohohl_winetype = 'Muscat(eller|)(?!n(u|\u00fc)ss)|Madeira|Strohwein|Tocka(y|i|j)er',
    alcohohl_spicedwine = 'H(y|i)po(k|c)ras',
    alcohohl_liquer = 'Liq(ue|eu)r|Malaga|Anisette',
    alcohohl_spirits = '(Zwetschg(e|en)|(N|R)u\u00df|Bitter)wasser',
    alcohohl_rum = 'Rhum',
    alcohohl_brandy = 'Brann(t|ten|dt|dten)wein|C(o|a)gnac',
    alcohohl_kirsch = 'Kir(\u00df|sch)wasser',
    alcohohl_other = 'Pfeffers Wasser',
    alcohohl_beer = 'Bier\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    alcohohl_champagne = 'Champagner',
    alcohohl_absinth = 'Abs(i|y)nth',
    alcohohl_punch = 'Punsch'
  )
  dict$neg <- list(
    immo = '(?<!Partikular)Keller|Landgu(t|th)|Stallung|Jucharten|(Boden|Mie(t|th))zin(s|\u00df)|Ackerfeld|Losament|Stube|Matten', # excludes immo ads with space for wine or to plant smth
    other = 'S(e|\u00f6)nebier|Colombier', # names etc contaiing 'Bier'
    ocr = 'dabier|allbier', # ocr mistakes containing 'bier
    job = 'Barbier', # job containing 'Bier'
    objects = 'leere|Trotte', # empty objects for drinking and storing alcohol, might also exclude some relevant ads
    colour = 'cognackerten', # description of colours
    health = 'rhumatis', # rheuma
    french = 'expliquer', # french words containing 'liquer'
    food = 'Mee(x|r)traube' # other food
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_milk <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    milk_general = 'Milch(?!\\\\-?\\\\s?pot|kr(u|\u00fc)g|k(a|\u00e4)nn|bro(d|t)|h(a|\u00e4)f|karr|geschir|joggi|flasche|glas|kuh|brent|h(a|\u00e4)us|secht|speis|m\u00e4dchen|geben|farb|wei(s|\u00df))'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_coffee <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    coffee_general = '(C|K)a(ff|f)(ee|e|\u00e9)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    coffee_surrogate = '(C|K)a(ff|f)(ee|e|\u00e9)(\\\\-|\\\\s|)(Extra(c|k)t|Essenz|Su(rr|r)ogat|Pulver)|(C|Z)ichorien|Chicort',
    coffee_origin = '(M|N)o(kk|k)a|Java|Le(b|v)antisch|Domini(q|g)ue'
  )
  dict$neg <- list(
    colour = 'caffenen' # coffee-coloured
  )
   dict$include <- c(
    'temp-1836-303-007' # Kaffe-EEssenz
   )
   dict$exclude <- c(
     '136f68c9-4b97-577f-917a-d418f0e31f0e/t10',
     '1b23a202-2a1e-59e1-ba8e-3da00a221b16/t7',
     '307276ec-6052-5f12-9059-a1d1f37c7868/t7',
     '3dd5b632-48b7-5540-84c7-4b0d4980121a/t3',
     '5bb38c28-b8d2-5c88-b353-e388f802dcdb/t33',
     '6a97169e-3a27-5dc4-bb2e-ba2b5ab41052/t1',
     '838903b6-b631-5d18-8f6d-0455789194e0/t12',
     'a9f4df03-1bfa-5d8b-93f2-4f89ca5b3b88/t7',
     'c23d0a12-73e7-50ef-b239-675e2986c426/t5',
     '4703042b-ffa4-5e6a-a0b9-8f09ec18b794/a6',
     '6bd25d86-1e60-5434-a17a-7071d2e1c95b/a9',
     '97e31ae6-d3d5-54cb-9406-ea702c95f92f/t6',
     'e94484e2-2139-5699-a0f2-271a4cd1c9c2/t24'
   )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_fish <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    fish_general = '(?<!auf\\\\sdem\\\\s)Fisch(?!\\\\-?\\\\s?k(\u00e4|a)st)',
    fish_herring = 'H(\u00e4|\u00fc)(r|rr)ing',
    fish_cod = 'Stoc(k|ki)fisch',
    fish_kipper = '(B\u00fc|Pi)cking',
    fish_other = 'Mol(i|l)fisch',
    fish_anchovies = 'Anchoix|Sardellen',
    fish_tuna = 'Tonfisch',
    fish_trout = 'Forelle',
    fish_pike = 'Hecht',
    fish_crab = 'Krebse',
    fish_salmon = 'Wintersalmen',
    fish_eels = '\\\\bAale'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_fruit <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    fruit_general = 'Obst\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    fruit_apple = '\\\\b(Ae|\u00c4|A|Aey)(p|)(ff|f)el(?!\\\\-?\\\\s?schnitz|wein|most)',
    fruit_plums = '(Mirabolanen|Re(nn|n)e(t|tt)en|Zwetschgen|Pfl(a|\u00e4)um(e|le))(?!\\\\-?\\\\s?wasser|s(i|y)r(u|ou)p|saft)',
    fruit_grapes = '(?<!Mee(x|r))Trauben',
    fruit_mulberries = 'Maulbeeren(?!\\\\-?\\\\s?wasser|s(i|y)r(u|ou)p|saft)',
    fruit_rosehip = 'Buttenmost',
    fruit_cherries = 'Weichsel(\\\\-|)Kirsche',
    fruit_quinces = '(Q|O)uitten'
  )
  dict$neg <- list(
    place = 'Strauben' # house name containing 'trauben'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_driedfruit <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    driedfruit_pears = '((Ae|\u00c4|A|Aey)p(ff|f)el|Bi(rr|r)en)schnitz',
    driedfruit_raisins = 'Rosin(lein|en)|Sultanin|Weinbeere',
    driedfruit_candied = 'Citronat|Ora(u|n)geat'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_tropicalfruit <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    tropicalfruit_general = 'S(u|ue|\u00fc)dfr(\u00fc|u)cht',
    tropicalfruit_fig = 'Feigen\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    tropicalfruit_orange = '(Po(m|mm)er(a|\u00e4)nt?zl?e?i?n?|(O|D)rang(e|en))\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    tropicalfruit_lemon = '(Z|C)itronn?en(?!\\\\-?\\\\s?gelb|Sa(f|ff)t)',
    tropicalfruit_seagrapes = 'Me(er|x|xr|rx)traub',
    tropicalfruit_dates = 'Da(t|tt)(le|el)n',
    tropicalfruit_melon = 'Melone'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # objects and plants related to tropical fruits
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_nuts <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    nuts_chestnut = '(Kastanien|Ma(rr|r)onen)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    nuts_hazelnut = 'Haselnu(\u00df|s)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    nuts_almond = 'Mand(le|el)n',
    nuts_acorn = 'Eich(el|le|ei)n\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # chestnut coffee
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_honey <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    honey_general = 'Honig|Miel\\\\sde'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_vegetable <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    vegetable_asparagus = 'Sparg(le|el)n',
    vegetable_turnip = 'R(\u00fc|u)ben\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    vegetable_potatoes = '(Erd(Ae|\u00c4|A|Ae)(pff|pf|y)el|Grundbirne)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)'
  )
  dict$neg <- list(
    verb = 'gruben', # contains 'ruben'
    adjective = 'tr\u00fcben' # contains 'r\u00fcben'
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_mineralwater <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    mineralwater_general = '(Mineral|Sau(e|)r)(\\\\-)?(\\\\s)?wasser(?!(\\\\-)?(\\\\s)?Kr(u|\u00fc)g|wasserkr(u|\u00fc)g)',
    mineralwater_names = '(St.\\\\sMoritze?r?|Sel(tes|tz|z|ters)(s|)er|(S(u|au)l(tz|z)|Fas?chinger|Pyrmonter|Schwa(ll|l))bacher|Bussanger|(Sau(r|lz)|Sel(tz|z|t)er|Spaa)(\\\\s|\\\\-)?wa(ss|s|\u00df)er\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|Eau\\\\sde\\\\sVals)(?!(\\\\-)?(\\\\s)?(wasser=?(\\\\-)?(\\\\s)?kr(u|\u00fc)g))'
  )
  dict$neg <- list(
    objects = 'bibedibabediboo'
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_preserves <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    preserves_sauerkraut = 'Sau(er|r)kraut(?!\\\\-?\\\\s?stand)',
    preserves_pickles = 'Cornichons|Essiggurken|Capris',
    preserves_olives = 'Oliven\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_chocolate <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    chocolate_general = '(Ch|Sch)o(c|k|ck)ola(t|d)e?n?',
    chocolate_cocoa = '(C|K)(u|a)(c|cc|k)(a|n)o'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  dict$exclude <- list(
    '1d4803a9-d996-5a31-9897-12096cf4e791/t3' # Schokoladenstein (Marmor)
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_grain <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    grain_flour = '(?<!Senf)Mehl\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    grain_general = 'Korn\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|(?<!ohne\\\\s)Kernen',
    grain_oats = '\\\\bHabe(r|rn)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|Flocken',
    grain_barley ='Gerste',
    grain_grits = 'Gries\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    grain_rye = 'Roggen\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    grain_wheat = 'W(\u00e4|a)i(gz|z)en',
    grain_rice = 'Reismehl|Caroli(n|na)-Rei(s|\u00df)|CarolinerRei(s|\u00df)'
  )
  dict$neg <- list(
    ocr = 'zu\\\\shaber', # ocr-mistake (haben)
    straw = 'Stroh|Heugabel' # straw and utensils
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_cheese <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    cheese_general = 'K\u00e4i?(s|se|\u00df)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    cheese_emmenthaler = 'E(mm|m)(en|e)thaler',
    cheese_gruyere = '(C|G)ruy(\u00e9|\u00e8|e)r'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_mushrooms <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    mushrooms_morel = 'Morch(e|le)n',
    mushrooms_truffel = 'Tr\u00fcffel(?!\\\\-?\\\\s?hund)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_pastry <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    pastries_general = '(Zucker|Back)(waaren|werk|sachen)|E(i|y)er\\\\-?speis|K\u00fcchlein|Fastenw(\u00e4|a|e)j?i?h?e',
    pastries_zwieback = 'Zwieback',
    pastries_gingerbread = 'Lebk(\u00fc|u)ch',
    pastries_cookies = 'Leckerl(y|i|e)|Offleten|H\u00fcppen',
    pastries_pie = 'Pasteten(?!b(e|a|\u00e4)(c|k))'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
   dict$include <- c(
    '986007a6-719c-5032-92ec-4c5950d19195/a7' # Zuckerbek [...] Waaren
   )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_syrup <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    syrup_general = '(?<!Zucker)\\\\-?\\\\s?S(y|i)r(o|ou|u)p',
    syrup_juice = 'Saft|(L|C)imonad|(?<!Butten)\\\\-?Most\\\\b'
  )
  dict$neg <- list(
    ocr = 'Saftan' # ocr mistake (Caftan or Safran?)
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_sugar <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    sugar_general = '(?<!mit\\\\s)(?<!(ohne|avec|sans)\\\\s)Zu(ck|c|cc|k|kk)er\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    sugar_form = 'Zu(ck|c|cc|k|kk)er\\\\-?(M|m)ehl|M(e|\u00e9)lis(?!sen)|(Mehl|Farine?|Brosam(en|)|St\u00fccklein|Kropf)\\\\-?(Z|z)u(ck|c|cc|k|kk)er',
    sugar_treacle = 'Zu(ck|c|cc|k|kk)er\\\\-?(S|s)(i|y)r(o|u)p'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  dict$exclude <- list(
    '2a3512cd-d6af-5f16-a08a-71250eb07c2c/t1', # Merzen
    '925ab12b-377b-5573-b3ac-1483fc2bfb23/t2' # Bleyzucker-Fabrik
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
# this dictionary is case sensitive!
tagfilter_tea <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    tea_general = '(?<!f\u00fcr\\\\s)\\\\bTh?(\u00e9|e)e?\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8e])(?!\\\\-)(?!\\\\-\\\\,?\\\\sund)',
    tea_choice = '(?<!f\u00fcr\\\\s)Th?(\u00e9|e)e?s?\\\\s?\\\\-?\\\\s?((G|g)att|(S|s)or|(C|K|k|c)rum)',
    tea_herbal = '(Kr\u00e4uter|Glarner|Schwei(tz|z)er|Blumen)\\\\-?\\\\s?(T|t)h?(e|\u00e9)e?s?\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])|Th?(e|\u00e9)e?\\\\-?\\\\s?(E|e)ssen(z|\\\\;|tz)',
    tea_type = '(P(e|o)rle?|Ka(i|y)ser|Caravanen|(B|P)l(o|oo)(m|mm)b?)\\\\s?\\\\-?\\\\s?(T|t)h?(e|\u00e9)e?s?\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])|Th?(e|\u00e9)e?s?\\\\s?\\\\-?\\\\s?(S|s)taub|Pe(c|lt)ao|Songlo|Pelioe|Chausson|T\u00e9hy|(P|p)oudre\\\\s(a|\u00e0)\\\\s(C|c)anon|(C|c)arava(nn|n)s?er|(H|h)a(y|i)(s|ss|g|f)(a|o)(n|r|m)|(S|s)o(a|u)(tsch|ch|l)ong?|(?<!O)(B|b)o(\u00e9|e|ey|\u00e9y|uy|ui)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|(P|p|B|b)e(c|cc|k|kk)o\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|(C|c)hion|(t|T)ongo'
  )
  dict$neg <- list(
    other = 'T\u00e9ophraste|T\u00e9lescope|Thesium|Theo\\\\sodor|The(ss|s)al', # other things/words containing 'The'
    place = 'Thessalien', # placename containing 'The'
    books = 'englisch|Bible|Holy|Thesaurus|Heft|(W|w)hole|(S|s)pectator|(P|p)oems|(V|v)ic(a|ai)r|(p|P)oetic|(W|w)orld|(c|C)hildren|(c|C)ronicle|(c|C)ountry|(L|l)ife|(A|a)dventurer|(h|H)istory|(G|g)lory|(D|d)ictionnary|(U|u)ndersigned|Flick|Sprache|\\\\b(v|V)ol\\\\.|Buch|B\u00fccher|Kufperstich(e|s|)|Atlas|(L|l)etters|Reepsake' # books and English book titles
  )
  dict$include <- tea_include()
  dict$exclude <- list(
    'aabf666c-5b4b-58bf-88f2-839118520ffb/t0', # ocr-mistake: TThe key...
    '026eb095-ac68-5f03-a40e-7438ca03c62f/t7', # ocr-mistake: The riac
    'bddaae35-3265-5acf-a0f3-ee43733e8664/t4' # ocr-mistake: The oder
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_oil <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    oil_oil = '(\u00d6h|\u00d6|Oe|Oeh)(l|le)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    oil_vinegar = 'E(ss|\u00df|s)ig\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|Vinai?gi?re'
  )
  dict$neg <- list(
    cabbage = 'K\u00f6hl', # cabbage (Kohl)
    non_food = 'Seife|(Brenn|Rosen|Holz|Haar|Lampen)\\\\-?(oe|oeh|\u00f6h|\u00f6)l|Vitriol' # products containing oil not for consumption
  )
  create_filter_output(dict)
}

#' @rdname tagfilter_food
#' @export
tagfilter_pasta <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    pasta_noodles = 'N(u|\u00fc|ue)d(le|el)n|Simouill?e|Fidelis',
    pasta_general = 'Teigw(aa|a)ren',
    pasta_macaroni = 'Ma(c|cc)aroni',
    pasta_vermicelli = 'Vermicelli',
    pasta_lasagna = 'Lasagnette'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_legumes <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    legumes_general = '\\\\bMue(\u00df|s|ss)\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    legumes_peas = 'Erbsen',
    legumes_lentil = 'Linsen',
    legumes_beans = 'Bohnen'
  )
  dict$neg <- list(
    optical = 'Brillen|Telescop|Fernrohr|Aparate', # optical instruments with 'Linsen'
    cocoa = 'Cacao' # cocoa beans
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_tobaccoproducts <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    tobacco_general = '(T(a|o)ba(k|ck|ks|cks|c|cs)e?\\\\b)(?!\\\\-)(?!\\\\-?\\\\s?(t?i(e|\u00e9|\u00e8)r|beutel|b(\u00fc|u)(chs|x)|(e|\u00e9)tuis|dose|kasten|kammer|pfei(f|ff)e|fabri(k|c)))',
    tobacco_smokesniff = '(Rauch|Schnupf)\\\\-?t(a|o)ba(k|ck|ks|cks|c|cs)e?\\\\b(?!\\\\-)(?!\\\\-?\\\\s?(beutel|b(\u00fc|u)(chs|x)|(e|\u00e9)tuis|dose|kasten|kammer|pfei(f|ff)e|fabri(k|c)))',
    tobacco_selection = '(T(a|o)ba(k|ck|ks|cks|c|cs)e?|(C|Z)iga(rr|r)e(n|s))\\\\-?\\\\s?(Lager|Verlag|Sorten|Anzeige|Blattern)',
    tobacco_cigar = '(C|Z)iga(rr|r)e(n|s)|Cabanas',
    tobacco_form = '(C|K)a?naster',
    tobacco_origin = 'Maryland|Havanna|(Porto\\\\-?\\\\s?ri(c|cc)o)|Domingo|Marocco|Marino|Macouba'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  dict$include <- list(
    'fa5ecbf0-4982-5a3c-8e10-a4a990769be1/a6', # Portoricco-Nauchtabak
    '716d29bc-bf38-5f58-8026-e021ccfa07e1/t16', # Canasler-Tabak
    '63aad19e-c613-5423-bb39-e6914ec251c7/t12', # Schnupf- und Rauch- Tabuc
    'temp-1835-083-008', # Schnupftabak-Liebhabern
    'fc950439-01b3-51eb-b83f-57de4fd354bd/t6' # Rauch-Tabat-Lager
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_butter <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    butter = '(?<!\\\\-)\\\\bButter\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)|\\\\bAn(k|c|ck|kk|cc)en\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    butter_specific = '(K\u00fcbel|Mai|Alpen)\\\\-?(Butter|An(k|c|ck|kk|cc)en)',
    butter_commission = '(Butter|\\\\bAn(k|c|ck|kk|cc)en)\\\\-?(C|K)o(m|mm)i(s|ss|\u00df)ion'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo' # placeholder
  )
  dict$exclude <- c(
    'd3565df2-d207-5196-bb23-cca112f60eed/t25' # 'Ancken-Kessel' with explanation, how much 'Ancken' it holds
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_eggs <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    eggs_general = '\\\\bE(y|i)ern?\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  dict$exclude <- list(
    '011c379c-7fe6-5eb4-8e77-e6c117140749/t37', # unclear why included
    'faca9c84-08db-5542-835d-a53859eefaa2/t12', # unclear why included
    'b63c75cc-f7b8-5825-aef2-d2e96f7be1fa/t27' # unclear why included
  )
  create_filter_output(dict)
}


#' @rdname tagfilter_food
#' @export
tagfilter_broth <- function(){
  dict <- list()
  dict$applicable <- list('saleoffer', 'saledemand', 'demand', 'offer', 'exchange', 'othernews', 'ps', 'auctions')
  dict$pos <- list(
    broth_general = '(?<!ge)(?<!Farb)Br\u00fche|(?<!zu\\\\s)(?<!als\\\\s)(?<!f\u00fcr\\\\s)(?<!Kinder)Su(pp|p)en?\\\\b(?![\u00fc\u00e9\u00f6\u00e4\u00e0\u00e8])(?!\\\\-)',
    broth_frog = 'Fr(\u00f6|o)sche?n?\\\\-?(kur|saft)',
    broth_snails = 'Schnecken\\\\-?(kur|saft)'
  )
  dict$neg <- list(
    placeholder = 'bibedibabediboo'
  )
  dict$exlude <- list(
    '19c03d13-8279-507c-9d4d-02f5ce05025c/t1' # 'br\u00fchen' for cleaning clothing
  )
  create_filter_output(dict)
}
