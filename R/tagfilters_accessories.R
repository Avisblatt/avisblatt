#' Dictionary Clothing and Garments (General)
#' @export
tagfilter_clothing <- function(){
  dict <- list()
  dict$applicable <- list("saleoffer", "lendoffer", "lenddemand", "saledemand", "demand", "offer", "exchange", "othernews", "auctions", "ps", "lostandfoundheader")
  dict$pos <- list(
    general = "(?<!zu\\s|für\\s)(?<!Nacht|Unter|Toten)(?<!Schlaf|Todten)(Kle(i|y)d|Ärmel|(?<!Sch)(?<!ge)Weste)\\w*?(?!(\\-|)(k(a|ä)st|schrank|mange|kästlein))(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    suit_dress = "(?<!zu\\s|für\\s)(?<!Meyen|Nacht|Unter|Toten)(?<!Schlaf|Todten)(?<!Bolingb|Uniform)(?<!Offizier|Exerzier)(?<!Offiziers)R(o|ö)ck\\w*?(?!(\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))|stroh)",
    apron = "(?<!zu\\s|für\\s)Tscho(b|p|pp)en|Fürtuch\\w*?(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    trousers = "(?<!zu\\s|für\\s)(?<!Bac|Buc|Alk|Alc)(?<!Kalc|Bauc|Alik|Alic|Alck|Ober|Frey)(?<!Alick)(?<!Waltig)Hose\\w*?(?!nli(e|)(ß|s|ss)mer|n(träger|kn(o|ö)pf))(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    dress = "(?<!zu\\s|für\\s)Jun(t|dt)e|J(u|ü)ppe\\w*?(?!(\\szu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))",
    shirt = "(?<!zu\\s|für\\s)((Herren|)Hem(d|db)|Chemise|(C|K)amis(o|oh)l)\\w*?(?!träger|kn(o|ö)pf|gufe)(?!\\s(zu(m|)\\s|)(vertilgen|stricken|dienlich|w(a|ä)schen|machen|glätten|nähen|putzen|stricken|mangen))"
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
