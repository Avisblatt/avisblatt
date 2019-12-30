### kwic for vocabularies for textiles

# load libararies and sources
library(readtext)
library(quanteda)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
avis_1834 <- readtext("data/avis_1834.csv")
avis_1834$doc_id <- avis_1834$text
avis_1834$text <- NULL

# ocr corrections
avis_1834$ad_content <- correct_ocr(avis_1834$ad_content)



corpus_1834 <- corpus(avis_1834,
                      text_field = "ad_content",
                      docid_field = "doc_id")


## Houeshold Textiles: see under household goods vocabularies

## Clothing
clothing_1834 <- kwic(corpus_1834,
                pattern = "[H|h]emd|[H|h]embd|[m|M]antel|Fürtuch|[C|c]hemise|
                [C|c]oat|[C|c]otte|[C|c]ols|[K|k]ragen|[R|r]ock|[S|s]ocke|[F|f]rack|
                [T|t]racht|[U|u]niform|[K|k]le[i|y]d|[J|j]un[t|dt]e|[K|k|C|c]amis[o|oh]l|
                [R|r]ock|[Ä|ä]rmel|[T|t]scho[b|p|pp]en|[H|h]ose|[S|s]abrack|Taufzeug|[K|k|C|c]orset|[F|f]ürtuch",
                valuetype = "regex")

# remove: Strümpf-Wolle (yarn), getrocknet/trocken (adjective, verb), Kleiderkasten (furniture)
# remove: Hosenträger (accessoire), Hemderknopf/Hembderknöpf (accessoire)

clothing_1834

clothing_corpus <- corpus_subset(corpus_1834,
                           docnames(corpus_1834) %in%
                             clothing_1834$docname)

clothing_corpus_clean <- clothing_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(clothing_corpus_clean),
                   max_words = 200)

## Shoes
shoes_1834 <- kwic(corpus_1834,
                     pattern = "[S|s]chuh|[S|s]chüh|[S|s]ohle|[S|s]öhle|[S|s]tiefel",
                     valuetype = "regex")

# remove: Handschuh (accessoire), Schuhmacher (occupation), Radschuh (thing), Schuhkraft (Schubkraft?)
# remove: numbers before Schuh or "lang, breit, dick, hoch, weit, Länge, hohe) after Schuh (length)

shoes_1834

shoes_corpus <- corpus_subset(corpus_1834,
                                docnames(corpus_1834) %in%
                                shoes_1834$docname)

shoes_corpus_clean <- shoes_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(shoes_corpus_clean),
                   max_words = 200)

## Textile Accessoires
texaccess_1834 <- kwic(corpus_1834,
                   pattern = "[K|k]appe|[H|h]aube|[S|s]tr[u|ü]mpf|[P|p]err[u|ü]cke|[S|s]nupftuch|[H|h]alstuch|
                   [H|h]andschuh|[H|h][u|ü]t|[P|p]araplui|Regenschirm|[C|c]hapeau|
                   [Sch|Ch]al|[C|c]rav[e|a]t|[S|s]chl[u|ü]pfer|[S|s]chürze|[F|f]oulard|[E|e]paulett",
                   valuetype = "regex")

# remove: Feurschütze (occupation), Schützenmatt (place), Schützen (occupation), Fingerhut/-hüte (thing)
# remove: Huttinge (place), Schönhut (place?), Hutmacherkunst (occupation), Waldshut (place),
# remove: Hutgasse (place), thut (verb), Strohhut-Fabrik (place?), Hutmacher (occupation)
# remove: Strümpf/Strumpf-Wolle (raw material)

texaccess_1834

texaccess_corpus <- corpus_subset(corpus_1834,
                              docnames(corpus_1834) %in%
                                texaccess_1834$docname)

texaccess_corpus_clean <- texaccess_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(texaccess_corpus_clean),
                   max_words = 200)

## Cloth and Fabric
cloth_1834 <- kwic(corpus_1834,
                       pattern = "[H|h]austuch|[F|f]uttertuch|[W|w]ollw[aa|ah]r|[W|w]oll-[W|w][aa|ah]r
                        [R|r]este|[L|l]einwa[nd|th]|Zeug|[S|s]acktuch",
                       valuetype = "regex")

# remove: Zeugnisse (work), Werkzeug (objects), überzeugt (verb), -bezeugung (noun), Lederzeug, Bettzeug (household textie)
# remove: Zeugwart (occupation),

cloth_1834

cloth_corpus <- corpus_subset(corpus_1834,
                                  docnames(corpus_1834) %in%
                                cloth_1834$docname)

cloth_corpus_clean <- cloth_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cloth_corpus_clean),
                   max_words = 50)

## Yarn
yarn_1834 <- kwic(corpus_1834,
                   pattern = "[F|f]lachs|[G|g]arn|[F|f]aden|[S|s]trickseide|[S|s]trickwolle|[S|s]trickbaum|
                  [S|s]tickseide|[S|s]tickwolle|[S|s]tickbaum",
                   valuetype = "regex")

# remove: garnies/ garniert/ garnirt (adjectives), Garnitur/ Garnirung (other objects), Garnison (military)
# remove: durchwirkt after Faden (decoration)

yarn_1834

yarn_corpus <- corpus_subset(corpus_1834,
                              docnames(corpus_1834) %in%
                               yarn_1834$docname)

yarn_corpus_clean <- yarn_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(yarn_corpus_clean),
                   max_words = 100)


## Animal Raw Materials
# very problematic category with dictionary, cause words like Wolle or Leder are often
# given as part of the description of clothing or other textiles
animalraw_1834 <- kwic(corpus_1834,
                    pattern = "Bettfehde|Bethfehde|Bettfede|Bettfehde|Rosshaar|Roßhaar",
                    valuetype = "regex")

animalraw_1834

animalraw_corpus <- corpus_subset(corpus_1834,
                               docnames(corpus_1834) %in%
                                 animalraw_1834$docname)

animalraw_corpus_clean <- animalraw_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(animalraw_corpus_clean),
                   max_words = 50)

## Non Textile Accessoires
nontexaccess_1834 <- kwic(corpus_1834,
                       pattern = "[K|k]n[o|ö]pf|[S|s]chnalle|[G|g]ürtel|[C|c]eintur|Hosenträger",
                       valuetype = "regex")

# remove: Knopfmacher

nontexaccess_1834

nontexaccess_corpus <- corpus_subset(corpus_1834,
                                  docnames(corpus_1834) %in%
                                    nontexaccess_1834$docname)

nontexaccess_corpus_clean <- nontexaccess_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(nontexaccess_corpus_clean),
                   max_words = 100)

## Bags and Purses
bag_1834 <- kwic(corpus_1834,
                          pattern = "[T|t]asche|[S|s]eckel|[B|b]eutel|[S|s]äcklein",
                          valuetype = "regex")

# remove: Seckelmeister (occupation), Taschenfeuerzeug (other object), Taschenkalender/-buch/-büchlein (book), Taschenuhr (watch)
# remove: Taschenspiel (game), Taschenformat (description), Taschen-Perspektiv (other object)

bag_1834

bag_corpus <- corpus_subset(corpus_1834,
                                     docnames(corpus_1834) %in%
                              bag_1834$docname)

bag_corpus_clean <- bag_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bag_corpus_clean),
                   max_words = 100)

### what is still missing? mostly types of cloth - problematic with a dictionary
# Jupon, Spitzen, Wollenwaaren, Stoff, Taffet, Mousseline, Thuhlle,
# Linon, Gewebe, Bast, Nansu, Marceline, Fichus, Palatine
