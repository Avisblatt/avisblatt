# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tagfilters_utils.R")
source("R/tagfilters_textiles.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")

avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

### checking and cleaning different tagfilters for textiles


## Clothing
clothing <- tagfilter_clothing()

clothing_ids <- clothing$filtrate(corpus_1834,ignore.case = F)

clothing_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                             clothing_ids)

clothing_texts <- clothing_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
clothing_kwic <- kwic(clothing_subset,
                  pattern = "[m|M]antel|[C|c]oat|[C|c]otte|[C|c]ols|[F|f]rack|[S|s]abrack|
                 [H|h]emd|[H|h]embd|[C|c]hemise|[K|k|C|c]amis[o|oh]l|[J|j]un[t|dt]e|Taufzeug|Tracht|
                 [U|u]niform|[S|s]ocke|[K|k|C|c]orset|[S|s]tr[u|ü]mpf|[H|h]ose|[T|t]scho[b|p|pp]en|Fürtuch|
                 [K|k]le[i|y]d|[R|r]ock|[Ä|ä]rmel|[K|k]ragen",
                  valuetype = "regex")
clothing_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
clothing_subset_clean <- clothing_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(clothing_subset_clean),
                   max_words = 100)


## Shoes
shoes <- tagfilter_shoes()

shoes_ids <- shoes$filtrate(corpus_1834,ignore.case = T)

shoes_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                shoes_ids)

shoes_texts <- shoes_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shoes_kwic <- kwic(shoes_subset,
                      pattern = "[S|s]chuh|[S|s]chüh|[S|s]tiefel[S|s]ohle|[S|s]öhle",
                      valuetype = "regex")
shoes_kwic

# The problem here is still the measurement of shoe: although most cases have been excluded in the dictionary, some remain
# e.g. "Länge 3 1/ 2 Schuh" - excluding these through regex of "Länge, Breite etc." in proximity of 3 words to "Schuh"?


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
shoes_subset_clean <- shoes_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(shoes_subset_clean),
                   max_words = 100)




## Textile Accessoires
##### very problematic category like this, a lot of false positives! Has to be split up in multiple dictionaries, i think
texaccess <- tagfilter_texaccess()

texaccess_ids <- texaccess$filtrate(corpus_1834,ignore.case = T)

texaccess_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    texaccess_ids)

texaccess_texts <- texaccess_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
texaccess_kwic <- kwic(texaccess_subset,
                 pattern = "[P|p]err[u|ü]ck|[C|c]rav[e|a]t|[H|h]alstuch|[F|f]oulard|[Sch|Ch]al|[S|s]nupftuch|
                 [S|s]chl[u|ü]pfer|[P|p]araplui|Regenschirm|[H|h]andschuh|[K|k]appe|[H|h]aube|[H|h][u|ü]t|[C|c]hapeau|
                 [E|e]paulett",
                 valuetype = "regex")

texaccess_kwic
write.csv2(texaccess_kwic, file = "data/texaccess_kwic.csv", fileEncoding = "UTF-8")

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
texaccess_subset_clean <- texaccess_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(texaccess_subset_clean),
                   max_words = 100)


## Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_ids <- cloth$filtrate(corpus_1834,ignore.case = F)

cloth_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                cloth_ids)

cloth_texts <- cloth_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cloth_kwic <- kwic(cloth_subset,
                       pattern = "[H|h]austuch|[F|f]uttertuch|[R|r]este|[L|l]einwa[nd|th]|Zeug|[S|s]acktuch|
                   [W|w]ollw[aa|ah]r|[W|w]oll-[W|w][aa|ah]r",
                       valuetype = "regex")

cloth_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cloth_subset_clean <- cloth_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cloth_subset_clean),
                   max_words = 100)


## Yarn
yarn <- tagfilter_yarn()

yarn_ids <- yarn$filtrate(corpus_1834,ignore.case = T)

yarn_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                               yarn_ids)

yarn_texts <- yarn_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
yarn_kwic <- kwic(yarn_subset,
                 pattern = "[F|f]lachs|[G|g]arn|[F|f]aden|[S|s]tickseide|[S|s]tickwolle|[S|s]tickbaum|
                 [S|s]trickseide|[S|s]trickwolle|[S|s]trickbaum",
                 valuetype = "regex")

yarn_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
yarn_subset_clean <- yarn_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(yarn_subset_clean),
                   max_words = 100)


## Animal Raw Materials
### This does not work, animalraw_ids = 0 - why? In kwic_textile it works with the exact same dictionary... have to look into it later!
animalraw <- tagfilter_animalraw()

animalraw_ids <- animalraw$filtrate(corpus_1834,ignore.case = T)

animalraw_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                 animalraw_ids)

animalraw_texts <- animalraw_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
animalraw_kwic <- kwic(animalraw_subset,
                   pattern = "Bettfehde|Bethfehde|Bettfede|Bettfehde|Rosshaar|Roßhaar",
                   valuetype = "regex")

animalraw_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
animalraw_subset_clean <- animalraw_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(animalraw_subset_clean),
                   max_words = 100)


## Non Textile Accessoires
nontexaccess <- tagfilter_nontexaccess()

nontexaccess_ids <- nontexaccess$filtrate(corpus_1834,ignore.case = T)

nontexaccess_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                       nontexaccess_ids)

nontexaccess_texts <- nontexaccess_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
nontexaccess_kwic <- kwic(nontexaccess_subset,
                    pattern = "[S|s]chnalle|Hosenträger|[G|g]ürtel|[C|c]eintur|[K|k]n[o|ö]pf",
                    valuetype = "regex")

nontexaccess_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
nontexaccess_subset_clean <- nontexaccess_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(nontexaccess_subset_clean),
                   max_words = 100)


## Bags and Purses
bag <- tagfilter_bag()

bag_ids <- bag$filtrate(corpus_1834,ignore.case = T)

bag_subset <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                              bag_ids)

bag_texts <- bag_subset$documents$texts

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bag_kwic <- kwic(bag_subset,
                       pattern = "[T|t]asche|[S|s]eckel|[B|b]eutel|[S|s]äcklein",
                       valuetype = "regex")

bag_kwic

# creating wordcloud for bed_subset for getting ideas for qualities etc. for further exploration
bag_subset_clean <- bag_subset %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bag_subset_clean),
                   max_words = 100)



### using existing categories for wordclouds and dfm to find missing objects in dictionaries
# creating subset of houshold objects for sale
textiles_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent) & grepl("01kauf", finance))

# cleaning subset
textiles_1834_clean <- textiles_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

# textplot of most frequent words
textplot_wordcloud(dfm(textiles_1834_clean),
                   max_words = 400)

# tokenize ads of subcorpus
textiles_tok <- textiles_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

# creating document feature matrix
textiles_dfm <- textiles_tok %>%
  dfm()

textiles_df <- docfreq(textiles_dfm)

textiles_s_df <- sort(textiles_df,decreasing = T)

# exporting csv with sorted document feature matrix to Desktop
write.csv2(textiles_s_df, file = "data/textiles_s_df_1834.csv", fileEncoding = "UTF-8")

### finding doc-ids of ads manually categorized as "01textilien" but not found with any of the above filters


