# Load libraries and source functions
library(avisblatt)

# During Development rather run
devtools::load_all()

# corpus 1734

avis_1734 <- readtext("data/groundtruth1734.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1734$text <- correct_ocr(avis_1734$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1734_all <- corpus(avis_1734,
                          docid_field = "doc_id")
corpus_1734 <- corpus_subset(corpus_1734_all,
                             (docvars(corpus_1734_all,"id") %in%
                                ids_by_lang$de))

# corpus 1834

avis_1834 <- readtext("data/groundtruth1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                          docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                             (docvars(corpus_1834_all,"id") %in%
                                ids_by_lang$de))

corpus_all <- c(corpus_1734, corpus_1834)

# negating %in% operator
`%notin%` <- Negate(`%in%`)



### checking and cleaning different tagfilters for textiles


## Clothing
clothing <- tagfilter_clothing()

clothing_1734 <- clothing$filtrate(corpus_1734, ignore.case = T)
clothing_1834 <- clothing$filtrate(corpus_1834, ignore.case = T)

clothing_all <- c(clothing_1734, clothing_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
clothing_kwic <- kwic(clothing_all,
                  pattern = "Tschako",
                  valuetype = "regex",
                  ignore.case = T)
clothing_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
clothing_subset_clean_all <- clothing_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(clothing_subset_clean_all),
                   max_words = 200)


# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_clothing_all <- validate_filter(corpus_all, clothing_all,
                                  search_col = "adcontent",
                                  pattern = "01textilien")
validation_clothing_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% clothing_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% clothing_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_clothing_all$filter_T_hc_T)
b_t$documents$texts[1:277]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_clothing_all$filter_T_hc_F)
b_f$documents$texts[1:18]


## Sleepwear
sleepwear <- tagfilter_sleepwear()

sleepwear_1734 <- sleepwear$filtrate(corpus_1734,ignore.case = T)
sleepwear_1834 <- sleepwear$filtrate(corpus_1834,ignore.case = T)

sleepwear_all <- c(sleepwear_1734, sleepwear_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
sleepwear_kwic <- kwic(sleepwear_all,
                      pattern = "Schlafrock|Schlafröck|Nachtärmel",
                      valuetype = "regex",
                      ignore.case = T)
sleepwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
sleepwear_subset_clean <- sleepwear_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(sleepwear_subset_clean),
                   max_words = 200)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_sleepwear_all <- validate_filter(corpus_all, sleepwear_all,
                                           search_col = "adcontent",
                                           pattern = "01textilien")
validation_sleepwear_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% sleepwear_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% sleepwear_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_sleepwear_all$filter_T_hc_T)
b_t$documents$texts[1:16]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_sleepwear_all$filter_T_hc_F)
b_f$documents$texts[0]



## Military/ Uniforms
uniform <- tagfilter_uniform()

uniform_1734 <- uniform$filtrate(corpus_1734,ignore.case = T)
uniform_1834 <- uniform$filtrate(corpus_1834,ignore.case = T)

uniform_all <- c(uniform_1734, uniform_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
uniform_kwic <- kwic(uniform_all,
                       pattern = "Militär-Effekt|Militäreffekt",
                       valuetype = "regex",
                       ignore.case = T)
uniform_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
uniform_subset_clean <- uniform_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(uniform_subset_clean),
                   max_words = 200)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_uniform_all <- validate_filter(corpus_all, uniform_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_uniform_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% uniform_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% uniform_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_uniform_all$filter_T_hc_T)
b_t$documents$texts[1:28]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_uniform_all$filter_T_hc_F)
b_f$documents$texts[1:6]



## Underwear
underwear <- tagfilter_underwear()

underwear_1734 <- underwear$filtrate(corpus_1734,ignore.case = T)
underwear_1834 <- underwear$filtrate(corpus_1834,ignore.case = T)

underwear_all <- c(underwear_1734, underwear_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
underwear_kwic <- kwic(underwear_all,
                     pattern = "Socke|Strumpf|Strümpf",
                     valuetype = "regex",
                     ignore.case = T)
underwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
underwear_subset_clean <- underwear_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(underwear_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_underwear_all <- validate_filter(corpus_all, underwear_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_underwear_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% underwear_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% underwear_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_underwear_all$filter_T_hc_T)
b_t$documents$texts[1:19]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_underwear_all$filter_T_hc_F)
b_f$documents$texts[1:5]


## Outerwear
outerwear <- tagfilter_outerwear()

outerwear_1734 <- outerwear$filtrate(corpus_1734,ignore.case = T)
outerwear_1834 <- outerwear$filtrate(corpus_1834,ignore.case = T)

outerwear_all <- c(outerwear_1734, outerwear_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
outerwear_kwic <- kwic(outerwear_all,
                       pattern = "Mantel|Mäntel|Coat|Cotte|Cols|Schbrack|Mantille|Kittelein|Pellerine",
                       valuetype = "regex",
                       ignore.case = T)
outerwear_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
outerwear_subset_clean <- outerwear_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(outerwear_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_outerwear_all <- validate_filter(corpus_all, outerwear_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_outerwear_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% outerwear_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% outerwear_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_outerwear_all$filter_T_hc_T)
b_t$documents$texts[1:150]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_outerwear_all$filter_T_hc_F)
b_f$documents$texts[1:2]


## Costumes/ Special Occasion Garments
costume <- tagfilter_costume()

costume_1734 <- costume$filtrate(corpus_1734,ignore.case = T)
costume_1834 <- costume$filtrate(corpus_1834,ignore.case = T)

costume_all <- c(costume_1734, costume_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
costume_kwic <- kwic(costume_all,
                       pattern = "Taufe",
                       valuetype = "regex",
                       ignore.case = T)
costume_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
costume_subset_clean <- costume_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(costume_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_costume_all <- validate_filter(corpus_all, costume_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_costume_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% costume_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% costume_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_costume_all$filter_T_hc_T)
b_t$documents$texts[1:12]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_costume_all$filter_T_hc_F)
b_f$documents$texts[0]


## Shoes
shoes <- tagfilter_shoes()

shoes_1734 <- shoes$filtrate(corpus_1734,ignore.case = T)
shoes_1834 <- shoes$filtrate(corpus_1834,ignore.case = T)

shoes_all <- c(shoes_1734, shoes_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
shoes_kwic <- kwic(shoes_all,
                      pattern = "[S|s]chuh|[S|s]chüh|[S|s]tiefel[S|s]ohle|[S|s]öhle|[P|p]antoffel",
                      valuetype = "regex")
shoes_kwic

# some "schuh" as measurement remain, e.g. "Länge 3 1/ 2 Schuh"
# excluding these through regex of "Länge, Breite etc." in proximity of 3 words to "Schuh"?


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
shoes_subset_clean <- shoes_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(shoes_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_shoes_all <- validate_filter(corpus_all, shoes_all,
                                          search_col = "adcontent",
                                          pattern = "01textilien")
validation_shoes_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% shoes_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% shoes_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_shoes_all$filter_T_hc_T)
b_t$documents$texts[1:61]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_shoes_all$filter_T_hc_F)
b_f$documents$texts[1:17]


## Handkerchiefs
handkerchief <- tagfilter_handkerchief()

handkerchief_1734 <- handkerchief$filtrate(corpus_1734,ignore.case = T)
handkerchief_1834 <- handkerchief$filtrate(corpus_1834,ignore.case = T)

handkerchief_all <- c(handkerchief_1734, handkerchief_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
handkerchief_kwic <- kwic(handkerchief_all,
                     pattern = "Schnupftuch|Mouchoir|Sacktuch|Sacktüch",
                     valuetype = "regex",
                     ignore.case = T)
handkerchief_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
handkerchief_subset_clean <- handkerchief_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(handkerchief_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_handkerchief_all <- validate_filter(corpus_all, handkerchief_all,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_handkerchief_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% handkerchief_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% handkerchief_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_handkerchief_all$filter_T_hc_T)
b_t$documents$texts[1:67]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_handkerchief_all$filter_T_hc_F)
b_f$documents$texts[1:2]


## Gloves and Muffs
hand <- tagfilter_hand()

hand_1734 <- hand$filtrate(corpus_1734,ignore.case = T)
hand_1834 <- hand$filtrate(corpus_1834,ignore.case = T)

hand_all <- c(hand_1734, hand_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
hand_kwic <- kwic(hand_all,
                      pattern = "Schlupfer|Schlüpfer|Handschuh",
                      valuetype = "regex",
                      ignore.case = T)
hand_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
hand_subset_clean <- hand_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(hand_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_hand_all <- validate_filter(corpus_all, hand_all,
                                           search_col = "adcontent",
                                           pattern = "01textilien")
validation_hand_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% hand_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% hand_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_hand_all$filter_T_hc_T)
b_t$documents$texts[1:138]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_hand_all$filter_T_hc_F)
b_f$documents$texts[1:13]


## Scarves, Colars, and Neckties
neck <- tagfilter_neck()

neck_1734 <- neck$filtrate(corpus_1734,ignore.case = T)
neck_1834 <- neck$filtrate(corpus_1834,ignore.case = T)

neck_all <- c(neck_1734, neck_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
neck_kwic <- kwic(neck_all,
                  pattern = "Halstuch|Foulard|Schal|Schawl|Shaul|Chal|Fichu",
                  valuetype = "regex",
                  ignore.case = T)
neck_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
neck_subset_clean <- neck_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(neck_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_neck_all <- validate_filter(corpus_all, neck_all,
                                       search_col = "adcontent",
                                       pattern = "01textilien")
validation_neck_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% neck_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% neck_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_neck_all$filter_T_hc_T)
b_t$documents$texts[1:205]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_neck_all$filter_T_hc_F)
b_f$documents$texts[1:4]



## Headdress and Wigs
headdress <- tagfilter_headdress()

headdress_1734 <- headdress$filtrate(corpus_1734,ignore.case = T)
headdress_1834 <- headdress$filtrate(corpus_1834,ignore.case = T)

headdress_all <- c(headdress_1734, headdress_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
headdress_kwic <- kwic(headdress_all,
                  pattern = "Haube",
                  valuetype = "regex",
                  ignore.case = T)
headdress_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
headdress_subset_clean <- headdress_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(headdress_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_headdress_all <- validate_filter(corpus_all, headdress_all,
                                       search_col = "adcontent",
                                       pattern = "01textilien")
validation_headdress_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% headdress_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% headdress_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_headdress_all$filter_T_hc_T)
b_t$documents$texts[1:210]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_headdress_all$filter_T_hc_F)
b_f$documents$texts[1:54]



## Certain Types of Fabric/ Textile Material
# includes some "Pfeifenspitzen", where "Pfeifen" is not right before "Spitzen" - how to exclude those?
texmaterial <- tagfilter_texmaterial()

texmaterial_1734 <- texmaterial$filtrate(corpus_1734,ignore.case = T)
texmaterial_1834 <- texmaterial$filtrate(corpus_1834,ignore.case = T)

texmaterial_all <- c(texmaterial_1734, texmaterial_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
texmaterial_kwic <- kwic(texmaterial_all,
                       pattern = "Wachslappen|Alépin",
                       valuetype = "regex",
                       ignore.case = T)
texmaterial_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
texmaterial_subset_clean <- texmaterial_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(texmaterial_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_texmaterial_1834 <- validate_filter(corpus_all, texmaterial_all,
                                            search_col = "adcontent",
                                            pattern = "01textilien")
validation_texmaterial_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% texmaterial_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% texmaterial_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_texmaterial_all$filter_T_hc_T)
b_t$documents$texts[1:560]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_texmaterial_1834$filter_T_hc_F)
b_f$documents$texts[1:62]


## Unspecified Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_1734 <- cloth$filtrate(corpus_1734,ignore.case = T)
cloth_1834 <- cloth$filtrate(corpus_1834,ignore.case = T)

cloth_all <- c(cloth_1734, cloth_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
cloth_kwic <- kwic(cloth_all,
                       pattern = "Reste|Zeug|Tuch|Tüch|Geflecht|Etoffe|Gewebe",
                       valuetype = "regex",
                   ignore.case = T)

cloth_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
cloth_subset_clean <- cloth_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(cloth_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_cloth_all <- validate_filter(corpus_all, cloth_all,
                                              search_col = "adcontent",
                                              pattern = "01textilien")
validation_cloth_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% cloth_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% cloth_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cloth_all$filter_T_hc_T)
b_t$documents$texts[1:342]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_cloth_all$filter_T_hc_F)
b_f$documents$texts[1:9]


## Yarn
yarn <- tagfilter_yarn()

yarn_1734 <- yarn$filtrate(corpus_1734,ignore.case = T)
yarn_1834 <- yarn$filtrate(corpus_1834,ignore.case = T)

yarn_all <- c(yarn_1734, yarn_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
yarn_kwic <- kwic(yarn_all,
                 pattern = "Strickseide|Strickwolle|Strickbaum",
                 valuetype = "regex",
                 ignore.case = T)

yarn_kwic

# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
yarn_subset_clean <- yarn_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(yarn_subset_clean),
                   max_words = 100)

# Validation
# found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
# found by filter AND NOT by HC ("oops") | neither filter nor hc
# only "yay" and "oops" relevant
validation_yarn_all <- validate_filter(corpus_all, yarn_all,
                                        search_col = "adcontent",
                                        pattern = "01textilien")
validation_yarn_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% yarn_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% yarn_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_yarn_all$filter_T_hc_T)
b_t$documents$texts[1:116]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_yarn_all$filter_T_hc_F)
b_f$documents$texts[1:5]





## Tablelinen
tablelinen <- tagfilter_tablelinen()

tablelinen_1734 <- tablelinen$filtrate(corpus_1734,ignore.case = T)
tablelinen_1834 <- tablelinen$filtrate(corpus_1834,ignore.case = T)

tablelinen_all <- c(tablelinen_1734, tablelinen_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
tablelinen_kwic <- kwic(tablelinen_all,
                               pattern = "Tisch(zeug|deck)|(Tisch|Tafel)t(ü|u)ch|Serviette|Handt(ü|u)ch",
                               valuetype = "regex")

tablelinen_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
tablelinen_subset_clean <- tablelinen_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(tablelinen_subset_clean),
                   max_words = 100)

## Validation
validation_tablelinen_all <- validate_filter(corpus_all, tablelinen_all,
                                                    search_col = "adcontent",
                                                    pattern = "01textilien")
validation_tablelinen_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% tablelinen_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% tablelinen_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tablelinen_all$filter_T_hc_T)
b_t$documents$texts[1:43]

# FALSE positives
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_tablelinen_all$filter_T_hc_F)
b_f$documents$texts[1:5]

## Carpets and Curtains
carpet <- tagfilter_carpet()

carpet_1734 <- carpet$filtrate(corpus_1734,ignore.case = T)
carpet_1834 <- carpet$filtrate(corpus_1834,ignore.case = T)

carpet_all <- c(carpet_1734, carpet_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
carpet_kwic <- kwic(carpet_all,
                               pattern = "T(e|a)(pp|p)i|Bodent(u|ü)ch|Vorh(a|ä)ng",
                               valuetype = "regex")

carpet_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
carpet_subset_clean <- carpet_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(carpet_subset_clean),
                   max_words = 100)

## Validation
validation_carpet_all <- validate_filter(corpus_all, carpet_all,
                                      search_col = "adcontent",
                                      pattern = "01textilien")
validation_carpet_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% carpet_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% carpet_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_carpet_all$filter_T_hc_T)
b_t$documents$texts[1:57]

# FALSE positives: both correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_carpet_all$filter_T_hc_F)
b_f$documents$texts[1:2]


## Bedding
bedding <- tagfilter_bedding()

bedding_1734 <- bedding$filtrate(corpus_1734,ignore.case = T)
bedding_1834 <- bedding$filtrate(corpus_1834,ignore.case = T)

bedding_all <- c(bedding_1734, bedding_1834)

# checking identified ads through analysis of kwic for positive dictionary (no negatives necessary, since already excluded in corpus subset)
bedding_kwic <- kwic(bedding_all,
                               pattern = "Deckbet|Hauszeug|Matrat|Madrat|Matraz|Bettdeck|Bettwer|Bethwer|Bettzeug|
    Bethzeug|Bettsack|Bethsack|Decke|Strochsack|Strohsäck|Kissen|Unterbe(tt|th)|Nachtsack|Betteingu(ß|ss)",
                               valuetype = "regex")

bedding_kwic


# creating wordcloud for subset for getting ideas for qualities etc. for further exploration
bedding_subset_clean <- bedding_all %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())

textplot_wordcloud(dfm(bedding_subset_clean),
                   max_words = 100)

## Validation
validation_bedding_all <- validate_filter(corpus_all, bedding_all,
                                                    search_col = "adcontent",
                                                    pattern = "01textilien")
validation_bedding_all

# Filters for TRUE and FALSE positives
doc_ids_all <- corpus_all$documents[,"id"]
filter_T_hc_T <- doc_ids_all[(doc_ids_all %in% bedding_all)]
filter_T_hc_F <- doc_ids_all[(doc_ids_all %notin% bedding_all)]

# TRUE positives
b_t <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bedding_all$filter_T_hc_T)
b_t$documents$texts[1:130]

# FALSE positives: almost all correctly recognised
b_f <- corpus_subset(corpus_all,
                     docvars(corpus_all,"id") %in%
                       validation_bedding_all$filter_T_hc_F)
b_f$documents$texts[1:35]


### using existing categories for wordclouds and dfm to find missing objects in dictionaries

# creating subset of textile objects for sale
textiles_manual_1734 <- corpus_subset(corpus_1734, grepl("01textilien", adcontent))

textiles_manual_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent))

# creating a corpus of all the ads (ids) not found by automatic classification of ads for sale/ to buy

textiles_aut_1734 <- c(docvars(clothing_1734, "id"), docvars(sleepwear_1734, "id"), docvars(uniform_1734, "id"), docvars(underwear_1734, "id"), docvars(outerwear_1734, "id"),
                       docvars(costume_1734, "id"), docvars(shoes_1734, "id"), docvars(handkerchief_1734, "id"), docvars(hand_1734, "id"), docvars(neck_1734, "id"),
                       docvars(headdress_1734, "id"), docvars(texmaterial_1734, "id"), docvars(cloth_1734, "id"), docvars(yarn_1734, "id"), docvars(bag_1734, "id"),
                       docvars(tablelinen_1734, "id"), docvars(carpet_1734, "id"), docvars(bedding_1734, "id"))

textiles_aut_1834 <- c(docvars(clothing_1834, "id"), docvars(sleepwear_1834, "id"), docvars(uniform_1834, "id"), docvars(underwear_1834, "id"), docvars(outerwear_1834, "id"),
                       docvars(costume_1834, "id"), docvars(shoes_1834, "id"), docvars(handkerchief_1834, "id"), docvars(hand_1834, "id"), docvars(neck_1834, "id"),
                       docvars(headdress_1834, "id"), docvars(texmaterial_1834, "id"), docvars(cloth_1834, "id"), docvars(yarn_1834, "id"), docvars(bag_1834, "id"),
                       docvars(tablelinen_1834, "id"), docvars(carpet_1834, "id"), docvars(bedding_1834, "id"))

textiles_missed_1734 <- corpus_subset(textiles_manual_1734, docvars(textiles_manual_1734, "id") %notin% docvars(textiles_aut_1734, "id"))

textiles_missed_1834 <- corpus_subset(textiles_manual_1834, docvars(textiles_manual_1834, "id") %notin% docvars(textiles_aut_1834, "id"))

write.csv2(textiles_missed_1734, file = "data/textiles_missed_1734.csv", fileEncoding = "UTF-8")

write.csv2(textiles_missed_1834, file = "data/textiles_missed_1834.csv", fileEncoding = "UTF-8")

# 21/01/2020: 88 missed ads (by automated classification)
# most often wrongly classified manually ("Bett" and "spanische Wand" and upholstery is very often classified as textile)
# some French ads
# not recognized due to ocr mistakes (Gane/Gaze)
# few with "Wolle" or "Atlas" not recognizes (difficult to sort out negatives)
# artificial flowers are not included (Blumenarbeit as another dictionary?)

# 20/02/2020: only 125 missed tex ads in 1734 and 1834 combined, most of them actually wrongly classified as textile manually (mistakes see above)


