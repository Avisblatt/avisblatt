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
source("R/tagfilters_household.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")
# negating %in% operator
`%notin%` <- Negate(`%in%`)

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


# creating corpora of textiles and sale adverts
textiles_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent) & grepl("01kauf", finance))
sale_1834 <- corpus_subset(corpus_1834, grepl("01kauf", finance) & grepl("01biete", adtype))

textiles_1734 <- corpus_subset(corpus_1734, grepl("01textilien", adcontent) & grepl("01kauf", finance))
sale_1734 <- corpus_subset(corpus_1734, grepl("01kauf", finance) & grepl("01biete", adtype))

textiles_all <- c(textiles_1734, textiles_1834)
sale_all <- c(sale_1734, sale_1834)

## Clothing
clothing <- tagfilter_clothing()

clothing_ids_sale_1734 <- clothing$filtrate(sale_1734,ignore.case = T)
clothing_ids_sale_1834 <- clothing$filtrate(sale_1834,ignore.case = T)

clothing_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        clothing_ids_sale_1734)
clothing_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        clothing_ids_sale_1834)

## Sleepwear
sleepwear <- tagfilter_sleepwear()

sleepwear_ids_sale_1734 <- sleepwear$filtrate(sale_1734,ignore.case = T)
sleepwear_ids_sale_1834 <- sleepwear$filtrate(sale_1834,ignore.case = T)

sleepwear_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         sleepwear_ids_sale_1734)
sleepwear_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         sleepwear_ids_sale_1834)


## Military/ Uniforms
uniform <- tagfilter_uniform()

uniform_ids_sale_1734 <- uniform$filtrate(sale_1734,ignore.case = T)
uniform_ids_sale_1834 <- uniform$filtrate(sale_1834,ignore.case = T)

uniform_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       uniform_ids_sale_1734)
uniform_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       uniform_ids_sale_1834)


## Underwear
underwear <- tagfilter_underwear()

underwear_ids_sale_1734 <- underwear$filtrate(sale_1734,ignore.case = T)
underwear_ids_sale_1834 <- underwear$filtrate(sale_1834,ignore.case = T)

underwear_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         underwear_ids_sale_1734)
underwear_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         underwear_ids_sale_1834)



## Outerwear
outerwear <- tagfilter_outerwear()

outerwear_ids_sale_1734 <- outerwear$filtrate(sale_1734,ignore.case = T)
outerwear_ids_sale_1834 <- outerwear$filtrate(sale_1834,ignore.case = T)

outerwear_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         outerwear_ids_sale_1734)
outerwear_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         outerwear_ids_sale_1834)


## Costumes/ Special Occasion Garments
costume <- tagfilter_costume()

costume_ids_sale_1734 <- costume$filtrate(sale_1734,ignore.case = T)
costume_ids_sale_1834 <- costume$filtrate(sale_1834,ignore.case = T)

costume_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       costume_ids_sale_1734)
costume_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       costume_ids_sale_1834)



## Shoes
shoes <- tagfilter_shoes()

shoes_ids_sale_1734 <- shoes$filtrate(sale_1734,ignore.case = T)
shoes_ids_sale_1834 <- shoes$filtrate(sale_1834,ignore.case = T)

shoes_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     shoes_ids_1734)
shoes_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     shoes_ids_1834)


## Handkerchiefs
handkerchief <- tagfilter_handkerchief()

handkerchief_ids_sale_1734 <- handkerchief$filtrate(sale_1734,ignore.case = T)
handkerchief_ids_sale_1834 <- handkerchief$filtrate(sale_1834,ignore.case = T)

handkerchief_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                            handkerchief_ids_sale_1734)
handkerchief_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                            handkerchief_ids_sale_1834)

## Umbrellas
umbrella <- tagfilter_umbrella()

umbrella_ids_sale_1734 <- umbrella$filtrate(sale_1734,ignore.case = T)
umbrella_ids_sale_1834 <- umbrella$filtrate(sale_1834,ignore.case = T)

umbrella_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        umbrella_ids_sale_1734)
umbrella_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        umbrella_ids_sale_1834)


## Gloves and Muffs
hand <- tagfilter_hand()

hand_ids_sale_1734 <- hand$filtrate(sale_1734,ignore.case = T)
hand_ids_sale_1834 <- hand$filtrate(sale_1834,ignore.case = T)

hand_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    hand_ids_sale_1734)
hand_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    hand_ids_sale_1834)

## Scarves, Colars, and Neckties
neck <- tagfilter_neck()

neck_ids_sale_1734 <- neck$filtrate(sale_1734,ignore.case = T)
neck_ids_sale_1834 <- neck$filtrate(sale_1834,ignore.case = T)

neck_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    neck_ids_sale_1734)
neck_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    neck_ids_sale_1834)


## Headdress and Wigs
headdress <- tagfilter_headdress()

headdress_ids_sale_1734 <- headdress$filtrate(sale_1734,ignore.case = T)
headdress_ids_sale_1834 <- headdress$filtrate(sale_1834,ignore.case = T)

headdress_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         headdress_ids_sale_1734)
headdress_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         headdress_ids_sale_1834)

## Certain Types of Fabric/ Textile Material
texmaterial <- tagfilter_texmaterial()

texmaterial_ids_sale_1734 <- texmaterial$filtrate(sale_1734,ignore.case = T)
texmaterial_ids_sale_1834 <- texmaterial$filtrate(sale_1834,ignore.case = T)

texmaterial_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                           texmaterial_ids_sale_1734)
texmaterial_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                           texmaterial_ids_sale_1834)

## Unspecified Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_ids_sale_1734 <- cloth$filtrate(sale_1734,ignore.case = T)
cloth_ids_sale_1834 <- cloth$filtrate(sale_1834,ignore.case = T)

cloth_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     cloth_ids_sale_1734)
cloth_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     cloth_ids_sale_1834)


## Yarn
yarn <- tagfilter_yarn()

yarn_ids_sale_1734 <- yarn$filtrate(sale_1734,ignore.case = T)
yarn_ids_sale_1834 <- yarn$filtrate(sale_1834,ignore.case = T)

yarn_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    yarn_ids_sale_1734)
yarn_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    yarn_ids_sale_1834)

## Animal Raw Materials
animalraw <- tagfilter_animalraw()

animalraw_ids_sale_1734 <- animalraw$filtrate(sale_1734,ignore.case = T)
animalraw_ids_sale_1834 <- animalraw$filtrate(sale_1834,ignore.case = T)

animalraw_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         animalraw_ids_sale_1734)
animalraw_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         animalraw_ids_sale_1834)

## Plant Raw Materials
plantraw <- tagfilter_plantraw()

plantraw_ids_sale_1734 <- plantraw$filtrate(sale_1734,ignore.case = T)
plantraw_ids_sale_1834 <- plantraw$filtrate(sale_1834,ignore.case = T)

plantraw_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        plantraw_ids_sale_1734)
plantraw_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        plantraw_ids_sale_1834)

## Mercery and Non Textile Accessoires
mercery <- tagfilter_mercery()

mercery_ids_sale_1734 <- mercery$filtrate(sale_1734,ignore.case = T)
mercery_ids_sale_1834 <- mercery$filtrate(sale_1834,ignore.case = T)

mercery_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       mercery_ids_sale_1734)
mercery_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       mercery_ids_sale_1834)



## Bags and Purses
bag <- tagfilter_bag()

bag_ids_sale_1734 <- bag$filtrate(sale_1734,ignore.case = T)
bag_ids_sale_1834 <- bag$filtrate(sale_1834,ignore.case = T)

bag_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   bag_ids_sale_1734)
bag_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                   bag_ids_sale_1834)

## Household Textiles
household_textile <- tagfilter_household_textile()

household_textile_ids_sale_1734 <- household_textile$filtrate(sale_1734,ignore.case = T)
household_textile_ids_sale_1834 <- household_textile$filtrate(sale_1834,ignore.case = T)

household_textile_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                                 household_textile_ids_sale_1734)
household_textile_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                                 household_textile_ids_sale_1834)

# Total textile ads recognised in sale ads

textiles_aut_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     c(clothing_ids_sale_1734, sleepwear_ids_sale_1734, uniform_ids_sale_1734, underwear_ids_sale_1734, outerwear_ids_sale_1734, costume_ids_sale_1734, shoes_ids_sale_1734,
                                       handkerchief_ids_sale_1734, umbrella_ids_sale_1734, hand_ids_sale_1734, neck_ids_sale_1734, headdress_ids_sale_1734, texmaterial_ids_sale_1734, cloth_ids_sale_1734, yarn_ids_sale_1734,
                                       animalraw_ids_sale_1734, plantraw_ids_sale_1734, mercery_ids_sale_1734, bag_ids_sale_1734, household_textile_ids_sale_1734))

textiles_aut_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     c(clothing_ids_sale_1834, sleepwear_ids_sale_1834, uniform_ids_sale_1834, underwear_ids_sale_1834, outerwear_ids_sale_1834, costume_ids_sale_1834, shoes_ids_sale_1834,
                                       handkerchief_ids_sale_1834, umbrella_ids_sale_1834, hand_ids_sale_1834, neck_ids_sale_1834, headdress_ids_sale_1834, texmaterial_ids_sale_1834, cloth_ids_sale_1834, yarn_ids_sale_1834,
                                       animalraw_ids_sale_1834, plantraw_ids_sale_1834, mercery_ids_sale_1834, bag_ids_sale_1834, household_textile_ids_sale_1834))

textiles_aut_1734_ts <- ts(textiles_aut_sale_1734, frequency = 12)

