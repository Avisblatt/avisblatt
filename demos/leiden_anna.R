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

## Subcorpora for Household Objects/ Sale ads
bed_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   bed_ids_1734)

bed_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                   bed_ids_1834)


pushchair_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         pushchair_ids_1734)

pushchair_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         pushchair_ids_1834)

chair_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     chair_ids_1734)

chair_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     chair_ids_1834)

cabinet_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       cabinet_ids_1734)

cabinet_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       cabinet_ids_1834)

stove_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     stove_ids_1734)

stove_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     stove_ids_1834)

mirror_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                      mirror_ids_1734)

mirror_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                      mirror_ids_1834)

timepiece_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         timepiece_ids_1734)

timepiece_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         timepiece_ids_1834)

table_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     table_ids_1734)

table_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     table_ids_1834)

tableware_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         tableware_ids_1734)

tableware_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         tableware_ids_1834)

bureau_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                      bureau_ids_1734)

bureau_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                      bureau_ids_1834)

storage_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       storage_ids_1734)

storage_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       storage_ids_1834)

toy_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   toy_ids_1734)

toy_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                   toy_ids_1834)

game_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    game_ids_1734)

game_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    game_ids_1834)

kitchen_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       kitchen_ids_1734)

kitchen_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       kitchen_ids_1834)

lighting_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        lighting_ids_1734)

lighting_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        lighting_ids_1834)

instrument_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          instrument_ids_1734)

instrument_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                          instrument_ids_1834)

building_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        building_ids_1734)

building_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        building_ids_1834)

wallpaper_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         wallpaper_ids_1734)

wallpaper_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         wallpaper_ids_1834)

suitcase_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        suitcase_ids_1734)

suitcase_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        suitcase_ids_1834)

cutlery_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       cutlery_ids_1734)

cutlery_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       cutlery_ids_1834)

measure_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       measure_ids_1734)

measure_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       measure_ids_1834)

divider_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       divider_ids_1734)

divider_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       divider_ids_1834)

petobject_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         petobject_ids_1734)

petobject_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         petobject_ids_1834)

upholstery_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          upholstery_ids_1734)

upholstery_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                          upholstery_ids_1834)

domestic_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        domestic_ids_1734)

domestic_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        domestic_ids_1834)

garden_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                      garden_ids_1734)

garden_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                      garden_ids_1834)

homedeco_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        homedeco_ids_1734)

homedeco_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        homedeco_ids_1834)

art_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   art_ids_1734)

art_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                   art_ids_1834)

bathobject_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          bathobject_ids_1734)

bathobject_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                          bathobject_ids_1834)

mischousehold_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                             mischousehold_ids_1734)

mischousehold_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                             mischousehold_ids_1834)

bedding_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       bedding_ids_1734)

bedding_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       bedding_ids_1834)

carpet_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                      carpet_ids_1734)

carpet_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                      carpet_ids_1834)

tablelinen_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          tablelinen_ids_1734)

tablelinen_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                          tablelinen_ids_1834)




# Total textile and household ads recognised in sale ads

textiles_aut_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     c(clothing_ids_sale_1734, sleepwear_ids_sale_1734, uniform_ids_sale_1734, underwear_ids_sale_1734, outerwear_ids_sale_1734,
                                       costume_ids_sale_1734, shoes_ids_sale_1734,handkerchief_ids_sale_1734, umbrella_ids_sale_1734, hand_ids_sale_1734, neck_ids_sale_1734,
                                       headdress_ids_sale_1734, texmaterial_ids_sale_1734, cloth_ids_sale_1734, yarn_ids_sale_1734,animalraw_ids_sale_1734, plantraw_ids_sale_1734,
                                       mercery_ids_sale_1734, bag_ids_sale_1734, bedding_ids_1734, carpet_ids_1734, tablelinen_ids_1734))

textiles_aut_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     c(clothing_ids_sale_1834, sleepwear_ids_sale_1834, uniform_ids_sale_1834, underwear_ids_sale_1834, outerwear_ids_sale_1834, costume_ids_sale_1834, shoes_ids_sale_1834,
                                       handkerchief_ids_sale_1834, umbrella_ids_sale_1834, hand_ids_sale_1834, neck_ids_sale_1834, headdress_ids_sale_1834, texmaterial_ids_sale_1834, cloth_ids_sale_1834, yarn_ids_sale_1834,
                                       animalraw_ids_sale_1834, plantraw_ids_sale_1834, mercery_ids_sale_1834, bag_ids_sale_1834, bedding_ids_1834, carpet_ids_1834, tablelinen_ids_1834))

household_aut_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          c(bed_ids_all, bedding_ids_all, tablelinen_ids_all, carpet_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
                                            timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                            chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                            instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                            petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                            wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))

household_aut_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                          c(bed_ids_all, bedding_ids_all, tablelinen_ids_all, carpet_ids_all, chair_ids_all, table_ids_all, tableware_ids_all,
                                            timepiece_ids_all, mirror_ids_all, stove_ids_all, cabinet_ids_all, bureau_ids_all, cabinet_ids_all,
                                            chair_ids_all, cutlery_ids_all, divider_ids_all, domestic_ids_all, game_ids_all, garden_ids_all,
                                            instrument_ids_all, kitchen_ids_all, lighting_ids_all, measure_ids_all, mischousehold_ids_all,
                                            petobject_ids_all, plantobject_ids_all, storage_ids_all, suitcase_ids_all, toy_ids_all, upholstery_ids_all,
                                            wallpaper_ids_all, pushchair_ids_all, art_ids_all, homedeco_ids_all, bathobject_ids_all))



