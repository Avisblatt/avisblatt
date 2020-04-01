### This is a R-script for the data I will use in Leiden, most of it is drawn from other R-scrips
### or only relevant for the paper, most can possibly deletad afterwards, archived or copied into
### relevant R-scripts

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
corpus_1834$documents$text[1:5]
corpus_all <- c(corpus_1734, corpus_1834)

## Counting instances of different textile and household objects in the manually classified sale category

# creating corpora of textiles and sale adverts
textiles_manual_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent) & grepl("01kauf", finance))
sale_1834 <- corpus_subset(corpus_1834, grepl("01kauf", finance) & grepl("01biete", adtype))
sale_things_1834 <- corpus_subset(sale_1834, grepl("textilien", adcontent) | grepl("hausrat", adcontent)
                                  | grepl("lebensmittel", adcontent) | grepl("ding", adcontent)
                                  | grepl("schmuck", adcontent))

textiles_manual_1734 <- corpus_subset(corpus_1734, grepl("01textilien", adcontent) & grepl("01kauf", finance))
sale_1734 <- corpus_subset(corpus_1734, grepl("01kauf", finance) & grepl("01biete", adtype))
sale_things_1734 <- corpus_subset(sale_1734, grepl("textilien", adcontent) | grepl("hausrat", adcontent)
                                  | grepl("lebensmittel", adcontent) | grepl("ding", adcontent)
                                  | grepl("schmuck", adcontent))

sale_all <- c(sale_1734, sale_1834)

# Clothing
clothing <- tagfilter_clothing()

clothing_ids_sale_1734 <- clothing$filtrate(sale_1734,ignore.case = T)
clothing_ids_sale_1834 <- clothing$filtrate(sale_1834,ignore.case = T)

clothing_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        clothing_ids_sale_1734)
clothing_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        clothing_ids_sale_1834)

# Sleepwear
sleepwear <- tagfilter_sleepwear()

sleepwear_ids_sale_1734 <- sleepwear$filtrate(sale_1734,ignore.case = T)
sleepwear_ids_sale_1834 <- sleepwear$filtrate(sale_1834,ignore.case = T)

sleepwear_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         sleepwear_ids_sale_1734)
sleepwear_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         sleepwear_ids_sale_1834)


# Military/ Uniforms
uniform <- tagfilter_uniform()

uniform_ids_sale_1734 <- uniform$filtrate(sale_1734,ignore.case = T)
uniform_ids_sale_1834 <- uniform$filtrate(sale_1834,ignore.case = T)

uniform_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       uniform_ids_sale_1734)
uniform_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       uniform_ids_sale_1834)


# Underwear
underwear <- tagfilter_underwear()

underwear_ids_sale_1734 <- underwear$filtrate(sale_1734,ignore.case = T)
underwear_ids_sale_1834 <- underwear$filtrate(sale_1834,ignore.case = T)

underwear_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         underwear_ids_sale_1734)
underwear_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         underwear_ids_sale_1834)



# Outerwear
outerwear <- tagfilter_outerwear()

outerwear_ids_sale_1734 <- outerwear$filtrate(sale_1734,ignore.case = T)
outerwear_ids_sale_1834 <- outerwear$filtrate(sale_1834,ignore.case = T)

outerwear_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         outerwear_ids_sale_1734)
outerwear_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         outerwear_ids_sale_1834)


# Costumes/ Special Occasion Garments
costume <- tagfilter_costume()

costume_ids_sale_1734 <- costume$filtrate(sale_1734,ignore.case = T)
costume_ids_sale_1834 <- costume$filtrate(sale_1834,ignore.case = T)

costume_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       costume_ids_sale_1734)
costume_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       costume_ids_sale_1834)



# Shoes
shoes <- tagfilter_shoes()

shoes_ids_sale_1734 <- shoes$filtrate(sale_1734,ignore.case = T)
shoes_ids_sale_1834 <- shoes$filtrate(sale_1834,ignore.case = T)

shoes_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     shoes_ids_1734)
shoes_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     shoes_ids_1834)


# Handkerchiefs
handkerchief <- tagfilter_handkerchief()

handkerchief_ids_sale_1734 <- handkerchief$filtrate(sale_1734,ignore.case = T)
handkerchief_ids_sale_1834 <- handkerchief$filtrate(sale_1834,ignore.case = T)

handkerchief_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                            handkerchief_ids_sale_1734)
handkerchief_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                            handkerchief_ids_sale_1834)


# Gloves and Muffs
hand <- tagfilter_hand()

hand_ids_sale_1734 <- hand$filtrate(sale_1734,ignore.case = T)
hand_ids_sale_1834 <- hand$filtrate(sale_1834,ignore.case = T)

hand_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    hand_ids_sale_1734)
hand_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    hand_ids_sale_1834)

# Scarves, Colars, and Neckties
neck <- tagfilter_neck()

neck_ids_sale_1734 <- neck$filtrate(sale_1734,ignore.case = T)
neck_ids_sale_1834 <- neck$filtrate(sale_1834,ignore.case = T)

neck_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    neck_ids_sale_1734)
neck_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    neck_ids_sale_1834)


# Headdress and Wigs
headdress <- tagfilter_headdress()

headdress_ids_sale_1734 <- headdress$filtrate(sale_1734,ignore.case = T)
headdress_ids_sale_1834 <- headdress$filtrate(sale_1834,ignore.case = T)

headdress_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         headdress_ids_sale_1734)
headdress_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         headdress_ids_sale_1834)

# Certain Types of Fabric/ Textile Material
texmaterial <- tagfilter_texmaterial()

texmaterial_ids_sale_1734 <- texmaterial$filtrate(sale_1734,ignore.case = T)
texmaterial_ids_sale_1834 <- texmaterial$filtrate(sale_1834,ignore.case = T)

texmaterial_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                           texmaterial_ids_sale_1734)
texmaterial_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                           texmaterial_ids_sale_1834)

# excluding ads already present in other categories from this category (often as description)
other_categories_ids_1734 <- c(clothing_ids_main_1734, shoes_ids_sale_1734, accessoires_ids_main_1734, householdtex_ids_main_1734)

texmaterial_subset_single_1734 <- corpus_subset(texmaterial_subset_sale_1734, docvars(texmaterial_subset_sale_1734, "id") %notin%
                                           other_categories_ids_1734)

texmaterial_ids_single_1734 <- texmaterial_subset_single_1734$id

other_categories_ids_1834 <- c(clothing_ids_main_1834, shoes_ids_sale_1834, accessoires_ids_main_1834, householdtex_ids_main_1834)

texmaterial_subset_single_1834 <- corpus_subset(texmaterial_subset_sale_1834, docvars(texmaterial_subset_sale_1834, "id") %notin%
                                           other_categories_ids_1834)
texmaterial_ids_single_1834 <- texmaterial_subset_single_1834$id


# Unspecified Cloth and Fabric
cloth <- tagfilter_cloth()

cloth_ids_sale_1734 <- cloth$filtrate(sale_1734,ignore.case = T)
cloth_ids_sale_1834 <- cloth$filtrate(sale_1834,ignore.case = T)

cloth_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     cloth_ids_sale_1734)
cloth_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     cloth_ids_sale_1834)


# Yarn
yarn <- tagfilter_yarn()

yarn_ids_sale_1734 <- yarn$filtrate(sale_1734,ignore.case = T)
yarn_ids_sale_1834 <- yarn$filtrate(sale_1834,ignore.case = T)

yarn_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                    yarn_ids_sale_1734)
yarn_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    yarn_ids_sale_1834)

# Bedding
bedding <- tagfilter_bedding()

bedding_ids_sale_1734 <- bedding$filtrate(sale_1734,ignore.case = T)
bedding_ids_sale_1834 <- bedding$filtrate(sale_1834,ignore.case = T)

bedding_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         bedding_ids_sale_1734)
bedding_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         bedding_ids_sale_1834)

# Carpet and Drapes
carpet <- tagfilter_carpet()

carpet_ids_sale_1734 <- carpet$filtrate(sale_1734,ignore.case = T)
carpet_ids_sale_1834 <- carpet$filtrate(sale_1834,ignore.case = T)

carpet_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         carpet_ids_sale_1734)
carpet_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         carpet_ids_sale_1834)

# Tablelinen
tablelinen <- tagfilter_tablelinen()

tablelinen_ids_sale_1734 <- tablelinen$filtrate(sale_1734,ignore.case = T)
tablelinen_ids_sale_1834 <- tablelinen$filtrate(sale_1834,ignore.case = T)

tablelinen_subset_sale_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         tablelinen_ids_sale_1734)
tablelinen_subset_sale_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         tablelinen_ids_sale_1834)



# bundling of textile sale ads in main categories

clothing_ids_main_1734 <- c(clothing_ids_sale_1734, sleepwear_ids_sale_1734, uniform_ids_sale_1734, underwear_ids_sale_1734, outerwear_ids_sale_1734, costume_ids_sale_1734)
clothing_main_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                                           clothing_ids_main_1734)

shoes_ids_sale_1734
shoes_main_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                      shoes_ids_sale_1734)

accessoires_ids_main_1734 <- c(hand_ids_sale_1734, handkerchief_ids_sale_1734, neck_ids_sale_1734, headdress_ids_sale_1734)
accessoires_main_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                      accessoires_ids_main_1734)

fabric_ids_main_1734 <- c(texmaterial_ids_single_1734, cloth_ids_sale_1734, yarn_ids_sale_1734)
fabric_main_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                      fabric_ids_main_1734)

householdtex_ids_main_1734 <- c(bedding_ids_sale_1734, carpet_ids_sale_1734, tablelinen_ids_sale_1734)
householdtex_main_1734 <- corpus_subset(corpus_1734, docvars(corpus_1734, "id") %in%
                                      householdtex_ids_main_1734)

clothing_main_1734
shoes_main_1734
accessoires_main_1734
fabric_main_1734
householdtex_main_1734

clothing_ids_main_1834 <- c(clothing_ids_sale_1834, sleepwear_ids_sale_1834, uniform_ids_sale_1834, underwear_ids_sale_1834, outerwear_ids_sale_1834, costume_ids_sale_1834)
clothing_main_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                      clothing_ids_main_1834)

shoes_ids_sale_1834
shoes_main_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                   shoes_ids_sale_1834)

accessoires_ids_main_1834 <- c(hand_ids_sale_1834, handkerchief_ids_sale_1834, neck_ids_sale_1834, headdress_ids_sale_1834)
accessoires_main_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                         accessoires_ids_main_1834)

fabric_ids_main_1834 <- c(texmaterial_ids_single_1834, cloth_ids_sale_1834, yarn_ids_sale_1834)
fabric_main_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                    fabric_ids_main_1834)

householdtex_ids_main_1834 <- c(bedding_ids_sale_1834, carpet_ids_sale_1834, tablelinen_ids_sale_1834)
householdtex_main_1834 <- corpus_subset(corpus_1834, docvars(corpus_1834, "id") %in%
                                          householdtex_ids_main_1834)

clothing_main_1834
shoes_main_1834
accessoires_main_1834
fabric_main_1834
householdtex_main_1834


# Total textile ads recognised in sale ads

textiles_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all,
                                       costume_ids_all, shoes_ids_all, handkerchief_ids_all, hand_ids_all, neck_ids_all,
                                       headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                                       bedding_ids_all, carpet_ids_all, tablelinen_ids_all))

textiles_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all,
                                       costume_ids_all, shoes_ids_all, handkerchief_ids_all, hand_ids_all, neck_ids_all,
                                       headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                                       bedding_ids_all, carpet_ids_all, tablelinen_ids_all))

textiles_aut_sale_1734
textiles_aut_sale_1834


## looking at use of advertising languages concerned with selection, politeness, service, quality and fashion
## in all as well as only textile and household ads in 1734 and 1834 by comparing to a dictionary (advertising_dict)


# dictionary for "Werbeanzeigen", words strongly suggesting advertising language and polite society
advertising_dict <- dictionary(list(selection = c("angelang*", "assortiert*", "auswahl*", "erfrisch*", "erinner*", "frisch*", "jüngst*","reichhaltig*", "sortiment*", "vollständig*", "befriedig*",
                                                  "verschieden*"),
                                    politeness = c("darf", "dienlich", "dürfe*", "ehre*", "freundlich*", "geehrt*", "geneigt*", "hochgeehrt*", "höflich*", "gütig*",
                                                   "verehrlich*", "zusprech*", "zusprich*", "zuspruch", "Interess*", "P(u|ü)blikum", "ergeben*", "gefällig*"),
                                    service = c("aufmerksam*", "bedien*", "befriedig*", "bequem*", "empfehl*", "empfhiehl*", "erinner*", "garantie*", "bestreb*", "Zutrauen*", "prompt*"),
                                    quality = c("besonder*", "besser", "best*", "einschlage*", "extra", "fein*", "frisch*", "gut*", "hübsch*", "gütig*",
                                                "schön*", "vorteil*", "vorzüglich*", "wohl*", "artig*", "bedeutend*", "sehr", "vorzüglich*", "versicher*"),
                                    fashion = c("begehrt*", "belieb*", "commod", "commoder", "commodes", "geschmack*", "mode*", "modi*", "neu*")))


library(lubridate)

docvars(sale_things_1734, "year") <- year(docvars(sale_things_1734, "date"))
docvars(sale_things_1734, "month") <- month(docvars(sale_things_1734, "date"))
docvars(sale_things_1734, "day") <- day(docvars(sale_things_1734, "date"))

docvars(sale_things_1734)

# tokenizing ads for corpus of 1734
token_sale_things_1734 <- sale_things_1734 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

summary(token_sale_things_1734)

# dictionary analysis for 1734
token_dict_1734 <- tokens_lookup(token_sale_things_1734, dictionary = advertising_dict)

token_dict_1734_dfma <- dfm(token_dict_1734)
topfeatures(token_dict_1734_dfma)

token_dict_1734_df <- convert(token_dict_1734_dfma,
        to = "data.frame")

write.csv(sale_things_1734, file = "data/sale_things_1734.csv", fileEncoding = "UTF-8", include_docvars = Y)
write.csv(token_dict_1734_dfma, file = "data/dict_allsale_1734_df.csv", fileEncoding = "UTF-8")

# plotting results of dictionary analysis for 1734
matplot(token_dict_1734_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topleft', col = 1:5, legend = c('selection', 'politeness', "service", "quality", "fashion"), lty = 1, bg = 'white')

# exporting dictionary analyisis to csv
token_dict_1734_df <- convert(token_dict_1734_dfma, to = "data.frame")
write.csv2(token_dict_1734_df, file = "data/dict_allsale_1734_df.csv", fileEncoding = "UTF-8")

# for 1834

docvars(sale_things_1834, "year") <- year(docvars(sale_things_1834, "date"))
docvars(sale_things_1834, "month") <- month(docvars(sale_things_1834, "date"))
docvars(sale_things_1834, "day") <- day(docvars(sale_things_1834, "date"))

docvars(sale_things_1834)

# tokenizing ads for corpus of 1834
token_sale_things_1834 <- sale_things_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

summary(token_sale_things_1834)

# dictionary analysis for 1834
token_dict_1834 <- tokens_lookup(token_sale_things_1834, dictionary = advertising_dict)

token_dict_1834_dfma <- dfm(token_dict_1834)
topfeatures(token_dict_1834_dfma)

token_dict_1834_df <- convert(token_dict_1834_dfma,
                              to = "data.frame")

write.csv(sale_things_1834, file = "data/sale_things_1834.csv", fileEncoding = "UTF-8")
write.csv(token_dict_1834_df, file = "data/dict_allsale_1834_df.csv", fileEncoding = "UTF-8")

# plotting results of dictionary analysis for 1834
matplot(token_dict_1834_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topleft', col = 1:5, legend = c('selection', 'politeness', "service", "quality", "fashion"), lty = 1, bg = 'white')

# exporting dictionary analyisis to csv
token_dict_1834_df <- convert(token_dict_1834_dfma, to = "data.frame")
write.csv2(token_dict_1834_df, file = "data/dict_allsale_1834_df.csv", fileEncoding = "UTF-8")




# all sale ads, important categories
# 1734

food_main_1734 <- corpus_subset(sale_1734, grepl("lebensmittel", adcontent))
jewellery_main_1734 <- corpus_subset(sale_1734, grepl("schmuck", adcontent))

food_main_ids_1734 <- food_main_1734$id

jewellery_main_ids_1734 <- c(jewellery_main_1734$id, jewellery_ids_1734)

furniture_main_ids_1734 <- c(bed_ids_1734, chair_ids_1734, cabinet_ids_1734, stove_ids_1734, mirror_ids_1734,
                             timepiece_ids_1734, table_ids_1734, bureau_ids_1734, upholstery_ids_1734, garden_ids_1734)
other_household_main_ids_1734 <- c(tableware_ids_1734, toy_ids_1734, game_ids_1734, kitchen_ids_1734, lighting_ids_1734,
                                   instrument_ids_1734, wallpaper_ids_1734, cutlery_ids_1734, divider_ids_1734,
                                   petobject_ids_1734, domestic_ids_1734, homedeco_ids_1734, art_ids_1734, bathobject_ids_1734,
                                   mischousehold_ids_1734, antique_ids_1734)
carriages_main_ids_1734 <- c(carriage_ids_1734, pushchair_ids_1734, trolley_ids_1734)

riding_main_ids_1734 <- c(riding_ids_1734)

storage_main_ids_1734 <- c(storage_ids_1734, barrel_ids_1734, container_ids_1734, wineobject_ids_1734)

buildingmaterials_main_ids_1734 <- c(building_ids_1734, wood_ids_1734, well_ids_1734)

rawmaterials_main_ids_1734 <- c(animalraw_ids_1734, plantraw_ids_1734)

health_main_ids_1734 <- c(health_ids_1734)

tools_main_ids_1734 <- c(tool_ids_1734, measure_ids_1734)

stationary_main_ids_1734 <- c(stationary_ids_1734)

tobacco_main_ids_1734 <- c(tobacco_ids_1734)

agriculture_main_ids_1734 <- c(hay_ids_1734, dung_ids_1734, plant_ids_1734, soil_ids_1734, agriculture_ids_1734, feed_ids_1734)

other_accessoires_main_ids_1734 <- c(suitcase_ids_1734, umbrella_ids_1734, bag_ids_1734, mercery_ids_1734, glasses_ids_1734, cane_ids_1734)

weapons_main_ids_1734 <- c(weapon_ids_1734)

other_objects_main_ids_1734 <- c(woodobject_ids_1734, naturalia_ids_1734, firestart_ids_1734, extinguisher_ids_1734, firework_ids_1734, key_ids_1734, measure_ids_1734,
                             rope_ids_1734, miscobject_ids_1734)

merchantequipment_main_ids_1734 <- c(shopequip_ids_1734, tavernobject_ids_1734)

textiles_main_ids_1734 <- c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all,
                                        costume_ids_all, shoes_ids_all, handkerchief_ids_all, hand_ids_all, neck_ids_all,
                                        headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                                        bedding_ids_all, carpet_ids_all, tablelinen_ids_all)

all_main_ids_1734 <- c(food_main_ids_1734, jewellery_main_ids_1734, furniture_main_ids_1734, other_household_main_ids_1734, carriages_main_ids_1734,
                       riding_main_ids_1734, storage_main_ids_1734, buildingmaterials_main_ids_1734, rawmaterials_main_ids_1734, health_main_ids_1734,
                       tools_main_ids_1734, stationary_main_ids_1734, tobacco_main_ids_1734, agriculture_main_ids_1734, other_accessoires_main_ids_1734,
                       weapons_main_ids_1734, other_objects_main_ids_1734, merchantequipment_main_ids_1734, textiles_main_ids_1734)

# subcorpora of main categories

furniture_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   furniture_main_ids_1734)
other_household_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                             other_household_main_ids_1734)
carriages_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                             carriages_main_ids_1734)
riding_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       riding_main_ids_1734)
storage_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                             storage_ids_1734)
buildingmaterials_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                             buildingmaterials_main_ids_1734)
rawmaterials_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       rawmaterials_main_ids_1734)
health_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          health_main_ids_1734)
tools_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                          tools_main_ids_1734)
stationary_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                        stationary_main_ids_1734)
tobacco_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                     tobacco_main_ids_1734)
agriculture_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                         agriculture_main_ids_1734)
other_accessoires_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   other_accessoires_main_ids_1734)
weapons_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                   weapons_main_ids_1734)
other_objects_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                       other_objects_main_ids_1734)
merchantequipment_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                               merchantequipment_main_ids_1734)
textiles_main_1734 <- corpus_subset(sale_1734, docvars(sale_1734, "id") %in%
                                               textiles_main_ids_1734)


furniture_main_1734
other_household_main_1734
carriages_main_1734
riding_main_1734
storage_main_1734
buildingmaterials_main_1734
health_main_1734
tools_main_1734
stationary_main_1734
tobacco_main_1734
agriculture_main_1734
other_accessoires_main_1734
instruments_main_1734
weapons_main_1734
other_objects_main_1734
merchantequipment_main_1734
food_main_1734
jewellery_main_1734
textiles_main_1734
rawmaterials_main_1734

missed_all_main_1734 <- corpus_subset(sale_things_1734, docvars(sale_things_1734, "id") %notin%
                                        all_main_ids_1734)

write.csv2(missed_all_main_1734, file = "data/missed_all_main_1734s.csv", fileEncoding = "UTF-8")

#1834

food_main_1834 <- corpus_subset(sale_1834, grepl("lebensmittel", adcontent))
jewellery_main_1834 <- corpus_subset(sale_1834, grepl("schmuck", adcontent))

food_main_ids_1834 <- food_main_1834$id

jewellery_main_ids_1834 <- c(jewellery_main_1834$id, jewellery_ids_1834)

furniture_main_ids_1834 <- c(bed_ids_1834, chair_ids_1834, cabinet_ids_1834, stove_ids_1834, mirror_ids_1834,
                             timepiece_ids_1834, table_ids_1834, bureau_ids_1834, upholstery_ids_1834, garden_ids_1834)
other_household_main_ids_1834 <- c(tableware_ids_1834, toy_ids_1834, game_ids_1834, kitchen_ids_1834, lighting_ids_1834,
                                   instrument_ids_1834, wallpaper_ids_1834, cutlery_ids_1834, divider_ids_1834,
                                   petobject_ids_1834, domestic_ids_1834, homedeco_ids_1834, art_ids_1834, bathobject_ids_1834,
                                   mischousehold_ids_1834, antique_ids_1834)
carriages_main_ids_1834 <- c(carriage_ids_1834, pushchair_ids_1834, trolley_ids_1834)

riding_main_ids_1834 <- c(riding_ids_1834)

storage_main_ids_1834 <- c(storage_ids_1834, barrel_ids_1834, container_ids_1834, wineobject_ids_1834)

buildingmaterials_main_ids_1834 <- c(building_ids_1834, wood_ids_1834, well_ids_1834)

rawmaterials_main_ids_1834 <- c(animalraw_ids_1834, plantraw_ids_1834)

health_main_ids_1834 <- c(health_ids_1834)

tools_main_ids_1834 <- c(tool_ids_1834, measure_ids_1834)

stationary_main_ids_1834 <- c(stationary_ids_1834)

tobacco_main_ids_1834 <- c(tobacco_ids_1834)

agriculture_main_ids_1834 <- c(hay_ids_1834, dung_ids_1834, plant_ids_1834, soil_ids_1834, agriculture_ids_1834, feed_ids_1834)

other_accessoires_main_ids_1834 <- c(suitcase_ids_1834, umbrella_ids_1834, bag_ids_1834, mercery_ids_1834, glasses_ids_1834, cane_ids_1834)

weapons_main_ids_1834 <- c(weapon_ids_1834)

other_objects_main_ids_1834 <- c(woodobject_ids_1834, naturalia_ids_1834, firestart_ids_1834, extinguisher_ids_1834, firework_ids_1834, key_ids_1834, measure_ids_1834,
                                 rope_ids_1834, miscobject_ids_1834)

merchantequipment_main_ids_1834 <- c(shopequip_ids_1834, tavernobject_ids_1834)

textiles_main_ids_1834 <- c(clothing_ids_all, sleepwear_ids_all, uniform_ids_all, underwear_ids_all, outerwear_ids_all,
                            costume_ids_all, shoes_ids_all, handkerchief_ids_all, hand_ids_all, neck_ids_all,
                            headdress_ids_all, texmaterial_ids_all, cloth_ids_all, yarn_ids_all,
                            bedding_ids_all, carpet_ids_all, tablelinen_ids_all)

all_main_ids_1834 <- c(food_main_ids_1834, jewellery_main_ids_1834, furniture_main_ids_1834, other_household_main_ids_1834, carriages_main_ids_1834,
                       riding_main_ids_1834, storage_main_ids_1834, buildingmaterials_main_ids_1834, rawmaterials_main_ids_1834, health_main_ids_1834,
                       tools_main_ids_1834, stationary_main_ids_1834, tobacco_main_ids_1834, agriculture_main_ids_1834, other_accessoires_main_ids_1834,
                       weapons_main_ids_1834, other_objects_main_ids_1834, merchantequipment_main_ids_1834, textiles_main_ids_1834)

# subcorpora of main categories

furniture_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       furniture_main_ids_1834)
other_household_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                             other_household_main_ids_1834)
carriages_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                       carriages_main_ids_1834)
riding_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    riding_main_ids_1834)
storage_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     storage_ids_1834)
buildingmaterials_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                               buildingmaterials_main_ids_1834)
rawmaterials_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                          rawmaterials_main_ids_1834)
health_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                    health_main_ids_1834)
tools_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                   tools_main_ids_1834)
stationary_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                        stationary_main_ids_1834)
tobacco_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     tobacco_main_ids_1834)
agriculture_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                         agriculture_main_ids_1834)
other_accessoires_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                               other_accessoires_main_ids_1834)
weapons_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                     weapons_main_ids_1834)
other_objects_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                           other_objects_main_ids_1834)
merchantequipment_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                               merchantequipment_main_ids_1834)
textiles_main_1834 <- corpus_subset(sale_1834, docvars(sale_1834, "id") %in%
                                      textiles_main_ids_1834)


furniture_main_1834
other_household_main_1834
carriages_main_1834
riding_main_1834
storage_main_1834
buildingmaterials_main_1834
health_main_1834
tools_main_1834
stationary_main_1834
tobacco_main_1834
agriculture_main_1834
other_accessoires_main_1834
instruments_main_1834
weapons_main_1834
other_objects_main_1834
merchantequipment_main_1834
food_main_1834
jewellery_main_1834
textiles_main_1834
rawmaterials_main_1834

missed_all_main_1834 <- corpus_subset(sale_things_1834, docvars(sale_things_1834, "id") %notin%
                                        all_main_ids_1834)

write.csv2(missed_all_main_1834, file = "data/missed_all_main_1834s.csv", fileEncoding = "UTF-8")

# crossing secondhand and textile filters


secondhand_clothing_1734 <- corpus_subset(clothing_main_1734, docvars(clothing_main_1734, "id") %in%
                                            secondhand_ids_1734)
secondhand_clothing_1834 <- corpus_subset(clothing_main_1834, docvars(clothing_main_1834, "id") %in%
                                            secondhand_ids_1834)

secondhand_shoes_1734 <- corpus_subset(shoes_main_1734, docvars(shoes_main_1734, "id") %in%
                                            secondhand_ids_1734)
secondhand_shoes_1834 <- corpus_subset(shoes_main_1834, docvars(shoes_main_1834, "id") %in%
                                       secondhand_ids_1834)

secondhand_accessoires_1734 <- corpus_subset(accessoires_main_1734, docvars(accessoires_main_1734, "id") %in%
                                             secondhand_ids_1734)
secondhand_accessoires_1834 <- corpus_subset(accessoires_main_1834, docvars(accessoires_main_1834, "id") %in%
                                             secondhand_ids_1834)

secondhand_fabric_1734 <- corpus_subset(accessoires_main_1734, docvars(accessoires_main_1734, "id") %in%
                                        secondhand_ids_1734)
secondhand_fabric_1834 <- corpus_subset(accessoires_main_1834, docvars(accessoires_main_1834, "id") %in%
                                        secondhand_ids_1834)

secondhand_householdtex_1734 <- corpus_subset(householdtex_main_1734, docvars(householdtex_main_1734, "id") %in%
                                              secondhand_ids_1734)
secondhand_householdtex_1834 <- corpus_subset(householdtex_main_1834, docvars(householdtex_main_1834, "id") %in%
                                              secondhand_ids_1834)
