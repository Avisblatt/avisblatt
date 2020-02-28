# Load libraries and source functions
# in pre-package state
library(readtext)
library(quanteda)
library(textcat)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
source("R/avis_stop.R")
source("R/ocr_corrections.R")
source("R/tagfilters_utils.R")
source("R/tagfilters_household.R")
source("R/tagfilters_quality.R")
source("R/tagfilters_main.R")
source("R/cleaners.R")
source("R/validate_filters.R")


# dictionary for "Werbeanzeigen", words strongly suggesting advertising language and polite society
advertising_dict <- dictionary(list(selection = c("angelang*", "assortiert*", "auswahl*", "erfrisch*", "erinner*", "frisch*", "jüngst*","reichhaltig*", "sortiment*", "vollständig*", "befriedig*",
                                                  "verschieden*"),
                                    politeness = c("darf", "dienlich", "dürfe*", "ehre*", "freundlich*", "geehrt*", "geneigt*", "hochgeehrt*", "höflich*", "gütig*",
                                                   "verehrlich*", "zusprech*", "zusprich*", "zuspruch", "Interess*", "P(u|ü)blikum", "ergeben*", "gefällig*"),
                                    service = c("aufmerksam*", "bedien*", "befriedig*", "bequem*", "empfehl*", "empfhiehl*", "erinner*", "garantie*", "bestreb*", "Zutrauen*", "prompt*"),
                                    quality = c("besonder*", "besser", "best*", "einschlage*", "extra", "fein*", "frisch*", "gut*", "hübsch*", "gütig*",
                                                "schön*", "vorteil*", "vorzüglich*", "wohl*", "artig*", "bedeutend*", "sehr", "vorzüglich*", "versicher*"),
                                    fashion = c("begehrt*", "belieb*", "commod", "commoder", "commodes", "geschmack*", "mode*", "modi*", "neu*")))

# preparing corpus for 1734
avis_1734 <- readtext("data/groundtruth1734.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1734$text <- correct_ocr(avis_1734$text)

avis_1734$text <- sapply(avis_1734$text, tolower)

ids_by_lang <- fromJSON("data/ids_by_lang.json")

corpus_1734_all <- corpus(avis_1734,
                          docid_field = "doc_id")

corpus_1734 <- corpus_subset(corpus_1734_all,
                             (docvars(corpus_1734_all,"id") %in%
                                ids_by_lang$de))

docvars(corpus_1734, "year") <- year(docvars(corpus_1734, "date"))
docvars(corpus_1734, "month") <- month(docvars(corpus_1734, "date"))
docvars(corpus_1734, "day") <- day(docvars(corpus_1734, "date"))

# tokenizing ads for corpus of 1734
token_1734 <- corpus_1734 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

summary(token_1734)

# dictionary analysis for 1734
token_dict_1734 <- tokens_lookup(token_1734, dictionary = advertising_dict)

token_dict_1734_dfma <- dfm(token_dict_1734)
topfeatures(token_dict_1734_dfma)

# plotting results of dictionary analysis for 1734
matplot(token_dict_1734_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topleft', col = 1:2, legend = c('all', 'selection'), lty = 1, bg = 'white')


# preparing corpus for 1834
avis_1834 <- readtext("data/groundtruth1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

avis_1834$text <- correct_ocr(avis_1834$text)

avis_1834$text <- sapply(avis_1834$text, tolower)

ids_by_lang <- fromJSON("data/ids_by_lang.json")
corpus_1834_all <- corpus(avis_1834,
                          docid_field = "doc_id")
corpus_1834 <- corpus_subset(corpus_1834_all,
                             (docvars(corpus_1834_all,"id") %in%
                                ids_by_lang$de))

docvars(corpus_1834, "year") <- year(docvars(corpus_1834, "date"))
docvars(corpus_1834, "month") <- month(docvars(corpus_1834, "date"))
docvars(corpus_1834, "day") <- day(docvars(corpus_1834, "date"))

# tokenizing ads for corpus of 1834
token_1834 <- corpus_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

summary(token_1834)

# dictionary analysis for 1834
token_dict_1834 <- tokens_lookup(token_1834, dictionary = advertising_dict)

token_dict_1834_dfma <- dfm(token_dict_1834)
topfeatures(token_dict_1834_dfma)

# plotting results of dictionary analysis for 1834
matplot(token_dict_1834_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topleft', col = 1:2, legend = c('advertising words'), lty = 1, bg = 'white')

# corpora for 1734 and 1834 with only textiles for sale (manually classified)
textiles_1734 <- corpus_subset(corpus_1734, grepl("01textilien", adcontent) & grepl("01kauf", finance))
textiles_1834 <- corpus_subset(corpus_1834, grepl("01textilien", adcontent) & grepl("01kauf", finance))

# corpora for 1734 and 1834 with only textiles for sale (automatically classified)
textiles_aut_sale_1734
textiles_aut_sale_1834

# tokenizing of textile corpora
token_textiles_1734 <- textiles_aut_sale_1734 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

token_textiles_1834 <- textiles_aut_sale_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

# dictionary analysis for textile corpora
token_tex_dict_1734 <- tokens_lookup(token_textiles_1734, dictionary = advertising_dict)
token_tex_dict_1734_dfma <- dfm(token_tex_dict_1734)
topfeatures(token_tex_dict_1734_dfma)

token_tex_dict_1834 <- tokens_lookup(token_textiles_1834, dictionary = advertising_dict)
token_tex_dict_1834_dfma <- dfm(token_tex_dict_1834)
topfeatures(token_tex_dict_1834_dfma)

# exporting dictionary analyisis to csv
token_tex_dict_1734_df <- convert(token_tex_dict_1734_dfma, to = "data.frame")
write.csv2(token_tex_dict_1734_df, file = "data/token_tex_dict_1734_df.csv", fileEncoding = "UTF-8")

token_tex_dict_1834_df <- convert(token_tex_dict_1834_dfma, to = "data.frame")
write.csv2(token_tex_dict_1834_df, file = "data/token_tex_dict_1834_df.csv", fileEncoding = "UTF-8")

# plotting results of dictionary analysis for textile corpora
matplot(token_tex_dict_1734_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topright', col = 1:5, legend = c('selection', 'politeness', 'service', 'quality', 'fashion'), lty = 1, bg = 'white')

matplot(token_tex_dict_1834_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topright', col = 1:5, legend = c('selection', 'politeness', 'service', 'quality', 'fashion'), lty = 1, bg = 'white')


# corpora for 1734 and 1834 with only household for sale (manually classified)
household_1734 <- corpus_subset(corpus_1734, grepl("hausrat", adcontent) & grepl("01kauf", finance))
household_1834 <- corpus_subset(corpus_1834, grepl("hausrat", adcontent) & grepl("01kauf", finance))

# corpora for 1734 and 1834 with only household for sale (automatically classified)
household_aut_sale_1734
household_aut_sale_1834

# tokenizing of household corpora
token_household_1734 <- household_aut_sale_1734 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

token_household_1834 <- household_aut_sale_1834 %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(avis_stop())

# dictionary analysis for housetile corpora
token_house_dict_1734 <- tokens_lookup(token_household_1734, dictionary = advertising_dict)
token_house_dict_1734_dfma <- dfm(token_house_dict_1734)
topfeatures(token_house_dict_1734_dfma)

token_house_dict_1834 <- tokens_lookup(token_household_1834, dictionary = advertising_dict)
token_house_dict_1834_dfma <- dfm(token_house_dict_1834)
topfeatures(token_house_dict_1834_dfma)

# exporting dictionary analyisis to csv
token_house_dict_1734_df <- convert(token_house_dict_1734_dfma, to = "data.frame")
write.csv2(token_house_dict_1734_df, file = "data/token_house_dict_1734_df.csv", fileEncoding = "UTF-8")

token_house_dict_1834_df <- convert(token_house_dict_1834_dfma, to = "data.frame")
write.csv2(token_house_dict_1834_df, file = "data/token_house_dict_1834_df.csv", fileEncoding = "UTF-8")

# plotting results of dictionary analysis for housetile corpora
matplot(token_house_dict_1734_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topright', col = 1:5, legend = c('selection', 'politeness', 'service', 'quality', 'fashion'), lty = 1, bg = 'white')

matplot(token_house_dict_1834_dfma, type = 'h', xaxt = 'n', lty = 1, ylab = 'Frequency')
grid()
legend('topright', col = 1:5, legend = c('selection', 'politeness', 'service', 'quality', 'fashion'), lty = 1, bg = 'white')
