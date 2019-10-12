library(readxl)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)


create_tokenized_tbl <- function(dta, token_size = 2){
  dta %>%
    unnest_tokens(ngram, ad_content,
                  token = "ngrams",
                  n = token_size)
  }

show_ad <- function(id_search, dta = min_1834){
  dta %>%
    filter(id %in% id_search) %>%
    select(ad_content) %>%
    lapply(as.character)


}


# basel specific stop words
bsl_stop <- c("in", "der", "die", "das","bis","no",
              "dem", "den", "im", "und", "e",
              "oder","auch","um", "zu", "so", "an",
              "wie", "mit", "von", "ein", "einer",
              "einen", "eine", "eines","einem",
              "er", "sie", "als", "ist", "sich",
              "es","auf", "für" , "am", "man",
              "sind", "werden", "bey","des","à",
              "et","a","wird", "haben", "gegen",
              "habe","dr","pr","le", "de","st",
              "diese","kann", "fr","dazu", "seyn",
              "mir","vor","u","da","la")

# TODO: remove totally empty ads !
# there might be empty ads !!! check this
# show_ad("ca0b9ae-9202-5f1d-be89-4e7f62d9a630/a22")
ads_1834 <- read_xlsx("data/ads_1834.xlsx")
ads_1834 <- ads_1834 %>%
  rename("id" = "@id", "ad_content" = "text") %>%
  mutate(ad_content = gsub("</?[^>]+>","", ad_content),
         id = gsub("https://avisblatt.freizo.org/iiif/anno/","", id)) %>%
  mutate(ad_content = gsub("- ", "", ad_content)) %>%
  mutate(ad_content = gsub(" [a-zA-z]{1} |^[a-zA-z]{1} "," ", ad_content))


# minimal clean set 1834 ads
# strip html, rename cols
# for better handling
min_1834 <- ads_1834 %>%
  select(id, ad_content)




# tf-idf on single words.
ad_words <- min_1834 %>%
  unnest_tokens(word, ad_content) %>%
  filter(!word %in% bsl_stop) %>%
  filter(grepl("[A-Za-z]", word)) %>%
  count(id, word, sort = TRUE)

total_words <- ad_words %>%
  group_by(id) %>%
  summarize(total = sum(n))

# note tf is simply n/total
ad_words <- left_join(ad_words, total_words) %>%
  group_by(id) %>%
  bind_tf_idf(word, id, n)

# important words by ad...
i_w_by_ad <- ad_words %>%
  filter(tf == max(tf))

ad_words_ungroup <-
  left_join(ad_words, total_words) %>%
  bind_tf_idf(word, id, n)


# bigrams alternative

ad_bigrams <- create_tokenized_tbl(min_1834)
ad_bigrams_clean <- ad_bigrams %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% bsl_stop) %>%
  filter(!word2 %in% bsl_stop) %>%
  filter(grepl("[A-Za-z]", word1)) %>%
  filter(grepl("[A-Za-z]", word2)) %>%
  mutate(ngram = paste(word1, word2, sep=" ")) %>%
  select(id, ngram) %>%
  count(id, ngram, sort = TRUE)

total_bigrams <- ad_bigrams_clean %>%
  group_by(id) %>%
  summarize(total = sum(n))


ad_bigrams_clean <- left_join(ad_bigrams_clean,
                              total_bigrams) %>%
  bind_tf_idf(ngram, id, n)


bi_tf_idf <- ad_bigrams_clean %>%
  arrange(desc(tf_idf)) %>%
  select(id,ngram,tf_idf) %>%
  print(n = 300)


max_tf_idf_by_ad <- bi_tf_idf %>%
  group_by(id) %>%
  filter(tf_idf == max(tf_idf)) %>%
  print(n = 300)


tail(max_tf_idf_by_ad)

show_ad("7d320071-6e91-558d-8e55-020ec1283b0f/a9")


# trigram alternative

ad_trigrams <- create_tokenized_tbl(min_1834,token_size = 3)
ad_trigrams_clean <- ad_trigrams %>%
  separate(ngram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% bsl_stop) %>%
  filter(!word2 %in% bsl_stop) %>%
  filter(!word3 %in% bsl_stop) %>%
  filter(grepl("[A-Za-z]", word1)) %>%
  filter(grepl("[A-Za-z]", word2)) %>%
  filter(nchar(word1) != 1) %>%
  filter(nchar(word2) != 1) %>%
  filter(nchar(word3) != 1) %>%
  mutate(ngram = paste(word1, word2, word3, sep=" ")) %>%
  select(id, ngram) %>%
  count(id, ngram, sort = TRUE)

total_trigrams <- ad_trigrams_clean %>%
  group_by(id) %>%
  summarize(total = sum(n))


ad_trigrams_clean <- left_join(ad_trigrams_clean,
                              total_trigrams) %>%
  bind_tf_idf(ngram, id, n)


tri_tf_idf <- ad_trigrams_clean %>%
  arrange(desc(tf_idf)) %>%
  select(id,ngram,tf_idf) %>%
  print(n = 300)


max_tri_tf_idf_by_ad <- tri_tf_idf %>%
  group_by(id) %>%
  filter(tf_idf == max(tf_idf)) %>%
  print(n = 300)

show_ad("43e46097-7730-59e3-91e2-64b8442fff22/a23")

# test find losaments :)
# yay 261 !
max_tri_tf_idf_by_ad %>%
  filter(grepl("losament|wohnung|zimmer|kammer|chambres",
               ngram, ignore.case = T))

show_ad("44c805ff-f6f7-5bcb-a661-181153617ca9/a35")

names(ads_1834)
ads_1834 %>%
  filter(adcontent == "08immo") %>%
  select(id) %>%
  print(n=300)

show_ad("3902d9ff-5cd2-544f-b214-dac146804a87/a27")






