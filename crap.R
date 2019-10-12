
bigrams_1834 <- min_1834 %>%
  unnest_tokens(bigram, ad_content,
                token = "ngrams", n = 2)

trigrams_1834 <- min_1834 %>%
  unnest_tokens(trigram, ad_content,
                token = "ngrams", n = 3)

bigrams_1834 %>%
  count(bigram, sort = TRUE) %>%
  print(n = 100)

trigrams_1834 %>%
  count(trigram, sort = TRUE) %>%
  print(n = 100)

# remove some basic stopwords

bigrams_separated <- bigrams_1834 %>%
  separate(bigram, c("word1", "word2"),
           sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% bsl_stop) %>%
  filter(!word2 %in% bsl_stop)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts %>%
  print(n = 100)


bigrams_1834 %>%
  filter(id == "0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1")

out <- ads_1834 %>%
  filter(id == "0d8e807a-3ecf-50a5-a62d-24ff22f96bcd/a0") %>%
  select(ad_content)

out$ad_content

bigrams_1834 <- min_1834 %>%
  unnest_tokens(bigram, ad_content,
                token = "ngrams", n = 2)

p <- bigrams_1834 %>%
  count(bigram, id, sort = TRUE) %>%
  print(n = 100)


ads_1834 %>%
  filter()

















# close to 0 means words occur in many ads
# and are not very specific to ads
ad_words %>%
  arrange(desc(tf_idf)) %>%
  print(n = 100)

# when it's grouped we could easily
# extract the most important token / bigram
# which in turn could be cool tag
# maybe also the 2 most important
ad_words %>%
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  print(n = 300)

# maybe we could develop a manual mapping
# of tags such tags to categories.... ?
# then categorize the ads and do
# analysis by category
# also identifying pawn ads could already
# be possible


ggplot(ad_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE)

freq_by_rank <- ad_words %>%
  group_by(id) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank %>%
  print(n = 40)


freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = id)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


rank_subset <- freq_by_rank %>%
  filter(rank < 50,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)




















# word count
# bigrams
# trigrams
# tf-idf
# custom stop words

# custom stopword dict
# cause english dicts ain't worth *@!$ here.



















