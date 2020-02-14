source("R/utils.R")
source("R/tagfilters_main.R")
labor <- tagfilter_labor()

labor_ids <- labor$filtrate(corpus_1834,ignore.case = F)
# Validation of Filters ----
## 2x2 Matrix containing number of ads
## found by filter AND hc ("yay!") | found by hc but not the filter ("we will get them, too")
## found by filter AND NOT by HC ("oops") | neither filter nor hc
o <- validate_filter(corpus_1834, labor_ids,
                     search_col = "adcontent",
                     pattern = "arbeit")

o$filter_T_hc_F

labor_TF <- get_text_by_id(corpus_1834, o$filter_T_hc_F)
labor_TF_corp <- get_subcorpus_by_id(corpus_1834,
                                     o$filter_T_hc_F)

labor_TF_tok <- labor_TF_corp %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(c(avis_stop(), stopwords("de")))


labor_TF_tok %>%
  dfm() %>%
  textstat_frequency()

labor_TF_tok %>%
  dfm() %>%
  topfeatures()

tok_count <- tibble(
  num_id = 1:length(labor_TF_tok),
  token_count = ntoken(labor_TF_tok),
  unique_token_count = ntype(labor_TF_tok)
)




labor_TF_dfm <- labor_TF_tok %>% dfm()
labor_TF_dfm %>%
  textplot_wordcloud(max_words = 150)


labor_TF_ngram <- labor_TF_corp %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(c(avis_stop(), stopwords("de"))) %>%
  tokens(ngrams = 2) %>%
  dfm()

textstat_frequency(labor_TF_ngram)

tok_count <- tibble(
  num_id = 1:length(tok_1834),
  token_count = ntoken(tok_1834),
  unique_token_count = ntype(tok_1834)
)

xx <- tidyr::pivot_longer(tok_count,
                          cols = c(token_count,
                                   unique_token_count))

gg <- ggplot(xx, aes(value))
gg + geom_histogram(binwidth = 5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  xlim(0, 200) +
  facet_wrap(~name)

# More than 50 Tokens seems to be special


