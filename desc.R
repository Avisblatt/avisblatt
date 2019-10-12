library(ggplot2)

# distribution of token count per document
tw <- total_words %>%
  filter(total < 150)
token_by_ad <- ggplot(tw, aes(total))
token_by_ad +
  geom_histogram() +
  stat_bin(binwidth = 5) +
  geom_vline(xintercept = 17, color = "blue") +
  geom_vline(xintercept = 27, color = "blue") +
  theme_minimal() +
  scale_x_continuous(breaks=c(0,20, 40, 60, 80,
                              100, 150, 200))


which.max(total_words$total)

summary(total_words$total)
# Longest ad
show_ad("2fc3ab80-2cf0-5680-a5c4-b9ba1820596e/a0")
