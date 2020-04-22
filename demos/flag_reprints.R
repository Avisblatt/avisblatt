# library(avisblatt)
# During Development rather run
devtools::load_all()
library(jsonlite)
library(quanteda)

### Flagging reprints in the groundtruth years,
#' distinguishing
#' A: (theoretically exactly identical) REPRINTS in the next issue
#' from
#' B: (often [slightly] rephrased) RENEWALS in later issues


corpus_raw <- avis_create_corpus("data/*.csv")

#' throw out ads with short garbage as text
#' (less than 10 characters), as they tend to crash
#' subsequent functions, and are rubbish anyway
corpus_clean <- corpus_subset(corpus_raw,
                           nchar(texts(corpus_raw)) > 10)

#' go through all issue dates, for each
#' compare all adds in current issue
#' to all ads in previousissues
issuedates <-sort(unique(corpus_clean$date))
results <- matrix(,0,2)
colnames(results) <- c("original","distance")

for (i in 2:length(issuedates)){
  date_current_issue <- issuedates[i]
  date_previous_issue <- issuedates[i-1]

  corpus_current <- corpus_subset(corpus_clean,
                               (corpus_clean$date %in% issuedates[i]))
  corpus_previous <- corpus_subset(corpus_clean,
                                (corpus_clean$date %in% issuedates[i-1]))

  current_dfm <- dfm(corpus_current)
  previous_dfm <- dfm(corpus_previous)

  uncorr_dist <- textstat_dist(current_dfm, previous_dfm)

#' textstat_dist as a metric for reprints
#' does not work independent of ad text length:
#' reprints of long ads can have from 5 to 8, while
#' 2 short ads with dist = 2.5 not remotely similiar.
#' Dividing dist by log(adlength*const) much better
#' const = 1/7 with threshhold = 1.51 works best
  current_lengths <- as.vector(nchar(texts(corpus_current)))
  previous_lengths <- as.vector(nchar(texts(corpus_previous)))
  m_c <- matrix(current_lengths,nrow(uncorr_dist),ncol(uncorr_dist))
  m_p <- matrix(previous_lengths,nrow(uncorr_dist),ncol(uncorr_dist),byrow = TRUE)
  distance <- uncorr_dist / log(pmin(m_c,m_p)/7)

  results_current <- as.matrix(apply(distance, 1, function(x) {ifelse(min(x) < 1.51, colnames(distance)[which.min(x)], FALSE)}))
  results_current <- cbind(results_current,apply(distance, 1, min))
  results_current <- subset(results_current, results_current[,1]!=FALSE)

  results <- rbind(results, results_current)
}


#' which 'original' ads turn out to be a reprint themselves?
#' for those cases in which the original B
#' of a reprint C is itself a reprint of A,
#' change  (C,B) to (C,A)
r <- subset(results, results[,1] %in% rownames(results))

while (length(r[,1]) > 0){
  for (z in 1:length(r[,1])){
    results[rownames(r)[z],1] <- results[r[z,1],1]
    # hm, change the distance metric as well? to what value?
  }
  r <- subset(results, results[,1] %in% rownames(results))
}


write_json(results,
           path =  "data/ids_of_reprints.json")


#' ----------------------------------------------
#' checking results
#' ----------------------------------------------
dim(results)

# Here the 50 most dissimiliar pairs of ads
# still included as reprints (dist < 1.51)
# to check on the false positives
results_ordered <- results[order(results[,2]), ]
for (j in (nrow(results_ordered)-50):nrow(results_ordered)){
  cat(texts(corpus_clean[rownames(results_ordered)[j]]),
      "---------",
      texts(corpus_clean[results_ordered[j]]),
      "---------",
      results_ordered[j,2],
      "_____________________________________________________",
      sep="\n")
}
