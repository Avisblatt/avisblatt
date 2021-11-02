#' @importFrom jsonlite fromJSON toJSON
#' @importFrom quanteda corpus docvars texts
#' @importFrom quanteda.textstats textstat_dist
#' @import data.table
#' @export
write_collection <- function(x,
                             name_on_disk,
                             pretty_json = TRUE,
                             zip = FALSE){
  # sanity checks
  stopifnot(inherits(x, "Collection"))
  stopifnot(inherits(x, "R6"))

  # name of the two files
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  # meta information and data are treated separately
  # following the swissdata idea (github.com/swissdata/demo)
  # Meta information to JSON ##############################
  # turn all environments to lists
  # environments work with reference and thus better than lists
  # for in memory updates. lists are easier to handle when writing
  # to a JSON string.
  message("Processing data description...")
  dtcols <- setdiff(names(x$meta),"id")
  dt_chunks <- split(x$meta[, ..dtcols], as.factor(1:nrow(x$meta)))
  names(dt_chunks) <- x$meta$id
  writeLines(
    toJSON(dt_chunks, pretty = pretty_json,
           auto_unbox = TRUE,
           null = "null",
           na = "string"),
    meta_file
  )
  message(sprintf("Meta information written to %s",meta_file))

  dt <- data.table(
    id = names(x$corpus),
    text = as.character(x$corpus),
    docvars(x$corpus))
  fwrite(dt, file = data_file, quote = TRUE)
  message(sprintf("Data written to %s",data_file))

  if(zip){
    zip_file <- paste0(name_on_disk,".zip")
    zip(zip_file, c(data_file, meta_file))
    file.remove(data_file)
    file.remove(meta_file)
    message("Zip archive containing data and meta data created.")
  }
}

#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom quanteda corpus
#' @export
read_collection <- function(name_on_disk, just_meta = FALSE){
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  if(!just_meta){
    dt <- fread(data_file, encoding="UTF-8")
    crps <- corpus(dt, docid_field = "id",
                   text_field = "text")
  }

  mi <- fromJSON(meta_file)
  d <- data.table::rbindlist(mi)
  d[, id := names(mi)]
  d[, date := as.Date(date)]
  setcolorder(d, neworder = c("id",
                              setdiff(names(d),"id")))
  if(just_meta){
    collect <- Collection$new(NULL, d)
  } else {
    collect <- Collection$new(crps, d)
  }

  collect
}



#' Might want to deprecate this with quanteda 2.0
#' @export
get_text_by_id <- function(corp, ids, n = NULL,
                           identifier = "id",
                           txt = "texts"){
  tf <- docvars(corp, identifier) %in%
    ids

  if(is.null(n)) {
    return(corp$documents[,txt][tf])
  }

  corp$documents[,txt][tf][1:n]


}


#' Might want to deprecate this with quanteda 2.0
#' @export
get_subcorpus_by_id <- function(corp, ids, idvar = "id"){
  corpus_subset(corp,
                (docvars(corp, idvar) %in% ids)
  )
}

#' Clean up Human Tags (groundtruth)
#'
#' Removes leading numbers from tag and turn comma separated strings
#' into true character vectors.
#'
#' @param x character vector or list of tags.
#' @export
clean_manual_tags <- function(x){
  unique(gsub("(^[0-9]{2})(.+)","\\2",
              unlist(strsplit(x, ","))))
}



# convenience function to read & merge multiple years to one working collection
gather_yearly_collections <- function(AVIS_YEARS, just_meta = TRUE, path = "../avis-data/collections/yearly_"){
  AVIS_YEARS <- sort(as.numeric(AVIS_YEARS))

  # meta information
  meta_dt <- data.table()
  for (i in AVIS_YEARS){
    meta_file <- paste0(path, i, ".json")
    mi <- fromJSON(meta_file)
    d <- data.table::rbindlist(mi)
    d[, id := names(mi)]
    meta_dt <- rbind(meta_dt, d)
  }
  meta_dt[, date := as.Date(date)]
  setcolorder(meta_dt, neworder = c("id",
                              setdiff(names(meta_dt),"id")))

  # corpus
  dt <- data.table()
  if(!just_meta){
    for (i in AVIS_YEARS){
      data_file <- paste0(path, i, ".csv")
      dt <- rbind(dt, fread(data_file, encoding="UTF-8"))
    }
    crps <- corpus(dt, docid_field = "id",
                 text_field = "text")
    }

  if(just_meta){
    collect <- Collection$new(NULL, meta_dt)
  } else {
    collect <- Collection$new(crps, meta_dt)
  }
  collect
}



purge_spacing <- function(txtlist){
  splits <- strsplit(txtlist, "\\s")
  more_than_1 <- lapply(splits, grepl, pattern = "\\S{2,}")
  single_digit <- lapply(splits, grepl, pattern = "^\\d{1}$")
  not_single_letter <- mapply("|", single_digit, more_than_1, SIMPLIFY = FALSE)
  not_single_letter_shifted <- mapply(c, FALSE, not_single_letter, SIMPLIFY = FALSE)
  not_single_letter_shifted <- lapply(not_single_letter_shifted, head, -1, SIMPLIFY = FALSE)
  tf <- mapply("|", not_single_letter, not_single_letter_shifted, SIMPLIFY = FALSE)
  # cumsum is an elegant way to keep track of T/F swaps
  tfcs <- lapply(tf, cumsum)
  out <- mapply(split, splits, tfcs)
  # reconstruct
  out <- lapply(out, sapply, paste, collapse = "")
  unlist(lapply(out, paste, collapse = " "))
}



advert_distance <- function(corpus_a, corpus_b, consider_length_diff = FALSE){
  if (!is.corpus(corpus_a)|!is.corpus(corpus_b)){
    stop("This function requires (exactly) two quanteda corpora.")
  }

  # STEP 1
  # Use quanteda's textstat_dist to measure distance
  # -> lower values, smaller distance / greater similarity
  dfm_a <- tokens(corpus_a, remove_punct = TRUE, remove_numbers = TRUE) %>%
    dfm() %>% dfm_weight("prop")
  dfm_b <- tokens(corpus_b, remove_punct = TRUE, remove_numbers = TRUE) %>%
    dfm() %>% dfm_weight("prop")
  dist <- as.matrix(textstat_dist(dfm_a, dfm_b))

  # measure is not independent of ad length, correcting for length.
  # Result will be: dist <= 1 indicates reprint, > 1 otherwise
  lengths_a <- as.vector(ntoken(as.character(corpus_a),
                                remove_punct = TRUE, remove_numbers = TRUE))
  lengths_b <- as.vector(ntoken(as.character(corpus_b),
                                remove_punct = TRUE, remove_numbers = TRUE))
  m_a <- matrix(lengths_a, nrow(dist), ncol(dist))
  m_b <- matrix(lengths_b, nrow(dist), ncol(dist), byrow = TRUE)
  x <- m_a + m_b
  dist <- dist * sqrt(x + 0.5 - 2*log(x + 0.5))
  # this approx is fine below 16,
  # a bit too high for values around 25,
  # and increasingly too low for higher N.
  # Correct for that:
  dist <- dist * 25/(24+abs(sqrt(pmax(16, x))-5))

  # STEP 2 (optional):
  # if text lengths of two ads are dissimilar,
  # dist might show similarity where there isn't
  # (true especially for very long ads).
  # Rule out such cases in reprint detection
  # by adding 100 to similarity measure
  # (pushing it recognizably above the regular values)
  # if length difference is too big.
  # One token plus 5% of combined token number is okay,
  # any larger difference rules out reprint
  if (consider_length_diff){
    m_length_diff_above_threshold <- (abs(m_a-m_b)-1)/(m_a+m_b) > 0.05
    # multiplying logical matrix with numeric value turns FALSE into 0, TRUE into 1:
    dist <- dist + 100 * m_length_diff_above_threshold
  }
  dist
}


available_years <- function(){
 list.files("../avis-data/collections", pattern=".json") %>%
    substr(8, 11) %>%
    as.numeric
}


# Collection creation will not succeed if there are invalid regex in the dict.
# Check beforehand using this function
tf_integrity <- function(){
  ns <- ls(envir = asNamespace("avisblatt"))
  tfs <- ns[grepl("tagfilter_",ns)]
  l <- lapply(tfs, function(x){
    getFromNamespace(x, ns = "avisblatt")()
  })
  names(l) <- tfs
  for(i in 1:length(l)){
    tf <- l[[i]][2]$tagfilters
    for(t in c(tf$neg, tf$pos)){
      err <- try(gsub(t, "", "", perl = T), silent = T)
      if (err!=""){
        message("\nIn ", names(l[i]), ", there is an invalid regular expression:")
        print(t)
      }
    }
  }
}
