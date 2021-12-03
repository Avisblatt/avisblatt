#' @export
rawdata_apply_ocr <- function(AVIS_YEARS = 1729:1844,
                              source_path = "../avis-databuffer/raw_data_uncorrected",
                              dest_path = "../avis-databuffer/raw_data_OCRed"){
  AVIS_YEARS <- intersect(AVIS_YEARS, list.files(source_path) %>% substr(6, 9) %>% as.numeric)
  for (i in AVIS_YEARS){
    fn <- sprintf("orig_%d.csv", i)
    dt <- fread(file.path(source_path, fn), encoding="UTF-8")
    dt$text <- correct_ocr(dt$text)
    fwrite(dt, file.path(dest_path, fn))
    message(sprintf("%d OCRed, written", i))
  }
}

#' @export
rawdata_header_creation <- function(AVIS_YEARS = 1729:1844,
                              source_path = "../avis-databuffer/raw_data_OCRed",
                              dest_path = "../avis-data/raw_data"){
  AVIS_YEARS <- intersect(AVIS_YEARS, list.files(source_path) %>% substr(6, 9) %>% as.numeric)
  for (i in AVIS_YEARS){
    fn <- sprintf("orig_%d.csv", i)
    data <- fread(file.path(source_path, fn), encoding="UTF-8")
    
    dt <- data[isheader == TRUE]
    dt$text <- gsub("[[:punct:][:blank:]]+", "", dt$text)
    crp <- corpus(dt, docid_field = "id")
    length(unique(dt$id))
    f <- get("tagfilter_merge_to_ad")
    hit_ids <- f()$filtrate(crp, return_corp = FALSE)
    data[shift(id) %in% hit_ids]$text <- paste(data[id %in% hit_ids]$text,
                                               data[shift(id) %in% hit_ids]$text,
                                               sep = " ")
    data <- data[!(id %in% hit_ids)]
    
    data[isheader == TRUE, "header_tag"] <- "unknown"
    for (tag in tf_header()){
      f <- get(sprintf("tagfilter_%s",tag))
      hit_ids <- f()$filtrate(crp, return_corp = FALSE)
      data[(isheader == TRUE & id %in% hit_ids), header_tag := tag]
    }
    
    by_header <- split(data, factor(cumsum(data$isheader)))
    
    by_header <- lapply(by_header, function(x){
      pos <- max(which(!is.na(x$header)))
      x$header_tag <- x$header_tag[pos]
      x
    })
    data <- rbindlist(by_header)
    
    fwrite(data, file.path(dest_path, fn))
    message(sprintf("Header for %d identified.", i))
  }
}

#' @export
rawdata_coll_creation <- function(AVIS_YEARS = 1729:1844,
                                    source_path = "../avis-data/raw_data",
                                    dest_path = "../avis-databuffer/collections_unflagged",
                                    gt_years = c(1734, 1754, 1774, 1834)){
  AVIS_YEARS <- intersect(AVIS_YEARS, list.files(source_path) %>% substr(6, 9) %>% as.numeric)
  # Prepare tagging:
  # find all function factories in the avisblatt package that follow
  # tagfilter_ naming conventions. Create functions dynamically and
  # apply all resulting filters to the collection and add tags accordingly.
  ns <- ls(envir = asNamespace("avisblatt"))
  tfs <- ns[grepl("tagfilter_",ns)]
  l <- lapply(tfs, function(x){
    getFromNamespace(x, ns = "avisblatt")()
  })
  names(l) <- tfs
  # remove header tagfilter,
  # those should only be stored in "tags_section", not in "tags"
  l <- l[!(names(l) %in% tf_header(prefix = T))]
  
  # Prepare language detection. For higher recognition rate, 
  # limitrecognition to the two languages occurring in the Avisblatt
  avis_profiles <- textcat::TC_byte_profiles[names(textcat::TC_byte_profiles) %in% c("german", "french")]
  
  for (i in AVIS_YEARS){
    fn <- sprintf("orig_%d.csv", i)
    chunk_out <- sprintf("yearly_%d", i)
    # Create Collection
    message(sprintf("Starting to create collection for %d ...", i))
    tryCatch({
      if(i %in% gt_years){
        coll <- Collection$new(file.path(source_path, fn),
                               docvars_to_meta = c("adcontent", "adtype",
                                                   "finance", "keyword"),
                               transform_docvars = clean_manual_tags)
        
      } else {
        coll <- Collection$new(file.path(source_path, fn))
      }
      message("Data read and collection initialized.")
      
      coll$meta[, language := "de"]
      langs <- textcat::textcat(as.character(coll$corpus), p = avis_profiles)
      coll$meta[id %in% names(langs[langs == "french"]), language := "fr"]
      message("Language detected.")

      # Apply tagfilters
      coll$apply_tagfilters(l)
      ut <- umbrella_terms()
      for (j in 1:length(ut)){
        ids <- coll$meta[grepl(ut[j], coll$meta$tags), id]
        coll$add_tags(ids, names(ut[j]))
      }
      message("Regular tag filters applied and umbrella terms added.")
      
      # add a section header column
      ht <- data.table(id = names(coll$corpus),
                       tags_section = docvars(coll$corpus)[,"header_tag"])
      # tag all ads in certain sections as no_advert,
      # headers are never no_advert
      coll$corpus$noadvert[coll$corpus$header_tag %in% c("curious", "death", "marriage", "election", "naturalisation", "denaturalisation", "insolvency", "official")] <- T
      coll$corpus$noadvert[coll$corpus$isheader] <- F
      message("Header tag written and noadvert info corrected.")
      # keep original sorting after join, cause this suits
      # avisblatt use / header better than alphabethical order of ids
      org_sorting <- copy(coll$meta$id)
      setkey(coll$meta, id)
      setkey(ht, id)
      coll$meta <- coll$meta[ht]
      setcolorder(coll$meta,c("id","isheader","date",
                              "tags_section","tags","tags_manual","language"))
      coll$meta <- coll$meta[order(match(coll$meta$id,org_sorting))]
      message("Records sorted.")
      
      # write result to 2 files: json for meta, .csv for text data
      write_collection(coll, file.path(dest_path, chunk_out))
      message("Collection written.\n")
    },
    error = function(e) message(
      sprintf("%d could not be processed to a collection.\n", i)))
  }
}