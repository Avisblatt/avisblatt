#' @export
store_yearwise <- function(coll = c_all, path){
  unique( c_all$meta$date
  message("Writing collections.")
  for (i in AVIS_YEARS){
    coll <- c_all$clone()
    start_date <- as.Date(sprintf("%d-01-01", i))
    end_date <- as.Date(sprintf("%d-01-01", i+1))
    coll$corpus <- corpus_subset(coll$corpus, date < end_date)
    coll$corpus <- corpus_subset(coll$corpus, date >= start_date)
    coll$meta <- coll$meta[as.Date(date) < end_date,]
    coll$meta <- coll$meta[as.Date(date) >= start_date,]
    write_collection(coll, 
                     file.path(path, sprintf("yearly_%d", i)))
  }
}


#' @export
fraternaltwin_processing <- function(coll = c_all, 
                                     dest_path = "../avis-data/collections"){
  message(sprintf("Building a lookup table for pairs of originals+reprints, will likely take more than 10 minutes."))
  startt <- Sys.time()
  n <- as.matrix(coll$meta[reprint_status=="reprint", which = T])
  n <- cbind(n, apply(n, 1, function(x) {coll$meta[id == coll$meta$potential_original[x], which = T]}), NA)
  dtime <- round(difftime(Sys.time(), startt, units = "min"),2)
  message(sprintf("Took %s minutes", dtime))
  
  fraternal_twins <- data.table(id_orig=character(), tags_orig=character(), id_reprint=character(), tags_reprint=character())
  new_tags <- data.table(row_no=integer(), tags=character())
  startt <- Sys.time()
  message(sprintf("Looking for fraternal twins"))
  for (i in 1:nrow(n)){
    r_tags <- coll$meta[n[i,1], 5]
    o_tags <- coll$meta[n[i,2], 5]
    if(length(r_tags) == 0){r_tags <- NA}
    if(length(o_tags) == 0){o_tags <- NA}
    if(!identical(r_tags, o_tags)){
      joint_tags <- o_tags
      joint_tags[1,1] <- c(na.omit(unique(c(unlist(r_tags), unlist(o_tags), "fraternal_twin"))))
      new_tags <- rbind(new_tags, 
                        cbind(n[i,1], as.list(joint_tags)),
                        cbind(n[i,2], as.list(joint_tags)))
      fraternal_twins <- rbind(fraternal_twins,
                               cbind(as.character(c_all$meta[n[i,2], id]),
                                     as.list(o_tags),
                                     as.character(c_all$meta[n[i,1], id]),
                                     as.list(r_tags)))
    }
    if(round(i/10000)==i/10000){
      message(paste0("Looked at ", i, " out of ", nrow(n), " records."))
    }
  }
  # update tags of fraternal twins
  coll$meta$tags[new_tags$row_no] <- new_tags$tags
  
  dtime <- round(difftime(Sys.time(), startt, units = "min"),2)
  message(sprintf("Took %s minutes", dtime))
  
  # document the cases, to study for tagfilter enhancements
  fwrite(fraternal_twins, file.path(dest_path, "fraternal_twins.tsv"), sep = "\t")
}


#' @export
redo_tags <- function(AVIS_YEARS = 1729:1844,
                      path = "../avis-data/collections",
                      fraternal_twins = T){
  AVIS_YEARS <- intersect(AVIS_YEARS, 
                          list.files(source_path, pattern = "csv") %>% 
                            substr(8, 11) %>% as.numeric)
  message("Loading collection(s).")
  c_all <- gather_yearly_collections(AVIS_YEARS, 
                                     just_meta = FALSE, 
                                     path = path)
  
  message("Collection(s) loaded, start retagging.")
  ns <- ls(envir = asNamespace("avisblatt"))
  tfs <- ns[grepl("tagfilter_",ns)]
  l <- lapply(tfs, function(x){
    getFromNamespace(x, ns = "avisblatt")()
  })
  names(l) <- tfs
  # remove header tagfilter
  l <- l[!(names(l) %in% tf_header(prefix = T))]
  
  c_all$apply_tagfilters(l)
  ut <- umbrella_terms()
  for (j in 1:length(ut)){
    ids <- c_all$meta[grepl(ut[j], c_all$meta$tags), id]
    coll$add_tags(ids, names(ut[j]))
  }
  message("Regular tag filters applied and umbrella terms added.")
  
  if(fraternal_twins){fraternaltwin_processing(c_all)}
  
  message("Writing collections.")
  store_yearwise(c_all, path)
  }
}