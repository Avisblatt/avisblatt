load_metadata <- function(AVIS_YEARS = 1729:1844, path){
  AVIS_YEARS <- sort(as.numeric(AVIS_YEARS))
  AVIS_YEARS <- intersect(AVIS_YEARS, 
                          list.files(path, pattern = "json") %>% 
                            substr(8, 11) %>% as.numeric)
  meta_dt <- data.table()
  for (i in AVIS_YEARS){
    meta_file <- paste0(path, "/yearly_", i, ".json")
    mi <- fromJSON(meta_file)
    d <- data.table::rbindlist(mi)
    d[, id := names(mi)]
    meta_dt <- rbind(meta_dt, d)
  }
  meta_dt[, date := as.Date(date)]
  setcolorder(meta_dt, neworder = c("id",
                                    setdiff(names(meta_dt),"id")))
  meta_dt
}


write_metadata <- function(meta_dt, path){
  AVIS_YEARS <- sort(unique(year(meta_dt$date)))
  for (i in AVIS_YEARS){
    start_date <- as.Date(sprintf("%d-01-01", i))
    end_date <- as.Date(sprintf("%d-01-01", i+1))
    meta_i <- meta_dt[as.Date(date) < end_date,]
    meta_i <- meta_i[as.Date(date) >= start_date,]
    dtcols <- setdiff(names(meta_i),"id")
    dt_chunks <- split(meta_i[, ..dtcols], as.factor(1:nrow(meta_i)))
    names(dt_chunks) <- meta_i$id
    fn <- paste0(path, sprintf("/yearly_%d", i), ".json")
    writeLines(
      toJSON(dt_chunks, pretty = TRUE,
             auto_unbox = TRUE,
             null = "null",
             na = "string"),
      fn
    )
    message(sprintf("Written to %s", fn))
  }
}



fraternaltwin_processing <- function(meta_dt = meta_dt,  
                                     path = "../avis-data/collections"){
  message(sprintf("\nBuilding a lookup table for pairs of originals+reprints, depending on number of years this can take several minutes."))
  startt <- Sys.time()
  n <- as.matrix(meta_dt[reprint_status=="reprint", which = T])
  n <- cbind(n, apply(n, 1, function(x) {meta_dt[id == meta_dt$potential_original[x], which = T]}), NA)
  dtime <- round(difftime(Sys.time(), startt, units = "min"),2)
  message(sprintf("Took %s minutes", dtime))
    
  fraternal_twins <- data.table(id_orig=character(), tags_orig=character(), id_reprint=character(), tags_reprint=character())
  new_tags <- data.table(row_no=integer(), tags=character())
  startt <- Sys.time()
  message(sprintf("Looking for fraternal twins"))
  for (i in 1:nrow(n)){
    r_tags <- meta_dt[n[i,1], 5]
    o_tags <- meta_dt[n[i,2], 5]
    if(length(r_tags) == 0){r_tags <- NA}
    if(length(o_tags) == 0){o_tags <- NA}
    if(!identical(r_tags, o_tags)){
      joint_tags <- o_tags
      joint_tags[1,1] <- c(na.omit(unique(c(unlist(r_tags), unlist(o_tags), "fraternal_twin"))))
      new_tags <- rbind(new_tags,  
                        cbind(n[i,1], as.list(joint_tags)),
                        cbind(n[i,2], as.list(joint_tags)))
      fraternal_twins <- rbind(fraternal_twins,
                               cbind(as.character(meta_dt[n[i,2], id]),
                                     as.list(o_tags),
                                     as.character(meta_dt[n[i,1], id]),
                                     as.list(r_tags)))
    }
    if(round(i/10000)==i/10000){
      message(paste0("Looked at ", i, " out of ", nrow(n), " records."))
    }
  }
  # update tags of fraternal twins
  meta_dt$tags[new_tags$row_no] <- new_tags$tags
    
  dtime <- round(difftime(Sys.time(), startt, units = "min"),2)
  message(sprintf("Took %s minutes", dtime))
    
  # document the cases, to study for tagfilter enhancements
  fwrite(fraternal_twins, file.path(path, "fraternal_twins.tsv"), sep = "\t")
    
  meta_dt
}