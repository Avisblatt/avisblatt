#' @export
rawdata_reprint_detection <- function(AVIS_YEARS = 1729:1844,
                                    source_path = "../avis-databuffer/collections_unflagged",
                                    dest_path = "../avis-databuffer/collections_reprint_detected"){
  AVIS_YEARS <- intersect(AVIS_YEARS, 
                          list.files(source_path, pattern = "csv") %>% 
                            substr(8, 11) %>% as.numeric)

  message("Loading collection(s).")
  c_all <- gather_yearly_collections(AVIS_YEARS, 
                                     just_meta = FALSE, 
                                     path = source_path)
  # Just looking at ads here
  corp <- corpus_subset(c_all$corpus,
                        !(c_all$corpus$isheader)
                        &!(c_all$corpus$noadvert))
  # throw out ads with short garbage as text
  # (5 characters or less), as they tend to crash
  # subsequent functions, and are rubbish anyway
  corp <- corpus_subset(corp, nchar(as.character(corp)) > 5)
  
  # go through all issue dates,
  # for each ad in current issue
  # find most similar in previous issues;
  # gather it in results-matrix.
  # In each step, we concurrently look for
  # potential original ads to ads in the current issue
  # potential reprints to ads in the previous issue
  issuedates <- sort(unique(corp$date))
  results_current  <- matrix("",0,2)
  results_previous <- matrix("",0,2)
  
  message("Collections loaded. Prepare a separate corpus for each issue:")
  
  # Prepare a separate corpus for each issue to feed into advert_distance()
  ci <- list()
  current_year <- as.numeric(substr(issuedates[1], 1, 4)) #just for messaging
  for (j in 1:length(issuedates)){
    ci[[j]] <- corpus_subset(corp, (corp$date %in% issuedates[j]))
    if(current_year != as.numeric(substr(issuedates[j], 1, 4))){
      message(paste0("Corpus created for all issues in ", current_year))
      current_year <- as.numeric(substr(issuedates[j], 1, 4))
    }
  }
  message(paste0("Corpus created for all issues in ", current_year, "\n"))
  
  current_year <- as.numeric(substr(issuedates[1], 1, 4)) #just for messaging
  ti <- Sys.time()
  for (j in 2:length(issuedates)){
    if (difftime(issuedates[j-1], issuedates[j]) < 10){
      s <- advert_distance(ci[[j]], ci[[j-1]], consider_length_diff = TRUE)
      r_current  <- as.matrix(apply(s, 1, function(x) {colnames(s)[which.min(x)]}))
      r_previous <- as.matrix(apply(s, 2, function(x) {rownames(s)[which.min(x)]}))
      r_current  <- cbind.data.frame(r_current,  round(apply(s, 1, min), 4))
      r_previous <- cbind.data.frame(r_previous, round(apply(s, 2, min), 4))
      colnames(r_current)  <- c("id1", "dist1")
      colnames(r_previous)  <- c("id1", "dist1")
      if (j > 2 & issuedates[j] > "1838-08-01"){
        s <- advert_distance(ci[[j]], ci[[j-2]], consider_length_diff = TRUE)
        r_current  <- cbind.data.frame(r_current,
                                       as.matrix(apply(s, 1, function(x) {colnames(s)[which.min(x)]})),
                                       round(apply(s, 1, min), 4))
        colnames(r_current)  <- c("id1", "dist1", "id2", "dist2")
        r_current$id1    <- ifelse(r_current$dist1 > 1 & (r_current$dist2 < r_current$dist1),
                                   as.vector(r_current$id2), as.vector(r_current$id1))
        r_current$dist1  <- ifelse(r_current$dist1 > 1 & (r_current$dist2 < r_current$dist1),
                                   r_current$dist2, r_current$dist1)
        r_current$id2    <- NULL
        r_current$dist2  <- NULL
      }
      if (j < length(issuedates) & issuedates[j] > "1838-08-01"){
        s <- advert_distance(ci[[j+1]], ci[[j-1]], consider_length_diff = TRUE)
        r_previous <- cbind.data.frame(r_previous,
                                       as.matrix(apply(s, 2, function(x) {rownames(s)[which.min(x)]})),
                                       round(apply(s, 2, min), 4))
        colnames(r_previous) <- c("id1", "dist1", "id2", "dist2")
        r_previous$id1   <- ifelse(r_previous$dist1 > 1 & (r_previous$dist2 < r_previous$dist1),
                                   as.vector(r_previous$id2), as.vector(r_previous$id1))
        r_previous$dist1 <- ifelse(r_previous$dist1 > 1 & (r_previous$dist2 < r_previous$dist1),
                                   r_previous$dist2, r_previous$dist1)
        r_previous$id2   <- NULL
        r_previous$dist2 <- NULL
      }
      # collect results
      results_current  <- rbind(results_current,  r_current)
      results_previous <- rbind(results_previous, r_previous)
      itime <- round(difftime(Sys.time(),ti, units = "min"),2)
      if(current_year != as.numeric(substr(issuedates[j], 1, 4))){
        message(paste0("Reprints detected for all ads from ", current_year, ", took ", itime, " minutes."))
        ti <- Sys.time()
        current_year <- as.numeric(substr(issuedates[j], 1, 4))
      }
    }
  }
  
  message(paste0("Reprints detected for all ads from ", current_year, ", took ", itime, " minutes."))
  
  # attach results and reprint_status column to meta data
  
  message("Writing reprint info to meta")
  cn <- c(colnames(c_all$meta), "reprint_status")
  meta_dt <- cbind(c_all$meta, NA)
  colnames(meta_dt) <- cn
  
  results_current <- cbind(rownames(results_current), results_current)
  colnames(results_current) <- c("id", "potential_original", "p_o_distance")
  results_current$potential_original <- as.character(results_current$potential_original)
  meta_dt <- merge(meta_dt, results_current, all = TRUE, by = "id")
  
  results_previous <- cbind(rownames(results_previous), results_previous)
  colnames(results_previous) <- c("id", "potential_reprint", "p_r_distance")
  results_previous$potential_reprint <- as.character(results_previous$potential_reprint)
  meta_dt <- merge(meta_dt, results_previous, all = TRUE, by = "id")
  
  # add reprint_status
  meta_dt$reprint_status[meta_dt$p_o_distance > 1 & meta_dt$p_r_distance <= 1] <- "orig_r"
  meta_dt$reprint_status[meta_dt$p_o_distance > 1 & meta_dt$p_r_distance >  1] <- "orig_u"
  meta_dt$reprint_status[meta_dt$p_o_distance < 1] <- "reprint"
  
  message("\nWrite metadata to disc, year-by-year:")
  write_metadata(meta_dt, dest_path)
}


















#' @export
rawdata_fraternaltwin_detection <- function(AVIS_YEARS = 1729:1844,
                                            source_path_csv  = "../avis-databuffer/collections_unflagged",
                                            source_path_json = "../avis-databuffer/collections_reprint_detected",
                                            dest_path = "../avis-data/collections"){
  message("Loading data.")
  meta_dt <- load_metadata(AVIS_YEARS, source_path_json)
  
  meta_dt <- fraternaltwin_processing(meta_dt, dest_path)
  
  message("Composing final collections.\nCopy data files (CSV) to folder")
  csv_files <- list.files(source_path_csv, pattern = "csv")
  copied <- file.copy(from = list.files(source_path_csv, 
                                        full.names = T, 
                                        pattern = "csv"), 
                      to = dest_path, 
                      copy.date = TRUE, overwrite = TRUE)
  message("Copied ", sum(copied), " files.")
  message("\nWrite metadata files (JSON) to folder, year-by-year:")
  write_metadata(meta_dt, dest_path)
}



#' @export
rawdata_redo_tags <- function(AVIS_YEARS = 1729:1844,
                      path = "../avis-data/collections",
                      path_no_ft = "../avis-databuffer/collections_reprint_detected",
                      path_no_rp = "../avis-databuffer/collections_unflagged",
                      correct_final = T,
                      correct_no_ft = T,
                      correct_no_rp = T){
  if(correct_final+correct_no_ft+correct_no_rp == 0){stop("You specified that no collection shall be updated.\nAt least one of correct_no_rp, correct_no_ft, correct_final has to be TRUE.")}

  message("Prepare tagfilters.\n")
  ns <- ls(envir = asNamespace("avisblatt"))
  tfs <- ns[grepl("tagfilter_",ns)]
  l <- lapply(tfs, function(x){
    getFromNamespace(x, ns = "avisblatt")()
  })
  names(l) <- tfs
  # remove header tagfilter
  l <- l[!(names(l) %in% tf_header(prefix = T))]
  
  # if it is specified that the final collections
  # shall not be updated, base the re-tagging
  # on the initial collection withgout reprints
  if(!correct_final){path = path_no_rp} 
  
  meta_dt <- data.table()
  for (i in AVIS_YEARS){
    startt <- Sys.time()
    message(sprintf("Start retagging the ads from %d.", i))
    fn <- file.path(path, sprintf("yearly_%d", i))
    c_year <- read_collection(fn)
    c_year$apply_tagfilters(l)
    ut <- umbrella_terms()
    for (j in 1:length(ut)){
      ids <- c_year$meta[grepl(ut[j], c_year$meta$tags), id]
      c_year$add_tags(ids, names(ut[j]))
    }
    meta_dt <- rbind(meta_dt, c_year$meta) 
    dtime <- round(difftime(Sys.time(), startt, units = "min"),2) ###
    message(sprintf("Tag filters applied and umbrella terms added, took %s minutes.\n", dtime))
  }

  if(correct_no_rp){
    startt <- Sys.time()
    message("\nUpdate metadata for collections without reprint flagging:")
    write_metadata(meta_dt[,1:9], path_no_rp)
    message(sprintf("Took %s minutes", 
                    round(difftime(Sys.time(), startt, units = "min"),2)))
  }
  
  if(correct_no_ft){
    startt <- Sys.time()
    message("\nUpdate metadata for collections without fraternal twin flagging:")
    write_metadata(meta_dt, path_no_ft)
    message(sprintf("Took %s minutes", 
                    round(difftime(Sys.time(), startt, units = "min"),2)))
  }
  
  if(correct_final){
    message("\nStart fraternal twin detection.")
    meta_dt <- fraternaltwin_processing(meta_dt, path)
    meta_dt$created <- Sys.time()
    
    startt <- Sys.time()
    message("\nUpdate metadata for final collections:")
    write_metadata(meta_dt, path)
    message(sprintf("Took %s minutes", 
                    round(difftime(Sys.time(), startt, units = "min"),2)))
    }
}



add_flair_data <- function(AVIS_YEARS = 1729:1844,
                           path_collections = "../avis-data/collections/",
                           path_collections_flaired = "../avis-data/collections_flaired/",
                           path_flair = "../avis-data/flair/"){
  for (i in AVIS_YEARS){
    mi <- fromJSON(paste0(path_collections, "yearly_", i, ".json"))
    dt_j <- data.table::rbindlist(mi)
    dt_j[, id := names(mi)]
    dt_j[, date := as.Date(date)]
    setcolorder(dt_j, neworder = c("id", setdiff(names(dt_j), "id")))
    
    dt_f <- fread(paste0(path_flair, "yearly_", i, "_ner.csv"), encoding = "UTF-8")
    dt <- merge(dt_j, dt_f, all.x = TRUE)
    #----------
    dt_location <- list()
    dt_people <- list()
    
    for (j in 1:nrow(dt)){
      dt_location <- rbind(dt_location, strsplit(dt$location[j], "; "))
      dt_people <- rbind(dt_people, strsplit(dt$people[j], "; "))
    }
    
    dt$locations <- dt_location
    dt$people <- dt_people
    #----------
    dt$people[lengths(dt$people) == 0] <- NA
    dt$locations[lengths(dt$locations) == 0] <- NA
    
    meta_file <- paste0(path_collections_flaired, "yearly_", i, ".json")
    
    dtcols <- setdiff(names(dt), "id")
    dt_chunks <- split(dt[, ..dtcols], as.factor(1:nrow(dt)))
    names(dt_chunks) <- dt$id
    
    write_json(dt_chunks, meta_file, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "string")
    # or
    #fwrite(dt_chunks, meta_file)
    
    message(sprintf("Added flair metadata for %d.", i))
  }
}

add_flair_data2 <- function(AVIS_YEARS = 1729:1844,
                           path_collections = "../avis-data/collections/",
                           path_collections_flaired = "../avis-data/collections_flaired/",
                           path_flair = "../avis-data/flair/"){
  for (i in AVIS_YEARS){
    mi <- fromJSON(paste0(path_collections, "yearly_", i, ".json"))
    dt_j <- data.table::rbindlist(mi)
    dt_j[, id := names(mi)]
    dt_j[, date := as.Date(date)]
    setcolorder(dt_j, neworder = c("id",
                                   setdiff(names(dt_j),"id")))
    
    dt_f <- fread(paste0(path_flair, "yearly_", i, "_ner.csv"), encoding = "UTF-8")
    dt <- merge(dt_j, dt_f, all.x = T)
  #----------
    dt_location <- list()
    dt_people <- list()
    
    for (j in 1:nrow(dt)){
      dt_location <- rbind(dt_location, strsplit(dt$location[j], "; "))
      dt_people <- rbind(dt_people, strsplit(dt$people[j], "; "))
    }
    
    dt$locations <- dt_location
    dt$people <- dt_people
  #----------
    dt$people[lengths(dt$people) == 0] <- NA
    dt$locations[lengths(dt$locations) == 0] <- NA
    
    meta_file <- paste0(path_collections_flaired, "yearly_", i, ".json")
    
    
    
    
    dtcols <- setdiff(names(dt),"id")
    dt_chunks <- split(dt[, ..dtcols], as.factor(1:nrow(dt_j)))
    names(dt_chunks) <- dt$id
    
    writeLines(
      toJSON(dt_chunks, pretty = TRUE,
             auto_unbox = TRUE,
             null = "null",
             na = "string"),
      meta_file
    )
    #check output file encoding, could be necessary to convert to UTF-8 when doing this on Windows...
    message(sprintf("Added flair metadata for %d.", i))
  }
}

