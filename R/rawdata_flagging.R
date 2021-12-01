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
    # account for missing years:
    # do not compare issues further than 10 days apart
    # (10 instead of 7 because issue dates can move 1-2 days)
    if (difftime(issuedates[j-1], issuedates[j]) < 10){
      s <- advert_distance(ci[[j]], ci[[j-1]], consider_length_diff = TRUE)
      # For each current ad, chose the most similar ad from previous week, and vice versa
      r_current  <- as.matrix(apply(s, 1, function(x) {colnames(s)[which.min(x)]}))
      r_previous <- as.matrix(apply(s, 2, function(x) {rownames(s)[which.min(x)]}))
      # Attach the similarity to it
      r_current  <- cbind.data.frame(r_current,  round(apply(s, 1, min), 4))
      r_previous <- cbind.data.frame(r_previous, round(apply(s, 2, min), 4))
      # name columns to avoid name matching problems in rbind further down
      colnames(r_current)  <- c("id1", "dist1")
      colnames(r_previous)  <- c("id1", "dist1")
      # in some cases, reprints skip an issue.
      # So we also need to check one issue further
      # As we concurrently look for
      # 1. a potential original ad of each ad in the current issue
      # 2. a potential reprint of each ad in the previous issue,
      # this needs to be done in two parts:
      # FIRST, for each ad in current issue,
      # determine potential original ad in issue before previous issue
      if (j > 2){
        s <- advert_distance(ci[[j]], ci[[j-2]], consider_length_diff = TRUE)
        # temporarily extend results for neighbouring issues
        # by results from comparing issues one apart
        r_current  <- cbind.data.frame(r_current,
                                       as.matrix(apply(s, 1, function(x) {colnames(s)[which.min(x)]})),
                                       round(apply(s, 1, min), 4))
        colnames(r_current)  <- c("id1", "dist1", "id2", "dist2")
        # For each ad,
        # id1 denotes most similiar ad from previous issue,
        # id2 most similiar ad from issue before that.
        # Now substitute id1 by id2 if and only if
        # - id1 does appear to not be a reprint/original
        #    (i.e. if dist1 > 1)
        # - id2 has a higher similarity (i.e. dist2 < dist1)
        r_current$id1    <- ifelse(r_current$dist1 > 1 & (r_current$dist2 < r_current$dist1),
                                   as.vector(r_current$id2), as.vector(r_current$id1))
        r_current$dist1  <- ifelse(r_current$dist1 > 1 & (r_current$dist2 < r_current$dist1),
                                   r_current$dist2, r_current$dist1)
        # remove temporary columns
        r_current$id2    <- NULL
        r_current$dist2  <- NULL
      }
      # SECOND, for each ad in previous issue,
      # determine potential reprint in issue AFTER current issue
      if (j < length(issuedates)){
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
      if(current_year != as.numeric(substr(issuedates[j], 1, 4))){
        itime <- round(difftime(Sys.time(),ti, units = "min"),2)
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
  c_all$meta <- cbind(c_all$meta, NA)
  colnames(c_all$meta) <- cn
  
  results_current <- cbind(rownames(results_current), results_current)
  colnames(results_current) <- c("id", "potential_original", "p_o_distance")
  results_current$potential_original <- as.character(results_current$potential_original)
  c_all$meta <- merge(c_all$meta, results_current, all = TRUE, by = "id")
  
  results_previous <- cbind(rownames(results_previous), results_previous)
  colnames(results_previous) <- c("id", "potential_reprint", "p_r_distance")
  results_previous$potential_reprint <- as.character(results_previous$potential_reprint)
  c_all$meta <- merge(c_all$meta, results_previous, all = TRUE, by = "id")
  
  # add reprint_status
  c_all$meta$reprint_status[c_all$meta$p_o_distance > 1 & c_all$meta$p_r_distance <= 1] <- "orig_r"
  c_all$meta$reprint_status[c_all$meta$p_o_distance > 1 & c_all$meta$p_r_distance >  1] <- "orig_u"
  c_all$meta$reprint_status[c_all$meta$p_o_distance < 1] <- "reprint"
  
  message("Writing collections.")
  store_yearwise(c_all, dest_path)
  }
}


#' @export
rawdata_fraternaltwin_detection <- function(AVIS_YEARS = 1729:1844,
                                      source_path = "../avis-databuffer/collections_reprint_detected",
                                      dest_path = "../avis-data/collections"){
  message("Loading collection(s).")
  c_all <- gather_yearly_collections(AVIS_YEARS, 
                                     just_meta = FALSE, 
                                     path = source_path)
  message("Collection(s) loaded.")
  fraternaltwin_processing(c_all)
  message("Writing collections.")
  store_yearwise(c_all, dest_path)
}