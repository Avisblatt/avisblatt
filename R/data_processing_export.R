#' @export
build_siblings_table <- function(AVIS_YEARS = 1729:1844,
                                 path = "../avis-data/collections/"){
  AVIS_YEARS <- intersect(AVIS_YEARS, list.files(path, pattern = "yearly") %>% substr(8, 11) %>% as.numeric)
  meta_x <- load_metadata(AVIS_YEARS, path)
  meta_x <- meta_x[order(date)]
  meta_x$original <- as.character()
  meta_x$siblings <- list()
  
  meta_x[is.na(reprint_status)]$reprint_status <- "header"
  
  message("Metadata compiled, start tracking siblings")
  
  for (i in 1:nrow(meta_x)){
    if(meta_x[i]$reprint_status == "orig_r"){
      orig <- meta_x[i]$id
      siblings_list <- as.character()
      position_list <- i
      meta_y <- meta_x[i:(i+20000)] #for speed, only look at the next 20000 ads
      j <- 1 #position within meta_y
      k <- 0 #counter for limiting to 100 loops
      is_reprint <- meta_y[j]$p_r_distance < 1
      while (is_reprint & k<100) {
        k <- k+1
        id <- meta_y[j]$potential_reprint
        j <- which(meta_y$id == id)
        if(is.na(meta_y[j]$p_r_distance)){
          is_reprint = FALSE} else {
            is_reprint = meta_y[j]$p_r_distance < 1
          }
        siblings_list <- append(siblings_list, id)
        position_list <- append(position_list, i+(j-1)) # position in meta_x
      }
      for (pos in position_list){
        meta_x[pos, siblings := as.vector(siblings_list)]
        meta_x[pos, original := orig]
      }
    }
    if(i/10000 == round(i/10000, 0)){message(sprintf("Siblings compiled for first %d records", i))}
  }
  meta_x[lengths(siblings) == 0]$siblings <- NA
  
  fwrite(select(meta_x, c(id, date, original, siblings)), 
         file = paste0(path, "siblings.tsv.gz"), sep = "\t")
}



create_hasdai_annotations <- function(AVIS_YEARS = 1729:1844,
                                      path_rawdata = "../avis-data/raw_data/",
                                      path_collections = "../avis-data/collections/",
                                      path_output = "../avis-for-hasdai/annotations/"){
  
  siblings <- fread(file = paste0(path_collections, "siblings.tsv.gz"), encoding = "UTF-8")
  siblings$siblings <- strsplit(siblings$siblings, "\\|")
  
  dir.create(path_output, showWarnings = FALSE)
  
  for (i in AVIS_YEARS){
    # get metadata from collection JSON
    mi <- fromJSON(paste0(path_collections, "yearly_", i, ".json"))
    dt_j <- data.table::rbindlist(mi)
    dt_j$is_original <- grepl("orig", dt_j$reprint_status)
    dt_j <- subset(dt_j, select = -c(isheader, date, tags_manual, ntokens, nchar, reprint_status, potential_original, p_o_distance, potential_reprint, p_r_distance))
    dt_j[, id := names(mi)]
    
    # get data from collection CSV
    dt_c <- fread(file = paste0(path_collections, "yearly_", i, ".csv"), encoding = "UTF-8")
    dt_c[, fragment:= lapply(transpose(.SD), c), .SDcols = c("fragment1", 
                                                             "fragment2", 
                                                             "fragment3", 
                                                             "fragment4", 
                                                             "fragment5", 
                                                             "fragment6", 
                                                             "fragment7", 
                                                             "fragment8", 
                                                             "fragment9", 
                                                             "fragment10")]
    for (k in 1:nrow(dt_c)){
      frags <- dt_c$fragment[[k]]
      frags <- frags[frags != ""]
      frags <- frags[!is.na(frags)]
      dt_c$fragment[[k]] <- frags
    }
    dt_c <- subset(dt_c, select = -c(rev, book, rnotes, expert_text, lang, fragment2, fragment3, fragment4, fragment5, fragment6, fragment7, fragment8, fragment9, fragment10))
    dt_c <- rename(dt_c, corrected = text, canvas = fragment1, is_header = isheader)
    dt_c$canvas <- paste0(substr(dt_c$canvas, 1, 51), "full/full/0/default.jpg")
    
    # get rawdata
    dt_r <- fread(file = paste0(path_rawdata, "orig_", i, ".csv"), encoding = "UTF-8")
    dt_r <- subset(dt_r, select = c(id, text))
    dt_r <- rename(dt_r, original = text)
    dt <- merge(dt_c, dt_r, by = "id", all = T)
    dt <- merge(dt, dt_j, by = "id", all = T)
    
    # prepare date element
    dt_x <- subset(dt, select = c(id, date))
    dt_x <- rename(dt_x, ref = date)
    dtcols <- setdiff(names(dt_x),"id")
    dt$date <- lapply(split(dt_x[, ..dtcols], as.factor(1:nrow(dt_x))),
                      function(row) as.list(row))
    
    
    
    # prepare transcription element  
    dt_x <- subset(dt, select = c(id, original, corrected))
    dtcols <- setdiff(names(dt_x),"id")
    dt$transcription <- lapply(split(dt_x[, ..dtcols], as.factor(1:nrow(dt_x))),
                               function(row) as.list(row))
    dt <- subset(dt, select = -c(original, corrected))
    
    # prepare type element (first get sibling info)
    dt_s <- siblings
    dt_s$date <- year(dt_s$date)
    dt_s <- dt_s[date == i] 
    dt_s <- subset(dt_s, select = c(id, original, siblings))
    dt <- merge(dt, dt_s, by = "id", all = T)
    dt <- dt[order(issue, pageno, readingorder), ]
    #-----
    dt_x <- subset(dt, select = c(id, header_tag, is_header, is_original, language, original))
    dt_x <- rename(dt_x, id2 = id, id = header_tag)
    dt_x$scheme <- "http://schemata.hasdai.org/avisblatt_types.json"
    dtcols <- setdiff(names(dt_x),"id2")
    dt$type <- lapply(split(dt_x[, ..dtcols], as.factor(1:nrow(dt_x))),
                      function(row) as.list(row))
    dt$type <- lapply(seq_len(nrow(dt)), function(k) c(dt$type[[k]], siblings = dt$siblings[k]))
   
    # prepare tags element  
    dt_x <- subset(dt, select = c(id, tags, is_header, noadvert))
    dt_x[noadvert == F]$tags <- lapply(dt_x[noadvert == F]$tag, function(x){c(x, "advert")})
    dt_x[noadvert == T & is_header == F]$tags <- lapply(dt_x[noadvert == T & is_header == F]$tag, function(x){c(x, "notice")})
    dt$tags <- lapply(dt_x$tags, function(x){list(tags = x, 
                                                  scheme = "http://schemata.hasdai.org/avisblatt_tags")})
    
    # prepare source element (first prepare selector sub element)
    dt_x <- subset(dt, select = c(id, pageno, readingorder, canvas, fragment))
    dtcols <- setdiff(names(dt_x),"id")
    dt$selector <- split(c(dt$pageno,
                           dt$readingorder,
                           dt$canvas,
                           dt$fragment),
                         as.factor(1:nrow(dt))) 
    dt$selector <- lapply(dt$selector, setNames, c("page", "readingorder", "canvas", "fragment"))
    #-----
    dt$source <- lapply(dt$issue, function(x){list(collection = "___000000___",
                                                   title = "___Avisblatt_title_of_that_year___",
                                                   issue = x)})
    dt$source <- lapply(seq_len(nrow(dt)), function(k) c(dt$source[[k]], selector = dt$selector[k]))
    
    
    # prepare meta element
    collection_creation <- format(max(file.info(paste0(path_collections, "yearly_", i, ".json"))$mtime,
                                      file.info(paste0(path_collections, "yearly_", i, ".csv"))$mtime),
                                  "%Y-%m-%dT%H:%M:%OS3Z")
    meta <- list(version = 1,
                 partOf = "___Avisblatt R package data collection creation___",
                 generated =  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z"),
                 actions = list(actor = list(list(id = "0000-0001-8225-7851",
                                                  scheme = "ORCID",
                                                  name = "Anna Reimann"),
                                             list(id = "0000-0002-8592-3124",
                                                  scheme = "ORCID",
                                                  name = "Alexander Engel"),
                                             list(id = "0000-0003-2419-4252",
                                                  scheme = "ORCID",
                                                  name = "Ina Serif"),
                                             list(id = "0000-0002-4511-1017",
                                                  scheme = "ORCID",
                                                  name = "Lars Dickmann"),
                                             list(id = "0000-0000-0000-0000",
                                                  scheme = "ORCID",
                                                  name = "Matthias Bannert")
                 ),
                 date = collection_creation,
                 summary = "___OCR correction, attribute section/headertag, reprint detection/siblings, tagging___",
                 classifiedAction = "__ACTION_ID__"
                 )
    )
    dt$meta <- rep(list(meta), nrow(dt_x))
    
    # prepare schema element
    dt$schema = "http://schemata.hasdai.org/historic-persons/historic-newspaper-snippet-v1.0.0.json"
    
    
    
    # generate output
    dt_x <- subset(dt, select = c(issue, schema, id, date, transcription, type, tags, source, meta))
    
    issues <- unique(dt_x$issue)
    dir.create(paste0(path_output, i), showWarnings = F)
    
    # write annotations for each issue in a file
    for (j in issues){
      fn <- paste0(path_output, i, "/anno_", i, "-", formatC(j, width = 2, flag = "0"), ".json")
      writeLines(
        toJSON(subset(dt_x[issue == j], select = -issue), 
               pretty = TRUE,
               auto_unbox = TRUE),
        fn,
        useBytes=T
      )
    }
    
    # write all annotations for one year in one file
    fn <- paste0(path_output, i, "/anno_", i, ".json")
    writeLines(
      toJSON(subset(dt_x, select = -issue), 
             pretty = TRUE,
             auto_unbox = TRUE),
      fn,
      useBytes=T
    )
    
    message(paste0("Written JSONs for ", i))
  }
}