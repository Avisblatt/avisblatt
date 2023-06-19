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



#----------------

# Helper function for create_hasdai_annotations:
# Transform a TSV record into JSON structure
transform_record <- function(record, dataversion = NA) {
  
  json <- list(
    schema = "http://schemata.hasdai.org/historic-persons/historic-newspaper-snippet-v1.0.0.json",
    date = list(ref = as.Date(record$date.ref, origin = "1970-01-01")),
    transcription = filter_empty_values(list(
      original = record["transcription.original"],
      corrected = if(length(record["transcription.corrected"]) > 0) record["transcription.corrected"] else NULL
    )),
    type = filter_empty_values(list(
      id = record["type.id"],
      is_header = as.logical(record["type.is_header"]),
      is_original = as.logical(record["type.is_original"]),
      language = record["type.language"],
      original = record["type.original"],
      scheme = "http://schemata.hasdai.org/avisblatt_types.json",
      siblings = record["type.siblings"]
    )),    tags = if(is.na(record["tags.tags"])) NULL else list(
      tags = record["tags.tags"],
      scheme = "http://schemata.hasdai.org/avisblatt_tags.json"
    ),
    entities = filter_empty_entities(list(
      list(value = record["entities.person"], type = "person"),
      list(value = record["entities.location"], type = "location")
    )),
    source = list(
      collection = record["source.collection"],
      title = record["source.title"],
      issue = as.integer(record["source.issue"]),
      selector = list(
        id = record["source.selector.id"],
        page = record["source.selector.page"],
        readingorder = as.integer(record["source.selector.readingorder"]),
        canvas = record["source.selector.canvas"],
        fragment = record["source.selector.fragment"]
      )
    ),
    meta = list(
      version = dataversion,
      generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.%OSZ"),
      actions = list(
        actors = list(
          list(
            id = "0000-0001-8225-7851",
            scheme = "ORCID",
            name = "Anna Reimann"
          ),
          list(
            id = "0000-0002-8592-3124",
            scheme = "ORCID",
            name = "Alexander Engel"
          ),
          list(
            id = "0000-0003-2419-4252",
            scheme = "ORCID",
            name = "Ina Serif"
          ),
          list(
            id = "0000-0002-4511-1017",
            scheme = "ORCID",
            name = "Lars Dickmann"
          ),
          list(
            id = "0000-0001-7901-9254",
            scheme = "ORCID",
            name = "Matthias Bannert"
          )
        ),
        summary = "OCR correction, attribute section/headertag, reprint detection/siblings, tagging"
      )
    )
  )
  
  if (length(json$tags) == 0) {
    json$tags <- NULL
  }
  if (length(json$entities) == 0) {
    json$entities <- NULL
  }
  return(json)
}

# Helper function for create_hasdai_annotations:
# Filter empty values from a list
filter_empty_values <- function(x) {
  x <- Filter(Negate(is.null), x)
  x <- Filter(Negate(function(val) is.character(val) && val == ""), x)
  x <- Filter(Negate(function(val) is.na(val)), x)
  x <- Filter(Negate(function(val) identical(val, as.name("list"))), x)
  x <- Filter(Negate(function(val) is.list(val) && length(val) == 0), x)
  x <- Filter(Negate(function(val) is.list(val) && all(sapply(val, is.null))), x)
  x <- Filter(Negate(function(val) is.list(val) && all(sapply(val, function(x) is.character(x) && x == ""))), x)
  lapply(x, function(val) if (is.list(val)) filter_empty_values(val) else val)
}


# Helper function for create_hasdai_annotations:
# Filter empty entities from the entities list
filter_empty_entities <- function(entities) {
  entities <- Filter(function(entity) entity$value != "", entities)
  if (length(entities) == 0) return(NULL)
  lapply(entities, function(entity) {
    if (length(entity) > 1)
      entity
    else
      entity[[1]]
  })
}


create_hasdai_annotations <- function(AVIS_YEARS = 1729:1844,
                                      path_rawdata = "../avis-databuffer/raw_data_uncorrected/",
                                      path_collections = "../avis-data/collections_flaired/",
                                      path_output = "../avis-for-hasdai/annotations/",
                                      data_version = NA){
  handles <- fread("hasdai_handles.csv", encoding = "UTF-8")
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
    
    for (j in 1:nrow(dt_j)){
      dt_j$tags[j][[1]] <- unique(gsub("\\d", "", dt_j$tags[j][[1]]))
    }
    
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
    #class(dt_c$issue) <- "character"
    class(dt_c$pageno) <- "character"
    #class(dt_c$readingorder) <- "character"
    
    # get rawdata
    dt_r <- fread(file = paste0(path_rawdata, "orig_", i, ".csv"), encoding = "UTF-8")
    dt_r <- subset(dt_r, select = c(id, text))
    dt_r <- rename(dt_r, original = text)
    dt <- merge(dt_c, dt_r, by = "id", all = F) #some records from the raw data are merged in the collection, so all = F
    dt <- merge(dt, dt_j, by = "id", all = T)
    
    dt <- subset(dt, select = -c(inscribed, adcontent, adtype, finance, keyword, tags_section))
    
    
    dt[corrected == original]$corrected <- NA
    
    dt_s <- siblings
    dt_s[lengths(siblings) == 0]$siblings <- ""
    dt_s$date <- year(dt_s$date)
    dt_s <- dt_s[date == i] 
    dt_s <- subset(dt_s, select = c(id, original, siblings))
    
    dt <- merge(dt, dt_s, by = "id", all = T)
    dt <- dt[order(issue, pageno, readingorder), ]
    
    
    dt[!is.na(tags) & noadvert == F & is_header == F]$tags <- lapply(dt[!is.na(tags) & noadvert == F & is_header == F]$tags, function(x){c(x, "advert")})
    dt[!is.na(tags) & noadvert == T & is_header == F]$tags <- lapply(dt[!is.na(tags) & noadvert == T & is_header == F]$tags, function(x){c(x, "notice")})
    
    dt[is.na(tags) & noadvert == F & is_header == F]$tags <- "advert"
    dt[is.na(tags) & noadvert == T & is_header == F]$tags <- "notice"
    
    dt[is_header == T]$tags <- NA
    
    dt <- subset(dt, select = -c(noadvert))
    colnames(dt) <- c("source.selector.id", 
                      "transcription.corrected", 
                      "source.issue", 
                      "date.ref", 
                      "source.selector.page", 
                      "source.selector.readingorder", 
                      "type.is_header", 
                      "source.selector.canvas", 
                      "type.id", 
                      "source.selector.fragment", 
                      "transcription.original", 
                      "tags.tags", 
                      "type.language", 
                      "entities.person", 
                      "entities.location", 
                      "type.is_original", 
                      "type.original", 
                      "type.siblings")
    
    dt$source.collection <- handles[year == i]$handle
    dt$source.title <- handles[year == i]$title
    
    setcolorder(dt, c("date.ref",
                      "transcription.original", 
                      "transcription.corrected", 
                      "type.id", 
                      "type.is_header", 
                      "type.is_original", 
                      "type.language", 
                      "type.original", 
                      "type.siblings", 
                      "tags.tags", 
                      "entities.person", 
                      "entities.location", 
                      "source.collection", 
                      "source.title", 
                      "source.issue", 
                      "source.selector.id", 
                      "source.selector.page", 
                      "source.selector.readingorder", 
                      "source.selector.canvas", 
                      "source.selector.fragment"))
    
    dt[entities.person == "NA"]$entities.person <- ""
    dt[entities.location == "NA"]$entities.location <- ""
    
    dt <- dt[type.id != ""] # remove leading record(s) without type.id, i.e.: records not placed under any header, like editor's poems
    
    # Transform each record into JSON structure
    json_data <- lapply(asplit(dt, 1), function(record) transform_record(record, dataversion = data_version))
    
    # Convert list of JSON objects to JSON array
    json_array <- toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
    
    # Remove rownames from the JSON array
    json_array <- gsub('"\\d+": \\{', "{", json_array)
    
    # write all annotations for one year in one file
    dir.create(paste0(path_output, i), showWarnings = F)
    fn <- paste0(path_output, i, ".json")
    write(json_array, fn)
    message(paste0("Written JSONs for ", i))
  }
}