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


# Helper function for transform_record:
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
# Transform a TSV record into JSON structure
transform_record <- function(record, dataversion = NA) {
  ep <- unname(record[["entities.person"]])
  el <- unname(record[["entities.location"]])
  ep_list = if (is.character(ep) && ep == "") NULL else ep_list = list(value = ep, type = "person")
  el_list = if (is.character(el) && el == "") NULL else el_list = list(value = el, type = "location")
  entities <- Filter(Negate(is.null), list(ep_list, el_list))
  
  json <- list(
    schema = "http://schemata.hasdai.org/historic-persons/historic-newspaper-snippet-v1.0.0.json",
    date = list(ref = as.Date(record$date.ref, origin = "1970-01-01")),
    transcription = filter_empty_values(list(
      original = unlist(record["transcription.original"]),
      corrected = if(length(record["transcription.corrected"]) > 0) unlist(record["transcription.corrected"]) else NULL
    )),
    snippet_type = list(
      id = as.character(record["type.id"]),
      language = as.character(record["type.language"]),
      is_reprint = as.logical(record["type.is_reprint"])
    ),
    tags = if(is.na(record["tags.tags"])) NULL else list(
      tags = record["tags.tags"],
      scheme = "http://schemata.hasdai.org/avisblatt_tags.json"
    ),
    entities = entities,
    source = list(
      id = as.character(record["source.id"]),
      collection = as.character(record["source.collection"]),
      title = as.character(record["source.title"]),
      issue = as.integer(record["source.issue"]),
      selector = list(
        page = as.character(record["source.selector.page"]),
        readingorder = as.integer(record["source.selector.readingorder"]),
        canvas = as.character(record["source.selector.canvas"]),
        fragment = unlist(record["source.selector.fragment"])
      )
    ),
    meta = list(
      version = dataversion,
      generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.%OSZ"),
      actions = list(
        list(
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
          summary = "Text recognition and correction, section attribution, reprint detection/siblings, tagging, entity recognition"
        ),
        list(
          actors = list(
            list(
              name = "Data Futures GmbH"
            )
          ),
          summary = "Data management, identifiers, repository work"
          )
        )
    )
  )
  if (length(json$tags) == 0) json$tags <- NULL
  if (length(json$entities) == 0) json$entities <- NULL
  
  ts <- record[["type.siblings"]]
  if (!(is.character(ts) && ts == "")) json$snippet_type$siblings = ts
  if (record["type.original"] != "") json$snippet_type$original = as.character(record["type.original"])
  json$snippet_type$additional_data = list(
                                        is_header = as.logical(record["type.is_header"]),
                                        is_advert = as.logical(record["type.is_advert"]),
                                        is_notice = as.logical(record["type.is_notice"])
  )
  json$snippet_type$scheme = "http://schemata.hasdai.org/avisblatt_types.json"

  return(json)
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
    dt_j$is_reprint <- grepl("reprint", dt_j$reprint_status)
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
    
    # get rawdata
    dt_r <- fread(file = paste0(path_rawdata, "orig_", i, ".csv"), encoding = "UTF-8")
    dt_r <- subset(dt_r, select = c(id, text))
    dt_r <- rename(dt_r, original = text)
    dt <- merge(dt_c, dt_r, by = "id", all = F) #some records from the raw data were merged in the collection creation, so all = F
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
    dt[is_header == TRUE]$tags <- NA
    
    colnames(dt) <- c("source.id", 
                      "transcription.corrected", 
                      "source.issue", 
                      "date.ref", 
                      "source.selector.page", 
                      "source.selector.readingorder", 
                      "type.is_header",
                      "type.is_notice",
                      "source.selector.canvas", 
                      "type.id", 
                      "source.selector.fragment", 
                      "transcription.original", 
                      "tags.tags", 
                      "type.language", 
                      "entities.person", 
                      "entities.location", 
                      "type.is_reprint", 
                      "type.original", 
                      "type.siblings")
    
    dt <- dt[type.id != ""] # remove leading record(s) without type.id, i.e.: records not placed under any header, like editor's poems
    dt$source.collection <- handles[year == i]$handle
    dt$source.title <- handles[year == i]$title
    dt$type.is_advert <- TRUE
    dt[type.is_notice == TRUE]$type.is_advert <- FALSE
    dt[type.is_header == TRUE]$type.is_advert <- FALSE

    dt[type.original == ""]$type.is_reprint <- FALSE # reminder: this modification of reprint status should also be reflected in the collections themselves...
    
    
    setcolorder(dt, c("date.ref",
                      "transcription.original", 
                      "transcription.corrected", 
                      "type.id", 
                      "type.language", 
                      "type.is_reprint",
                      "type.original", 
                      "type.siblings", 
                      "type.is_header", 
                      "type.is_advert",
                      "type.is_notice",
                      "tags.tags", 
                      "entities.person", 
                      "entities.location", 
                      "source.id", 
                      "source.collection", 
                      "source.title", 
                      "source.issue", 
                      "source.selector.page", 
                      "source.selector.readingorder", 
                      "source.selector.canvas", 
                      "source.selector.fragment"))
    
    dt[entities.person == "NA"]$entities.person <- ""
    dt[entities.location == "NA"]$entities.location <- ""
    
    # Transform each record into JSON structure
    json_data <- lapply(asplit(dt, 1), function(record) transform_record(record, dataversion = data_version))
    
    # Convert list of JSON objects to JSON array
    json_array <- toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
    
    # Remove rownames from the JSON array
    json_array <- gsub('"\\d+": \\{', "{", json_array)
    
    # write all annotations for one year in one file
    fn <- paste0(path_output, i, ".json")
    write(json_array, fn)
    message(paste0("Written JSONs for ", i))
  }
}