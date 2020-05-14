library(jsonlite)
library(R6)






write_collection <- function(x,
                             name_on_disk,
                             pretty_json = TRUE,
                             zip = FALSE){
  # sanity checks
  stopifnot(inherits(x, "AvisCollection"))
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
  ee <- eapply(x$meta, as.list.environment)
  # data slots need to be updated when collection class
  # changes as this set helps to distinguish non-data slots
  # such as functions from data slots...
  data_slots <- c("id","tags","date","language")
  li <- lapply(ee, function(e){
    n <- names(e)
    sel <- n[n %in% data_slots]
    e[sel]
  })

  writeLines(
    toJSON(li, pretty = pretty_json,
           auto_unbox = TRUE,
           null = "null"),
    meta_file
  )
  message(sprintf("Meta information written to %s",meta_file))

  dt <- data.table(
    id = names(x$corpus),
    collection_text = texts(x$corpus),
    docvars(x$corpus))
  fwrite(dt, file = data_file)
  message(sprintf("Data written to %s",data_file))

  if(zip){
    zip_file <- paste0(name_on_disk,".zip")
    zip(zip_file, c(data_file, meta_file))
    file.remove(data_file)
    file.remove(meta_file)
    message("Zip archive containing data and meta data created.")
  }
}

name_on_disk = "collection_1834.csv"

read_collection <- function(name_on_disk){
  data_file <- paste0(name_on_disk, ".csv")
  meta_file <- paste0(name_on_disk, ".json")

  dt <- fread(data_file)
  crps <- corpus(dt, docid_field = "id",
                 text_field = "collection_text")
  mi <- fromJSON(meta_file)
  ac <- AvisCollection$new(crps, mi)
  ac
}


io <- read_collection("collection_1834")
identical(io$corpus,
          avis_1834$corpus,
          ignore.environment = TRUE)


str(avis_1834$meta)

io$count_records_in_collect()
io$search_tags("more")


io$meta$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1`
m <- write_collection(avis_1834,"collection_1834")
