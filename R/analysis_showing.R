#' Show Records
#'
#' @param coll Collection object
#' @param ids NULL or numeric vector of document ids to show
#' @param show_date logical; whether to show date
#' @param show_text logical; whether to show text
#' @param show_id logical; whether to show id
#' @param show_tags logical; whether to show tags
#' @param show_header logical; whether to show header
#' @param show_position logical; whether to show position
#'
#' @export
show_records <- function(coll,
                         ids = NULL,
                         show_date = TRUE,
                         show_text = TRUE,
                         show_id = TRUE,
                         show_tags = FALSE,
                         show_header = FALSE,
                         show_position = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections to be able to search in texts")}
  invalid_ids <- setdiff(ids, coll$meta$id)
  ids <- setdiff(ids, invalid_ids)
  dt_c <- convert(coll$corpus[sort(ids)], to = "data.frame", pretty = FALSE)
  dt_m <- coll$meta[coll$meta$id %in% ids]
  dt <- merge(dt_c, dt_m, by.x = "doc_id", by.y = "id")
  setorder(dt, date.x, pageno, readingorder)
  if (show_date){p_date <- paste0("[", dt$date.x, "] ")} else
  {p_date <-""}
  if (show_text){p_text <- dt$text} else
  {p_text <-""}
  if (show_id){p_id <- paste0(" (", dt$doc_id, ")")}  else
  {p_id <-""}
  if (show_tags){p_tags <- paste0("\n Tags: ", dt$tags)} else
  {p_tags <-""}
  if (show_header){p_header <- paste0("\n Header: ", dt$tags_section)} else
  {p_header <-""}
  if (show_position){p_pos <- paste0(" issue ", dt$issue, ", page ", dt$pageno, ", readingorder ", dt$readingorder)} else
  {p_pos <-""}
  output <- paste0(p_date, p_text, p_id, p_tags, p_header, p_pos, "\n\n")
  cat(output)
  if (length(invalid_ids)>0){
    message("Records with the following IDs could not be shown, as they were not found in the collection: \n")
    cat(invalid_ids, sep = "\n")
  }
}


#' Write records to a file
#'
#' This function writes records to a file.
#'
#' @param ids A vector of record IDs to write.
#' @param coll A Collection object.
#' @param show_date Whether to show the date.
#' @param show_text Whether to show the text.
#' @param show_id Whether to show the ID.
#' @param show_tags Whether to show the tags.
#' @param show_header Whether to show the header.
#' @param show_edit Whether to show the edit link.
#' @param fn The output file name.
#'
#' @return Nothing is returned; the function writes records to a file.
#'
#' @export
write_records = function(coll,
                         ids = NULL,
                         show_date = TRUE,
                         show_text = TRUE,
                         show_id = TRUE,
                         show_tags = TRUE,
                         show_header = TRUE,
                         show_edit = TRUE,
                         fn = "../output.tsv"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections to be able to search in texts")
  } else{
    if (show_date){p_date <- paste0("[", coll$corpus[ids]$date, "] ")}
    else {p_date <-""}
    if (show_text){p_text <- as.vector(as.character(coll$corpus)[ids])}
    else {p_text <-""}
    if (show_id){p_id <- paste0(" (", names(coll$corpus[ids]), ")")}
    else {p_id <-""}
    if (show_tags){p_tags <- coll$meta$tags[coll$meta$id %in% ids]}
    else {p_tags <-""}
    if (show_header){p_header <- coll$meta$tags_section[coll$meta$id %in% ids]}
    else {p_header <-""}
    if (show_edit){p_edit <- paste0("https://avisblatt.freizo.org/iiif/anno/", names(coll$corpus[ids]), "/edit")}
    else {p_edit <-""}
    output <- cbind(p_date, p_text, p_id, p_tags, p_header, p_edit)
    if (show_date){output <- output[order(p_date),]}
    output <- as.data.table(output)
    names(output)[1] <- paste0('\xEF\xBB\xBF', names(output)[1]) #add BOM, then easier to open in Excel
    fwrite(output, fn, sep="\t")
  }
}

#' Show IIIF images for specified record IDs
#'
#' This function produces a plot of IIIF(s) for each record.
#'
#' @param ids A vector of record IDs to display.
#' @param coll A Collection object.
#' @param show_record A logical indicating whether to display record metadata.
#' @param max.plot An integer indicating the maximum number of plots to display.
#'
#' @return A plot of IIIF(s) for each record.
#'
#' @examples
#' show_iiif(ids = c("id1", "id2"), coll = my_collection)
#' @import magick
#' @export
show_iiif <- function(coll,
                      ids = NULL,
                      show_record = TRUE,
                      max.plot = 10){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections to be able to display iiifs.")
  }
  ids <- intersect(ids, names(coll$corpus))
  if(length(ids)>max.plot){
    stop(sprintf("More than %d IDs provided. This function produces a plot of iiif(s) for each record, per default it handles no more than ten. If you want to plot more, set max.plot to a higher value.", max.plot))
  }
  if(length(ids)==0){
    stop("Could not find any record with the specified ID(s) in this collection.")
  }
  counter <- 0
  for(r in 1:length(ids)){
    id <- ids[r]
    if (show_record){
      output <- paste0("[", coll$corpus[id]$date, "] ",
                       as.character(coll$corpus)[id],
                       "\n Tags: ", coll$meta$tags[coll$meta$id == id],
                       "\n Header: ", coll$meta$tags_section[coll$meta$id == id],
                       "\n")
      cat(output)
    }
    frags_list <- list()
    frags_list[1]  <- coll$corpus[id]$fragment1
    frags_list[2]  <- coll$corpus[id]$fragment2
    frags_list[3]  <- coll$corpus[id]$fragment3
    frags_list[4]  <- coll$corpus[id]$fragment4
    frags_list[5]  <- coll$corpus[id]$fragment5
    frags_list[6]  <- coll$corpus[id]$fragment6
    frags_list[7]  <- coll$corpus[id]$fragment7
    frags_list[8]  <- coll$corpus[id]$fragment8
    frags_list[9]  <- coll$corpus[id]$fragment9
    frags_list[10] <- coll$corpus[id]$fragment10
    # if a fragment value does not contain iiif, it's not a iiif link,
    # so set frag to NA for easier handling it below:
    frags_list[!grepl("iiif", frags_list, fixed = TRUE)] <- NA
    n <- 10-sum(is.na(frags_list))
    # If it's all NA, abort:
    if(n == 0){
      message(sprintf("For record %s, no iiif are specified.", id))
    } else {
      imgs <- magick::image_read(frags_list[[1]])
      for(i in 2:10){
        if(!is.na(frags_list[[i]])){
          imgs <- c(imgs, magick::image_read(frags_list[[i]]))
        }
      }
      if(n>2){
        combined <- magick::image_append(imgs, stack=F)
      } else {
        combined <- magick::image_append(imgs, stack=T)
      }
      plot(combined)
      counter <- counter+1
    }
  }
  if(counter == 1){
    message("\n1 plot created (might take a moment to load)")
  } else {
    message(sprintf("\n%d plot(s) created  (might take a moment to load)", counter))
  }
}


#' Shows tags for a given set of document IDs
#'
#' This function shows the tags associated with a given set of document IDs.
#'
#' @param ids A vector of document IDs to show tags for.
#' @param coll A collection object.
#' @param manual A logical value indicating whether to show manual tags (TRUE) or automatic tags (FALSE).
#'
#' @return A vector of tags associated with the specified documents.
#'
#' @export
show_tags <- function(coll,
                      ids,
                      manual = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids) == 1)
  {
    if(ids == "all"){
      ids <- coll$meta$id
    }
  }
  if(manual){
    coll$meta$tags_manual[coll$meta$id %in% ids] |>
      unlist() |>
      unique() |>
      sort()
  } else {
    coll$meta$tags[coll$meta$id %in% ids] |>
      unlist() |>
      unique() |>
      sort()
  }
}


#' Shows headers for all documents in a collection
#'
#' This function shows the headers associated with all documents in a collection.
#'
#' @param coll A collection object.
#'
#' @return A vector of headers associated with all documents in the collection.
#'
#' @export
show_headers <- function(coll){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  unique(unlist(coll$meta$tags_section))
}


#' Shows metadata fields for all documents in a collection
#'
#' This function shows the metadata fields associated with all documents in a collection.
#'
#' @param coll A collection object.
#'
#' @return A character vector of metadata fields associated with all documents in the collection.
#'
#' @export
show_metadatafields <- function(coll){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  colnames(coll$meta)
}


