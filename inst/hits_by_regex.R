library(purrr)
library(dplyr)
library(stringr)

### main function is hits_by_regex()
### it uses three helper functions, find_hits(), add_metadatum() and find_hits_corpus()
### To create a dataframe using an already existing dictionary, the function
### dict_to_df() can be used

# helper function to find hits for multiple regex in one text
# text is any character string, tags is a dataframe with col 1 = tag and col 2 = regex (names do not matter)
find_hits <- function(text, tags){
  # locate all hits of every regex with start and end point of each hit
  hits <- str_locate_all(text, tags[,2])
  # rename hits with tag-names from dataframe
  names(hits) <- tags[,1]
  # use only start points and tag names
  t <- map(hits, ~ .x[,1])
  # make dataframe with tag name/ start point pairings for each hit and order
  df <- stack(t)[2:1]
  df <- df[order(df[,2]),]
  # paste tags in order of occurrence for text
  paste(df[,1])
}
# Add meta datum
# Input:  lmeta = named list of meta datum to be added. names are the ids
#         coll = collection to add metadata to
#         name = name of new meta datum
# This function adds a new meta datum to an existing collection by merging
# it with lmeta; lmeta
add_metadatum <- function(lmeta, coll, name){
  # Deal with the case where there already is a meta datum of the same name
  if(is.element(name,names(coll$meta))){
    print(paste("Warning: Meta data with name ", name, " already exists.", sep = ""))
    bool <- TRUE
    counter <- 0
    while(bool){
      s <- paste(name, ".", counter,sep = "")
      if(!is.element(s, names(coll$meta))){
        bool <- FALSE
        name <- s
      }
      counter <- counter + 1
    }
    print(paste("New meta data added as ", name, ".", sep = ""))
  }
  # Add a list entry (empty) to meta for the new meta datum
  coll$meta[[length(coll$meta) + 1]] <- vector(mode = "list", length = length(coll$meta$id))
  # set the name of the new meta datum
  names(coll$meta)[length(coll$meta)] <- name
  # Actual merge: seq_along(lmeta) takes every item of lmeta as index
  invisible(lapply(seq_along(lmeta),
                   # counter set to "i"; takes id of ith entry of lmeta
                   function(i) {
                     id <- names(lmeta)[i]
                     # if ith entry of lmeta contains new meta datum
                     if(length(lmeta[[i]])>0){
                       # go into new (empty) list of recently added new meta datum
                       # and set entry corresponding to id of ith entry to the new
                       # meta datum for ith entry
                       coll$meta[[length(coll$meta)]][which(coll$meta$id == id)] <- list(lmeta[[i]])
                     }
                   }))
}


# Wrapper to apply find_hits() to a corpus with selection by ids
# By taking all ids of a corpus into account, we can avoid using
# which() in a loop, cf. looping over selection of ids
find_hits_corpus <- function(ids, corpus, hits){
  # output of lapply is a list
  # if lapply loops over setNames(iterator, named_set), then the resulting list is
  # a named list with names in named_set, chosen according to iterator
  lHits <- lapply(setNames(seq_along(corpus), names(corpus)),
                  function(i) if(is.element(names(corpus)[i],ids)){
                    find_hits(corpus[i], hits)
                  })
  return(lHits)
}


# Wrapper to apply find_hits() to a corpus in a collection and extend the
# meta data accordingly
# Inputs: ids = vector of ids to be checked
#         coll = collection
#         hits = df of hits in 1st column and corresponding regex in 2nd column
#         name = name to be given to new meta datum as string
hits_by_regex <- function(ids, coll, hits, name){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids) == 1){
    if(ids == "all"){
      ids <- coll$meta$id
    }
  }
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections to be able to tag by regex")
  } else {
    lHits <- find_hits_corpus(ids, coll$corpus, hits)
    add_metadatum(lHits, coll, name)
  }
}


