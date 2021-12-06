# match regex in data frame consisting of two columns:
# 1: tags, 2: regex
# with text in ad and assigns corresponding tag
# input: corpus, data frame
# output: list of tags, named by corresponding ad id
identify_tags_by_regex <- function(ad,df_tags){
  adtext <- as.character(ad)
  tags <- df_tags[which(grepl_vect(df_tags[,2],adtext)),1]
  return(tags)
}


# appends tags as defined in taglist to the elements of a corpus
# input:  corpus
#         taglist - list of tags to append to ads named by id
# output: corpus
append_tags_by_regex <- function(corpus,taglist){
  for(iTag in 1:length(taglist)){
    sId <- names(taglist[iTag])
    vTags <- corpus$meta[corpus$meta$id == sId,]$tags[[1]]
    corpus$meta[corpus$meta$id == sId,]$tags <- c(vTags,taglist[[iTag]])
  }
  return(corpus)
}

# append tags in a corpus by matching regex in data frame with ad text
# input: corpus
#        data frame, consisting of two columns:  1: tags, 2: regex
# output: corpus

tag_by_regex <- function(corpus,df_tags){
  return(append_tags(corpus,identify_tags(corpus,df_tags)))
}
