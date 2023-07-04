# TODO bring this back to R/ folder
# escape non-ascii characters
# remove non-function part at the beginning.

# test collection
bTestCollection <- file.exists("../avis-data/test_collection.csv") & file.exists("../avis-data/test_collection.json")
if(bTestCollection){
  c_test_collection <- read_collection("../avis-data/test_collection")
  sTestIds <- c("145562fd-0404-5ef9-9883-ec88f81c1701/t10",
                "145562fd-0404-5ef9-9883-ec88f81c1701/t11",
                "ffed5536-3941-59fe-b343-f2d30f8e822a/t25")
  # tags for testing
  df_test <- data.frame(tag = c("St. Peter","Münster","Kanzel", "Allerhand"),reg = c("Pe(t|i|l)er(s|)(-|)","M(ü|u)nster", "(K|C)an(|t)zel", "Aller(h|b)and"))
}


# vectorize grepl
# makes grepl() applicable for more than one pattern
grepl_vect  <- function(patterns,strings){
  bWhichTags <- sapply(patterns,grepl,perl = TRUE,x = strings)
  return(bWhichTags)
}

# identify tags in a single string as a first step:
# match regex in data frame consisting of two columns:
# 1: tag, 2: reg
# with string and return tags appearing in string as a vector
# input: adtext as string, df_tags as data frame
# output: vector of tags found
identify_tags_in_single_ad <- function(adtext,df_tags){
  return(df_tags$tag[which(grepl_vect(df_tags$reg,adtext))])
}

# Unit test for identify_tags_in_single_ad
if(bTestCollection){
  if(!setequal(identify_tags_in_single_ad(as.character(c_test_collection$corpus)[4],df_test),c("Münster","Kanzel"))){
    message("identify_tags_in_single_ad test failed")
  }
}

# identify tags in numerous strings as a second step:
# match regex in data frame consisting of two columns:
# 1: tag, 2: reg
# with strings (= text in ads) and assign corresponding tag
# input: collection, data frame
# output: list of tags, named by corresponding ad id
identify_tags <- function(ids, collection, df_tags){
  if(length(ids) == 1 & ids[1] == "all"){
    adtext <- as.character(collection$corpus)
    taglist <- lapply(adtext,identify_tags_in_single_ad,df_tags = df_tags)
    return(taglist)
  } else {
    temp_collection <- collection$clone(deep=T)
    temp_collection$subset_collect(ids)
    adtext <- as.character(temp_collection$corpus)
    taglist <- lapply(adtext,identify_tags_in_single_ad,df_tags = df_tags)
    return(taglist)
  }
}

# Unit test for identify_tags
if(bTestCollection){
  if(!setequal(identify_tags(sTestIds,c_test_collection,df_test)[[2]],character(0))){
    stop("identify_tags test failed")
  }
}

# append tags as defined in taglist to the elements of a corpus in a collection
# input:  collection
#         taglist - list of tags to append to ads named by id
# (taglist is generated in identify_tags() which is called during tag_by_regex())
# output: collection
append_tags <- function(collection, taglist){
  for(iTag in 1:length(taglist)){
    # id of iTag-th entry
    sId <- names(taglist)[iTag]
    # old tags
    vOldTags <- collection$meta[collection$meta$id == sId,]$tags[[1]]
    # replace old tags by concatenation of
    # old tags and new tags
    vNewTags <- taglist[[iTag]]
    if(length(vNewTags)>0){
    if(is.na(vOldTags[1])){
      collection$meta[collection$meta$id == sId,]$tags <- vNewTags
    } else {
      collection$meta[collection$meta$id == sId,]$tags <- unique(c(vOldTags,vNewTags))
    }
    }
  }
}

# Unit test for append_tags
if(bTestCollection){
  append_tags(c_test_collection,identify_tags(sTestIds,c_test_collection, df_test))
  if(is.element(FALSE,(c_test_collection$meta$tags[[1]] == c("transactiontype_offer1","St. Peter")))){
    message("Unexpected result in append_tags!")
  }
}

# append tags in a collection by matching regex in data frame with ad text
# input: ids
#        collection
#        data frame, consisting of two named columns:  1: tag, 2: reg
# output: void (environment is changed, no return needed)
tag_by_regex <- function(ids, collection, df_tags){
  stopifnot(inherits(collection, "Collection"))
  stopifnot(inherits(collection, "R6"))
  if(is.null(collection$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections to be able to tag")
  }
  if(!setequal(names(df_tags),c("tag","reg"))){
    stop("df_tags must have two columns, one named tag, one named reg.")
  }
  append_tags(collection,identify_tags(ids, collection,df_tags))
}

# Unit test for tag_by_regex
if(bTestCollection){
  tag_by_regex(sTestIds,c_test_collection,df_test)
  lResult <- list(
    c("transactiontype_offer1","St. Peter"),
    c("transactiontype_offer1"),
    c("extinguisher","transactiontype_offer1","ut_things_devicesNcomponents"),
    c("chair","churchseat","transactiontype_offer1","ut_household"),
    c("Allerhand")
  )
  for(listElement in 1:length(c_test_collection$meta$tags)){
    if(!setequal(c_test_collection$meta$tags[[listElement]],lResult[[listElement]])){
      stop("tag_by_regex test failed")
    }
  }
}

# append a chosen tag (not depending on a regex) to a subset of the collection
# given by a vector of ids
# input: ids
#        collection
#        tag to be added as a string
# output: void (environment is changed, no return needed)
tag_selection <- function(ids, collection, tag){
  df_tags <- data.frame(tag = tag, reg = "")
  tag_by_regex(ids, collection, df_tags)
}

# Unit test for tag_selection
if(bTestCollection){
  tag_selection(c(sTestIds[1],sTestIds[3]),c_test_collection,"selected_id")
  lResult <- list(
    c("transactiontype_offer1","St. Peter","selected_id"),
    c("transactiontype_offer1"),
    c("extinguisher","transactiontype_offer1","ut_things_devicesNcomponents"),
    c("chair","churchseat","transactiontype_offer1","ut_household"),
    c("Allerhand","selected_id")
  )
  for(listElement in 1:length(c_test_collection$meta$tags)){
    if(!setequal(c_test_collection$meta$tags[[listElement]],lResult[[listElement]])){
      stop("tag_selection test failed")
    }
  }
}

if(!bTestCollection){
  message("test_by_regex loaded without unit tests")
}
