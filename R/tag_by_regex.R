require(avisblatt)

devtools::load_all()
# test collection
if(file.exists("../avis-data/test_collection.csv") & file.exists("../avis-data/test_collection.json")){
  c_test_collection <- read_collection("../avis-data/test_collection")
} else {
  c_all <- gather_yearly_collections(available_years(), just_meta = FALSE)
  test_ids <- c("145562fd-0404-5ef9-9883-ec88f81c1701/t10","145562fd-0404-5ef9-9883-ec88f81c1701/t11",
                "145562fd-0404-5ef9-9883-ec88f81c1701/t12", "1950674a-6b7e-5c0e-be0d-571c4a007f95/t1")
  c_test_collection <- c_all$clone(deep=T)
  c_test_collection$subset_collect(test_ids)
  write_collection(c_test_collection, "../avis-data/test_collection")
}
# tags for testing
df_test <- data.frame(tag = c("St. Peter","Münster","Kanzel"),reg = c("Pe(t|i|l)er(s|)(-|)","M(ü|u)nster", "(K|C)an(|t)zel"))

# vectorize grepl
grepl_vect  <- Vectorize(grepl)

# match regex in data frame consisting of two columns:
# 1: tag, 2: reg
# with string and return tags appearing in string as a vector
# input: adtext as string, df_tags as data frame
# output: vector of tags found
identify_tags_in_single_ad <- function(adtext,df_tags){
  return(df_tags$tag[which(grepl_vect(df_tags$reg,adtext))])
}
identify_tags_in_single_ad(as.character(c_test_collection$corpus)[4],df_test)

# match regex in data frame consisting of two columns:
# 1: tag, 2: reg
# with text in ad and assigns corresponding tag
# input: collection, data frame
# output: list of tags, named by corresponding ad id
identify_tags <- function(collection, df_tags){
  adtext <- as.character(collection$corpus)
  tags <- lapply(adtext,identify_tags_in_single_ad,df_tags = df_tags)
  return(tags)
}
identify_tags(c_test_collection,df_test)

# appends tags as defined in taglist to the elements of a corpus in a collection
# input:  collection
#         taglist - list of tags to append to ads named by id
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
    collection$meta[collection$meta$id == sId,]$tags <- unique(c(vOldTags,vNewTags))
  }
  return(collection)
}
c_test_collection_appended <- append_tags(c_test_collection,identify_tags(c_test_collection, df_test))
# Unit test for append_tags
if(is.element(FALSE,(c_test_collection_appended$meta$tags[[1]] == c("transactiontype_offer1","St. Peter")))){
  print("Unexpected result in append_tags!")
}

# append tags in a collection by matching regex in data frame with ad text
# input: collection
#        data frame, consisting of two columns:  1: tags, 2: regex
# output: collection

tag_by_regex <- function(collection, df_tags){
  return(append_tags(collection,identify_tags(collection,df_tags)))
}
c_test_collection_tagged <- tag_by_regex(c_test_collection,df_test)

