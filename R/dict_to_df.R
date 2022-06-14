# create a data frame from a dictionary with tag/regex pairings

dict_to_df <- function(tagfilter){
  df <- data.frame(tag = names(tagfilter$tagfilters$pos),
                   reg = "")
  for (i in 1:length(tagfilter$tagfilters$pos)) {
    df[,2][i] <- tagfilter$tagfilters$pos[[i]]
  }
  df
}
