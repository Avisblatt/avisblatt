#' fetch tsv from Freizo, preprocess, store as csv

library(data.table)

#years <- c(1729, 1734, 1744, 1754, 1764, 1774, 1784, 1794, 1799, 1804, 1834)
years <- c(1744)
#' number of columns varies, last columns (iiif-links)
#' not named yet (DF is on it). This breaks FREAD
#' in all years but 1744

for (j in seq_along(years)){
  y <- years[j]


  freizo_url <- paste("https://avisblatt.freizo.org/curator/annotsv.cgi?year=", y, sep="")
  tsv <- fread(freizo_url, fill=TRUE, sep="\t", encoding="UTF-8")

  #last column always just contains "NA", so delete it.
  #sorry, couldn't do it in a not roundabout way...
  lastcol <- paste("V", ncol(tsv), sep="")
  setnames(tsv, lastcol, "superfluous")
  tsv[,superfluous:=NULL]

  #' this can go once df fixed it
  setnames(tsv,"@id", "id")

  #' imagefile columns are still unnamned in Freizo, so change it here
  #' this can go once df fixed it
  for (k in 40:ncol(tsv)){
    oldcol <- paste("V", k, sep="")
    newcol <- paste("image", k-39, sep="")
    setnames(tsv, oldcol, newcol)
  }

  #' groundtruth years still have some unmotivated
  #' linebreaks (DF is on it). Deleting such lines
  #' for now. This can go once DF has fixed it.
  setkey(tsv,id)
  tsv <- tsv[!"http?"]

  #stripping and cropping
  tsv$id <- gsub("https://avisblatt.freizo.org/iiif/anno/", "", tsv$id)
  tsv$text <- gsub("<p>", "", tsv$text)
  tsv$text <- gsub("</p>", "", tsv$text)

  #throwing out lines without text
  setkey(tsv,text)
  tsv <- tsv[!""]

  # throwing out lines generated in comparing different recognition procedures in Transkribus
  # may want to disable this for studying the recognition processes using R
  setkey(tsv,set)
  tsv <- tsv[!"test"]

  # throwing out duplicates
  setkey(tsv,id)
  unique(tsv)

  filename <- paste("..\\avis-data\\raw_data\\original_", y, ".csv", sep="")
  write.csv(tsv, file = filename)
}
