#' fetch tsv from Freizo, preprocess, store as csv

#' available years
#' 1729, 1734, 1744, 1754, 1764,
#' 1774, 1784, 1794, 1799, 1804,
#' 1834

library(data.table)
library(jsonlite)
years <- c(1729:1738)
available_years <- vector()

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
  tsv <- tsv[id %like% "http"]

  #throwing out lines without text
  setkey(tsv,text)
  tsv <- tsv[!""]
  tsv <- tsv[!is.na(text)]

  if(nrow(tsv)>0) {
    #stripping and cropping
    tsv$id <- gsub("https://avisblatt.freizo.org/iiif/anno/", "", tsv$id)
    tsv$text <- gsub("<p>", "", tsv$text)
    tsv$text <- gsub("</p>", "", tsv$text)

    # throwing out lines generated in comparing different recognition procedures in Transkribus
    # may want to disable this for studying the recognition processes using R
    setkey(tsv,set)
    tsv <- tsv[!"test"]

    # throwing out duplicates
    setkey(tsv,id)
    unique(tsv)

    filename <- paste("..\\avis-data\\raw_data\\original_", y, ".csv", sep="")
    write.csv(tsv, file = filename)
    available_years <- c(available_years, y)
  }
}

write_json(available_years,
             path =  "../avis-data/raw_data/available_years.json")
