# Interactive exploratory
# data analysis based on
# avisblatt years with available groundtruth
library(readtext)
library(quanteda)
library(jsonlite)
library(dplyr)
library(textcat)
library(ggplot2)
library(hrbrthemes)
source("R/avis_stop.R")
source("R/ocr_corrections.R")


## Setup for including available years and fancy log file to follow what happens
sFolder <- "/Users/serina00/Documents/repositories/avisblatt/"
## Log-file
sLogFile <- paste(sFolder,"temp/deskriptivebuecher.log",sep="")
file.create(sLogFile)
sLogHeader <- paste("Running deskriptivebuecher.Rnw\n",date(),
                    "\n---------------------------------------------\n",sep="")
write(sLogHeader,file=sLogFile,append=TRUE)
remove(sLogHeader)

## General dataframe
sFolderData <- paste(sFolder,"data/",sep = "")
sAllFiles <- list.files(sFolderData)
sDataSets <- sAllFiles[which(startsWith(sAllFiles,"groundtruth"))]
## Log entry START
sOmittedFiles <- setdiff(sAllFiles,sDataSets)
write("\nOmitted files\n---------------------------------------------\n",file=sLogFile,append=TRUE)
for(sFile in sOmittedFiles){
  write(paste("- ",sFile,sep=""),file=sLogFile,append=TRUE)
}
## Log entry STOP
remove(sAllFiles,sOmittedFiles)
iDataSets <- length(sDataSets)
if(iDataSets == 0){
  print("No datasets found")
} else {
  sFilename <- paste(sFolderData,sDataSets[1],sep="")
  df_combined <- read.csv(paste(sFilename,sep=""),
                 sep=",",
                 stringsAsFactors=FALSE,
                 encoding = "UTF-8")
  for(iSet in 2:iDataSets){
    sFilename <- paste(sFolderData,sDataSets[iSet],sep="")
    df_combined <- rbind(df_combined,read.csv(sFilename,sep=",",stringsAsFactors=FALSE, encoding = "UTF-8"))
  }
}

## split dates from one column yyyy-mm-dd to three columns yyyy, mm, dd to be able to distinguish issues
df_combined <- cbind(df_combined,year = rep.int(0,nrow(df_combined)),month= rep.int(0,nrow(df_combined)),day=rep.int(0,nrow(df_combined)))
lDate <- strsplit(df_combined$date,"-")
sDate <- matrix(unlist(lDate),ncol=3,byrow=TRUE)
df_combined$year <- as.integer(sDate[,1])
df_combined$month <- as.integer(sDate[,2])
df_combined$day <- as.integer(sDate[,3])

## for years available:
## Frequency of adtypes that include 05drucksachen

## Either: make local subset for frequency count of one year:
## df_1734 <- subset(df_combined, year == 1734)
## Then work with df_1734 for the time being.
##

## Or subset frequency for all existing groundtruth years:
for(iYear in unique(df_combined$year)){
df_loc <- subset(df_combined,year == iYear)
## make frequency table for tags that include 05drucksachen
df_freq_loc <- data.frame(tags = unique(df_loc[which(grepl("05",df_loc$adcontent)),]$adcontent),stringsAsFactors = FALSE)
df_freq_loc <- cbind(df_freq_loc,frequency=rep(0,nrow(df_freq_loc)))
## Fill frequency table df_loc_freq
for(i in c(1:nrow(df_freq_loc))){
  df_freq_loc[i,]$frequency <- length(which(df_loc$adcontent == df_freq_loc[i,]$tags))
}
df_freq_loc <- df_freq_loc[order(df_freq_loc$frequency, decreasing = TRUE),]
print(knitr::kable(
df_freq_loc,
col.names = c("Kategorie", "Anzahl"),
caption = paste("Vorkommen",iYear,sep=" ")
))
write.table(df_freq_loc,paste(sFolder,"temp/frequency_",iYear,".csv",sep=""),sep = ",")
}

## correct OCR and stop words; get rid of weirdly decoded "etc." symbol
df_loc$text <- correct_ocr(df_loc$text)
df_loc$text <- gsub("\ua75bc", "etc",df_loc$text)

corpus_df_loc <- corpus(df_loc[which(grepl("05",df_loc$adcontent)),])

extra_stopwords_books <- c("br", "zur", "nach", "basel", "zum", "bei", "sehr", "nebst", "allen", "herrn", "ich", "gr", "kr", "alle", "noch", "aller", "über", "nicht", "fl", "vom", "durch", "w", "s", "ohne", "r", "°", "m")

corpus_df_loc <- corpus_df_loc %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(avis_stop())
corpus_df_loc <- corpus_df_loc %>%
  tokens_remove(tokens(extra_stopwords_books))

dfm_df_loc <- dfm(corpus_df_loc)

## topfeatures(dfm_df_loc,30)
a <- names(topfeatures(dfm_df_loc,30))
print(knitr::kable(
a,
col.names = "Kategorie",
caption = "dfm",
escape = TRUE
))


## Some plots

freq_per_week <- data.frame(week = unique(df_combined[df_combined$year == 1734,]$date))
freq_per_week <- cbind(freq_per_week,frequency=rep(0,nrow(freq_per_week)))
## Fill frequency table freq_per_week
for(i in c(1:nrow(freq_per_week))){
  freq_per_week[i,]$frequency <- length(df_combined[df_combined$date == freq_per_week[i,]$week,]$adcontent)
}

barplot(freq_per_week$frequency,names.arg = freq_per_week$week)

###
freq_per_week <- data.frame(week = unique(df_combined$date))
freq_per_week <- cbind(freq_per_week,frequency=rep(0,nrow(freq_per_week)))
## Fill frequency table freq_per_week
for(i in c(1:nrow(freq_per_week))){
  freq_per_week[i,]$frequency <- length(df_combined[df_combined$date == freq_per_week[i,]$week,]$adcontent)
}

barplot(freq_per_week$frequency,names.arg = freq_per_week$week)


#### Notes for me
## other way for obtaining a subset of drucksachen
# "05" is tag for drucksachen in adcontent
# a <- unique(docvars(corpus_1734, field = "adcontent"))
# b <- which(grepl("05", a))
# c <- a[b]

# d <- unique(docvars(corpus_1754, field = "adcontent"))
# e <- which(grepl("05", d))
# f <- d[e]

# g <- unique(docvars(corpus_1834, field = "adcontent"))
# h <- which(grepl("05", g))
# i <- g[h]

## and another way for subset drucksachen
## List of entries in adcontent
# lAdcontents <- unique(corpus_1734[, "adcontent"])
## Indices of lAdcontents with entries pertaining to Drucksachen (encoded by substring "05")
## Note: grepl("05",lAdcontents) is a vector;
##      v[i] is TRUE if and only if lAdcontents[i] contains "05"
##       as a substring.
##       which(v) is the vector of indices of v which is TRUE
##      e.g. w[1] = 3 if and only if v[3] = TRUE
# iAdcontentsDruck <- which(grepl("05", lAdcontents))
## Given a vector w with integer entries and a vector u, the vector
## returned by u[w] is the vector containing only the
## entries for indices w
## e.g. let w <- c(1,3,4,8) and u <- c("a","b","c","a","b","c","d","a","x")
## we have
## > u[w]
## [1] "a" "c" "a" "a"
# filter_print <- lAdcontents[iAdcontentsDruck]

# druck_set_1734 <- corpus_subset(corpus_1734,(docvars(corpus_1734, "adcontent")%in% c))
# druck_set_1754 <- corpus_subset(corpus_1754,(docvars(corpus_1754, "adcontent")%in% f))
# druck_set_1834 <- corpus_subset(corpus_1834,(docvars(corpus_1834, "adcontent")%in% i))

# df_druckset <- data.frame("adtype"= docvars(druck_set_1734, field = "adtype"),
#                          "finance" = docvars(druck_set_1734, field = "finance"),
#                          "combined" = paste(docvars(druck_set_1734, field = "adtype"),
#                                             docvars(druck_set_1734, field = "finance"),
#                                             sep=","))
# counts_finance <- table(df_druckset$finance)
# barplot(counts_finance)
# counts_mode <- table(df_druckset$adtype)
# barplot(counts_mode)
# counts_combined <- table(df_druckset$combined)
# barplot(counts_combined)
