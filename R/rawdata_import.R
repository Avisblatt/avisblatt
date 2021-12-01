#' @import data.table
#' @import dplyr
#' @export
fetch_from_freizo <- function(AVIS_YEARS = 1729:1844,
                              dest_path = "../avis-databuffer/raw_data_uncorrected",
                              gt_years = c(1734, 1754, 1774, 1834)){
  for (i in AVIS_YEARS){
    fn <- sprintf("orig_%d.csv", i)
    tryCatch({
      download.file(
        sprintf(
          "https://avisblatt.freizo.org/curator/annotsv.cgi?year=%d", i),
        destfile = fn)
      Sys.sleep(5)
      message("Downloaded data.")
      raw_data <- RawData$new(i, URL = fn, local = TRUE)
      message("R6 class for raw_data created.")
      raw_data$booleanize(c("isheader","noadvert"))
      message("Booleanized.")
      raw_data$process_data(ocr_correction = FALSE, drop_na_cols = FALSE)
      message("data processed.")
      drop_cols <- c("at","set","withincat","year","month","date",
                     "canvas","name","avistable", "wrkprofession",
                     "wrkcompetence", "wrkcondition", "wrkmodality",
                     "textyp", "texmod", "texatt", "texzus",
                     "hautyp", "haumod", "hauatt", "hauzus", "hauort",
                     "editlink")
      if(!(i %in% gt_years)){
        drop_cols <- union(drop_cols,
                           c("adcontent", "adtype", "finance", "keyword"))
      }
      drop_cols <- intersect(colnames(raw_data$data),
                             drop_cols)
      raw_data$drop_cols(col = drop_cols)
      message(sprintf("%s columns dropped.", paste(drop_cols, collapse = ",")))
      setnames(raw_data$data,old = "isodate", "date", skip_absent = TRUE)
      raw_data$write_log_status()
      message("log status written.")
      
      raw_data$write_csv(fn = file.path(dest_path, fn))
      file.remove(fn)
      dir(recursive = T)
    }, error = function(e){
      message(
        sprintf("%d could not be processed properly.", i)
      )
    })
    message("\n\n")
  }
}  
  
#' @import data.table
#' @import dplyr
#' @import xml
#' @export
xml_direct_import <- function(AVIS_YEARS = 1729:1844,
                              dest_path = "../avis-databuffer/raw_data_uncorrected",
                              gt_years = c(1734, 1754, 1774, 1834)){
  # Before running the script,
  # download xml for desired years from Transkribus.
  # Store them to the avis-data repo, foldr xml/years.
  # Preserve the folder structure in the download,
  # i.e. the page XMLs for, say, 1729 should be in folder
  # xml/years/1729/1729/pages
  
  # make a list of all pages that are actually part of any issue
  meta_info <- fread("../avis-databuffer/xml/meta/meta.csv", encoding="UTF-8")
  meta_info <- meta_info[type == "avis"]
  avis_files <- paste0(meta_info$file_id, ".xml")
  
  #get mastheads
  mastheads <- fread("../avis-databuffer/xml/meta/mastheads.csv", encoding="UTF-8")
  mastheads <- cbind(mastheads, year= as.integer(substr(mastheads$date,1,4)))
  
  AVIS_YEARS <- intersect(AVIS_YEARS, list.files("../avis-databuffer/xml/years") %>% as.numeric)
  
  # have some protocol dt for structure type problems
  stp_missingmulti <- data.table()
  stp_missingcont <- data.table()
  
  for (i in AVIS_YEARS){
    issues <- mastheads[year==i]
    issues <- issues[order(issue),]
    if (identical(row.names(issues), as.character(issues$issue))){
      #prepare data table of all text regions on all pages
      y_ads <- data.table(id=character(), pageno=integer(), readingorder=integer(), structuretype=character(), text=character())
      pages <- list()
      # get xml with the orderd list of files that make up this year's volume
      pagelistxml <- xmlParse(file.path("..", "avis-data", "xml", "years", i, i, "mets.xml"))
      pagelistxml <- xmlChildren(xmlRoot(pagelistxml))[[3]]
      # Navigation in xml tree is REALLY clumsy here and in the following...
      pagelistxml <- xmlChildren(pagelistxml)[[1]]
      pagelistxml <- xmlChildren(pagelistxml)[[1]]
      pages <- vector("list", xmlSize(pagelistxml))
      # Iterate through the nodes and extract the filename
      for (p in 1:xmlSize(pagelistxml)){
        pages[p] <- xmlGetAttr(xmlChildren(xmlChildren(pagelistxml)[[p]])[[1]], name = "href")
      }
      #Remove the path, and then pick only those files that are part of issues
      pages <- gsub("page/", "", pages)
      pages <- intersect(pages, avis_files)
      meta_i <- meta_info[year == i]
      for (p in pages){
        fp <- file.path("..", "avis-data", "xml", "years", i, i, "page", p)
        pxml <- xmlParse(fp)
        page <- xmlChildren(xmlRoot(pxml))[[2]]
        img_pageno <- as.integer(substr(fp,nchar(fp)-7,nchar(fp)-4))
        if(xmlSize(page)>1){
          for(j in 1:xmlSize(page)){
            tr <- xmlChildren(page)[[j]]
            #pick a node only if it's a textregion
            if(xmlName(tr) == "TextRegion"){
              custom <- xmlGetAttr(tr, name = "custom")
              ro <- as.integer(gsub("[^0-9]", "",  custom))
              if(grepl("structure", custom)){
                st <- gsub(".*structure \\{type\\:", "",  custom)
                st <- substr(st,1,nchar(st)-2)
              } else {
                st <- "None"
              }
              txt <- xmlValue(tr[[xmlSize(tr)]])
              meta_p <- meta_i[meta_i$file_id == substr(p,1,nchar(p)-4)]
              pageno <- meta_p$book_manifest_sort_order
              id <- paste("temp", i, sprintf("%03d", img_pageno), sprintf("%03d", ro), sep = "-")
              y_ads <- rbind(y_ads, cbind(id, pageno, ro, st, txt), use.names=FALSE)
            }
          }
        }
      }
      message(sprintf("Ingested all pages for %d.", i))
      
      # check if there are continuations without multiarticle tag in front of it
      for (j in nrow(y_ads):2){
        if(y_ads$structuretype[j] == "Continuation"){
          if(!(y_ads$structuretype[j-1] %in% c("Continuation", "Multi_article", "Multi_no_advert"))){
            stp_missingmulti <- rbind(stp_missingmulti, y_ads[j-1])
          }
        }
      }
      # check if there are multiarticle tags not followed by continuation
      for (j in 2:nrow(y_ads)){
        if(y_ads$structuretype[j-1] %in% c("Multi_article", "Multi_no_advert")){
          if(y_ads$structuretype[j] != "Continuation"){
            stp_missingcont <- rbind(stp_missingcont, y_ads[j])
          }
        }
      }
      
      # now merge continuations with previous record, from last to first, so that a sequence of "Continuation"s is correctly folded up
      for (j in nrow(y_ads):2){
        if(y_ads$structuretype[j] == "Continuation"){
          y_ads$text[j-1] <- paste0(y_ads$text[j-1], "\n", y_ads$text[j])
        }
      }
      y_ads <- y_ads[structuretype != "Continuation"]
      
      # compose and clean up data table
      y_ads <- cbind(y_ads, grepl("Header", y_ads$structuretype))
      y_ads <- cbind(y_ads, grepl("No_advert", y_ads$structuretype))
      y_ads[,structuretype:=NULL]
      y_ads <- cbind("N", 1, issues$book[1], y_ads)
      names(y_ads) <- c("rev", "issue", "book", "id", "pageno", "readingorder", "text", "isheader", "noadvert")
      
      # Compile orig file
      if(i %in% gt_years){
        orig <- fread("../avis-databuffer/xml/orig_gt.csv", encoding="UTF-8")
      } else {
        orig <- fread("../avis-databuffer/xml/orig.csv", encoding="UTF-8")
      }
      class(y_ads$pageno) <- "integer"
      class(y_ads$readingorder) <- "integer"
      class(orig$id) <- "character"
      class(orig$rev) <- "character"
      class(orig$pageno) <- "integer"
      class(orig$readingorder) <- "integer"
      class(orig$text) <- "character"
      orig <- bind_rows(orig, y_ads)
      
      # update issue, date
      for (j in 1:nrow(issues)){
        issue_startpage <- issues$pageno[j]
        orig$issue[orig$pageno >= issue_startpage] <- issues$issue[j]
        orig$date[orig$pageno >= issue_startpage] <- issues$date[j]
      }
      class(orig$date) <- "Date"
      
      #Clean up the text
      orig[,text := gsub("([A-Z][a-z])\\\n([a-z]\\w)", "\\1\\2", text)] # catch some missing hyphens
      orig[,text := gsub("-\\\n", "", text)]
      orig[,text := gsub("\\\n", " ", text)]
      orig[,text := purge_spacing(text)]
      orig[,text := gsub(" {2,}", " ", text)]
      
      fwrite(orig, file.path(dest_path, sprintf("orig_%d.csv", i)))
      message(sprintf("Raw data written for %d.", i))
      
    } else {
      message("Aborted this year. Issue list is not correct.")
    }
  }
  
  if(length(stp_missingcont)==0){
    stp_missing <- cbind("multi?", stp_missingmulti)
  } else if(length(stp_missingmulti)==0){
    stp_missing <- cbind("cont?", stp_missingcont)
  } else {
    stp_missing <- rbind(cbind("cont?", stp_missingcont),
                         cbind("multi?", stp_missingmulti))
  }
  
  if(nrow(stp_missingcont)>0){
    stp_missing[,text := gsub("\\\n", " ", text)]
    fwrite(stp_missing[order(id),], "../avis-databuffer/xml/stp_missingcont.tsv", sep = "\t")
    message("Records with doubtful structure types written to avis-data/xml/stp_missingcont.tsv")
  } else {
    message("No records with doubtful structure type found.")
  }
}