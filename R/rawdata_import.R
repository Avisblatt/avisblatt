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
  

#' @import XML
#' @export
xml_direct_import <- function(AVIS_YEARS = 1729:1844,
                              source_path = "../avis-databuffer/xml",
                              dest_path = "../avis-databuffer/raw_data_uncorrected",
                              gt_years = c(1734, 1754, 1774, 1834)){
  # Before running the script,
  # download xml for desired years from Transkribus.
  # Store them to the avis-data repo, foldr xml/years.
  # Preserve the folder structure in the download,
  # i.e. the page XMLs for, say, 1729 should be in folder
  # xml/years/1729/1729/pages
  
  # make a list of all pages that are actually part of any issue
  meta_info <- fread(file.path(source_path, "meta", "meta.csv"), encoding="UTF-8")
  meta_info <- meta_info[type == "avis"]
  avis_files <- paste0(meta_info$file_id, ".xml")
  
  #get mastheads
  mastheads <- fread(file.path(source_path, "meta", "mastheads.csv"), encoding="UTF-8")
  mastheads <- cbind(mastheads, year= as.integer(substr(mastheads$date,1,4)))
  
  AVIS_YEARS <- intersect(AVIS_YEARS, list.files(file.path(source_path, "years")) %>% as.numeric)
  
  # have some protocol dt for structure type problems
  stp_missingmulti <- data.table()
  stp_missingcont <- data.table()
  
  for (i in AVIS_YEARS){
    issues <- mastheads[year==i]
    issues <- issues[order(issue),]
    if (identical(row.names(issues), as.character(issues$issue))){
      #prepare data table of all text regions on all pages
      y_ads <- data.table(id=character(), 
                          pageno=integer(), 
                          readingorder=integer(), 
                          structuretype=character(), 
                          text=character(), 
                          inscribed=character(),
                          fragment=character())
      pages <- list()
      # get xml with the orderd list of files that make up this year's volume
      pagelistxml <- xmlParse(file.path(source_path, "years", i, i, "mets.xml"))
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
        fp <- file.path(source_path, "years", i, i, "page", p)
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
              points <- xmlGetAttr(tr[[1]], name = "points")
              points <- strsplit(points, "\\s") %>% unlist
              points <- strsplit(points, ",") %>% unlist %>% as.integer
              # pick meta infos for dt entry compilation
              meta_p <- meta_i[meta_i$file_id == substr(p,1,nchar(p)-4)]
              pageno <- meta_p$book_page_order_df
              inscribed <- meta_p$Inscribed
              canvas <- meta_p$canvas
              # dt entry compilation
              n <- as.integer(length(points)/2) #how many points are there? can be more than four!
              xp <- seq.int(1, 2*n-1, by = 2) #for picking x coordinates
              yp <- seq.int(2, 2*n, by = 2) #for picking y coordinates
              x <- min(points[xp])
              y <- min(points[yp])
              xdiff <- max(points[xp]) - x
              ydiff <- max(points[yp]) - y
              coord <- paste(x, y, xdiff, ydiff,
                             sep = ",")
              fragment <- paste0("https://iiif.avisblatt.freizo.org/image/",
                                 canvas,
                                 "_0000/",
                                 coord,
                                 "/full/0/default.jpg")
              id <- paste("temp", i, sprintf("%03d", pageno), sprintf("%03d", ro), sep = "-")
              y_ads <- rbind(y_ads, cbind(id, pageno, ro, st, txt, inscribed, fragment), use.names=FALSE)
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
          y_ads$fragment[j-1] <- paste(y_ads$fragment[j-1], y_ads$fragment[j], sep = " ")
        }
      }
      y_ads <- y_ads[structuretype != "Continuation"]
      
      # compose and clean up data table
      y_ads <- cbind(y_ads, grepl("Header", y_ads$structuretype))
      y_ads <- cbind(y_ads, grepl("No_advert", y_ads$structuretype))
      y_ads[,structuretype:=NULL]
      y_ads <- cbind("N", 1, issues$book[1], y_ads)
      names(y_ads) <- c("rev", "issue", "book", "id", "pageno", "readingorder", "text", "inscribed", "fragment", "isheader", "noadvert")
      
      # clean reading order
      y_ads$readingorder <- ave(y_ads$id, y_ads$pageno, FUN = seq_along)
      
      # distribute fragments over columns,
      # remove fragment list column
      frags <- strsplit(y_ads$fragment, "\\s") %>%
        lapply('length<-', 10) %>%
        transpose %>%
        data.frame
      colnames(frags) <- c("fragment1", "fragment2", "fragment3", "fragment4", "fragment5", "fragment6", "fragment7", "fragment8", "fragment9", "fragment10")
      y_ads$fragment <- NULL
      y_ads <- cbind(y_ads, frags)
      
      # Compile orig file
      if(i %in% gt_years){
        orig <- fread(file.path(source_path, "orig_gt.csv"), encoding="UTF-8")
      } else {
        orig <- fread(file.path(source_path, "orig.csv"), encoding="UTF-8")
      }
      class(y_ads$pageno) <- "integer"
      class(y_ads$readingorder) <- "integer"
      class(orig$pageno) <- "integer"
      class(orig$readingorder) <- "integer"
      
      class(orig$id) <- "character"
      class(orig$rev) <- "character"
      class(orig$text) <- "character"
      class(orig$inscribed) <- "character"
      class(orig$fragment1) <- "character"
      class(orig$fragment2) <- "character"
      class(orig$fragment3) <- "character"
      class(orig$fragment4) <- "character"
      class(orig$fragment5) <- "character"
      class(orig$fragment6) <- "character"
      class(orig$fragment7) <- "character"
      class(orig$fragment8) <- "character"
      class(orig$fragment9) <- "character"
      class(orig$fragment10) <- "character"

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
      orig[,text := gsub("(-\\\n)([a-z])", "\\2", text)]
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
    fwrite(stp_missing[order(id),], file.path(source_path, "stp_missingcont.tsv"), sep = "\t")
    message(paste0("Records with doubtful structure types written to ", file.path(source_path, "stp_missingcont.tsv")))
  } else {
    message("No records with doubtful structure type found.")
  }
}