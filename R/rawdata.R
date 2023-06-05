#' @import quanteda
#' @import data.table
#' @importFrom R6 R6Class
#' @rawNamespace import(dplyr, except = c(first, last, between))
#' @export
RawData <- R6Class("RawData", list(
  data = NULL,
  year = NULL,
  status = NULL,
  message = NULL,
  initialize = function(year,
                        URL = "https://avisblatt.freizo.org/curator/annotsv.cgi?year=",
                        local = FALSE){
    self$year <- year
    out <- tryCatch({
      if(!is.numeric(year)) stop("year needs to be numeric.")
      if(!local) URL <- sprintf('%s%s', URL, year)
      res <- fread(URL,
                   fill=TRUE,
                   sep="\t",
                   quote="",
                   encoding="UTF-8")
      if(nrow(res) == 0) stop("returned empty data.table.")

      if(length(unique(self$data$year)) > 1 ) stop("Corrupted FREIZO input. More than one year detected!")
      self$status <- "SUCCESS"
      self$message <- "Step 1 of 2 ok (download)."
      self$data <- res
    }, error = function(e){
      self$status <- "FAILURE"
      self$message <- "Step 1 of 2 failed (download)."
    })
  },
  booleanize = function(cols){
    for(i in cols){
      self$data[is.na(get(i)), (i) := 0]
      self$data[, (i) := as.logical(get(i))]
    }
  },
  drop_on_condition = function(col, value){
    self$data <- self$data[!get(col) %in% value,]
  },
  drop_cols = function(col){
    self$data[, (col) := NULL]
  },
  process_data = function(clean_formatting = TRUE,
                          ocr_correction = TRUE,
                          remove_test_data = TRUE,
                          remove_invalid_data = TRUE,
                          remove_duplicates = TRUE,
                          drop_na_cols = TRUE,
                          gt_years = c(1734,1754,1774,1834),
                          test_users = c("Dan Granville","Peter Cornwell")){
    out <- tryCatch({
      if(any(grepl("@",names(self$data)))){
        setnames(self$data, "@id","id")
      }

      # I moved the throwing out of lines up here, to be applied
      # before clean formatting and reading order calculation
      # (no need and potential source of problems to process
      # the records that are to be dropped) (AE)
      # ...and somehow this seems also to cure the problem that
      # cleaning, mast/table dropping, and reading order creation
      # work for gt-years 1734 and 1774, but not 1754 and 1834

      # if there are duplicates, then most likely because
      # data reimported from Transkribus to Freizo has been
      # accidently appended to old data instead of overwriting it,
      # so keep the later duplicates (fromLast)
      if(remove_duplicates){
        self$data <- self$data[!(duplicated(id, fromLast = T)),]
      }

      if(remove_test_data){
        setkey(self$data,name)
        self$data <- self$data[!test_users]
        # I added the next line as "test" is found in the set, not name column (AE)
        setkey(self$data,set)
        self$data <- self$data[!"test"]
      }

      if(remove_invalid_data){
        # throwing out lines not referring to ads
        setkey(self$data,set)
        self$data <- self$data[!"tables"]
        self$data <- self$data[!"mast"]
      }

      if(clean_formatting){
        if(!("textutils" %in% rownames(installed.packages()))){
          stop("Package textutils is not installed. Please install the package from CRAN or set clean_formatting to FALSE.")
        }

        # sanitize line breaks
        self$data[,text := gsub("([A-Z][a-z])\\\\n([a-z]\\w)", "\\1\\2", text)] # catch some missing hyphens
        self$data[,text := gsub("(-\\\\n)([a-z])", "\\2", text)]
        self$data[,text := gsub("\\\\n", " ", text)]

        # Decode HTML (stuff like &amp; -> &)
        self$data[, text := textutils::HTMLdecode(text)]
        self$data[, rnotes := textutils::HTMLdecode(rnotes)]

        # strip ids and HTML
        self$data[,id := gsub("https://avisblatt.freizo.org/iiif/anno/", "", id)]
        self$data[,text := gsub("<.*?>", "", text)]
        self$data[,rnotes := gsub("<.*?>", "", rnotes)]

        # remove redundant blanks (better BEFORE ocr correction)
        self$data[,text := purge_spacing(text)]
        self$data[,text := gsub(" {2,}", " ", text)]
        }

      if(ocr_correction){
        # apply ocr corrections
        self$data[, text := correct_ocr(self$data$text)]
        }

      if(self$year %in% gt_years){
        message("GT year detected, starting special treatment...")
        if (self$year == 1754){
          updated_tags <- fread("../avis-data/freizo-corrections/tags_1754.csv")
          message(nrow(updated_tags))
          # only use those updates that are still present to avoid problems with recycling
          updated_tags <- updated_tags[(id %in% self$data$id),]
          # records in updated_tags are sorted differently than in self$data,
          # so have to sort updated_tags in that same order first. Use merge:
          ids <- as.data.table(self$data[(id %in% updated_tags$id)]$id)
          colnames(ids) <- "id"
          updated_tags <- merge(ids, updated_tags, sort = F)

          self$data[(id %in% updated_tags$id), adcontent := updated_tags$adcontent]
          self$data[(id %in% updated_tags$id), adtype := updated_tags$adtype]
          self$data[(id %in% updated_tags$id), finance := updated_tags$finance]
        }

        # implement code to determine the reading order
        # of ads on a given page. This wasn't done for
        # groundtruth years, but is now necessary to
        # inherit section headers


        # extract coordinates from URL using a backreferencing regexp
        # in order to easily turn into a data.table
        coords <- gsub("(.+)/([0-9]+,[0-9]+,[0-9]+,[0-9]+)(/.+)","\\2", self$data$fragment1)
        coordinates <- as.data.table(tstrsplit(coords, ",", type.convert = TRUE, fixed = TRUE))
        names(coordinates) <- c("x","y","width","height")

        # compute average in min in order to create the concept of
        # page columns
        average_ad_width <- mean(coordinates$width, trim = 0.1)
        leftmost_x <- min(coordinates$x)
        pagewidth_covered_by_ads <- max(coordinates$width + coordinates$x) - leftmost_x
        # add 2nd and 3rd cols first, followin Alex Engel
        coordinates[, column_on_page := 1]
        coordinates[(x > leftmost_x + average_ad_width*2/3), column_on_page := 2]
        coordinates[(x > leftmost_x + average_ad_width*5/3), column_on_page := 3]

        # Use pseudo 4th column
        # for end-of-page-ads that
        # span whole page
        coordinates[(width > pagewidth_covered_by_ads*2/3), column_on_page := 4]
        coord_dt <- cbind(id = self$data$id, pageno =  self$data$pageno, coordinates)
        coords_ordered <- coord_dt[order(pageno, column_on_page, y),]
        coords_ordered$readingorder <- rowid(coords_ordered$pageno)

        # make sure coords data.table has the some order of rows as the initital dataset
        self$data$readingorder <- coords_ordered[(match(self$data$id, coords_ordered$id)), "readingorder"]
      }

      if(drop_na_cols){
        nms <- names(self$data)[apply(self$data, 2, function(x) all(is.na(x)))]
        self$data[, (nms) := NULL]
      }

      # make sure it's ordered in a very last step, because this important for
      # header based inheritance
      self$data <- self$data[order(pageno,readingorder),]
      #renumber the reading order, as those are stored as integers,
      #but we have something like "24.5" in the tsv for several years
      #(for an ad later added between ads #24 and #25)
      self$data <- self$data |>
        group_by(pageno) |>
        mutate(readingorder = 1:n()) |>
        as.data.table()
      self$status <- "SUCCESS"
      self$message <- "Step 2 of 2 (processing) ok."
    }, error = function(e){
      self$status <- "FAILURE"
      self$message <- "Step 2 of 2 (processing) failed."
    })
  },
  write_log_status = function(
    log_file = "README.md",
    mark_down_table_log_format = TRUE,
    line_end = "\n"
  ){
    sink(log_file, append = TRUE)
    if(mark_down_table_log_format){
      msg <- sprintf('| %s | %s | %s | %s |%s', Sys.time(), self$year, self$status,
                     self$message, line_end)
    } else {
      msg <- sprintf('"%s";%d;"%s";"%s"%s', Sys.time(), self$year, self$status,
                     self$message, line_end)
    }
    cat(msg)
    sink()
  },
  write_csv = function(fn = NULL){
    if(is.null(fn)){
      filename <- file.path("raw_data", sprintf("orig_%s.csv", self$year))
    } else {
      filename <- fn
    }
    fwrite(self$data, file = filename)
  },
  create_header_tags = function(){
    # re-written this to ignore spaces and punctuation here,
    # as that drives recognition rates up quite a lot
    header_taglist <- c("saledemand",
                        "saleoffer",
                        "lendoffer",
                        "lenddemand",
                        "lostandfound",
                        "death",
                        "marriage",
                        "labourinfo",
                        "auctions",
                        "othernews",
                        "ps",
                        "tariffs",
                        "bookstore",
                        "travelheader",
                        "exchange",
                        "charityheader",
                        "foreigners",
                        "curious",
                        "registry",
                        "prices",
                        "election",
                        "naturalisation",
                        "denaturalisation",
                        "propertysaleoffer",
                        "insolvency",
                        "demand",
                        "offer")

    dt <- self$data[isheader == TRUE]
    dt$text <- gsub("[[:punct:][:blank:]]+", "", dt$text)
    crp <- corpus(dt, docid_field = "id")

    # there are a number of records marked as headers
    # that are not actually heading a section of ads,
    # but are a header WITHIN an add.
    # Those 'false header' texts need to be copied to the
    # beginning of the ad text of the following record,
    # and the 'false header' record then be removed.
    f <- get("tagfilter_merge_to_ad")
    hit_ids <- f()$filtrate(crp, return_corp = FALSE)
    self$data[shift(id) %in% hit_ids]$text <- paste(self$data[id %in% hit_ids]$text,
                                                    self$data[shift(id) %in% hit_ids]$text,
                                                    sep = " ")
    self$data <- self$data[!(id %in% hit_ids)]

    self$data[isheader == TRUE, "header_tag"] <- "unknown"
    for (tag in header_taglist){
      f <- get(sprintf("tagfilter_%s",tag))
      hit_ids <- f()$filtrate(crp, return_corp = FALSE)
      self$data[(isheader == TRUE & id %in% hit_ids), header_tag := tag]
    }

    by_header <- split(self$data, factor(cumsum(self$data$isheader)))

    by_header <- lapply(by_header, function(x){
      # get the position of the max non NA value
      # to take this as the header which is passed on by inheritance
      pos <- max(which(!is.na(x$header)))
      x$header_tag <- x$header_tag[pos]
      x
    })
    self$data <- rbindlist(by_header)
  }
))
