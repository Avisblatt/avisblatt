#' @import quanteda
#' @import data.table
#' @importFrom R6 R6Class
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

      # removing duplicate entries which we do further down the road
      # is obv. not the same as removing duplicate ids which is also necessary
      # atm, but shouldn't! This asks for more freizo corrections.
      self$data <- self$data[!(duplicated(id)),]


      # I moved the throwing out of lines up here, to be applied
      # before clean formatting and reading order calculation
      # (no need and potential source of problems to process
      # the records that are to be dropped) (AE)
      # ...and somehow this seems also to cure the problem that
      # cleaning, mast/table dropping, and reading order creation
      # work for gt-years 1734 and 1774, but not 1754 and 1834

      # Doesn't render line 60 this one useless? (AE)
      if(remove_duplicates){
        self$data <- unique(self$data)
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
        self$data[,text := gsub("-\\\\n", " ", text)]
        self$data[,text := gsub("\\\\n", " ", text)]

        # strip ids and HTML
        self$data[,id := gsub("https://avisblatt.freizo.org/iiif/anno/", "", id)]
        self$data[,text := gsub("<.*?>", "", text)]
        self$data[,rnotes := gsub("<.*?>", "", rnotes)]

        # Decode HTML (stuff like &amp; -> &)
        self$data[, text := textutils::HTMLdecode(text)]
        self$data[, rnotes := textutils::HTMLdecode(rnotes)]
      }

      if(self$year %in% gt_years){
        message("GT year detected, starting special treatment...")
        if (self$year == 1754){
          updated_tags <- fread("freizo-corrections/tags_1754.csv")
          # only use those updates that are still present to avoid problems with recycling
          updated_tags <- updated_tags[(id %in% self$data$id),]
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
    add_header_tags <- function(dt, tag, did = "id"){
      f <- get(sprintf("tagfilter_%s",tag))
      crp <- corpus(dt, docid_field = did)
      hit_ids <- f()$filtrate(crp, return_corp = FALSE)
      dt[(isheader == TRUE & id %in% hit_ids), header_tag := tag]
    }

    add_header_tags(self$data, "saledemand")
    add_header_tags(self$data, "saleoffer")
    add_header_tags(self$data, "lendoffer")
    add_header_tags(self$data, "lenddemand")
    add_header_tags(self$data, "lostandfound")
    add_header_tags(self$data, "death")
    add_header_tags(self$data, "marriage")
    add_header_tags(self$data, "labourinfo")
    add_header_tags(self$data, "auctions")
    add_header_tags(self$data, "othernews")
    add_header_tags(self$data, "taxes")
    add_header_tags(self$data, "bookstore")
    add_header_tags(self$data, "travel")
    add_header_tags(self$data, "exchange")
    add_header_tags(self$data, "charityheader")
    add_header_tags(self$data, "foreigners")
    add_header_tags(self$data, "merkwuerdig")
    add_header_tags(self$data, "registry")
    add_header_tags(self$data, "prices")
    add_header_tags(self$data, "election")
    add_header_tags(self$data, "demand")
    add_header_tags(self$data, "offer")

    self$data[(isheader == TRUE &
                 !(header_tag %in%
                     c( "saledemand",
                        "saleoffer",
                        "lendoffer",
                        "lenddemand",
                        "lostandfound",
                        "death",
                        "marriage",
                        "labourinfo",
                        "auctions",
                        "othernews",
                        "taxes",
                        "bookstore",
                        "travel",
                        "exchange",
                        "charityheader",
                        "foreigners",
                        "merkwuerdig",
                        "registry",
                        "prices",
                        "election",
                        "demand",
                        "offer"))),"header_tag"] <- "unknown"


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
