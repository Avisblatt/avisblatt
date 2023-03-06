#' @export
fetch_from_hasdai <- function(AVIS_YEARS = 1729:1844,
                              dest_path = "../avis-databuffer/raw_data_uncorrected"){
  handles <- fread("hasdai_handles.csv")
  for (i in AVIS_YEARS){
    fn <- sprintf("orig_%d.csv", i)
    tryCatch({
      download.file(url = handles[year == i]$tsv, destfile = fn)
      Sys.sleep(3)
      raw_data <- RawData$new(i, URL = fn, local = TRUE)
      raw_data$booleanize(c("isheader","noadvert"))
      raw_data$process_data(ocr_correction = FALSE, drop_na_cols = FALSE)
      drop_cols <- c("at","set","withincat","year","month","date",
                     "canvas","name","avistable", "wrkprofession",
                     "wrkcompetence", "wrkcondition", "wrkmodality",
                     "textyp", "texmod", "texatt", "texzus",
                     "hautyp", "haumod", "hauatt", "hauzus", "hauort",
                     "editlink")
      drop_cols <- intersect(colnames(raw_data$data),
                             drop_cols)
      raw_data$drop_cols(col = drop_cols)
      setnames(raw_data$data,old = "isodate", "date", skip_absent = TRUE)
      message(
        sprintf("%d processed and written to disc.", i)
      )
      
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