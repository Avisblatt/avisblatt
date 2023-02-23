#' @export
fetch_from_hasdai <- function(AVIS_YEARS = 1729,
                              dest_path = "../avis-databuffer/raw_data_uncorrected"){
  for (i in AVIS_YEARS){
    fn <- sprintf("orig_%d.csv", i)
    tryCatch({
      download.file(
          "https://avisblatt.dev.hasdai.org:5005/records/1kadb-dcp02/files/annotations-transcribus-only-v2.tsv?download=1",
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