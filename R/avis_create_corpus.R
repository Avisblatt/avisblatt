#' Reads Avisblatt Input CSV into Quantedata Corpus
#'
#' Covenience function to read avisblatt input .csvs into
#' quanteda 2.0 corpuses.
#'
#' @importFrom data.table fread
#' @importFrom quanteda corpus
#' @export
avis_create_corpus <- function(f,
                               text_field = "text",
                               docid_field = "id",
                               encoding = "UTF-8",
                               apply_ocr_correct = TRUE){
  txt <- fread(f)

  if(apply_ocr_correct){
    txt$text <- correct_ocr(txt$text)
  }

  corpus(txt,
         text_field = text_field,
         docid_field = docid_field,
         encoding = encoding)
}
