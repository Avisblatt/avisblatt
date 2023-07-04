#' Reads Avisblatt Input CSV into Quantedata Corpus
#'
#' Convenience function to read avisblatt input .csvs into
#' quanteda 2.0 corpuses.
#'
#' @param f character location of the file to read.
#' @param text_field name of the text column.
#' @param docid_field name of the document id field.
#' @param encoding character defaults to UTF-8.
#' @param apply_ocr_correct boolean Defaults to FALSE.
#' @importFrom data.table fread
#' @importFrom quanteda corpus
#' @export
avis_create_corpus <- function(f,
                               text_field = "text",
                               docid_field = "id",
                               encoding = "UTF-8",
                               apply_ocr_correct = FALSE){
  txt <- fread(f,
               encoding = encoding)

  if(apply_ocr_correct){
    txt$text <- correct_ocr(txt$text)
  }

  corpus(txt,
         text_field = text_field,
         docid_field = docid_field)
}
