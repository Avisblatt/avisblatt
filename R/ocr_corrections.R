correct_ocr <- function(x){
  # h and b seem to be mixed at times
  gsub("Lebrling","Lehrling",x)
}
