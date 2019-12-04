correct_ocr <- function(x){
  # h and b seem to be mixed at times
  gsub("Lebrling","Lehrling",x)
  gsub("büte","hüte",x, ignore.case = TRUE)
  # ; and z seem to be mixed at times
  gsub("Mer;","Merz",x, ignore.case = TRUE)
  # f and s seem to be mixed at times
  gsub("Hofen","Hosen",x, ignore.case = TRUE)
  # a and u seem to be mixed at times
  gsub("fust","fast",x)
  # ä and a seem to be mixed at times
  gsub("fäst","fast",x)
  }
