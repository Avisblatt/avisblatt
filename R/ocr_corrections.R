correct_ocr <- function(x){
  # h and b seem to be mixed at times
  x <- gsub("Lebrling","Lehrling",x)
  x <- gsub("büte","hüte",x, ignore.case = TRUE)
  # ; and z seem to be mixed at times
  x <- gsub("Mer;","Merz",x, ignore.case = TRUE)
  # f and s seem to be mixed at times
  x <- gsub("Hofen","Hosen",x, ignore.case = TRUE)
  # a and u seem to be mixed at times
  x <- gsub("fust","fast",x)
  # ä and a seem to be mixed at times
  x <- gsub("fäst","fast",x)
  x <- gsub("Zeug nisse","Zeugnisse",x)
  x <- gsub("Elifabet", "Elisabet",x)
  x <- gsub("Seeretär", "Secretär", x)
  x <- gsub("Nahagoni", "Mahagoni", x)
  x <- gsub("Käsig", "Käfig", x)
  x <- gsub("Cortrait", "Portrait", x)
  x <- gsub("Beschirr", "Geschirr", x)
  x <- gsub("Korbwäelein", "Korbwägelein", x)
  x <- gsub("Mesung-Mörser", "Messing-Mörser", x)
  x <- gsub("Raspar", "Kaspar", x)
  x <- gsub("Zurückgabel", "Zurückgabe", x)
  x <- gsub("Ghar-à-banc", "Char-à-banc", x)
  x <- gsub("Bögeleisen", "Bügeleisen", x)
  x <- gsub("Rothfall", "Notfall", x)
  x <- gsub("Rovität", "Novität", x)
  x <- gsub("Moskan", "Moskau", x)
  x <- gsub("Trumoau", "Trumeau", x)
  x <- gsub("BlecHosen", "Blechofen", x)
  x <- gsub("Jalousre-Läden", "Jalousie-Läden", x)
  x
  }
