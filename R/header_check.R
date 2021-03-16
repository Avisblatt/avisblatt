library(avisblatt)
library(data.table)
library(quanteda)
AVIS_YEARS <- c(1729, 1730, 1734, 1739, 1742, 1743, 1744, 1745, 1748, 1749,
                1750, 1751, 1754, 1755, 1759, 1760, 1761, 1764, 1765, 1766,
                1769, 1770, 1771, 1772, 1773, 1774, 1775, 1776, 1777, 1779,
                1784, 1789, 1794, 1795, 1796, 1797, 1798, 1799, 1800, 1801,
                1802, 1803, 1804, 1809, 1814, 1819, 1824, 1829, 1832, 1833,
                1834, 1839, 1844)
unknown_headers <- data.table()
unknown_headers_corr <- data.table()
stats <- data.table()
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
                    "offer")
for (i in AVIS_YEARS){
  #read the raw data for year i
  fn <- sprintf("../avis-data/raw_data/orig_%d.csv", i)
  dt <- fread(fn, encoding="UTF-8")
  #gather all unknown headers
  dt_uh <- dt[isheader == TRUE & header_tag == "unknown"]
  unknown_headers <- rbind(unknown_headers,dt_uh, fill=TRUE)
  #create stats
  total <- nrow(dt[isheader == TRUE])
  unknown <- nrow(dt[isheader == TRUE & header_tag == "unknown"])
  rate <- unknown/total
  #re-run header tagging, without space/punctuation
  dt_uh$text <- gsub("[[:punct:][:blank:]]+", "", dt_uh$text)
  crp <- corpus(dt_uh, docid_field = "id")
  for (tag in header_taglist){
    f <- get(sprintf("tagfilter_%s",tag))
    hit_ids <- f()$filtrate(crp, return_corp = FALSE)
    dt_uh[id %in% hit_ids, header_tag := tag]
  }
  unknown_headers_corr <- rbind(unknown_headers_corr, dt_uh[header_tag == "unknown"], fill=TRUE)
  #stats
  unknown_corr <- nrow(dt_uh[header_tag == "unknown"])
  rate_corr <- unknown_corr/total
  stats <- rbind(stats, cbind(i, total, unknown, rate, unknown_corr, rate_corr))
}
View(stats)
#Headers still unkown, extension of dict and/or OCR-corr necessary
uh <- unknown_headers_corr[text != ""]
uh <- uh[nchar(text) < 50]
View(table(uh$text))
View(unknown_headers_corr)
