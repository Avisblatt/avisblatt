### load relevant libraries
# library(avisblatt)
devtools::load_all()
library(stringr)
library(tidyr)
library(dplyr)
library(quanteda)
source("../avis-analysis/R/filtering.R")


### ------------------- PREPARATION ------------------- ###

AVIS_YEARS <- c(1798, 1800, 1834, 1839)

c_all <- gather_yearly_collections(AVIS_YEARS, just_meta = FALSE)

show_tags(c_all)
unique(get_headers(c_all, text = TRUE))
hlist <- c("lendoffer", "lenddemand")
mietgebot <- select_by_tags("all", c_all, "real_estate", "lendoffer")
mietgesuch <- select_by_tags("all", c_all, "real_estate", "lenddemand")


### filter rental offer
show_records(mietgebot[1:10], c_all) #zeigt die ersten 10 Einträge

### filter rental demand
show_records(mietgesuch[1:10], c_all)

### filter by text
brunnen <- select_by_text(mietgebot, c_all, "[b|B]runnen")
show_records(brunnen, c_all)

### filter by season/date within year
messe <- select_by_season(mietgesuch, c_all, "Fair", 14)
show_records(messe, c_all)

show_records(select_by_season(mietgesuch, c_all, "06-01", 7), c_all)
