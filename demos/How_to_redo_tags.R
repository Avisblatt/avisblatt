library(data.table)
library(quanteda)
library(dplyr)

setwd("~/GitHub/avisblatt")
devtools::load_all()


# This script shows you how to update the tags 
# of the basic yearly collections
# without rebuilding it from scratch.
#
# Please note that if you change HEADER tagfilters,
# you DO have to rebuild the collections 
# starting at step 3 in build_collections.R


tf_integrity()

start <- Sys.time()
rawdata_redo_tags(1729:1844)

message(sprintf("\nOverall, the process took %s minutes", 
                round(difftime(Sys.time(), start, units = "min"),2)))


# Per default, re-tagging is done for all data 
# produced along the collection building process, 
# i.e
# - initial collections without reprint detection
# - collection with rp detection, but no fraternal twin detection
# - final collections on avis-data
#
# You can skip any of these 
# by using the parameters
# correct_no_rp = FALSE,
# correct_no_ft = FALSE
# correct_final = FALSE,
# 
# But note that if not all data
# is brought up-to-date, 
# and the collection building process is re-run,
# older tagging will be carried through
# to the final collections.