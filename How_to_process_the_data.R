setwd("~/GitHub/avisblatt")
devtools::load_all()
library(jsonlite)

# This script describes the different steps
# of importing Avisblatt data from Hasdai,
# processing it to receive the basic yearly
# collections on which all analysis is build,
# and the generation of JSON to transfer 
# the main metadata added for transfer to Hasdai.
#
# The scripts provided here are an extract 
# from a larger R-package of the Avisblatt project.
# The extract contains all functionality and information
# to replicate the processing of the tsv data
# from Hasdai and the creation of the JSON output.
#
# Each of the following steps
# grabs the files from the previous step, 
# transforms it, and stores it 
# in a different folder.
#
# Each step is given as a function.
# The default path names of the source and 
# destination folder reflect the working
# environment in which this was originally
# done; in replicating the process, 
# they can of course be adapted as needed.
#
# The range of years to be treated
# in a step can be given as a parameter.

AVIS_YEARS = 1729:1844

# Currently, data for 18 years is not consistent/correct:
AVIS_YEARS = c(1734, 1749, 1759, 1764, 1774, 1784, 1787, 1790, 1794, 1801, 1809, 1810, 1812, 1816, 1820, 1837, 1839, 1844)

#records from pages which are not part of any issue (but mostly of type AVISMETA)
AVIS_YEARS = c(1749, 1759, 1784, 1787, 1794, 1801, 1809, 1810, 1816, 1844)

# Missing records in
c(1734, 1764, 1774, 1809, 1812, 1816, 1820, 1839)

# 1837:	all 281 records in issue 6 have a wrong date (1738-02-09 instead of 1837-02-09) and wrong book number (23202 instead of 69338), although the records seem otherwise correct
1837

# -> all corrected in tsv except missing pages 1816, 1839


#-------------------------------
# (1) Get data
#-------------------------------

# Data is downloaded from Hasdai. 
# Data in Hasdai has been imported 
# by DataFutures from Transkribus
# page xml via Freizo. DataFutures
# added IDs and created iiif links
# for the image(s) of the record.
#
# Here, this data is brought into
# a specific format, and purged from any 
# HTML code and s p a c i n g.

dest_path = "../avis-databuffer/raw_data_uncorrected"
start <- Sys.time()
#fetch_from_hasdai(AVIS_YEARS, dest_path)
fetch_tsv_local(AVIS_YEARS, dest_path)


message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))



#-------------------------------
# (2) Processing data: OCR
#-------------------------------

# In R/ocr_corrections.R there is a list
# of typical OCR mistakes that are to be corrected
# throughout all records.

start <- Sys.time()
rawdata_apply_ocr(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))



#-------------------------------
# (3) Processing data: header and ID
#-------------------------------

# A first enrichment is to look at all records
# that are flagged as section headers in the Avisblatt
# (like "Zu verkaufen"). Using a dictionary, 
# the type of any such header is determined here
# ("Zu verkaufen", "Verkauf" etc. -> type "for sale").
# Each record inherits the section header above it.

start <- Sys.time()
rawdata_header_and_id(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))



#-------------------------------
# (4) Creating yearly collections
#-------------------------------

# All records will be organized in collections. 
# The basis are collections that contain
# all records from a given year.
#
# In creating these basic collections,
# further metadata is added: 
# - language of ads is determined
# - ads are tagged (using tagfilters)

# Creation of basic collections will fail
# if there are invalid regex in the dictionaries 
# of the tagfilters, so check those first:

tf_integrity()

# If all is correct, proceed to create yearly collections:
start <- Sys.time()
rawdata_coll_creation(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))



#-------------------------------
# (5) Detecting reprints
#-------------------------------

# Per default, an advertisement in the Avisblatt
# was reprinted in the next* issue, unless 
# the publisher was told that it already 
# fulfilled it's purpose.

# To identify reprints, each advertisement
# is compared to all ads in the next issue,
# and the most similar one declared a potential reprint,
# and if the measure of similarity is beyond a threshold,
# actually declared a reprint. 
# All this information is added to the metadata.
# 
# *(In the final years, reprints sometimes happened 
# not in the next issue, but issue after that, 
# because the paper appeared with such high frequency.)

AVIS_YEARS = 1729:1844

start <- Sys.time()
rawdata_reprint_detection(AVIS_YEARS)
message(sprintf("Took %s minutes", 
               round(difftime(Sys.time(),start, units = "min"),2)))



#-------------------------------
# (6) Unifying tagging of 'fraternal twin' ads
#-------------------------------

# Advert twins (an original and its reprint)
# should theoretically have the same tagging, 
# i.e. be "identical twins".

# OCR problems etc. might prevent this,
# resulting in tagging differences,
# i.e. there are "fraternal twins".

# Here, fraternal twins are identified 
# and their tags unified,  
# by joining the tags of the two twins.

start <- Sys.time()
rawdata_fraternaltwin_detection(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))



#-------------------------------
# (7) Extracting main added metadata for transfer to Hasdai
#-------------------------------

# Newly generated information for each as such as 
# - header under which it is printed
# - detected language
# - tags predicting if pertinent for a specific topic/aspect
# - being an original or reprint, and potential siblings of the ad
#   (the whole chain of original and its consequent reprints)
# will be composed into metadata JSON to be provided in Hasdai
#
# First, a table of siblings for each ads is generated

start <- Sys.time()
build_siblings_table(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))


# Then, the JSONs are build:

start <- Sys.time()
create_hasdai_annotations(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))
