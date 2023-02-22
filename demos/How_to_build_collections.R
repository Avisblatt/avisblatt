setwd("~/GitHub/avisblatt")
devtools::load_all()

# This script describes the different steps
# of importing Avisblatt data from outside sources
# and processing it to receive the basic yearly
# collections on which all analysis is build.
#
# Each of the following steps
# grabs the files from the previous step, 
# transforms it, and stores it 
# in a different avis-databuffer folder
# (in the case of raw data and
#  final collections, to the 
#  avis-data repo).
#
# Each step is given as a function;
# the path names of the source and destination folder
# are set as their default parameters.
#
# The range of years to be treated
# in a step can be given as a parameter,
# default is AVIS_YEARS = 1729:1844.


# IMPORTANT: 
# Make sure that you operate on the same
# branch in avis-data and avis-databuffer repo,
# e.g. "XDI" in both cases. 
# Otherwise data from different processing stages
# of different branches gets mixed up!


#-------------------------------
# (1) Get data
#-------------------------------

# The data is brought into a specific format, and
# purged from any HTML code and s p a c i n g.


# Data in Hasdai has been imported by DataFutures
# from Transkribus page xml via Freizo. DataFutures adds IDs
# and creates iiif links for the image(s) of the record

AVIS_YEARS <- 1729
dest_path = "../avis-databuffer/raw_data_uncorrected"
start <- Sys.time()
fetch_from_hasdai(AVIS_YEARS, dest_path = dest_path)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))
# Last time took 48 minutes



#-------------------------------
# (2) Processing data: OCR
#-------------------------------

# In avisblatt/ocr_corrections.R there is a list
# of typical OCR mistakes that are to be corrected
# throughout all records.

start <- Sys.time()
rawdata_apply_ocr(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))
# Last time took 218 minutes



#-------------------------------
# (3) Processing data: header and ID
#-------------------------------

# A first enrichment is to look at all records
# that are flagged as section headers in the Avisblatt
# (like "Zu verkaufen"). Using a dictionary, 
# the type of any such header is determined here
# ("Zu verkaufen", "Verkauf" etc. -> type "for sale").
# Each record inherits the section header above it.
# Also, Freizo's record IDs are mapped 
# to the records imported from Transkribus,
# replacing temp IDs as far as possible.

start <- Sys.time()
rawdata_header_and_id(AVIS_YEARS)
message(sprintf("Took %s minutes", 
                round(difftime(Sys.time(),start, units = "min"),2)))
# Last time took 1.74 minutes



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
t4 <- round(difftime(Sys.time(),start, units = "min"),2)
# Last time took 308 minutes


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
#
AVIS_YEARS <- 1729:1844
start <- Sys.time()
rawdata_reprint_detection(AVIS_YEARS)
message(sprintf("Took %s minutes", 
               round(difftime(Sys.time(),start, units = "min"),2)))
t5 <- round(difftime(Sys.time(),start, units = "min"),2)
# Last time took 208.9 minutes.
 


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
t6 <- round(difftime(Sys.time(),start, units = "min"),2)
# Last time took 52.0 minutes.

# The resulting collections are the final yearly collections 
# and put on the avis-data repo.