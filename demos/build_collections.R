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

# Either from Freizo, or by importing Transkribus page-xml directly.
#
# Independent of how procured, 
# the data is brought into a specific format
# (some columns carried in Freizo are dropped,
#  as they are no longer of interest), and
# purged from any HTML code and s p a c i n g.


# Freizo
# Data in Freizo has been imported by DataFutures
# from Transkribus page xml. DataFutures adds IDs
# and creates iiif links for the image(s) of the record

# already imported T -> F, but not free of bugs
AVIS_YEARS <- c(1733, 1740, 1741, 1746, 1747, 
                1778, 1787, 1808, 1812, 1813, 
                1816, 1820, 1821, 1822, 1823, 
                1825, 1826, 1827, 1828, 1830, 
                1831, 1837, 1838)

# necessary to redo T -> F at least for
AVIS_YEARS <- c(1738, 1741, 1746)

# final package, still to be imported  T -> F
AVIS_YEARS <- c(1835, 1836, 1840, 1841, 1842, 1843)

AVIS_YEARS <- 1729:1844
dest_path = "../avis-databuffer/raw_data_uncorrected"
fetch_from_freizo(AVIS_YEARS, dest_path = dest_path)

# XDI from Transkribus
# You can create collections by making a xml direct ingest
# from Freizo. Data then contains a temporary ID and no iiif links.
#
# First, download xml for desired years from Transkribus.
# Store them to the avis-databuffer repo, folder xml/years.
# Preserve the folder structure in the download,
# i.e. the page XMLs for, say, 1729 should be in folder
# xml/years/1729/1729/pages

AVIS_YEARS <- 1729:1844
start <- Sys.time()
xml_direct_import(AVIS_YEARS)
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
# Last time took 205 minutes



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
# Last time took 0.79 minutes



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
# Last time took 217 minutes


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
# Last time took 202 minutes.
 


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
# Last time took 44 minutes.

# The resulting collections are the final yearly collections 
# and put on the avis-data repo.


