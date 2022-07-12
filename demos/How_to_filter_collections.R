# library(avisblatt)
devtools::load_all()


#-----------------------------------------
# 0 Intro --------------------------------
#-----------------------------------------


# those are all the years for which there are collections
available_years()

# ...but let's just pick four for this demo,
# not to have too many results
AVIS_YEARS <- c(1729, 1730, 1734, 1836)

# The following function loads yearly collections for given years
# and merges them into one collection to work with.
# just_meta = TRUE implies that the texts, iiif links etc. 
# of the ads are not loaded, speeding things up.
# Sometimes though you might need the texts,
# as in many cases down below. So here:
# just_meta = FALSE (which is the default anyway)
c_all <- gather_yearly_collections(AVIS_YEARS, just_meta = FALSE)


# CONCEPT
#
# The basic idea of the filtering approach
# is to pick out interesting sub-sections of the collection
# by making use of a number of functions ("select_by_...") that do that,
# and deliver all the IDs of the ads in that subsection.
# If you do not specifiy a collection in any of the 
# subsequent functions, it is assumed that there is a
# collection named "c_all" to which it refers.
#
# You can then put this set of IDs as a starting point
# in another select_by-function, to further narrow down the set,
# and you can also join / intersect sets of IDs.
# This last part is explained at the bottom of this script.

# TAGS
#
# In the collection, most adverts are tagged, so one can select ads by tags.
# Which are tags we could search for?
#
# show all the tags
show_tags("all", c_all)

# Note: "ut_" denotes "umbrella term",
# i.e. this unites a group of other tags.
# Here are the definitions:
umbrella_terms()

# Show all the manual tags from the groundtruth years
# (1734, 1754, 1774, 1834. We included one of them in c_all)
show_tags("all", c_all, manual = TRUE)

# Show all section headers
show_headers(c_all)

# List the text of all actual header records
get_headers(c_all, text = TRUE)

# Same for "no-adverts", i.e. death and marriage notices, etc.
# just the ids here (text = FALSE is default setting)
get_noadverts(c_all)


# LOOKING AT ADS IN A SET
#
# You can create a set of IDs by simply listing them
# (mostly though, you will let one
#  of the functions below do the work)
ids <- c("ba46c1ff-3e66-549e-87e3-eed6068673d5/t2",
         "948216df-cbeb-527a-8e8c-e1bb6fbe72aa/t34")

# Use show_records() to get date, text and id of ads for a set of IDs
show_records(ids, c_all)

# If you don't want date, or text, or id in the results,
# you can set show_date, show_text and/or show_id to FALSE
show_records(ids, c_all, show_id = FALSE)

# If, on the other hand, you want to see
# the tags, the section (header), or have
# a line of code to open the ad in Freizo,
# set show_tags, show_header, and/or show_edit = TRUE
show_records(ids, c_all, show_tags = T, show_header = T, show_edit = T)

# If you would like the output as a tsv-file,
# use write_records. The parameters are the same,
# you just need a (relative) path and a file name ending with ".tsv"
write_records(ids, c_all, show_tags = T, show_header = T, show_edit = T, fn = "../testoutput.tsv")

# Not sure if the text of your ad contains weird OCR errors?
# Have a look at the image(s) of the ad, i.e. the iiif. 
# The following function prints iiifs for up to 10 ads
# (you can raise that ceiling using the max.plot parameter).
show_iiif(ids, c_all)

# Very long records with multiple fragments 
# can take a minute (or more) to compile and to load.
# Here is one with seven (!) columns:
show_iiif("temp-1836-394-000", c_all)


# WORDCLOUDS
#
# Instead of looking at all the ads in the set,
# you can also create a wordcloud
# (per default excluding numbers, punctuation,
#  and words shorter than 3 characters)
#
# Let's have a wordcloud of the marriage, death, election etc. notices:
ids <- get_noadverts(c_all)
show_wordcloud(ids, c_all)

# You can remove specific words from the cloud by using
# stopwords in the form of a character vector
# (those are not case-sensitive and NOT regex),
# and you can change the number of words included
# by max_words (default = 200)
show_wordcloud(ids, c_all, remove = c("herr", "herrn", "frau"), max_words = 100)
#
# Use "all" as a shorthand to take ALL the ads in a collection
# (this will also work for the select_by-functions down below)
show_wordcloud("all", c_all, 50)

# Some standard stopwords are never
# included. You can list them by calling
avis_stop()


# Now use the select_by_xxx() functions to filter records.
# The first two arguments of each of these functions
# are always the same:
# - ids: the set of ids of the records to be filtered. Default = NULL. "all" gives whole collection
# - coll: the collection. This is always a necessary argument!


#-----------------------------------------
# 1 Filter records by tags / headers
#-----------------------------------------
#
# select_by_tags()
#
# Up to three more arguments beyond ids & coll:
# - tags (created from all our tagfilters)
# - headers
# - manual tags from groundtruth years
#
# This function gives just those IDs # which
# records match ALL the tags/headers given (intersection)

# If just one argument beyond ids and coll is given,
# headers and manual tags are ignored.
# Let's count the resulting ids:
length(select_by_tags("all", c_all, "secondhand"))
length(select_by_tags("all", c_all, "table"))

# Each argument can be a list of search tags,
# results will be all ads that have ALL these tags
# (logical AND, i.e. intersection of the sets)
tlist <- c("secondhand", "table")
length(select_by_tags("all", c_all, tlist))
show_records(select_by_tags("all", c_all, tlist), c_all)

# giving two arguments beyond ids & coll:
# that's tags and headers then
hlist <- "offer"
length(select_by_tags("all", c_all, tlist, hlist))

# you can use regular expressions
hlist <- "demand|offer"
length(select_by_tags("all", c_all, tlist, hlist))

# Note: choice of regex or list gives you logical OR and AND
# tlist <- c("apples", "oranges") gives you records containing apples AND oranges (intersection)
# tlist <- "apples|oranges"       gives you records containing apples OR oranges  (union)

# Finally, manual tags. Note that manual tags necessarily confine this
# to records from gt years (1734, 54, 74, 1834)
# as there are no manual tags outside groundtruth
length(select_by_tags("all", c_all, manualtagslist = "hausrat"))
length(select_by_tags("all", c_all, "ut_household", hlist, "hausrat"))



#-----------------------------------------
# 2 Filter ads by reprint status & language
#-----------------------------------------
#
# select_by_reprint_status()
# select_by_language()
#
# in both cases just one argument beyond ids & coll: status
#
# For reprint_status, it can have one of six values
# "reprinted_orig"   : original postings that were reprinted in one of the two following issues
# "unreprinted_orig" : original postings that were not reprinted in that way
# "postings"         : any original posting, regardless if reprinted
# "reprints"         : records that are reprints of original postings
# "other"            : headers & "no advert" (= marriage, death, election notices)
# "ads"              : anything BUT headers and "no advert", i.e. all 'proper' ads
#
# For language, there are three values
# "de"      : German
# "fr"      : French
# "unknown" : language detection could neither identify it as German nor as French


# How many reprints in the whole collection?
length(select_by_reprint_status("all", c_all, status = "reprints"))

# How many reprints among those ads that concern household_goods?
ids <- select_by_tags("all", c_all, "ut_household")
length(select_by_reprint_status(ids, c_all, status = "reprints"))

# Breakdown of all ads according to reprint status
ids1 <- select_by_reprint_status("all", c_all, status = "reprinted_orig")
ids2 <- select_by_reprint_status("all", c_all, status = "unreprinted_orig")
ids3 <- select_by_reprint_status("all", c_all, status = "postings")
ids4 <- select_by_reprint_status("all", c_all, status = "reprints")
ids5 <- select_by_reprint_status("all", c_all, status = "other")
message(sprintf("%d ads in the collection were posted and then became reprinted,\n", length(ids1)),
        sprintf("another %d postings were not reprinted,\n", length(ids2)),
        sprintf("so there are %d original ads in this collection.\n", length(ids3)),
        sprintf("The other %d ads in this collection are reprints of the first group of postings.\n", length(ids4)),
        sprintf("And finally there are %d records which are not proper ads, but headers and notices about deaths, marriages, elections etc.", length(ids5)))

# Breakdown of all ads according to their language
ids1 <- select_by_language("all", c_all, status = "de")
ids2 <- select_by_language("all", c_all, status = "fr")
message(sprintf("There are %d ads in this collection in German,\n", length(ids1)),
        sprintf("the other %d are in French.\n", length(ids2)))

#Let's look at first forty French ads
show_records(ids2[1:40], c_all)


# Note: For quite a number of questions,
# you might want to analyse just the original postings.
# You can set this as your universe (Grundgesamtheit),
# and then operate in it, instead of in "all" records

universe <- select_by_reprint_status("all", c_all, "postings")
length(universe)
length(select_by_tags("all", c_all, "ut_household"))
length(select_by_tags(universe, c_all, "ut_household"))



#-----------------------------------------
# 3 Filter records by searching the texts.
#-----------------------------------------
#
# select_by_text()
#
# one additional argument beyond ids&coll:
# - searchlist (a list of strings/regex)
#
# Similiar to select_by_tags, but this one
# searches the full text of each record/ad.
# Works only if just_meta = FALSE, of course,
# (as we did in line 43).
#

ids <- select_by_text("all", c_all, "Hund")
show_records(ids, c_all)

# same, but just postings
# (see universe definition in line 227)
ids <- select_by_text(universe, c_all, "Hund")
show_records(ids, c_all)



#-----------------------------------------
# 4 Filter ads by length of ad text
#-----------------------------------------
#
# select_length()
#
# up to 3 arguments beyond ids and collection:
# - min (default 0)
# - max (default 1,000,000)
# - unit, either "tokens" (default) or "char")

# Start with all the ads for household_goods as example
ids <- select_by_tags("all", c_all, "ut_household")

#all household_goods ads with at least 10 tokens (i.e. ~ words)
length(select_by_length(ids, c_all, min = 10))

#at least 10 tokens, no more than 30
length(select_by_length(ids, c_all, min = 10, max = 30))

#Now with characters instead of tokens
length(select_by_length(ids, c_all, min = 10, max = 100, unit = "char"))


#-----------------------------------------
# 5 Filter ids by date
#-----------------------------------------
#
# select_by_date()
# select_by_season()
#
# Those two functions have slightly different aims.
#
# select_by_date() only keeps records from the timeframe
# given by two boundaries:
# - min [default = "1729-01-01"]
# - max [default = "1844-12-31"]
# (results always including the boundary dates min & max)
#
# select_by_season() only keeps records from a certain
# timeframe WITHIN EACH YEAR. This timeframe is given by
# - date_MM_DD  (within year)
# - days_before (default = 0)
# - days_after  (default = 0)
# The "date_MM_DD" is either given in format MM-DD,
# or as one of the follwoing texts:
# - "Easter"    (will be Easter Sunday for each year)
# - "Christmas" (Christmas Day, 25th)
# - "Fair"      (Duration of the Basel autumn fair, i.e. a time span)
# Days_before/after will extend the timespan.

# Start with all offerings of household_goods, just postings
universe <- select_by_reprint_status("all", c_all, "postings")
ids <- select_by_tags(universe, c_all, "ut_household", "offer")
length(ids)

# just the stuff since Christmas Eve 1730:
length(select_by_date(ids, c_all, min = "1730-12-24"))

# just 1734-1739
length(select_by_date(ids, c_all, min = "1734-01-01", max = "1739-12-31"))

# housing ads from the week before Christmas, two weeks around Easter Sunday, and during the fair
ids <- select_by_tags(universe, c_all, "housing")
length(ids)

xmas <- select_by_season(ids, c_all, "Christmas", days_before = 7)
show_records(xmas, c_all)

eastern <- select_by_season(ids, c_all, "Easter", days_before = 7, days_after = 7)
show_records(eastern, c_all)

fair <- select_by_season(ids, c_all, "Fair")
show_records(fair, c_all)


#-----------------------------------------
# 6 Counting and averaging
#-----------------------------------------
#
# count_records_by_date()
# count_records_by_length()
# average_length_by_date()
#
# Let's try all this offerings of household goods
universe <- select_by_reprint_status("all", c_all, "postings")
ids <- select_by_tags(universe, c_all, tagslist = "ut_household", headerlist = "offer")

# You can count records grouped by
# - "year" (default)
# - "quarter"
# - "month"
# - "week"
#
count_records_by_date(ids, c_all, "month")
count_records_by_date(ids, c_all, "quarter")

# You can count IDs grouped by length of ads
# this function has up to two arguments:
# - boundaries: vector of interval boundaries (default: 0, 10, 20, 40, 80, 160, 1000)
# - unit, either "tokens" (default) or "char")
#
count_records_by_length(ids, c_all)
count_records_by_length(ids, c_all, c(0, 5, 10, 15, 20, 30, 40, 60, 80, 100, 200, 1000))
count_records_by_length(ids, c_all, c(0, 5, 10, 15, 20, 30, 40, 60, 80, 100, 200, 1000), unit = "char")


# You can also get the average length of ads in a set
# (year-wise, quarter-wise, month-wise or week-wise)
# up to 2 arguments beyond ids and collection:
# - level, either "year" (default), "quarter", "month", or "week"
# - unit, either "tokens" (default) or "char"
#
# average length of all ads in the collection
average_length_by_date("all", c_all)

# average length of ads offering household goods
average_length_by_date(ids, c_all)
average_length_by_date(ids, c_all, "quarter", "char")


#-----------------------------------------
# 7 Combinations
#-----------------------------------------

# You can easily make a chain of select_by-functions,
# as each is taking a list of IDs and gives out a subset of it
#
# The count/average-functions can be put
# at the end of the chain.


# How many ads offering household goods
# in no more than 20 words
# were placed (not printed, i.e. no reprints)
# in the years covered in this collection?
ids <- select_by_tags("all", c_all, "ut_household","offer")
ids <- select_by_length(ids, c_all, 0, 20)
ids <- select_by_reprint_status(ids, c_all, "postings")
count_records_by_date(ids, c_all, level = "year")

# Exactly the same written down using the pipe operator (%>%)
# (put the function output as input in the next function,
# literally chaining them).
select_by_tags("all", c_all, "ut_household", "offer") %>%
  select_by_length(c_all, 0, 20) %>%
  select_by_reprint_status(c_all, "postings") %>%
  count_records_by_date(c_all, level = "year")


# Finally, you can always take two sets of ids and simply join and/or intersect them
ids1 <- select_by_tags("all", c_all, "ut_household", "offer") %>%
  select_by_length(c_all, 0, 20) %>%
  select_by_reprint_status(c_all, "postings")
ids2 <- select_by_tags("all", c_all, "ut_household", "offer") %>%
  select_by_length(c_all, 15, 40) %>%
  select_by_reprint_status(c_all, "postings")
length(ids1)
length(ids2)

# intersection
# (in ids1 OR in ids2)
ids3 <- intersect(ids1, ids2)
length(ids3)

# union
# (in ids1 AND in ids2)
ids3 <- union(ids1, ids2)
length(ids3)
# Please DO NOT use c() for this!
# If an ID is in both sets,
# c() will add it twice to the resulting set
# and rename one of it.

# relative complement
# (in ids1 BUT NOT in ids2)
ids3 <- setdiff(ids1, ids2)
length(ids3)
