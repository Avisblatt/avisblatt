# library(avisblatt)
devtools::load_all()

#------------------------------------------------
#
# 1) Get yourself a collection
# 2) Compile a list of search results,
#      for example by using group-records_by functions
# 3) Use calculate_frequency_data() and put the
# 4) resulting dataframe in plot_frequency_data()
#
#------------------------------------------------

# Working with all available years here,
# loading the collections will take some time (ca. 5 minutes),
# even though we can confine ourselves to metadata,
# leaving out the texts
AVIS_YEARS <- available_years()
c_all <- gather_yearly_collections(AVIS_YEARS, just_meta = TRUE)

#------------------------------------------------
# 1 Overall overview: postings & reprints -------
#------------------------------------------------


# A "result" is a list of IDs
# so a list of results is a list of lists.
# Create one:
results=list(list())

# Fill it with different results
results[[4]] = select_by_reprint_status("all", c_all, "unreprinted_orig")
results[[3]] = select_by_reprint_status("all", c_all, "reprinted_orig")
results[[2]] = select_by_reprint_status("all", c_all, "reprints")
results[[1]] = select_by_reprint_status("all", c_all, "other")

length(results[[4]])+length(results[[3]])+length(results[[2]])+length(results[[1]])

# It is MANDATORY to name the different results in the result list!
# otherwise the following functions won't work
names(results) <- c("Headers & announcements", "Reprints", "Reprinted postings", "Unreprinted postings")

# Now get a frequency table for the result list
# This function has up to four arguments:
# 1) results [necessary]
# 2) collection [necessary]
# 3) universe [a 'Grundgesamheit' for calculating shares in it; default = whole collection]
# 4) aggregation_level [default is "year", alternatives: "quarter", month", "week"]

# Will take a few moments to calculate
fdata <- calculate_frequency_data(results, c_all)



# Finally, plot the data.
# This function has up to three arguments:
# 1) data [necessary, use input from calculate_frequency_data]
# 2) data_type [necessary], pick of one of these:
#      N             = number of records in each search result in time intervals (default: year-by-year)
#      N_index       = N as index numbers (100 = average N over all years)
#      N_per1k       = N per 1000 inhabitants
#      N_per1k_index = N1k as index numbers (100 = average N1k over all years)
#      share         = share of N among all records in the universe, in time intervals (default: year-by-year)
#      share_index   = share as index numbers (100 = average N1k over all years)
#      length        = average number of tokens per record
#      length_index  = average number of tokens per record as index number
# 3) plot_type =[necessary, either "line", "stack" or "heatmap"]
# 4) diagram_title
# 5) legend_title [default = "Type of record"]
# 6) colour [Viridis colour scheme, default = "D", alternatives: A-E)


diagram_title <-"Share of postings and reprints"
plot_frequency_data(fdata, "share", "stack", diagram_title, "share", "E")

diagram_title <-"Number of postings and reprints"
plot_frequency_data(fdata, "N", "stack", diagram_title, "N", "E")

diagram_title <-"Postings and reprints per 1,000 inhabitants, year-by-year"
plot_frequency_data(fdata, "N_per1k", "stack", diagram_title, "N", "E")

# recalculate for quartes instead of years
fdata <- calculate_frequency_data(results, c_all, "all", "quarter")

diagram_title <-"Postings and reprints per 1,000 inhabitants, quarter-by-quarter"
plot_frequency_data(fdata, "N_per1k", "stack", diagram_title, "N", "E")



#------------------------------------------------
# 2 Comparing different main types of ads -------
#------------------------------------------------

# To create results, you can also use the group_records_by functions
# (for tags, headers, and text)
# up to four parameters
# - ids which are to be grouped ("all" for all in collection)
# - coll, the underlying collection [necessary]
# - a list of search terms (depending on function: tags, headers, or texts) [necessary]
# - a list of names for the different group, same length as search term list [optional, default will be the list of search terms]


# Confine to postings
universe <- select_by_reprint_status("all", c_all, "postings")

# Tagfilter for main type of ads
tlist <- c("ut_consumables", "ut_clothing", "ut_textiles", "ut_household", "ut_things", "print", "employment", "housing", "board", "churchseat", "finance", "lottery")
nlist <- c("Consumables", "Clothing", "Textiles", "Household items", "Other things", "Books", "Employment", "Housing", "Boarding", "Churchseats", "Moneylending", "Lotteries")
results1 <- group_records_by_tags(universe, c_all, tlist, nlist)

# Now anything under lostandfound header
hlist <- "lostandfound"
nlist <- "Lost & found"
results2 <- group_records_by_header(universe, c_all, hlist, nlist)

# Simply concatenate those results to join them into one
results <- c(results1, results2)

# Calculate frequencies
fdata <- calculate_frequency_data(results, c_all, universe, "year")

#Plotting
diagram_title <- "Share of postings bearing a certain tag (one posting can have several tags))"
plot_frequency_data(fdata, "share", "line", diagram_title, "share", "A")

diagram_title <- "Number of postings, year by year"
plot_frequency_data(fdata, "N", "stack", diagram_title, "N", "C")

diagram_title <- "Number of postings per 1,000 inhabitents, year by year"
plot_frequency_data(fdata, "N_per1k", "stack", diagram_title, "N", "C")

diagram_title <- "Number of postings per 1,000 inhabitents, year by year"
plot_frequency_data(fdata, "N_per1k", "heatmap", diagram_title, "N", "B")

diagram_title <- "When did a topic have its highest/lowest number of postings?"
plot_frequency_data(fdata, "N_per1k_index", "heatmap", diagram_title, "100 =\naverage\npostings\nper topic\nover time", "B")

diagram_title <- "When did a topic have its highest/lowest share amongst all postings?"
plot_frequency_data(fdata, "share_index", "heatmap", diagram_title, "100 =\naverage\nshare\nover time", "B")


# New: you can use length and length_index to look at average number of tokens in ads
diagram_title <- "Average number of tokens per posting"
plot_frequency_data(fdata, "length", "heatmap", diagram_title, "length", "A")

diagram_title <- "When were ads of a certain type at its shortest/longest?"
plot_frequency_data(fdata, "length_index", "heatmap", diagram_title, "100 =\naverage tokens\nper record\nover time", "A")


# Removing lotteries and books for being odd ones out (at least regarding length)
results[12] <- NULL
results[6] <- NULL
fdata <- calculate_frequency_data(results, c_all, universe, "year")

diagram_title <- "Average number of tokens per posting"
plot_frequency_data(fdata, "length", "heatmap", diagram_title, "length", "A")

diagram_title <- "When did a topic have its highest/lowest share amongst all postings?"
plot_frequency_data(fdata, "share_index", "heatmap", diagram_title, "100 =\naverage\nshare\nover time", "B")



#---Now zooming in on 1794-1804

universe <- select_by_date(universe, c_all, "1794-01-01", "1804-12-31")
# restore the original result list, to include books and lotteries
results <- c(results1, results2)

for (i in 1:length(results)){
  results[[i]] <- select_by_date(results[[i]], c_all, "1794-01-01", "1804-12-31")
}

# now we want quarter-by-quarter frequency data
fdata <- calculate_frequency_data(results, c_all, universe, aggregation_level = "quarter")

diagram_title <- "Number of postings per 1,000 inhabitents, quarter by quarter"
plot_frequency_data(fdata, "N_per1k", "heatmap", diagram_title, "N", "B")

diagram_title <- "Share of postings bearing a certain tag (one posting can have several tags))"
plot_frequency_data(fdata, "share", "heatmap", diagram_title, "share", "B")

diagram_title <- "When did a topic have its highest/lowest share amongst all postings?"
plot_frequency_data(fdata, "share_index", "heatmap", diagram_title, "100 = average share over time", "B")



#------------------------------------------------
# 3 Short and long ads --------------------------
#------------------------------------------------

#group all ids by text length (number of tokens) into classes

universe <- select_by_reprint_status("all", c_all, "postings")
results <- group_records_by_length(universe, c_all, c(0, 5, 10, 15, 20, 30, 40, 60, 80, 100, 200, 1000))

#calculate and plot:
fdata <- calculate_frequency_data(results, c_all, universe)

diagram_title <- "Share of records of a certain length (number of tokens)"
plot_frequency_data(fdata, "share", "stack", diagram_title, "N", "D")
plot_frequency_data(fdata, "share", "heatmap", diagram_title, "N", "B")

diagram_title <- "Number of records of a certain length (number of tokens)"
plot_frequency_data(fdata, "N", "heatmap", diagram_title, "N", "B")

# Next one is totally ruined by multitude of extremly short ads in 1734:
diagram_title <- "Index of the number of records of a certain length (number of tokens)"
plot_frequency_data(fdata, "N_index", "heatmap", diagram_title, "100 =\naverage\nnumber of\nrecords\nover time", "B")

# ... so drop ads < 5 tokens.
fdata <- group_records_by_length(universe, c_all, c(5, 10, 15, 20, 30, 40, 60, 80, 100, 200, 1000)) %>%
  calculate_frequency_data(c_all, universe)
plot_frequency_data(fdata, "N_index", "heatmap", diagram_title, "100 =\naverage\nnumber of\nrecords\nover time", "B")

diagram_title <- "When did ads of certain length\nhad their highest/lowest share amongst all postings?"
plot_frequency_data(fdata, "share_index", "heatmap", diagram_title, "100 =\naverage\nshare\nover time", "B")



#------------------------------------------------
# 4 Some things ---------------------------------
#------------------------------------------------

universe <- select_by_reprint_status("all", c_all, "postings")
tlist <- c("bed", "textile", "cabinet", "stove", "mirror", "timepiece", "table", "tableware", "toy")
nlist <- c("Betten", "Textilien", "Schränke", "Öfen", "Spiegel", "Uhren", "Tische", "Geschirr", "Spielzeug")
results <- group_records_by_tags(universe, c_all, tlist, nlist)

fdata <- calculate_frequency_data(results, c_all, universe)

diagram_title <-"Number of postings concerning household goods"
plot_frequency_data(fdata, "N", "stack", diagram_title, "N", "E")

diagram_title <-"Postings per 1,000 inhabitants"
plot_frequency_data(fdata, "N_per1k", "heatmap", diagram_title, "N", "B")

diagram_title <- "When did a topic have its highest/lowest number of postings?"
plot_frequency_data(fdata, "N_per1k_index", "heatmap", diagram_title, "100 =\naverage\npostings\nper topic", "B")

#dropping toys
results[9] <- NULL
fdata <- calculate_frequency_data(results, c_all, universe)

diagram_title <- "When did a topic have its highest/lowest number of postings?"
plot_frequency_data(fdata, "N_per1k_index", "heatmap", diagram_title, "100 =\naverage\npostings\nper topic", "B")

diagram_title <- "When did a topic have its highest/lowest share amongst all postings?"
plot_frequency_data(fdata, "share_index", "heatmap", diagram_title, "100 =\naverage\npostings\nper topic", "B")

#narrowing universe to all household goods
household_goods <- select_by_tags(universe, c_all, "ut_household")
fdata <- calculate_frequency_data(results, c_all, household_goods)

diagram_title <- "When did a topic have its highest/lowest share\namongst all postings on household goods?"
plot_frequency_data(fdata, "share_index", "heatmap", diagram_title, "100 =\naverage\nshare\nover time", "B")