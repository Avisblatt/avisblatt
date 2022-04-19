# library(avisblatt)
devtools::load_all()

#-----------------------------------------
# 0 Intro --------------------------------
#-----------------------------------------


# those are all the years
available_years()

# But let's just take two years
AVIS_YEARS <- c(1794, 1795)

c_all <- gather_yearly_collections(AVIS_YEARS, just_meta = FALSE)


#--------------------------------
# 1 Finding similiar ads
#--------------------------------
#
# list_similar()
#
# This needs TWO sets of ids and the coll as basic arguments
# - ids1
# - ids2
# - coll
# Then there up to four more arguments
# - N - default is 5, i.e. the five most similar ads will be returned (less if there are not enough to return)
# - min_dist (default = 0), lower limit of ad distance measure to be taken into account here. 0 means identical, 1 just similar enough to be a reprint
# - max_dist (default = 1000), upper limit
# - matrix_limit just prevents one from accidently computing very large matrices (default = 10,000,000 elements in the distance matrix, with a matrix of that size, calculation should take just a few seconds). Just lift the limit if you want (and have the time).


# Let's take the ads from the time of the 1794 fair and for each look for the 6 most similar during the 1795 fair.
fair_1794 <- select_by_date("all", c_all, "1794-01-01", "1794-12-31") %>% select_by_season(c_all, "Fair")
fair_1795 <- select_by_date("all", c_all, "1795-01-01", "1795-12-31") %>% select_by_season(c_all, "Fair")
sim <- list_similar(fair_1794, fair_1795, c_all, N = 6)

# Pick the seventh ad of the 1794 fair and see the most similar ads from 1795
id <- rownames(sim)[7]
show_records(id, c_all)
show_records(sim[id,], c_all)
show_iiif(id, c_all)
show_iiif(fair_1794[1:11], c_all, max.plot = 12)

# The function never gives you the ad ITSELF as one of the most similar ads,
# in cases in which the ad shows up in both sets
# (like when you look for the most similar ads WITHIN a set of ids).
# Each ad has by definition the smallest distance to itself,
# but that is not what we are looking for, obviously
ids <- select_by_date("all", c_all, "1794-01-01", "1794-01-10")
View(list_similar(ids, ids, c_all, N = 1, max_dist = 2))


# Now whole years
all_1794 <- select_by_date("all", c_all, "1794-01-01", "1794-12-31")
all_1795 <- select_by_date("all", c_all, "1795-01-01", "1795-12-31")

# Comparing two whole years makes for a huge number of ad comparisons
# (number of ads in year 1 times number of ads in year2):
all <- list_similar(all_1794, all_1795, c_all, max_dist = 2)

# Let'S try it anyway and see how long it takes
# (59.2 seconds on my laptop...)
start <- Sys.time()
all <- list_similar(all_1794, all_1795, c_all, max_dist = 2, matrix_limit = 62000000)
end <- Sys.time()
dtime <- difftime(end,start, units = "secs")
message(sprintf("This took %.1f seconds", dtime))
