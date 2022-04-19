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
# 1 Add a tag to a (subset of) ad(s) on the basis of a regular expression
#--------------------------------
#
# The function tag_by_regex appends tags in a collection by matching
# regular expressions in a data frame with ad text
# input: 1) ids
#        2) collection
#        3) data frame, consisting of two named columns:  1: tag, 2: reg
# output: void (environment is changed, no return needed)

df_regex <- data.frame(
            tag = "Peterskirche",
            reg = "Pe(t|i|l)er(s|)(-|)(K|k)i(r|l)ch")

tag_by_regex("all", c_all, df_regex)

#--------------------------------
# 2 Add a tag to a (subset of) ad(s)
#--------------------------------
#
#
# append a chosen tag (not depending on a regular expression)
# to a subset of a collection given by a vector of ids
# input: 1) id(s)
#        2) collection
#        3) tag as string
# output: void (environment is changed, no return needed)

tag_selection("00c946d3-83cc-5394-880a-3d7c130ee7eb/t10", c_all, "added tag")



