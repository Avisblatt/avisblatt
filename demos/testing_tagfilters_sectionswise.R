# Testing change to tagfilter_util:
# If dict$applicable is given,
# aplly tagfilter only to ads
# with specoofed header tag(s)
#
# delete this file before merging into experimental!

devtools::load_all()

corpus_1799 <- avis_create_corpus("../avis-data/raw_data/orig_1799.csv")

#original filter without applicable specification
labinfo1 <- tagfilter_labor()
test1 <- labinfo1$filtrate(corpus_1799,ignore.case = F)
test1$header_tag

#with specification: just sections "labourinfo" & "death"
labinfo2 <- tagfilter_labor2()
test2 <- labinfo2$filtrate(corpus_1799,ignore.case = F)
test2$header_tag
