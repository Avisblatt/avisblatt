# Load libraries and source functions
library(avisblatt)

# During Development rather run
devtools::load_all()


### corpus

avis_1804 <- avis_create_corpus("data/1804.csv")

### subcorpus with ads classified as titles
# forming a subcorpus with only those ads categorised as "isheader"
# if dictionaries work well enough, this step can maybe be skipped
# and thus titles wrongly not categorised as "isheader" could
# also be identified
titles_1804 <- corpus_subset(avis_1804, grepl("1", isheader))



### Sale offers
saleoffer <- tagfilter_saleoffer()

saleoffer_1804 <- saleoffer$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long (threshold has to be adapted to each type of title)
# and therefore are either falsely classified as titles or the text region is too large
# and contains text from other ads
saleoffer_toolong_1804 <- corpus_subset(saleoffer_1804, ntoken(saleoffer_1804, remove_punct =T) > 6)

## Necessary addition for those ads  classified as "isheader"
## but not found by dictionaries for titles, then either dismissing those as titles
## or expanding and correcting the dictionaries

### Sale demands
saledemand <- tagfilter_saledemand()

saledemand_1804 <- saledemand$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
saledemand_toolong_1804 <- corpus_subset(saledemand_1804, ntoken(saledemand_1804, remove_punct =T) > 6)

### Lend offers
lendoffer <- tagfilter_lendoffer()

lendoffer_1804 <- lendoffer$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
lendoffer_toolong_1804 <- corpus_subset(lendoffer_1804, ntoken(lendoffer_1804, remove_punct =T) > 6)

### Lend demands
lenddemand <- tagfilter_lenddemand()

lenddemand_1804 <- lenddemand$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
lenddemand_toolong_1804 <- corpus_subset(lenddemand_1804, ntoken(lenddemand_1804, remove_punct =T) > 5)

### Lost and found
lostandfound <- tagfilter_lostandfound()

lostandfound_1804 <- lostandfound$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
lostandfound_toolong_1804 <- corpus_subset(lostandfound_1804, ntoken(lostandfound_1804, remove_punct =T) > 4)

### Death notices
death <- tagfilter_death()

death_1804 <- death$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
death_toolong_1804 <- corpus_subset(death_1804, ntoken(death_1804, remove_punct =T) > 3)

### Marriage announcements
marriage <- tagfilter_marriage()

marriage_1804 <- marriage$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
marriage_toolong_1804 <- corpus_subset(marriage_1804, ntoken(marriage_1804, remove_punct =T) > 2)

### Labour and Information
labourinfo <- tagfilter_labourinfo()

labourinfo_1804 <- labourinfo$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
labourinfo_toolong_1804 <- corpus_subset(labourinfo_1804, ntoken(labourinfo_1804, remove_punct =T) > 8)

### Auctions
auctions <- tagfilter_auctions()

auctions_1804 <- auctions$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
auctions_toolong_1804 <- corpus_subset(auctions_1804, ntoken(auctions_1804, remove_punct =T) > 1)

### Other news
othernews <- tagfilter_othernews()

othernews_1804 <- othernews$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
othernews_toolong_1804 <- corpus_subset(othernews_1804, ntoken(othernews_1804, remove_punct =T) > 4)

### Official notices
official <- tagfilter_official()

official_1804 <- official$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
official_toolong_1804 <- corpus_subset(official_1804, ntoken(official_1804, remove_punct =T) > 4)
# threshold of ntokens difficult for this category (a lot of variety in title length)

### Taxes
taxes <- tagfilter_taxes()

taxes_1804 <- taxes$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
taxes_toolong_1804 <- corpus_subset(taxes_1804, ntoken(taxes_1804, remove_punct =T) > 2)

### Bookstore
bookstore <- tagfilter_bookstore()

bookstore_1804 <- bookstore$filtrate(titles_1804,ignore.case = T)
# threshold of ntokens for too long titles don't make sense with this category, bc there is a lot of variety
# and very long titles

## Necessary addition for those ads  classified as "isheader"
## but not found by dictionaries for titles, then either dismissing those as titles
## or expanding and correcting the dictionaries


# still working on that: titles_missed_1804 <- corpus_subset(!(titles_1804), c(saleoffer_1804, saledemand_1804, lendoffer_1804, lenddemand_1804,
                                                           #death_1804, marriage_1804, labourinfo_1804, auctions_1804, othernews_1804,
                                                           #official_1804, taxes_1804, bookstore_1804))





