# Load libraries and source functions
library(avisblatt)

# During Development rather run
devtools::load_all()
library(quanteda)
library(jsonlite)
library(tibble)

# negating %in% operator
`%notin%` <- Negate(`%in%`)


### corpus

avis_1804 <- avis_create_corpus("data/1804.csv")

### small corpus
## working with a small corpus here of only one issue from 1804, to make loading faster and try out multiple approaches

trial_1804 <- avis_create_corpus("data/1804_04_19untagged.csv")
docvars(trial_1804)

### subcorpus with ads classified as titles
# forming a subcorpus with only those ads categorised as "isheader"
# if dictionaries work well enough, this step can maybe be skipped
# and thus titles wrongly not categorised as "isheader" could
# also be identified
titles_1804 <- corpus_subset(trial_1804, grepl("1", isheader))

docvars(titles_1804)

# Using dictionaries to classify different types of titles

### Sale offers
## adtype: 01biete; finance: 01kauf
saleoffer <- tagfilter_saleoffer()

saleoffer_1804 <- saleoffer$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long (threshold has to be adapted to each type of title)
# and therefore are either falsely classified as titles or the text region is too large
# and contains text from other ads
saleoffer_toolong_1804 <- corpus_subset(saleoffer_1804, ntoken(saleoffer_1804, remove_punct =T) > 6)


### Sale demands
## adtype: 02suche; finance: 01kauf
saledemand <- tagfilter_saledemand()

saledemand_1804 <- saledemand$filtrate(titles_1804,ignore.case = T)

saledemand_1804$readingorder
saledemand_1804$page

# identifying title-ads that are too long
saledemand_toolong_1804 <- corpus_subset(saledemand_1804, ntoken(saledemand_1804, remove_punct =T) > 6)

### Lend offers
## adtype: 01biete; finance: 02miete
lendoffer <- tagfilter_lendoffer()

lendoffer_1804 <- lendoffer$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
lendoffer_toolong_1804 <- corpus_subset(lendoffer_1804, ntoken(lendoffer_1804, remove_punct =T) > 6)

### Lend demands
## adtype: 02suche; finance: 02miete
lenddemand <- tagfilter_lenddemand()

lenddemand_1804 <- lenddemand$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
lenddemand_toolong_1804 <- corpus_subset(lenddemand_1804, ntoken(lenddemand_1804, remove_punct =T) > 5)

### Lost and found
## adtype: unknown (could be 01biete or 02suche); finance: 04fundsache
lostandfound <- tagfilter_lostandfound()

lostandfound_1804 <- lostandfound$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
lostandfound_toolong_1804 <- corpus_subset(lostandfound_1804, ntoken(lostandfound_1804, remove_punct =T) > 4)

### Death notices
## adtype: 05nachricht
death <- tagfilter_death()

death_1804 <- death$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
death_toolong_1804 <- corpus_subset(death_1804, ntoken(death_1804, remove_punct =T) > 3)

### Marriage announcements
## adtype: 05nachricht
marriage <- tagfilter_marriage()

marriage_1804 <- marriage$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
marriage_toolong_1804 <- corpus_subset(marriage_1804, ntoken(marriage_1804, remove_punct =T) > 2)

### Labour and Information
## adtype: unknown (could be 01biete or 02suche); adcontent: 10arbeitsstelle
labourinfo <- tagfilter_labourinfo()

labourinfo_1804 <- labourinfo$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
labourinfo_toolong_1804 <- corpus_subset(labourinfo_1804, ntoken(labourinfo_1804, remove_punct =T) > 8)

### Auctions
## adtype: 03ankuendigung; finance: 07versteigerung
auctions <- tagfilter_auctions()

auctions_1804 <- auctions$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
auctions_toolong_1804 <- corpus_subset(auctions_1804, ntoken(auctions_1804, remove_punct =T) > 1)

### Other news
## adtype: unknown (diverse subjects in these ads); finance: unknown
## maybe new keyword "other news" to identify these at least according to their placement in this category?
othernews <- tagfilter_othernews()

othernews_1804 <- othernews$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
othernews_toolong_1804 <- corpus_subset(othernews_1804, ntoken(othernews_1804, remove_punct =T) > 4)

### Official notices
## adtype: 05nachricht
official <- tagfilter_official()

official_1804 <- official$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
official_toolong_1804 <- corpus_subset(official_1804, ntoken(official_1804, remove_punct =T) > 4)
# threshold of ntokens difficult for this category (a lot of variety in title length)

### Taxes
## from another workflow Preis/Schlag/Tax
taxes <- tagfilter_taxes()

taxes_1804 <- taxes$filtrate(titles_1804,ignore.case = T)

# identifying title-ads that are too long
taxes_toolong_1804 <- corpus_subset(taxes_1804, ntoken(taxes_1804, remove_punct =T) > 2)

### Bookstore
## adtype: 01biete (always?); finance: unknown (various options); adcontent: 05drucksachen
bookstore <- tagfilter_bookstore()

bookstore_1804 <- bookstore$filtrate(titles_1804,ignore.case = T)
# threshold of ntokens for too long titles don't make sense with this category, bc there is a lot of variety
# and very long titles


### False negatives

# Finding those ads classified as "isheader" but not found by dictionaries for titles
# for being able to either dismissing those as titles or expanding and correcting the dictionaries

titles_found_1804 <- c(saleoffer_1804, saledemand_1804, lendoffer_1804, lenddemand_1804, lostandfound_1804,
                       death_1804, marriage_1804, labourinfo_1804, auctions_1804, othernews_1804,
                       official_1804, taxes_1804, bookstore_1804)

titles_missed_1804 <- corpus_subset(titles_1804, titles_1804 %notin% titles_found_1804)


### Inheriting characteristics of titles for following ads

# A short explanation:
# this is my very clumsy first try at making a subcorpus with all the relevant ads connected to the title for "sale offers" in this case
# it works - but it has a lot of steps to it. Especially making subcorporas first for every single title is a bit annoying
# and there is surely a better and shorter way to produce the same results - any ideas?
# it has to be adapted to be functionable for large corporas without thousands of lines of code - any help with that is also very much appreciated =)
# A problem with upscaling this way is the dependance on the same order of titles, e.g. sale offer, sale demand, lend offer, lend demand etc.
# This is the case in large part of the Avisblatt, but not always... Therefore it would be better to just take the pagenr and reading order of the
# next available title (classified as "isheader") and not make it dependend on certain types of titles as in the example below.


## Subcorpus sale offers
# showing readingorder of title saleoffer
saleoffer_1804$readingorder

# showing pagenumber of title saleoffer
saleoffer_1804$page

trial_1804_saleoffer <- corpus_subset(trial_1804,
                                      (page >= saleoffer_1804$page &
                                         page < saledemand_1804$page &
                                         readingorder >= saleoffer_1804$readingorder &
                                         noadvert != 1) |
                                        # finds the title and connected ads on the same page as the title
                                        # if there is only one title on this page and on following pages
                                        # if these pages have no new titles
                                        # excluding no adverts
                                        (page == saleoffer_1804$page &
                                           page == saledemand_1804$page &
                                           readingorder > saleoffer_1804$readingorder &
                                           readingorder < saledemand_1804$readingorder &
                                           noadvert != 1) |
                                        # finds connected ads on the same page as the title
                                        # if there is another title on this page
                                        # excluding no adverts
                                        (page > saleoffer_1804$page &
                                           page == saledemand_1804$page &
                                           readingorder >= saleoffer_1804$readingorder &
                                           readingorder < saledemand_1804$readingorder &
                                           noadvert != 1)
                                        # finds connected ads on the first page with a new title
                                        # if they have a lower readingorder than the next title
                                        # excluding no adverts
)

# attaching relevant adtype and finance for "saleoffer" ads to subcorpus
trial_1804_saleoffer$adtype <- "01biete"
trial_1804_saleoffer$finance <- "01kauf"

# now this has to be repeated for every single title... and then somehow the subcorpora have to be merged together again
# or maybe there is a simple way (that I just cannot see at the moment) to add the new docvars h to the original corpus (here trial_1804)
# by matching the ids?



