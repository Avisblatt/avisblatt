# idea avis collection object
avis_1834 <- AvisCollection$new("data/avis_1834.csv", year = 1834)
class(avis_1834)
# contains meta information
avis_1834$meta$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1`

# contains a quanteda corpus




secondhand <- tagfilter_secondhand()
sh_corpus <- secondhand$filtrate(corpus_1834, ignore.case = T)
xx <- names(sh_corpus)






mc$bulk_update_language(lang = "de")
mc$records$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1`

mc$records$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a4`$tags
mc$show_distinct_tags()





x <- serialize(list(1,2,3), NULL)
?serialize()

aa <- serialize(mc, NULL,xdr = FALSE)
tt <- unserialize(aa)

class(tt)
tt$records$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1`
object.size(tt)

class(corpus_1834)

aa <- corpus_1834[1:5]

str(corpus_1834)

attributes(aa[1])$docvars$id

aa[['avis_1834.csv.2']]

names(aa)

summary(aa)[,1:4]

ids <- docvars(corp, identifier)[sel]



avis_1834$text <- correct_ocr(avis_1834$text)

corpus_1834 <- corpus(avis_1834,
                      docid_field = "doc_id")

corpus_1834 <- avis_create_corpus("data/avis_1834.csv")

secondhand <- tagfilter_secondhand()
sh_ids <- secondhand$filtrate(corpus_1834, ignore.case = T)

debug(secondhand$filtrate)


labor <- tagfilter_labor()
l_corp <- labor$filtrate(corpus_1834)
summary(l_corp)


# funny idea for avisblatt operator to return the id.
# overwrites the standard operator
`[[.corpus` <- function(x,id){
  pos <- grep(paste0(id,"$"),
              docvars(x,"id"))
  x[[pos]]
}

attributes(corpus_1834)

tt <- texts(aa)
class(tt)

corpus_1834[2]$id

corpus_1834[["0c86562e-3f0b-54f5-97a5-4187d147925d/a1"]]
corpus_1834[['id']]
names(corpus_1834[1:3])

vectorIndex

head(docvars(corpus_1834,"id"))


pos <- grep("0c86562e-3f0b-54f5-97a5-4187d147925d/a1$",
            docvars(corpus_1834,"id"))
