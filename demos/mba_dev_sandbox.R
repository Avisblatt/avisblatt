summary(corpus_1834)

class(corpus_1834)

aa <- corpus_1834[1:5]

str(corpus_1834)

attributes(aa[1])$docvars$id

aa[['avis_1834.csv.2']]

names(aa)

summary(aa)[,1:4]

ids <- docvars(corp, identifier)[sel]


avis_1834 <- readtext("data/avis_1834.csv",
                      text_field = "text",
                      encoding = "UTF-8")

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
