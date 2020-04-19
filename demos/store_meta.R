library(jsonlite)
library(R6)

sh <- names(sh_corpus)

MetaInfoRecord <- R6Class("MetaInfoRecord", list(
  id = NULL,
  year = NULL,
  language = NULL,
  date = NULL,
  tags = NULL,
  initialize = function(id, year = NULL,
                        tags = NULL,
                        language = NULL){
    self$id <-id
    self$year <- year
    self$tags <- tags
    self$language <- tags
  },
  add_tag = function(tags){
    self$tags <- unique(c(self$tags,tags))
  },
  overwrite_tag = function(tags){
    self$tags <- unique(tags)
  },
  set_language = function(language){
    self$language <- unique(language)
  }
))

AvisCollection <- R6Class("AvisCollection",list(
  corpus = NULL,
  meta = NULL,
  initialize = function(crps, year = NULL){
    # either specify path to avisblatt type of .csv
    # quanteda corpus object
    if(is.character(crps)){
      self$corpus <- avis_create_corpus(crps)
    } else{
      stopifnot(inherits(crps,"corpus"))
      self$corpus <- crps
    }

    l <- lapply(names(self$corpus), function(x){
      MetaInfoRecord$new(id = x, year)
    })
    names(l) <- names(self$corpus)
    self$meta <- list2env(l)
  },
  bulk_update_tags = function(ids = NULL, tags){
    if(is.null(ids)) {
      list_of_env <- mget(ls(self$meta),
                          self$meta)
    } else{
      list_of_env <- mget(ids, self$meta)
    }

    names(list_of_env) <- ids
    lapply(list_of_env, function(x) x$add_tag(tags))
    invisible(self)
  },
  bulk_update_language = function(ids = NULL, lang){
    if(is.null(ids)) {
      list_of_env <- mget(ls(self$meta),
                          self$meta)
    } else{
      list_of_env <- mget(ids, self$meta)
    }

    names(list_of_env) <- ids
    lapply(list_of_env, function(x) x$set_language(lang))
    invisible(self)
  },
  show_distinct_tags = function(){
    l <- unlist(eapply(self$meta, function(x) x$tags),
           recursive = F)
    unique(l)
  }

))



# JSON serialize this object
# question remains: serialize as regular JSON or as
# NDJSON...?
mc <- MetaInfoCollection$new(xx, year = 1734)

l1 <- list(
     list(
       lang = mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`$language,
       tags = mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`$tags
     ))

l2 <- list(
  list(
    lang = mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`$language,
    tags = mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`$tags
  ))

names(l1) <- "de6bc0ed-8b47-5172-9908-fedf6423a876/a27"
names(l2) <- "de6bc0ed-8b47-5172-9908-fedf6423a876/a27"

list(l1,l2)

json <- toJSON(list(l1,l2),pretty = T)
write(json,file = "some.json")
u <- unlist(fromJSON("some.json",simplifyDataFrame = F),recursive = F)

u$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`
u$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`

write_json(list(l1,l2),"this.json",pretty=T)
r <- read_json("this.json")


r[[1]]


mc$bulk_update_language(lang = "de")
mc$bulk_update_tags(sh, c("second-hand","anna"))
mc$bulk_update_tags(tags = "")

oo <- mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`

ff <- list(id = oo$id,
              lang = oo$language,
              year = oo$year,
              tags = oo$tags
)

library(dplyr)
cc <- tibble(id = oo$id,
       lang = oo$language,
       tags = list(oo$tags))

dd <- tibble(id = oo$id,
             lang = oo$language,
             tags = list(oo$tags))


con_out <- file("test.json", open = "wb")

stream_out(rbind(cc,dd), con_out)
close(con_out)

con_out <- file("test.json",open = "r")
stream_in(con_out)



mc$show_distinct_tags()

serialize_collection <- function(mc){

}

oo <- as.list(mc$records)

mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`$tags

mc$records$`0066a6d4-fcaf-5b7d-b7aa-68e3d971725d/a1`$tags

toJSON(as.list(mc$records),pretty = TRUE)

stream_out(mc$records$`de6bc0ed-8b47-5172-9908-fedf6423a876/a27`$tags)

x <- iris[1:3,]
toJSON(x)
stream_out(x)


df1 <- tibble(
  g = c(1, 2, 3),
  data = list(
    tibble(x = 1, y = 2),
    tibble(x = 4:5, y = 6:7),
    tibble(x = 10)
  )
)


toJSON()


dput(mc)


a <- mget(sh, mc$records)
class(a)
a$`0c86562e-3f0b-54f5-97a5-4187d147925d/a12`$add_tag("wow")

mc$records$`0c86562e-3f0b-54f5-97a5-4187d147925d/a12`



zz <- lapply(a, function(x){
  x
})

zz$`0c86562e-3f0b-54f5-97a5-4187d147925d/a12`

x <- MetaInfoRecord$new(id = 2, 1734, c('secondhand',"furniture"))
y <- MetaInfoRecord$new(id = 3, 1734, c('labor'))
y <- MetaInfoRecord$new(id = 4)

x$add_tag('moo')

x$overwrite_tag("cat")



object.size(e)

xx <- names(sh_corpus)
xx <- names(corpus_1834)

l <- list()
for(i in 1:length(xx)) l[[i]] <- list2env(list(year = 1734))
names(l) <- xx

e <- list2env(l)

e$`29c1a2d2-89bb-5e42-b639-3beef104c240/a13`$year



toJSON(eapply(e,as.list))

assign("tags",c("secondhand","labor"), envir = )

mi <- '{
"abc": {
     "date": ["1734-01-01"],
     "tags": ["labor"]
},
"xyz": {
     "date": ["1735-01-01"],
     "tags": ["secondhand","furniture"]
}}'

out <- fromJSON(mi,simplifyDataFrame = F)

m <- list2env(out)

m$abc
m <- new.env()
records <- c("abc","def","ghi","jkl")


as.list()

list2env(as.list())


