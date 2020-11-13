test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

ct <- Client(app_id = "my_app", api_base = "https://r4.smarthealthit.org")

pt <- Patient(list(id = "326b4675-0bc8-4dbd-b406-a5564c282401"))
pt <- Read(ct, pt)
BirthDate(pt)
as_json(pt)[1:2]

library(magrittr)
library(uuid)
pt <- Patient(list(id = "326b4675-0bc8-4dbd-b406-a5564c282401"))
pt1 <- Read(ct, pt)

## $
pt1$id
pt1$id <- NULL

## Create
## pt1c <- Create(ct, pt1)
id <- ct %>% Create(pt1) %$% id

## Read
ct %>% Read(pt) %>% as_json %>% as_json_tbl
ct %>% Read(pt) %>% BirthDate
ct %>% Read(pt) %>% getName

## Update
pt1 <- ct %>% Read(Patient(list(id = id)))
pt1$name[[1]]$given <- list("Bradly", "Middle")
getName(pt1)
pt1u <- ct %>% Update(pt1)
pt1u <- Patient(list(id = id))
ct %>% Read(pt1u) %>% getName

## Delete
ct %>% Delete(pt1u)

## All classes
pt <- Model("patient", "Patient", list(id = "326b4675-0bc8-4dbd-b406-a5564c282401"))
getModel("humanname")
hn <- Model("humanname", "HumanName", list(id = id, family = "A", given = list("B", "C")))
pt1$name[[1]] <- py(hn)
getName(pt1)
pt1u <- Update(ct, pt1)
pt1u <- Patient(list(id = id))
ct %>% Read(pt1u) %>% getName

## Search
library(httr)
library(jsonlite)
ss <- GET("http://hapi.fhir.org/baseR4/Observation?code=http%3A%2F%2Floinc.org%7C2339-0")
j1 <- fromJSON(content(ss, "text"))
pt <- j1$entry$resource$subject$reference

lk <- j1$link
while(nrow(lk)>=2 & "next" %in% lk$relation){
    u1 <- lk$url[lk$relation == "next"]
    j <- fromJSON(content(GET(u1), "text"))
    pt <- c(pt, j$entry$resource$subject$reference)
    lk <- j$link
}
length(unique(pt))

res <- Search(ct, "observation", "Observation",
              list(code = "http://loinc.org|2339-0"), page = "all")
res <- Search(ct, "observation", "Observation",
              list(code = "http://loinc.org|2339-0"), page = "first")

getModel("observation")
Obs <- Model("observation", "Observation", NULL)
sc <- Obs$where(list(code = "http://loinc.org|2339-0"))
sc <- sc$include('subject')
bd <- sc$perform(ct$server)

bd$link

ids <- lapply(bd$entry, function(x)x$as_json()$resource$id)
bd$entry[[1]]$as_json()$resource$id

## tbl_json
library(jsonlite)
library(tidyjson)
library(tidyverse)
ptj <- as_json(pt)

lapply(res$entry, function)
df <- enframe(unlist(res))

res <- Search(ct, "observation", "Observation",
              list(code = "http://loinc.org|2339-0"), page = "all")
##res %>% filter(str_detect(name, 'subject')) %>% unique %>% nrow
res %>% filterBundle('subject') %>% unique %>% nrow
