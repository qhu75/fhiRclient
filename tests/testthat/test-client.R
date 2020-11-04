test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

ct <- Client(app_id = "my_app", api_base = "https://r4.smarthealthit.org")

pt <- Patient(list(id = "326b4675-0bc8-4dbd-b406-a5564c282401"))
pt <- Read(ct$server, pt)
BirthDate(pt)
as_json(pt)[1:2]

library(magrittr)
pt <- Patient(list(id = "326b4675-0bc8-4dbd-b406-a5564c282401"))

ct$server %>% Read(pt) %>% as_json
ct$server %>% Read(pt) %>% BirthDate
ct$server %>% Read(pt) %>% HumanName
