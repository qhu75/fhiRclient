test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

ct <- Client(app_id = "my_app", api_base = "https://r4.smarthealthit.org")

pt <- Patient(list(id = "fc200fa2-12c9-4276-ba4a-e0601d424e55"))
pt <- Read(pt, ct$server)
pt$birthDate$isostring
as_json(pt)[1:2]
