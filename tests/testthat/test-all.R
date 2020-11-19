ct <- Client(app_id = "my_app", api_base = "https://r4.smarthealthit.org")
pid <- "326b4675-0bc8-4dbd-b406-a5564c282401"
pt <- Patient(list(id = pid))
pt1 <- Read(ct, pt)

test_that("Client connection", {
    expect_equal(pt1$id, pid)
})

pt1$id <- NULL
out1 <- Create(ct, pt1)
test_that("Create", {
    expect_true(out1$id != pid)
})

bd <- ct %>% Read(pt) %>% BirthDate
test_that("Read", {
    expect_equal(bd, "1991-07-20")
})

pt1 <- ct %>% Read(Patient(list(id = out1$id)))
pt1$name[[1]]$given <- list("Bradly", "Middle")    
pt1u <- ct %>% Update(pt1)
pt1u <- Patient(jsonDict = list(id = out1$id))
new_name <- ct %>% Read(pt1u) %>% getName
test_that("Updaste", {
    expect_equal(new_name[[1]], "Mr. Bradly Middle Douglas")
})

dout <- ct %>% Delete(pt1u)
test_that("Delete", {
    expect_equal(dout$resourceType, "OperationOutcome")
})
