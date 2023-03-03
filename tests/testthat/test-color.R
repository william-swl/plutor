test_that("select_color", {
  expect_identical(
    select_color(5),
    c("#AC6966", "#827E2B", "#308974", "#4981A1", "#9A6D8F")
  )
})
