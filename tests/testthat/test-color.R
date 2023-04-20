test_that("select_color", {
  expect_identical(
    select_color(5),
    c("#AC6966", "#90792C", "#308974", "#4981A1", "#9A6D8F")
  )
})


test_that("plot_col", {
  vdiffr::expect_doppelganger(
    title = "plot_col",
    fig = plot_col(select_color(6)),
    writer = pl_svg
  )
})
