test_that("plot_colors", {
  vdiffr::expect_doppelganger(
    title = "plot_colors",
    fig = plot_colors(brewer_colors("Blues", 5)),
    writer = pl_svg
  )
})
