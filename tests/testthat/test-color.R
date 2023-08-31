test_that("plot_colors", {
  vdiffr::expect_doppelganger(
    title = "plot_colors",
    fig = plot_colors(brewer_colors("Blues", 5)),
    writer = pl_svg
  )
})


test_that("assign_colors", {
  expect_snapshot(
    assign_colors(mini_diamond, cut,
      colors = sci_colors("nejm", 8)
    ) %>%
      dplyr::count(cut, assigned_colors)
  )
})


test_that("assign_colors, fill na", {
  expect_snapshot(
    assign_colors(mini_diamond, clarity,
      colors = sci_colors("nejm", 3)
    ) %>%
      dplyr::count(clarity, assigned_colors)
  )
})
