test_that("theme_pl", {
  vdiffr::expect_doppelganger(
    "theme_pl",
    ggplot(mini_diamond, aes(x = x, y = y, color = clarity)) +
      geom_point(size = 2) +
      facet_grid(. ~ cut) +
      labs(title = "title", tag = "tag", caption = "caption") +
      theme_pl()
  )
})
