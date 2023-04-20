test_that("theme_pl", {
  vdiffr::expect_doppelganger(
    title = "theme_pl",
    fig = {
      ggplot(mini_diamond, aes(x = x, y = y, color = clarity)) +
        geom_point(size = 2) +
        facet_grid(. ~ cut) +
        labs(title = "title", tag = "tag", caption = "caption") +
        theme_pl()
    }, writer = pl_svg
  )
})
