pl_init(w = 4, h = 3, res = 300)

test_that("StatMeanPL", {
  vdiffr::expect_doppelganger(
    title = "StatMeanPL",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_text(aes(label = price), stat = "meanPL"),
    writer = pl_svg
  )
})
