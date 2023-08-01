pl_init(w = 4, h = 3, res = 300)

test_that("geom_describe", {
  vdiffr::expect_doppelganger(
    title = "geom_describe",
    fig = mini_diamond %>% ggplot(aes(x = cut, y = price)) +
      geom_point() +
      geom_describe(),
    writer = pl_svg
  )
})


test_that("geom_describe, flip", {
  vdiffr::expect_doppelganger(
    title = "geom_describe, flip",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_describe(),
    writer = pl_svg
  )
})
