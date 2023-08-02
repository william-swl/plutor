test_that("scale_y_log10_pl", {
  vdiffr::expect_doppelganger(
    title = "scale_y_log10_pl",
    fig = mini_diamond %>% ggplot(aes(x = cut, y = price)) +
      geom_point() +
      geom_text(stat = "meanPL", lab_pos = 30000) +
      scale_y_log10_pl(show_minor_breaks = TRUE, limits = c(100, 100000)),
    writer = pl_svg
  )
})

test_that("scale_y_continuous_pl", {
  vdiffr::expect_doppelganger(
    title = "scale_y_continuous_pl",
    fig = mini_diamond %>% ggplot(aes(x = cut, y = price)) +
      geom_point() +
      geom_text(stat = "meanPL", lab_pos = 25000) +
      scale_y_continuous_pl(limits = c(0, 40000), minor_break_step = 2500),
    writer = pl_svg
  )
})

test_that("scale_x_log10_pl", {
  vdiffr::expect_doppelganger(
    title = "scale_x_log10_pl",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_text(stat = "meanPL", lab_pos = 30000) +
      scale_x_log10_pl(show_minor_breaks = TRUE, limits = c(100, 100000)),
    writer = pl_svg
  )
})

test_that("scale_x_continuous_pl", {
  vdiffr::expect_doppelganger(
    title = "scale_x_continuous_pl",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_text(stat = "meanPL", lab_pos = 25000) +
      scale_x_continuous_pl(limits = c(0, 40000), minor_break_step = 2500),
    writer = pl_svg
  )
})
