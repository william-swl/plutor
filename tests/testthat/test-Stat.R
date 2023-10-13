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

test_that("StatCountPL", {
  vdiffr::expect_doppelganger(
    title = "StatCountPL",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_text(aes(label = price), stat = "countPL"),
    writer = pl_svg
  )
})

test_that("StatFuncPL", {
  lab_func <- function(x) {
    str_glue("mean = {round(mean(x))}\nn = {length(x)}")
  }
  vdiffr::expect_doppelganger(
    title = "StatFuncPL",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_text(aes(label = price),
        stat = "funcPL",
        lab_func = lab_func, lab_pos = 25000
      ) +
      xlim(0, 30000),
    writer = pl_svg
  )
})
