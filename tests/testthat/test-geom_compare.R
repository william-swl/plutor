pl_init()

test_that("geom_compare", {
  vdiffr::expect_doppelganger(
    title = "geom_compare",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("p", "right_deno_fc"), y_position = 20000) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, log10", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, log10",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("p", "right_deno_fc"), y_position = 4.5) +
      scale_y_log10(limit = c(200, 100000)),
    writer = pl_svg
  )
})


test_that("geom_compare, ignore ns", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, ignore ns",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("psymbol", "right_deno_fc"), y_position = 20000,
                   ignore_ns = TRUE) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, ignore ns partly", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, ignore ns partly",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("psymbol", "right_deno_fc"), y_position = 20000,
                   ignore_ns = c("right_deno_fc")) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, fc1", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, fc1",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_label = c("psymbol", "fc1"), y_position = 20000, tip_length = 0,
        comparisons = list(c("Fair", "Good"), c("Ideal", "Good"))
      ) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, paired", {
  data <- mini_diamond %>%
    pivot_longer(c(x, y), names_to = "xy", values_to = "value")
  vdiffr::expect_doppelganger(
    title = "geom_compare, paired",
    fig = ggplot(data = data, mapping = aes(x = xy, y = value)) +
      geom_point() +
      geom_compare(aes(paired_by = id), paired = TRUE,
                   cp_label = c("p", "right_deno_fc"), y_position = 9) +
      ylim(3, 10),
    writer = pl_svg
  )
})
