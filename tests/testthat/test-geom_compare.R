pl_init(w = 4, h = 3, res = 300)

test_that("geom_compare", {
  vdiffr::expect_doppelganger(
    title = "geom_compare",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("p", "right_deno_fc")) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, log10", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, log10",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("p", "right_deno_fc"), lab_pos = 40000) +
      scale_y_log10(limit = c(200, 100000)),
    writer = pl_svg
  )
})

test_that("geom_compare, facet", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, facet",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(cp_label = c("p", "right_deno_fc"), lab_pos = 20000) +
      ylim(0, 25000) +
      facet_wrap(~clarity),
    writer = pl_svg
  )
})

test_that("geom_compare, ignore ns", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, ignore ns",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_label = c("psymbol", "right_deno_fc"), lab_pos = 20000,
        ignore_ns = TRUE
      ) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, ignore ns partly", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, ignore ns partly",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_label = c("psymbol", "right_deno_fc"), lab_pos = 20000,
        ignore_ns = c("right_deno_fc")
      ) +
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
        cp_label = c("psymbol", "fc1"), lab_pos = 20000, tip_length = NA,
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
      geom_compare(aes(paired_by = id),
        paired = TRUE,
        cp_label = c("p", "right_deno_fc"), lab_pos = 9
      ) +
      ylim(3, 10),
    writer = pl_svg
  )
})



test_that("geom_compare, inline", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, inline",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_inline = TRUE, brackets_widen = -0.1,
        comparisons = list(c("Fair", "Good"), c("Good", "Ideal"))
      ),
    writer = pl_svg
  )
})



test_that("geom_compare, cp_ref", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, cp_ref",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_label = c("p", "right_deno_fc"),
        cp_ref = "Good", lab_pos = 20000
      ) +
      ylim(0, 25000),
    writer = pl_svg
  )
})

test_that("geom_compare, cp_ref and inline", {
  vdiffr::expect_doppelganger(
    title = "geom_compare, cp_ref and inline",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_label = c("psymbol", "fc1"), cp_ref = "Fair",
        cp_inline = TRUE, lab_pos = 20000
      ) +
      geom_compare(
        cp_label = c("psymbol", "fc1"), cp_ref = "Good",
        cp_inline = TRUE, lab_pos = 22000
      ) +
      geom_compare(
        cp_label = c("psymbol", "fc1"), cp_ref = "Ideal",
        cp_inline = TRUE, lab_pos = 24000
      ) +
      ylim(0, 25000),
    writer = pl_svg
  )
})


test_that("extract_compare", {
  p <- ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
    geom_point() +
    geom_compare(
      cp_label = c("psymbol", "fc1"), cp_ref = "Fair",
      cp_inline = TRUE, lab_pos = 20000
    ) +
    geom_compare(
      cp_label = c("psymbol", "fc1"), cp_ref = "Good",
      cp_inline = TRUE, lab_pos = 22000
    ) +
    geom_compare(
      cp_label = c("psymbol", "fc1"), cp_ref = "Ideal",
      cp_inline = TRUE, lab_pos = 24000
    ) +
    ylim(0, 25000)
  expect_snapshot(extract_compare(p))
})


test_that("geom_compare, manual comparisons table", {
  p <- ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
    geom_point() +
    geom_compare(cp_label = c("p", "right_deno_fc"), lab_pos = 20000) +
    ylim(0, 25000)

  modified_labs <- extract_compare(p) %>%
    dplyr::mutate(label = c("lab1", "lab2", "lab3"))

  vdiffr::expect_doppelganger(
    title = "geom_compare, manual comparisons table",
    fig = ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
      geom_point() +
      geom_compare(
        cp_label = c("p", "right_deno_fc"),
        lab_pos = 20000, cp_manual = modified_labs
      ) +
      ylim(0, 25000),
    writer = pl_svg
  )
})
