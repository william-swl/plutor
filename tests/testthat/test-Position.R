pl_init(w = 4, h = 3, res = 300)

test_that("PositionFloatyPL", {
  vdiffr::expect_doppelganger(
    title = "PositionFloatyPL",
    fig = mini_diamond %>% ggplot(aes(x = cut, y = price)) +
      geom_point() +
      geom_text(aes(label = price),
        stat = "meanPL",
        lab_pos = 20000, position = position_floatyPL()
      ),
    writer = pl_svg
  )
})

test_that("PositionFloatxPL", {
  vdiffr::expect_doppelganger(
    title = "PositionFloatxPL",
    fig = mini_diamond %>% ggplot(aes(y = cut, x = price)) +
      geom_point() +
      geom_text(aes(label = price),
        stat = "meanPL",
        lab_pos = 20000, position = position_floatxPL()
      ),
    writer = pl_svg
  )
})
