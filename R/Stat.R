#' StatMeanPL
#' @export
#'
StatMeanPL <- ggproto("StatMeanPL", Stat,
  required_aes = c("x", "y"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params,
      main_is_orthogonal = TRUE,
      group_has_equal = TRUE,
      main_is_optional = TRUE
    )
    return(params)
  },
  compute_group = function(data, scales, flipped_aes, na.rm,
                           lab_pos = "max+sd", digits = 2) {
    if (flipped_aes == FALSE) {
      revert_func <- revert_pos_scale(scales$y)
      trans_func <- trans_pos_scale(scales$y)
    } else if (flipped_aes == TRUE) {
      revert_func <- revert_pos_scale(scales$x)
      trans_func <- trans_pos_scale(scales$x)
    }


    data <- ggplot2::flip_data(data, flipped_aes)

    # label value
    lab_data <- data %>% dplyr::summarise(
      mean = mean(y, na.rm = na.rm),
      sd = sd(y, na.rm = na.rm),
      max = max(y, na.rm = na.rm),
      label = revert_func(mean) %>% baizer::signif_round_string(digits),
      .by = c(x, group, PANEL)
    )

    # label position, can be character string, number or function
    if (is.character(lab_pos)) {
      if (lab_pos == "max+sd") {
        data <- lab_data %>% dplyr::mutate(y = max + sd)
      } else if (lab_pos == "max") {
        data <- lab_data %>% dplyr::mutate(y = max)
      } else if (lab_pos == "mean+sd") {
        data <- lab_data %>% dplyr::mutate(y = mean + sd)
      }
    } else if (is.numeric(lab_pos)) {
      data <- lab_data %>% dplyr::mutate(y = trans_func(lab_pos))
    } else if (is(lab_pos, "function")) {
      y_data <- data %>% dplyr::summarise(
        y = lab_pos(y),
        .by = c(x, group, PANEL)
      )
      data <- lab_data %>% dplyr::mutate(y = y_data$y)
    }

    data <- ggplot2::flip_data(data, flipped_aes) %>%
      dplyr::select(y, group, PANEL, label, x)

    return(data)
  }
)

#' StatCountPL
#' @export
#'
StatCountPL <- ggproto("StatCountPL", Stat,
  required_aes = c("x", "y"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params,
      main_is_orthogonal = TRUE,
      group_has_equal = TRUE,
      main_is_optional = TRUE
    )
    return(params)
  },
  compute_group = function(data, scales, flipped_aes, na.rm,
                           lab_pos = "max+sd", fmt = "n = {n}") {
    data <- ggplot2::flip_data(data, flipped_aes)

    if (flipped_aes == FALSE) {
      revert_func <- revert_pos_scale(scales$y)
      trans_func <- trans_pos_scale(scales$y)
    } else if (flipped_aes == TRUE) {
      revert_func <- revert_pos_scale(scales$x)
      trans_func <- trans_pos_scale(scales$x)
    }

    # label value
    lab_data <- data %>% dplyr::summarise(
      mean = mean(y, na.rm = na.rm),
      sd = sd(y, na.rm = na.rm),
      max = max(y, na.rm = na.rm),
      n = dplyr::n(),
      label = str_glue(fmt),
      .by = c(x, group, PANEL)
    )

    # label position, can be character string, number or function
    if (is.character(lab_pos)) {
      if (lab_pos == "max+sd") {
        data <- lab_data %>% dplyr::mutate(y = max + sd)
      } else if (lab_pos == "max") {
        data <- lab_data %>% dplyr::mutate(y = max)
      } else if (lab_pos == "mean+sd") {
        data <- lab_data %>% dplyr::mutate(y = mean + sd)
      }
    } else if (is.numeric(lab_pos)) {
      data <- lab_data %>% dplyr::mutate(y = trans_func(lab_pos))
    } else if (is(lab_pos, "function")) {
      y_data <- data %>% dplyr::summarise(
        y = lab_pos(y),
        .by = c(x, group, PANEL)
      )
      data <- lab_data %>% dplyr::mutate(y = y_data$y)
    }

    data <- ggplot2::flip_data(data, flipped_aes) %>%
      dplyr::select(y, group, PANEL, label, x)

    return(data)
  }
)

#' StatFuncPL
#' @export
#'
StatFuncPL <- ggproto("StatFuncPL", Stat,
  required_aes = c("x", "y"),
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params,
      main_is_orthogonal = TRUE,
      group_has_equal = TRUE,
      main_is_optional = TRUE
    )
    return(params)
  },
  compute_group = function(data, scales, flipped_aes, na.rm,
                           lab_pos = "max+sd", lab_func, digits = 2) {
    if (flipped_aes == FALSE) {
      revert_func <- revert_pos_scale(scales$y)
      trans_func <- trans_pos_scale(scales$y)
    } else if (flipped_aes == TRUE) {
      revert_func <- revert_pos_scale(scales$x)
      trans_func <- trans_pos_scale(scales$x)
    }


    data <- ggplot2::flip_data(data, flipped_aes)

    # label value
    lab_data <- data %>%
      dplyr::mutate(y_raw = revert_func(y)) %>%
      dplyr::summarise(
        mean = mean(y, na.rm = na.rm),
        sd = sd(y, na.rm = na.rm),
        max = max(y, na.rm = na.rm),
        label = lab_func(y_raw),
        .by = c(x, group, PANEL)
      )

    # label position, can be character string, number or function
    if (is.character(lab_pos)) {
      if (lab_pos == "max+sd") {
        data <- lab_data %>% dplyr::mutate(y = max + sd)
      } else if (lab_pos == "max") {
        data <- lab_data %>% dplyr::mutate(y = max)
      } else if (lab_pos == "mean+sd") {
        data <- lab_data %>% dplyr::mutate(y = mean + sd)
      }
    } else if (is.numeric(lab_pos)) {
      data <- lab_data %>% dplyr::mutate(y = trans_func(lab_pos))
    } else if (is(lab_pos, "function")) {
      y_data <- data %>% dplyr::summarise(
        y = lab_pos(y),
        .by = c(x, group, PANEL)
      )
      data <- lab_data %>% dplyr::mutate(y = y_data$y)
    }

    data <- ggplot2::flip_data(data, flipped_aes) %>%
      dplyr::select(y, group, PANEL, label, x)

    return(data)
  }
)
