#' StatCompare
#' @export
StatCompare <- ggproto("StatCompare", Stat,
  required_aes = c("x", "y"),
  optional_aes = c("paired_by"),
  dropped_aes = c("paired_by", "colour"),
  setup_data = function(self, data, params) {
    # init group
    data <- dplyr::mutate(data, group = 1)
    return(data)
  },
  compute_group = function(data, scales, na.rm, lab_pos, step_increase,
                           cp_label, tip_length,
                           ignore_ns, fc_method, comparisons, paired,
                           alternative, test_method, ns_symbol, cp_ref,
                           cp_inline, fc_digits) {
    # p value compare
    # position scale transformation like scale_y_log10 will compute
    # before this, so just use 'identity'
    if (paired == TRUE) {
      if (is.null(data$paired_by)) {
        stop("please use mapping=aes(paired_by=col) to
             indicate pairs by an extra column!")
      }
      data_p <- baizer::stat_test(data,
        x = x, y = y, .by = PANEL, digits = 2,
        trans = "identity", method = test_method, paired = TRUE,
        paired_by = paired_by,
        alternative = alternative, ns_symbol = ns_symbol
      ) %>% dplyr::select(-y)
    } else {
      data_p <- baizer::stat_test(data,
        x = x, y = y, .by = PANEL, digits = 2,
        trans = "identity", method = test_method, paired = FALSE,
        alternative = alternative, ns_symbol = ns_symbol
      ) %>% dplyr::select(-y)
    }

    # fold change compare
    # fold change is a little complicated, you may want the fold change
    # between means or geometric means
    # so we should get the raw value before position scale transformation
    revert_func <- revert_pos_scale(scales$y)
    trans_func <- trans_pos_scale(scales$y)
    data_fc <- dplyr::mutate(data, y = revert_func(y))

    if (str_detect(scales$y$trans$name, "^log")) {
      if (is.null(fc_method)) fc_method <- "geom_mean"
    } else {
      if (is.null(fc_method)) fc_method <- "mean"
    }


    data_fc1 <- baizer::stat_fc(data_fc,
      x = x, y = y, .by = PANEL,
      method = fc_method, digits = fc_digits
    ) %>%
      dplyr::rename("right_deno_fc" = "fc_fmt")
    data_fc2 <- baizer::stat_fc(data_fc,
      x = x, y = y, .by = PANEL,
      method = fc_method, rev_div = TRUE, digits = fc_digits
    ) %>%
      dplyr::rename("left_deno_fc" = "fc_fmt")
    data_fc <- baizer::left_expand(
      data_fc1, data_fc2,
      by = c("group1", "group2")
    ) %>% dplyr::select(-y)
    data <- dplyr::left_join(data_p, data_fc,
      by = c("PANEL", "group1", "group2")
    )



    # comparisons
    if (length(comparisons) > 0) {
      comparisons <- purrr::map(
        comparisons,
        ~ scales$x$map(.x) %>% as.character()
      )

      cp_tb <- baizer::list2df(comparisons) %>%
        set_names("group1", "group2")

      cp_tb1 <- cp_tb %>%
        left_join(data, by = c("group1", "group2")) %>%
        dplyr::mutate(fc1 = right_deno_fc, fc2 = left_deno_fc)
      cp_tb2 <- cp_tb %>%
        left_join(
          data %>% dplyr::rename(
            "group1" = "group2", "group2" = "group1", "n1" = "n2", "n2" = "n1",
            "y1" = "y2", "y2" = "y1"
          ),
          by = c("group1", "group2")
        ) %>%
        dplyr::mutate(fc1 = left_deno_fc, fc2 = right_deno_fc)

      data <- baizer::rewrite_na(cp_tb1, cp_tb2, by = c("group1", "group2"))

      data <- dplyr::rename(data, x = group1, xend = group2) %>%
        dplyr::mutate(x = as.integer(x), xend = as.integer(xend))
    } else if (!is.null(cp_ref)) {
      # comparisons ref
      cp_ref <- scales$x$map(cp_ref)

      cp_tb1 <- data %>%
        dplyr::filter(data[["group1"]] == cp_ref) %>%
        dplyr::mutate(fc1 = left_deno_fc, fc2 = right_deno_fc) %>%
        dplyr::rename(x = group1, xend = group2)
      cp_tb2 <- data %>%
        dplyr::filter(data[["group2"]] == cp_ref) %>%
        dplyr::mutate(fc1 = right_deno_fc, fc2 = left_deno_fc) %>%
        dplyr::rename(x = group2, xend = group1)
      data <- bind_rows(cp_tb1, cp_tb2) %>%
        dplyr::mutate(x = as.integer(x), xend = as.integer(xend))
    } else {
      data <- dplyr::rename(data, x = group1, xend = group2) %>%
        dplyr::mutate(x = as.integer(x), xend = as.integer(xend))
    }

    # ignore NS
    if (all(ignore_ns == TRUE)) {
      data <- data %>% dplyr::filter(p < 0.05)
    } else if (is.character(ignore_ns)) {
      data <- data %>%
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(ignore_ns),
            ~ ifelse(p < 0.05, .x, NA)
          )
        )
    }


    # label
    label_data <- data[, cp_label] %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(~ ifelse(is.na(.x), "", .x))
    label_col <- tidyr::unite(label_data, "label", sep = "\n") %>%
      unlist() %>%
      unname()
    # move '\n' from from the label string end to the start
    label_col_blank <- str_extract(label_col, "\n+$")
    label_col_blank[is.na(label_col_blank)] <- ""
    label_col <- str_replace(label_col, "\n+$", "")
    label_col <- str_c(label_col_blank, label_col)
    data$label <- label_col

    # reorder the compare
    data <- data %>% dplyr::arrange(xend - x, x)

    # compare bracket y position
    y_range <- scales$y$range$range
    if (is.null(lab_pos)) {
      lab_pos <- (y_range[2] - y_range[1]) * 0.8 + y_range[1]
    }
    y_step_increase <- (y_range[2] - y_range[1]) * step_increase
    if (cp_inline == TRUE) y_step_increase <- 0
    data <- data %>% dplyr::mutate(
      cp_step = seq_len(dplyr::n()) - 1,
      y = trans_func(lab_pos) + y_step_increase * cp_step, yend = y
    )

    return(data)
  }
)



#' GeomCompare
#' @export
GeomCompare <- ggproto("GeomCompare", Geom,
  required_aes = c("x", "y", "label"),
  default_aes = aes(
    colour = "black",
    linewidth = lpt(1), linetype = 1, alpha = NA,
    size = tpt(10), angle = 0, hjust = 0.5, vjust = -0.15,
    family = "", fontface = 1, lineheight = 0.8
  ),
  setup_params = function(self, data, params) {
    params$cp_result <- data
    return(params)
  },
  setup_data = function(self, data, params) {
    if (!is.null(params$cp_manual)) {
      data <- params$cp_manual
    }
    return(data)
  },
  draw_group = function(data, panel_params, coord, cp_label,
                        ns_lineheight_just, tip_length, cp_ref, cp_inline,
                        brackets_widen, cp_manual, cp_result, lineend, ...) {
    # ns lineheight justify
    if ("psymbol" %in% cp_label) {
      data <- data %>% dplyr::mutate(
        lineheight =
          ifelse(p >= 0.05, lineheight + ns_lineheight_just, lineheight)
      )
    }

    y_range <- panel_params$y.range
    if (!is.null(cp_ref) && (cp_inline == TRUE)) {
      # label
      label_data <- dplyr::mutate(data, x = xend)
      # bracket
      bracket <- data %>%
        dplyr::mutate(
          xx = min(min(x), min(xend)) - brackets_widen,
          xend = max(max(x), max(xend)) + brackets_widen,
          x = xx
        ) %>%
        dplyr::slice(1)
    } else {
      # label
      label_data <- dplyr::mutate(data, x = (x + xend) / 2)
      # bracket
      bracket <- data %>% dplyr::mutate(
        x = ifelse(x < xend, x - brackets_widen, x + brackets_widen),
        xend = ifelse(x < xend, xend + brackets_widen, xend - brackets_widen)
      )
      # tip
      tip_left <- bracket %>%
        dplyr::mutate(
          xend = x,
          yend = y - tip_length * (y_range[2] - y_range[1])
        )
      tip_right <- bracket %>%
        dplyr::mutate(
          x = xend, yend =
            y - tip_length * (y_range[2] - y_range[1])
        )
    }


    grid::gList(
      GeomSegment$draw_panel(bracket, panel_params,
        coord,
        lineend = lineend, ...
      ),
      if (!is.na(tip_length) && cp_inline == FALSE) {
        GeomSegment$draw_panel(tip_left, panel_params,
          coord,
          lineend = lineend, ...
        )
      },
      if (!is.na(tip_length) && cp_inline == FALSE) {
        GeomSegment$draw_panel(tip_right, panel_params,
          coord,
          lineend = lineend, ...
        )
      },
      GeomText$draw_panel(label_data, panel_params, coord, ...)
    )
  }
)



#' add p value and fold change on a plot
#'
#' @inheritParams ggplot2::geom_segment
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to `ggplot2::geom_segment()`.
#' @param lab_pos position of the label brackets
#' @param step_increase the increase height for next bracket,
#' a ratio according to the whole panel height
#' @param tip_length the length for tips at the ends of the brackets,
#' a ratio according to the whole panel height
#' @param cp_label which values will be add on the plot, a character vector
#' with some of `psymbol, p, right_deno_fc, left_deno_fc` in it.
#' If `comparisons` is assigned, you can also include `fc1, fc2`
#' @param ns_lineheight_just if show `psymbol` in the label, justify
#' the `NS` labels to make the lineheights look balanced
#' @param ignore_ns if `TRUE` will ignore all label items if p >= 0.05,
#' or you can assign a character vector like `cp_label` to ignore some items
#' of the label
#' @param fc_method fold change method, default is `mean`. If you use `log10` or
#' `log2` axis, default is `geom_mean`.
#' @param comparisons a list of two-element vector, to assign the comparisons
#' should be performed
#' @param paired paired test or not, `FALSE` as default. If `TRUE`, you should
#' use mapping=aes(paired_by=col) to indicate pairs by an extra column
#' @param alternative one of `two.sided, greater, less`
#' @param test_method `wilcoxon` as default, one of `wilcoxon, t`
#' @param cp_ref reference item, the others will be compared with it
#' @param cp_inline draw in line or not, default is `FALSE`
#' @param fc_digits fold change digits
#' @param cp_manual manual comparisons table,
#' please refer to `extract_compare()`
#' @param ns_symbol the symbol of non-significant, `NS` as default
#' @param brackets_widen widen the brackets, can be a negative value
#' @param cp_result comparation result tibble
#'
#' @return `ggplot` object
#'
#' @export

geom_compare <- function(mapping = NULL, data = NULL,
                         stat = "compare", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, lab_pos = NULL,
                         step_increase = 0.1,
                         tip_length = 0.02, lineend = "round",
                         cp_label = c("psymbol"),
                         ns_lineheight_just = 0.2,
                         ignore_ns = FALSE, fc_method = NULL,
                         comparisons = NULL, paired = FALSE,
                         alternative = "two.sided", test_method = "wilcoxon",
                         ns_symbol = "NS", cp_ref = NULL, cp_inline = FALSE,
                         brackets_widen = 0, fc_digits = 2,
                         cp_result = NULL, cp_manual = NULL) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomCompare,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, lab_pos = lab_pos, step_increase = step_increase,
      tip_length = tip_length, cp_label = cp_label, ignore_ns = ignore_ns,
      ns_lineheight_just = ns_lineheight_just, fc_method = fc_method,
      comparisons = comparisons, paired = paired, alternative = alternative,
      test_method = test_method, ns_symbol = ns_symbol, cp_ref = cp_ref,
      cp_inline = cp_inline, brackets_widen = brackets_widen,
      fc_digits = fc_digits, cp_result = cp_result, cp_manual = cp_manual,
      lineend = lineend, ...
    )
  )
}

#' geom2trace.GeomCompare
#' @param data,params,plot params
#' @export
#' @return no return value
geom2trace.GeomCompare <- function(data, params, plot) {} # nolint



#' extract the result of `geom_compare` from a `ggplot` object
#'
#' @param p ggplot object
#'
#' @return compare tibble
#' @export
#'
extract_compare <- function(p) {
  if (!is.ggplot(p)) {
    stop("Please input a ggplot object!")
  }
  # render but do not need to return
  xxx <- suppressWarnings(ggplot_build(p)) # nolint

  l <- p$layers %>% map(~ .x[["computed_geom_params"]][["cp_result"]])
  l <- l[!map_lgl(l, is.null)]
  if (length(l) == 1) {
    return(l[[1]])
  } else {
    return(l)
  }
}
