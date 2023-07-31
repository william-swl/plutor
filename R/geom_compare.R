#' @export
StatCompare <- ggproto("StatCompare", Stat,
  required_aes = c("x", "y"),
  optional_aes = c("paired_by"),
  dropped_aes = c("paired_by"),
  setup_data = function(self, data, params) {
    # init group
    data <- dplyr::mutate(data, group = 1)
    return(data)
  },
  compute_group = function(data, scales, na.rm, y_position, step_increase,
                           cp_label, tip_length,
                           ignore_ns, fc_method, comparisons, paired,
                           alternative, test_method, ns_symbol) {
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
    y_trans <- scales$y$trans$name
    if (y_trans == "log-10") {
      data_fc <- dplyr::mutate(data, y = 10^y)
      if (is.null(fc_method)) fc_method <- "geom_mean"
    } else if (y_trans == "log-2") {
      data_fc <- dplyr::mutate(data, y = 2^y)
      if (is.null(fc_method)) fc_method <- "geom_mean"
    } else if (y_trans %in% c("identity", "reverse")) {
      data_fc <- data
      if (is.null(fc_method)) fc_method <- "mean"
    }

    data_fc1 <- baizer::stat_fc(data_fc,
      x = x, y = y, .by = PANEL,
      method = fc_method
    ) %>%
      dplyr::rename(right_deno_fc = fc_fmt)
    data_fc2 <- baizer::stat_fc(data_fc,
      x = x, y = y, .by = PANEL,
      method = fc_method, rev_div = TRUE
    ) %>%
      dplyr::rename(left_deno_fc = fc_fmt)
    data_fc <- baizer::left_expand(
      data_fc1, data_fc2,
      by = c("group1", "group2")
    ) %>% dplyr::select(-y)
    data <- dplyr::left_join(data_p, data_fc,
      by = c("PANEL", "group1", "group2")
    )

    # comparisons
    comparisons <- purrr::map(
      comparisons,
      ~ scales$x$map(.x) %>% as.character()
    )

    if (length(comparisons) > 0) {
      cp_tb <- baizer::list2tibble(comparisons) %>%
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

      data <- baizer::replace_na(cp_tb1, cp_tb2, by = c("group1", "group2"))
    }

    # variable to aes scales
    data <- dplyr::rename(data, x = group1, xend = group2) %>%
      dplyr::mutate(x = as.integer(x), xend = as.integer(xend))

    # ignore NS
    if (all(ignore_ns == TRUE)) {
      data <- data %>% dplyr::filter(p < 0.05)
    } else if (is.character(ignore_ns)) {
      data <- data %>%
        dplyr::mutate(across(any_of(ignore_ns), ~ ifelse(p < 0.05, .x, NA)))
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
    if (is.null(y_position)) {
      y_position <- (y_range[2] - y_range[1]) * 0.8 + y_range[1]
    }
    y_step_increase <- (y_range[2] - y_range[1]) * step_increase
    data <- data %>% dplyr::mutate(
      cp_step = seq_len(dplyr::n()) - 1,
      y = y_position + y_step_increase * cp_step, yend = y
    )

    return(data)
  }
)


#' @export
GeomCompare <- ggproto("GeomCompare", Geom,
  required_aes = c("x", "y", "label"),
  default_aes = aes(
    colour = "black",
    linewidth = .5, linetype = 1, alpha = NA,
    size = 3.88, angle = 0, hjust = 0.5, vjust = -0.1,
    family = "", fontface = 1, lineheight = 1
  ),
  draw_group = function(data, panel_params, coord, cp_label,
                        ns_lineheight_just, tip_length, ...) {
    # ns lineheight justify
    if ("psymbol" %in% cp_label) {
      data <- data %>% dplyr::mutate(
        lineheight =
          ifelse(p >= 0.05, lineheight + ns_lineheight_just, lineheight)
      )
    }
    # tip
    y_range <- panel_params$y.range
    tip_left <- data %>%
      dplyr::mutate(xend = x, yend = y - tip_length * (y_range[2] - y_range[1]))
    tip_right <- data %>%
      dplyr::mutate(x = xend, yend = y - tip_length * (y_range[2] - y_range[1]))
    # label
    label_data <- dplyr::mutate(data, x = (x + xend) / 2)

    grid::gList(
      GeomSegment$draw_panel(data, panel_params, coord, ...),
      if (!is.na(tip_length)) {
        GeomSegment$draw_panel(tip_left, panel_params, coord, ...)
      },
      if (!is.na(tip_length)) {
        GeomSegment$draw_panel(tip_right, panel_params, coord, ...)
      },
      GeomText$draw_panel(label_data, panel_params, coord, ...)
    )
  }
)

#' add p value and fold change on a plot
#'
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to `layer()`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @param y_position y position of the brackets
#' @param step_increase the increase height for next bracket,
#' a ratio according to the whole panel height, default as 0.05
#' @param tip_length the length for tips at the ends of the brackets,
#' a ratio according to the whole panel height, default as 0.02
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
#' @param ns_symbol the symbol of non-significant, `NS` as default
#' @export
#'
#' @examples
#'
#' ggplot(data = mini_diamond, mapping = aes(x = cut, y = price)) +
#'   geom_point() +
#'   geom_describe(show_error = FALSE, color = "red") +
#'   geom_compare(
#'     cp_label = c("psymbol", "right_deno_fc"),
#'     y_position = 20000, step_increase = 0.22
#'   ) +
#'   ylim(0, 30000)
#'
geom_compare <- function(mapping = NULL, data = NULL,
                         stat = "compare", position = "identity",
                         ..., na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, y_position = NULL,
                         step_increase = 0.05,
                         tip_length = 0.02, cp_label = c("psymbol"),
                         ns_lineheight_just = 0.2,
                         ignore_ns = FALSE, fc_method = NULL,
                         comparisons = NULL, paired = FALSE,
                         alternative = "two.sided", test_method = "wilcoxon",
                         ns_symbol = "NS") {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomCompare,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, y_position = y_position, step_increase = step_increase,
      tip_length = tip_length, cp_label = cp_label, ignore_ns = ignore_ns,
      ns_lineheight_just = ns_lineheight_just, fc_method = fc_method,
      comparisons = comparisons, paired = paired, alternative = alternative,
      test_method = test_method, ns_symbol = ns_symbol, ...
    )
  )
}

#' @export
geom2trace.GeomCompare <- function(data, params, plot) {} # nolint
