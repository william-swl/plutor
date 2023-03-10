% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_pl}
\alias{theme_pl}
\title{a new extensible theme}
\usage{
theme_pl(
  base_size = 10,
  size_scales = c(5, 5, 6, 8, 10, 10),
  base_lw = 1,
  margin_factor = 0.5,
  plot_margin_factor = 1.2,
  legend_spacing_factor = 1.2,
  font_family = "",
  ...
)
}
\arguments{
\item{base_size}{base size of fonts and margins}

\item{size_scales}{a vector of element size scales, namely:
\enumerate{
\item base size, used by legend text, axis text, caption
\item used by annotation labels
\item used by legend title, axis title, strip text
\item used by subtitle
\item used by title
\item used by tag
}}

\item{base_lw}{base line width}

\item{margin_factor}{the factor of margin to size, default is 0.25}

\item{plot_margin_factor}{factor to adjust the plot margins, default is 1.2}

\item{legend_spacing_factor}{factor to adjust the space of legend items,
default is 1.2}

\item{font_family}{font family}

\item{...}{}
}
\value{
theme object of ggplotusethis::use_version()
}
\description{
a new extensible theme
}
\examples{
ggplot(mini_diamond, aes(x=x, y=y, color=clarity)) +
geom_point(size=2 + facet_grid(.~cut) +
labs(title='title', tag='tag', caption='caption') +
theme_pl()

}
