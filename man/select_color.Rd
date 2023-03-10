% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color.R
\name{select_color}
\alias{select_color}
\title{select most distant colors among a color spectrum}
\usage{
select_color(n, color_set = base_color, type = "munsell")
}
\arguments{
\item{n}{number of colors}

\item{color_set}{selection from}

\item{type}{color_set type, 'munsell' as default}
}
\value{
hex value of selected colors
}
\description{
select most distant colors among a color spectrum
}
\examples{
select_color(5)
}
