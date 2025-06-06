% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launchDarwinDashboardApp.R
\name{launchDarwinDashboardApp}
\alias{launchDarwinDashboardApp}
\title{launchDarwinDashboardApp}
\usage{
launchDarwinDashboardApp(appStructure, title = NULL, async = FALSE)
}
\arguments{
\item{appStructure}{(\code{list(list())}) A list of named lists, containing modules.
The level of nesting groups or separates modules in menu items \code{"_"} will be read as a space.}

\item{title}{(\code{character(1)}: \code{NULL}) Title of the app}

\item{async}{(\code{logical(1)}: \code{FALSE}) Run app asynchronously}
}
\value{
\code{NULL}
}
\description{
launchDarwinDashboardApp
}
\examples{
library(DarwinShinyModules)

base <- Text$new("**base**")
nested_a <- Text$new("**nested A**")
nested_b <- Text$new("**nested B**")
sub_a <- Text$new("**sub A**")
sub_b <- Text$new("**sub B**")
comb_a <- Text$new("**comb A**")
comb_b <- Text$new("**comb B**")
comb_c <- Text$new("**comb C**")

if (interactive()) {
  appStructure <- list(
    base = base,
    nested = list(nested_a, nested_b),
    nested_sub = list(
      sub_a = sub_a,
      sub_b = sub_b
    ),
    nested_combined = list(
      comb_a_b = list(comb_a, comb_b),
      comb_c = comb_c
    )
  )

  launchDarwinDashboardApp(appStructure)
}
}
