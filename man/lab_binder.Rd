% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab_binder.R
\name{lab_binder}
\alias{lab_binder}
\title{Lab excel files binder}
\usage{
lab_binder(
  hospital,
  ptlist_file,
  lab_dir_path,
  lab_info_file = NULL,
  clean = TRUE
)
}
\arguments{
\item{hospital}{Select the hospital: seoul, bucheon, or cheonan.}

\item{ptlist_file}{Patient list file which included "id" and "orderdate" column.}

\item{lab_dir_path}{Directory path included excel files.}

\item{lab_info_file}{Path of information file.}

\item{clean}{Make a cleaned data or not.}
}
\description{
This function tie multiple excel files containing laboratory data to one file.
}
\keyword{lab_binder}
