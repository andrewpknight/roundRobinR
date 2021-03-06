% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdConstruct.pdSRM.R
\name{pdConstruct.pdSRM}
\alias{pdConstruct.pdSRM}
\title{Construct pdSRM Object}
\usage{
\method{pdConstruct}{pdSRM}(
  object,
  value = numeric(0),
  form = stats::formula(object),
  nam = nlme::Names(object),
  data = sys.frame(sys.parent()),
  ...
)
}
\arguments{
\item{object}{an object inheriting from pdSRM}

\item{value}{an optional initialization value}

\item{form}{an optional one-sided linear formula}

\item{nam}{an optional vector of character strings}

\item{data}{an optional data frame in which to evaluate the variables}

\item{...}{optional arguments for some methods}
}
\value{
a pdSRM object representing a positive-definite matrix
}
\description{
This function is used internally as part of the construction of the pdSRM
object that will fit the appropriate structure for the SRM.
}
\examples{
\dontrun{
o = lme(liking ~ 1, random=list(groupId=pdBlocked(list(pdIdent(~1),
pdSRM(~-1 + a1 + a2 + a3 + a4 + p1 + p2 + p3 + p4)))),
correlation=corCompSymm(form=~1 | groupId/pdSRM_dyad_id),
data=d, na.action=na.omit)
}
}
