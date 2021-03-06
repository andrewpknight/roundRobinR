% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/srmRun.R
\name{srmRun}
\alias{srmRun}
\title{Run the Social Relations Model using Multilevel Modeling}
\usage{
srmRun(dv, groupId, actId, partId, feVars = NULL, data)
}
\arguments{
\item{dv}{string giving the name of the directed dyadic criterion variable}

\item{groupId}{string giving the name of the group identifier variable}

\item{actId}{string giving the name of the actor identifier variable}

\item{partId}{string giving the name of the partner identifier variable}

\item{feVars}{vector containing the names of fixed effect variables}

\item{data}{data.frame at the directed dyad level}
}
\value{
a list with two items:
\itemize{
\item lme.output is the full lme output object
\item srm.output is the variance decomposition with reciprocity correlations
}
}
\description{
This is a wrapper function to efficiently run the social relations model
with a basic directed dyad-level dataset. The function will create the
necessary dummy variables and build out the correct formula for lme.
}
\examples{
o = srmRun(dv="liking", groupId="groupId", actId="actId", partId="partId",
feVars=c("actEx", "partEx", "contact"),
data=sampleDyadData[sampleDyadData$timeId==1, ])
}
