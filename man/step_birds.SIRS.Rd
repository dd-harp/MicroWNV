% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birds_SIRS.R
\name{step_birds.SIRS}
\alias{step_birds.SIRS}
\title{Update SIRS bird population}
\usage{
\method{step_birds}{SIRS}(model)
}
\arguments{
\item{model}{an object from \link[MicroMoB:make_MicroMoB]{MicroMoB::make_MicroMoB}}
}
\description{
This function dispatches on the second argument of \code{model$bird}
for stochastic or deterministic behavior.
}
\details{
see \link{step_birds.SIRS_deterministic} and \link{step_birds.SIRS_stochastic}
}
