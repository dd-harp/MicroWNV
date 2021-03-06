% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birds_interface.R
\name{compute_wfB}
\alias{compute_wfB}
\title{Compute bird biting weights (\eqn{w_{f_{B}}})}
\usage{
compute_wfB(model)
}
\arguments{
\item{model}{an object from \link[MicroMoB:make_MicroMoB]{MicroMoB::make_MicroMoB}}
}
\value{
a vector of length \code{p}
}
\description{
This method dispatches on the type of \code{model$bird}.
}
