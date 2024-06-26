% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clarkeTest.R
\name{nparams}
\alias{nparams}
\alias{nparams.glm}
\alias{nparams.lm}
\alias{nparams.orlm}
\alias{nparams.polr}
\alias{nparams.clm}
\alias{nparams.multinom}
\alias{nparams.negbin}
\title{Find number of parameters in model}
\usage{
nparams(model)

\method{nparams}{glm}(model)

\method{nparams}{lm}(model)

\method{nparams}{orlm}(model)

\method{nparams}{polr}(model)

\method{nparams}{clm}(model)

\method{nparams}{multinom}(model)

\method{nparams}{negbin}(model)
}
\arguments{
\item{model}{A statistical model object.}
}
\value{
A scalar giving the number of parameters estimated in the model.
}
\description{
Finds the number of parameters that were estimated in each
model.
}
\details{
The function funds the number of parameters generally by counting
the number of estimated parameters in the model's output.

For the purposes of the \code{clarke_test} function, the \code{nparams}
functions are not intended to be called directly by the user.
}
