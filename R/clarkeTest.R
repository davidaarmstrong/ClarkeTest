##' Print non-nested test results
##' @description Prints results of the \code{clarke_test} function.
##'
##' @param x A result from the `nonnest` function
##' @param digits Number of digits to print in the output
##' @param ... Other arguments passed down to print
##'
##' @rdname print
##' @export
##' @return Printed output that summarises the results of the
##' \code{clarke_test} function.
##' @method print nonnest.test
##'

print.nonnest.test <- function(x, digits = x$digits, ...)
{
  ## calculate p-value from test statistic
    b <- min(x$stat, x$nobs - x$stat)
    p <- 2 * pbinom(b, x$nobs, 0.5)
    pref <- if (x$stat > x$nobs - x$stat) 1 else 2

  testname <- "Clarke"
  cat("\n", testname, " test for non-nested models\n", sep = "")

  cat("\nModel 1 log-likelihood:", format(sum(x$loglik1), digits = digits))
  cat("\nModel 2 log-likelihood:", format(sum(x$loglik2), digits = digits))
  cat("\nObservations:", x$nobs)
  cat("\nTest statistic:", format(x$stat, digits = digits))
  if (x$test == "clarke")
    cat(" (", round(100* x$stat / x$nobs), "%)", sep = "")
  cat("\n")

  fp <- format.pval(p, digits = digits)  # turns very low p into "<2e-12"
  if (substr(fp, 1L, 1L) != "<") {
    fp <- paste("=", fp)
  } else if (substr(fp, 2L, 2L) != " ") {
    fp <- paste(substr(fp, 1L, 1L), substr(fp, 2L, nchar(fp)))
  }
  if (p < x$level) {
    cat("\nModel ", pref, " is preferred (p ", fp, ")\n\n", sep = "")
  } else {
    cat("\nNeither model is significantly preferred (p ", fp, ")\n\n",
        sep = "")
  }

  invisible(x)
}


#' Clarke Test
#'
#' @description
#' `clarke_test` returns results from Kevin Clarke's distribution-free test
#' of non-nested models.
#'
#' @details
#' `clarke_test` is a more modularized version of the [clarke()] function from
#' the [games] package.  The main innovation is that the `nonnest` function
#' calls a generic `indivLogLiks` function, so additional methods can be easily
#' written for different classes of models. The function
#' currently supports binomial, poisson and negative
#' binomial GLMs, ordinal models estimated with either
#' \code{polr} from the \code{MASS} package
#' or \code{clm} from the \code{ordinal}
#' package and multinomial models estimated with either
#' \code{multinom} from the \code{nnet}
#' package.  Users can also write new
#' methods for both \code{indivLogLiks} and \code{nparams}
#' that would get called by the generic function.
#'
#'
#'
##' @usage clarke_test(model1, model2, level=0.05, digits=2)
##' @param model1 A fitted statistical model of a supported class
##' @param model2 A fitted statistical model of a supported class
##'  whose dependent variable is the same as that of \code{model1}
##' @param level Numeric: significance level for the test.
##' @param digits Integer: number of digits to print
##' @references Kevin Clarke.  2007.  "A Simple Distribution-Free Test for
##' Nonnested Hypotheses."  \emph{Political Analysis} 15(3): 347--363.
##' @return Typical use will be to run the function interactively and examine
##' the printed output.  The functions return an object of class
##' \code{nonnest.test}, which is a list containing: \describe{
##' \item{\code{stat}}{The test statistic}
##' \item{\code{level}}{Significance level for the test}
##' \item{\code{digits}}{Number of digits to print}
##' \item{\code{loglik1}}{Vector of observationwise log-likelihoods for
##' \code{model1}}
##' \item{\code{loglik2}}{Vector of observationwise log-likelihoods for
##' \code{model2}}
##' \item{\code{nparams}}{Integer vector containing the number of parameters
##' fitted in \code{model1} and \code{model2} respectively}
##' \item{\code{nobs}}{Number of observations of the dependent variable being
##' modeled}}
##' @returns An object of class \code{nonnest.test} with the following values:
##' \describe{
##' \item{stat}{The number of times model 1 is better than model 2}
##' \item{test}{Will always be "clarke".}
##' \item{level}{The chosen confidence level for the test}
##' \item{digits}{The number of digits to print}
##' \item{loglik1}{Individual log-likelihoods for model 1}
##' \item{loglik2}{Individual log-likelihoods for model 2}
##' \item{nparams}{A vector giving the number of parameters in models 1 and 2,
##' respectively}
##' \item{nobs}{Number of observations in the model}
##' }
##'
##' @examples
##' data(conflictData)
##' ## Linear Model
##' lm1 <- lm(riots ~ log(rgdpna_pc) + log(pop*1000) +
##'     polity2, data=conflictData)
##' lm2 <- lm(riots ~ rgdpna_pc + pop +
##'     polity2, data=conflictData)
##' clarke_test(lm1, lm2)
##'
##' ## Binomial GLM
##' glm1 <- glm(conflict_binary ~ log(rgdpna_pc) +
##'           log(pop*1000) + polity2, data=conflictData,
##'           family=binomial)
##' glm2 <- glm(conflict_binary ~ rgdpna_pc + pop +
##'           polity2, data=conflictData,
##'           family=binomial)
##' clarke_test(glm1, glm2)
##'
##' ## Poisson GLM
##' glm1a <- glm(riots ~ log(rgdpna_pc) +
##'               log(pop*1000) + polity2,
##'              data=conflictData,
##'              family=poisson)
##' glm2a <- glm(riots ~ rgdpna_pc + pop +
##'               polity2, data=conflictData,
##'             family=poisson)
##' clarke_test(glm1a, glm2a)
##'
##' ## Negative Binomial GLM
##' library(MASS)
##' glm1b <- glm.nb(riots ~ log(rgdpna_pc) +
##'                log(pop*1000) + polity2,
##'                data=conflictData)
##' glm2b <- glm.nb(riots ~ rgdpna_pc + pop +
##'                polity2, data=conflictData)
##' clarke_test(glm1b, glm2b)
##'
##' ## Ordered Logit: polr
##' library(MASS)
##' ol1 <- polr(as.factor(Amnesty) ~ log(rgdpna_pc) +
##'                   log(pop*1000) + polity2,
##'                 data=conflictData)
##' ol2 <- polr(as.factor(Amnesty) ~ scale(rgdpna_pc) +
##'             scale(pop) + polity2,
##'             data=conflictData)
##' clarke_test(ol1, ol2)
##'
##' ## Ordered Logit: clm
##' library(ordinal)
##' ol1a <- clm(as.factor(Amnesty) ~ log(rgdpna_pc) +
##'               log(pop*1000) + polity2,
##'             data=conflictData)
##' ol2a <- clm(as.factor(Amnesty) ~ scale(rgdpna_pc) +
##'             scale(pop) + polity2,
##'             data=conflictData)
##' clarke_test(ol1a, ol2a)
##'
##' ## Multinomial Logit: multinom
##'
##' library(nnet)
##' ml1 <- multinom(as.factor(Amnesty) ~ log(rgdpna_pc) +
##'               log(pop*1000) + polity2,
##'             data=conflictData)
##' ml2 <- multinom(as.factor(Amnesty) ~ scale(rgdpna_pc) +
##'               scale(pop) + polity2,
##'             data=conflictData)
##' clarke_test(ml1, ml2)
##'
##'
##' ## Multinomial Logit: multinom
##'
##'
##' @export
##' @author Brenton Kenkel (\email{brenton.kenkel@@gmail.com}) modified by
##' Dave Armstrong (\email{dave@@quantoid.net})


clarke_test <- function(model1, model2, level = 0.05, digits = 2){

##' @importFrom stats coef dnbinom dnorm dpois family fitted.values hatvalues
##' logLik model.frame model.response nobs pbinom pnorm residuals var weights
##' fitted predict

    x <- nonnest(model1, model2)

  correction <- (x$p1 - x$p2) * (log(x$n) / (2*x$n))
  stat <- sum(x$loglik1 - x$loglik2 > correction)

  ans <- list(stat = stat,
              test = "clarke",
              level = level,
              digits = digits,
              loglik1 = x$loglik1,
              loglik2 = x$loglik2,
              nparams = c(x$p1, x$p2),
              nobs = x$n)
  class(ans) <- "nonnest.test"

  return(ans)
}

nonnest <- function(model1, model2){

    n <- nobs(model1)
    if (nobs(model2) != n)
      stop("model1 and model2 have different numbers of observations")

    y1 <- try(model.response(model.frame(model1)))
    if(inherits(y1, "try-error")){
      y1 <- model1$y
      if(is.null(y1)){
        stop("must be able to extract y with model.response(model.frame(obj)) or obj$y\n")
      }
    }
    y2 <- try(model.response(model.frame(model1)))
    if(inherits(y2, "try-error")){
      y2 <- model1$y
      if(is.null(y2)){
        stop("must be able to extract y with model.response(model.frame(obj)) or obj$y\n")
      }
    }

    ## check for equality of dependent variables
    if (!all.equal(y1, y2, check.attributes = FALSE))
      stop("models do not have same dependent variable")

    anyWeights <- function(x) !is.null(weights(x)) && !all(weights(x)==1)
    if (anyWeights(model1) || anyWeights(model2))
      stop("'clarke_test' does not yet support models with weights")

    loglik1 <- indivLogLiks(model1)
    loglik2 <- indivLogLiks(model2)
    p1 <- nparams(model1)
    p2 <- nparams(model2)

    return(list(n = n, loglik1 = loglik1, loglik2 = loglik2, p1 = p1, p2 = p2))
  }


##
## INPUT:
## model: fitted statistical model of class "lm", or "glm"
## outcome: optional, column of predict(model) to focus on, only when 'model' is
## of class "game"
##
## RETURN:
## vector of observationwise log-likelihoods from 'model'
##


##' Calculate individual log-likelihood values
##'
##' @param model A statistical model object.
##'
##' @details The \code{indivLogLiks} function calls
##' the appropriate method for calculating individual
##' log likelihood values for the model.  The function
##' currently supports binomial, poisson and negative
##' binomial GLMs, ordinal models estimated with either
##' \code{polr} from the \code{MASS} package
##' or \code{clm} from the \code{ordinal}
##' package and multinomial models estimated with either
##' \code{multinom} from the \code{nnet}
##' package.  Users can also write new
##' methods for both \code{indivLogLiks} and \code{nparams}
##' that would get called by the generic function.
##'
##' For the purposes of the \code{clarke_test} function, the \code{indivLogLiks}
##' functions are not intended to be called directly by the user.
##'
##' @return A vector of individual log-likelihood values for the model.
##'
##' @rdname indivLogLiks
##' @export

indivLogLiks <- function(model){
  UseMethod("indivLogLiks")
}

##' @rdname indivLogLiks
##' @method indivLogLiks glm
##' @export
indivLogLiks.glm <- function(model)
{
  ## get weights (only relevant for lm and glm models)
  if(!(family(model)$family %in% c("binomial", "poisson", "gaussian")))
    stop("Only gaussian, binomial and poisson families currently supported")
  if(family(model)$family == "binomial"){
    ans <- ll_fun.binomial(model)
  }
  if(family(model)$family == "poisson"){
    ans <- ll_fun.poisson(model)
  }
  if(family(model)$family == "gaussian"){
    ans <- ll_fun.gaussian(model)
  }
  return(ans)
}

##' @rdname indivLogLiks
##' @export
##' @method indivLogLiks lm
indivLogLiks.lm <- function(model){
  ans <- ll_fun.gaussian(model)
  return(ans)
}

##' @rdname indivLogLiks
##' @export
##' @method indivLogLiks orlm
indivLogLiks.orlm <- function(model){
  ans <- ll_fun.orlm(model)
  return(ans)
}


##' @rdname indivLogLiks
##' @export
##' @method indivLogLiks polr
indivLogLiks.polr <- function(model){
  y <- as.numeric(model.response(model.frame(model)))
  probs <- predict(model, type="probs")
  probs <- probs[cbind(1:length(y), y)]
  ans <- log(probs)
  return(ans)
}

##' @rdname indivLogLiks
##' @export
##' @method indivLogLiks clm
indivLogLiks.clm <- function(model){
  probs <- predict(model, type="prob")$fit
  ans <- log(probs)
  return(ans)
}

##' @rdname indivLogLiks
##' @export
##' @method indivLogLiks multinom
indivLogLiks.multinom <- function(model){
  y <- as.numeric(model.response(model.frame(model)))
  probs <- predict(model, type="probs")
  probs <- probs[cbind(1:length(y), y)]
  ans <- log(probs)
  return(ans)
}

# ##' @rdname indivLogLiks
# ##' @export
# ##' @method indivLogLiks mlogit
# indivLogLiks.mlogit <- function(model){
#   probs <- fitted(model)
#   ans <- log(probs)
#   return(ans)
# }

##' @rdname indivLogLiks
##' @export
##' @method indivLogLiks negbin
indivLogLiks.negbin <- function(model){
  y <- model.response(model.frame(model))
  yhat <- fitted(model)
  probs <- dnbinom(y, size=model$theta, mu=yhat)
  ans <- log(probs)
  return(ans)
}


ll_fun.binomial <- function(model){
  y <- model.response(model.frame(model))
  probs <- ifelse(y == 1, fitted.values(model), 1-fitted.values(model))
  log(probs)
}

ll_fun.poisson <- function(model){
  y <- model.response(model.frame(model))
  probs <- dpois(y, fitted.values(model))
  log(probs)
}

ll_fun.gaussian <- function(model){
  y <- model.response(model.frame(model))
  res <- residuals(model)
  npar <- length(coef(model))
  sigma <- sqrt(var(res)*((nobs(model)-1)/(nobs(model)-npar)))
  probs <- dnorm(y, fitted.values(model), sigma)
  log(probs)
}

ll_fun.orlm <- function(obj){
  y <- obj$y
  res <- obj$residuals
  sigma <- c(var(res) * (length(y) - 1)/(length(y) - length(obj$b.restr)))
  dnorm(y, obj$fitted.values, sqrt(sigma), log=TRUE)
}



##' Find number of parameters in model
##'
##' @description Finds the number of parameters that were estimated in each
##' model.
##' @param model A statistical model object.
##'
##' @details The function funds the number of parameters generally by counting
##' the number of estimated parameters in the model's output.
##'
##' For the purposes of the \code{clarke_test} function, the \code{nparams}
##' functions are not intended to be called directly by the user.
##'
##' @return A scalar giving the number of parameters estimated in the model.
##' @rdname nparams
##' @export

nparams <- function(model){
  UseMethod("nparams")
}

##' @rdname nparams
##' @method nparams glm
##' @export

nparams.glm <- function(model){
  attr(logLik(model), "df")
}

##' @rdname nparams
##' @method nparams lm
##' @export
nparams.lm <- function(model){
 sum(hatvalues(model))
}

##' @rdname nparams
##' @method nparams orlm
##' @export
nparams.orlm <- function(model){
  length(model$b.restr)
}

##' @rdname nparams
##' @method nparams polr
##' @export
nparams.polr <- function(model){
  length(coef(model)) + length(model$zeta)
}

##' @rdname nparams
##' @method nparams clm
##' @export
nparams.clm <- function(model){
  length(coef(model))
}

##' @rdname nparams
##' @method nparams multinom
##' @export
nparams.multinom <- function(model){
  length(c(coef(model)))
}

# ##' @rdname nparams
# ##' @method nparams mlogit
# ##' @export
# nparams.mlogit <- function(model){
#   length(coef(model))
# }

##' @rdname nparams
##' @method nparams negbin
##' @export
nparams.negbin <- function(model){
  length(coef(model)) + 1
}

##' @export
##' @method nobs multinom
nobs.multinom <- function(object, ...){
  length(object$weights)
}

##' @export
##' @method nobs orlm
nobs.orlm <- function(object, ...){
  length(object$y)
}


# ##' @method nobs mlogit
# nobs.mlogit <- function(object, ...){
#   length(object$fitted.values)
# }
