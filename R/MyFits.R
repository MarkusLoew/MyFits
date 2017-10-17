#' Fits either an inverse quadratic, a bell-shaped or a waiting-in-line curve to data.
#' @description Fits one of three curves to x, y data using \code{nls}. Available fitting functions are inverse quadratic, bell-shaped, or waiting-in-line.
#' @param x x values
#' @param y y values 
#' @param data data frame with the x and y values
#' @param fit Name of the fit equation to be used. Either \code{Inverse_quad} for an inverse quadratic equation \code{y ~ x / (a + b*x + c*x^2)}, \code{Bell} for a bell shaped curve \code{y ~ amp * exp(-0.5*((x - xmean)/xsd)^2)}, or \code{Wait} for waiting-in-line \code{y ~ A * kw * x * exp(-kw*x)}, after Ritchie 2008).
#' @param model Logical. Should the \code{nls} model be returned? Default is FALSE, only coefficient and R2 are returned.
#' @param coords Logical. If \code{TRUE} the model predictions are returned as data frame. Usful for plotting the model result. See \code{stat_MyFits} for a ggplot2-implementation.
#' @param start.a optional initial guess for the curve fit coefficients \code{a}, \code{amp}, or \code{A} (depends on which equation was selected). Default is NA, i.e. an internal guess is used to determine \code{start.a}.
#' @param start.b optional initial guess for the second curve fit coefficient for the \code{b}, \code{xmean} or \code{kw} parameter. Default is NA, i.e. an internal guess is used to determine \code{start.b}.
#' @param start.c optional initial guess for the third curve fit coefficient. Either \code{c} in case of \code{Inverse_quad} or \code{xsd} in case of \code{Bell}. Not used for \code{Wait} as it only uses two coefficients. Default is NA, i.e. an internal guess is used to determine \code{start.c}.
#' @seealso \code{nls}
#' @examples
#' d <- data.frame(x = 1:10,
#'                 y = c(0.5, 2, 10, 9, 8, 6.5, 5, 4, 3, 2.5))
#' MyFits(x = x, y = y, data = d)
#' MyFits(x = x, y = y, data = d, fit = "Wait")
#' nls.fit <- MyFits(x = x, y = y, data = d, fit = "Inverse_quad", model = TRUE)
#' my.x <- seq(min(d$x), max(d$x), length.out = 100)
#' my.predicted <- predict(nls.fit, data.frame(x = my.x))
#' mod.pred <- data.frame(x = my.x,
#'                        y = my.predicted)
#' plot(d)
#' lines(y ~ x, data = mod.pred)
#' @export


MyFits <- function(x, y, data, 
                   fit = "Inverse_quad",
                   model = FALSE, coords = FALSE,
                   start.a = NA, start.b = NA, start.c = NA) {

  stopifnot(fit %in% c("Inverse_quad", "Bell", "Wait"))
  
  arguments <- as.list(match.call())
  x <- eval(arguments$x, data)
  y <- eval(arguments$y, data)
  
  # make sure y, y are numeric, as it could be time 
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  
  if (is.na(start.a) & is.na(start.b) & is.na(start.b)) {
    message("Determining start values")
    
    if (fit == "Inverse_quad") {
      my.eq   <- stats::formula(y ~ x / (a + b*x + c*x^2))
      start.a <- min(x[x>0], na.rm = TRUE) / 10
      start.b <- max(y,      na.rm = TRUE) 
      start.c <- 1/start.a
      start.para <- c(start.a, start.b, start.c)
      names(start.para) <- c("a", "b", "c")
    }
    
    if (fit == "Bell") {
      my.eq <- stats::formula(y ~ amp * exp(-0.5*((x - xmean)/xsd)^2))
      start.amp   <- max(y,  na.rm = TRUE) 
      start.xmean <- mean(x, na.rm = TRUE)
      start.xsd   <- stats::sd(x,   na.rm = TRUE)
      start.para  <- c(start.amp, start.xmean, start.xsd)
      names(start.para) <- c("amp", "xmean", "xsd")
    }
    
    if (fit == "Wait") {
      my.eq    <- stats::formula(y ~ A * kw * x * exp(-kw*x))
      start.A  <- max(y, na.rm = TRUE) # scaling constant for height of curve
      # find x-value of maximum
      x.max      <- x[which.max(y)]
      start.kw   <- 1/x.max # location of saturation
      start.para <- c(start.A, start.kw)
      names(start.para) <- c("A", "kw")
    }
    
  } else {
    message("Using provided start values")
    if (fit == "Inverse_quad") {
      start.a <- start.a
      start.b <- start.b
      start.c <- start.b
      start.para <- c(start.a, start.b, start.c)
      names(start.para) <- c("a", "b", "c")
    }
    
    if (fit == "Bell") {
      start.amp   <- start.a
      start.xmean <- start.b
      start.xsd   <- start.c
      start.para  <- c(start.amp, start.xmean, start.xsd)
      names(start.para) <- c("amp", "xmean", "xsd")
    }
    
    if (fit == "Wait") {
      start.A  <- start.a
      start.kw <- start.b
      start.para <- c(start.A, start.kw)
      names(start.para) <- c("A", "kw")
    }
  }
    
  the.names <- c(names(start.para), "R2")
  
  nls.fit <- try(stats::nls(my.eq,
                     data = data,
                     trace = FALSE, 
                     start = start.para,
                     control = stats::nls.control(minFactor = 1/40000, 
                                           maxiter = 8000)))

  if(inherits(nls.fit, "try-error")) {
     out <- rep(NA, length(the.names))
     names(out) <- the.names
     if (model == TRUE) {
       return("No fit")
     } else {
       return(out)
     }
  } else {
   if (model == TRUE) {
     if (coords == FALSE) {
       # return the bare model
       return(nls.fit)
     } else {
      # do prediction
      my.x <- seq(min(x, na.rm = TRUE), 
                  max(x, na.rm = TRUE), 
                  length.out = length(x) * 4)
      my.predicted <- stats::predict(nls.fit, data.frame(x = my.x))
      mod.pred <- data.frame(x = my.x,
                             y = my.predicted)
      return(mod.pred)
     }
   } else {
     # provide coefficients and R2
     out <- stats::coef(nls.fit)
     r2  <- 1 - (stats::var(stats::residuals(nls.fit)) / stats::var(y, na.rm = TRUE))
     out <- c(out, r2)
     names(out) <- the.names
     return(out)
     }
  }
}

