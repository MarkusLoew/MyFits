#' Use MyFits to add a curve fits to a ggplot2 object.
#' 
#' @description Calculates either an inverse quadratic, a bell-shaped or a waiting-in-line fit of data as a \code{ggplot2} layer. Calculated using \code{MyFits::MyFits}.
#' @param fit Name of the fit-equation to be used. Either \code{Inverse_quad} for an inverse quadratic equation \code{y ~ x / (a + b*x + c*x^2)}, \code{Bell} for a bell shaped curve \code{y ~ amp * exp(-0.5*((x - xmean)/xsd)^2)}, or \code{Wait} for waiting-in-line \code{y ~ A * kw * x * exp(-kw*x)}. See \code{MyFits}.
# '@inheritParams StatMyFits
#' @inheritParams ggplot2::layer
#' @seealso  \code{MyFits}, \code{ggplot2}
#' @examples
#' d <- data.frame(x = 1:10,
#'                 y = c(0.5, 2, 10, 9, 8, 6.5, 5, 4, 3, 2.5))
#' require(ggplot2)
#' p <- ggplot(d, aes(x = x, y = y))
#'  p <- p + geom_point()
#'  p <- p + stat_myfits(fit = "Inverse_quad", colour = "blue")
#'  p <- p + stat_myfits(fit = "Bell", colour = "red")
#'  p <- p + stat_myfits(fit = "Wait", colour = "green")
#' p 
#' 
#' @export stat_myfits

stat_myfits <- function(mapping = NULL, data = NULL, geom = "path",
                        position = "identity", na.rm = FALSE, show.legend = NA, 
                        inherit.aes = TRUE, fit,...) {
  ggplot2::layer(
    stat = StatMyFits, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, fit = fit,...)
  )
}

StatMyFits <- ggplot2::ggproto("StatMyFits", ggplot2::Stat, 
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, fit = fit) {
    MyFits(x = data$x, y = data$y, data = data, fit,
           model = TRUE, coords = TRUE)
  }
)

