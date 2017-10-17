# ReadAxfBOM

[![Build Status](https://travis-ci.org/MarkusLoew/MyFits.svg?branch=master)](https://travis-ci.org/MarkusLoew/MyFits)

R package to add either a inverse-quadratic, bell-shaped, or "waiting-in-line" curve fit to data.

See 

	help(package = "MyFits") 

for details on the function provided by this package.
The import functions returns either a list of curve fit coefficients and r2, or the nls-model, or the predicted values.


### Installation

Installation straight from github (if package "devtools" is already installed) via

```{r}
devtools::install_github("MarkusLoew/MyFits")
```

### Example session
```{r}
d <- data.frame(x = 1:10,
                 y = c(0.5, 2, 10, 9, 8, 6.5, 5, 4, 3, 2.5))
MyFits(x = x, y = y, data = d)
MyFits(x = x, y = y, data = d, fit = "Wait")
nls.fit <- MyFits(x = x, y = y, data = d, fit = "Bell", model = TRUE)
my.x <- seq(min(d$x), max(d$x), length.out = 100)
my.predicted <- predict(nls.fit, data.frame(x = my.x))
mod.pred <- data.frame(x = my.x,
                       y = my.predicted)
plot(d)
lines(y ~ d, data = mod.pred)

require(ggplot2)
p <- ggplot(d, aes(x = x, y = y))
 p <- p + geom_point()
 p <- p + stat_myfits(fit = "Inverse_quad", colour = "blue")
 p <- p + stat_myfits(fit = "Bell", colour = "red")
 p <- p + stat_myfits(fit = "Wait", colour = "green")
p 
```
