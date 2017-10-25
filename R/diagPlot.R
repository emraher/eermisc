# From https://rstudio-pubs-static.s3.amazonaws.com/43190_f9bd428cf5b94562a1b46e00b454e05a.html
#' My diagplot
#' @description My diagplot
#' @param model an lm model
#' @return a list of ggplots
#' @author Emrah Er
#' @examples
#'
#' @export diagPlot

require(ggplot2)
require(broom)

diagPlot <- function(model) {
  p1 <- ggplot(broom::augment(model), aes(.fitted, .resid)) + geom_point()
  p1 <- p1 + stat_smooth(method = "loess") + geom_hline(yintercept = 0, col = "red", linetype = "dashed")
  p1 <- p1 + xlab("Fitted values") + ylab("Residuals")
  p1 <- p1 + ggtitle("Residual vs Fitted Plot") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  p2 <- ggplot(broom::augment(model), aes(qqnorm(.std.resid)[[1]], .std.resid)) + geom_point(na.rm  =  TRUE)
  p2 <- p2 + xlab("Theoretical Quantiles") + ylab("Standardized Residuals")
  p2 <- p2 + ggtitle("Normal Q-Q") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  p3 <- ggplot(broom::augment(model), aes(.fitted, sqrt(abs(.std.resid)))) + geom_point(na.rm = TRUE)
  p3 <- p3 + stat_smooth(method = "loess", na.rm  =  TRUE) + xlab("Fitted Value")
  p3 <- p3 + ylab(expression(sqrt("|Standardized residuals|")))
  p3 <- p3 + ggtitle("Scale-Location") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  p4 <- ggplot(broom::augment(model), aes(seq_along(.cooksd), .cooksd)) + geom_bar(stat = "identity", position = "identity")
  p4 <- p4 + xlab("Obs. Number") + ylab("Cook's distance")
  p4 <- p4 + ggtitle("Cook's distance") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  p5 <- ggplot(broom::augment(model), aes(.hat, .std.resid)) + geom_point(aes(size = .cooksd), na.rm = TRUE)
  p5 <- p5 + stat_smooth(method = "loess", na.rm = TRUE)
  p5 <- p5 + xlab("Leverage") + ylab("Standardized Residuals")
  p5 <- p5 + ggtitle("Residual vs Leverage Plot")
  p5 <- p5 + scale_size_continuous("Cook's Distance", range = c(1,5))
  p5 <- p5 + theme_bw() + theme(legend.position = "bottom") + theme(plot.title = element_text(hjust = 0.5))

  p6 <- ggplot(broom::augment(model), aes(.hat, .cooksd)) + geom_point(na.rm = TRUE) + stat_smooth(method = "loess", na.rm = TRUE)
  p6 <- p6 + xlab("Leverage hii") + ylab("Cook's Distance")
  p6 <- p6 + ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6 <- p6 + geom_abline(slope = seq(0,3,0.5), color = "gray", linetype = "dashed")
  p6 <- p6 + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

  return(list(rvfPlot = p1, qqPlot = p2, sclLocPlot = p3, cdPlot = p4, rvlevPlot = p5, cvlPlot = p6))
}
