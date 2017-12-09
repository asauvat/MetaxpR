#' Correlation matrix
#'
#' Slightly modified version of Chart.Correlation from PerformanceAnalytics package
#'
#'
#' @param R data set 
#' @param histogram shall histogram be depicted on diagonal?
#' @param method method argument from cor function
#'
#' @return a correlation matrix plot
#'
#' @author Allan Sauvat, \email{allan.sauvat@gustaveroussy.fr}
#'
#' @export
#'

cor.matrix = function (R, histogram = TRUE, method = c("pearson", "kendall", 
                                          "spearman"),...) 
{
  x = as.matrix(R)
  if (missing(method)) 
    method = method[1]
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      #cex <- 0.8/strwidth(txt)
    cex <- 1
    test <- cor.test(x, y, method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    #text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.5, 0.5, txt, cex = cex)
    text(0.8, 0.8, Signif, cex = cex+0.2, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  hist.panel = function(x, ...) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = 100)
    #lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  
  mypanel.smooth = function(x,y,...){
    #panel.smooth(x,y,cex=0.5,...)
    points(x,y,cex=0.5,...)
    abline(lm(x~y), col="red")
  }
  
  if (histogram) 
    pairs(x, gap = 0, lower.panel = mypanel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel, method = method, ...)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
             method = method, ...)
}