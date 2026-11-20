## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.height = 5,
  fig.width = 5,
  fig.align = "center",
  out.width = "70%"
)
options(digits = 4)
options(rgl.useNULL = TRUE)   # rgl: draw off-screen, embed with rglwidget()
library(genridge)

## ----longley-data-------------------------------------------------------------
data(longley)
str(longley)

## ----longley-ridge------------------------------------------------------------
lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08)
lform <- Employed ~ GNP + Unemployed + Armed.Forces + Population + Year + GNP.deflator
lridge <- ridge(lform, data = longley, lambda = lambda)
lridge

## ----longley-interfaces-------------------------------------------------------
lridge2 <- ridge(longley$Employed, data.matrix(longley[, c(2:6, 1)]), lambda = lambda)
longley.lm <- lm(lform, data = longley)
lridge3 <- ridge(longley.lm, lambda = lambda)
all.equal(coef(lridge), coef(lridge3))

## ----longley-df---------------------------------------------------------------
lridge.df <- ridge(lform, data = longley, df = c(6, 5.5, 5, 4.5, 4))
lridge.df$lambda

## ----longley-best-------------------------------------------------------------
best(lridge)

## ----longley-vif--------------------------------------------------------------
vridge <- vif(lridge)
vridge

## ----longley-vif-plot---------------------------------------------------------
clr <- c("black", "red", "darkgreen", "blue", "cyan4", "magenta")
pch <- c(15:18, 7, 9)
plot(vridge, X = "df", Y = "sqrt", col = clr, pch = pch, cex = 1.2, xlim = c(4, 6.5))

## ----longley-traceplot--------------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.1)
traceplot(lridge, xlim = c(-0.02, 0.08))

## ----longley-traceplot-df-----------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.1)
traceplot(lridge, X = "df", xlim = c(4, 6.5))

## ----longley-plot-------------------------------------------------------------
op <- par(mfrow = c(2, 2), mar = c(4, 4, 1, 1) + 0.1)
lambdaf <- c(expression(~widehat(beta)^OLS), ".005", ".01", ".02", ".04", ".08")
for (i in 2:5) {
  plot(lridge, variables = c(1, i),
       radius = 0.5, cex.lab = 1.5, col = clr,
       labels = NULL, fill = TRUE, fill.alpha = 0.2)
  text(lridge$coef[1, 1], lridge$coef[1, i],
       expression(~widehat(beta)^OLS), cex = 1.5, pos = 4, offset = .1)
  text(lridge$coef[-1, c(1, i)], lambdaf[-1], pos = 3, cex = 1.3)
}
par(op)

## ----longley-pairs------------------------------------------------------------
pairs(lridge, radius = 0.5, diag.cex = 2, fill = TRUE, fill.alpha = 0.1)

## ----longley-plot3d-----------------------------------------------------------
plot3d(lridge, variables = c("GNP", "Year", "Population"), radius = 0.5)
rgl::rglwidget(width = 700, height = 650)

## ----longley-precision--------------------------------------------------------
pridge <- precision(lridge)
pridge

## ----longley-precision-plot---------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.2)
plot(pridge, labels = "df", label.prefix = "df:", criteria = lridge$criteria)

## ----longley-pca--------------------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.1)
plridge <- pca(lridge)
traceplot(plridge)

## ----longley-pca-pairs--------------------------------------------------------
pairs(plridge)

## ----longley-biplot-----------------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.2)
biplot(plridge, radius = 0.5, ref = FALSE, asp = 1,
       var.cex = 1.15, cex.lab = 1.3, col = clr,
       fill = TRUE, fill.alpha = 0.2, prefix = "Dimension ")
text(plridge$coef[, 5:6], lambdaf, pos = 2, cex = 1.3)

## ----detroit------------------------------------------------------------------
data(Detroit)
Det <- as.data.frame(scale(Detroit[, c(1, 2, 4, 6, 7, 11)]))
Det <- cbind(Det, Homicide = Detroit[, "Homicide"])
dridge <- ridge(Homicide ~ ., data = Det, df = seq(6, 4, -0.5))
vif(dridge)

## ----detroit-traceplot--------------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.1)
traceplot(dridge, X = "df")

## ----detroit-pairs------------------------------------------------------------
pairs(dridge, radius = 0.5)

## ----detroit-pca--------------------------------------------------------------
par(mar = c(4, 4, 1, 1) + 0.1)
dpridge <- pca(dridge)
traceplot(dpridge, X = "df")

