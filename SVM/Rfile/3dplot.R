data(volcano)
# install.packages("rgl")
library(rgl)

z <- 2 * volcano        # Exaggerate the relief

x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

zlim <- range(y)
zlen <- zlim[2] - zlim[1] + 1

colorlut <- terrain.colors(zlen) # height color lookup table

col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point

surface3d(x, y, z, color = col, back = "lines") # you can also use terrain

xvec = 1:10
yvec = 3:12
f2 = function(x,y){return(x+y)}
fz <- outer(xvec, yvec, FUN=f2)
f2(xvec,yvec)

f1 <- function(x) x
curve(f1, from=-1, to=1)


install.packages('scatterplot3d')
library(scatterplot3d)

z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue",
              col.grid="lightblue", main="scatterplot3d - 1", pch=20)

#example 3
temp <- seq(-pi, 0, length = 50)
x <- c(rep(1, 50) %*% t(cos(temp)))
y <- c(cos(temp) %*% t(sin(temp)))
z <- c(sin(temp) %*% t(sin(temp)))
scatterplot3d(x, y, z, highlight.3d=TRUE,
              col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=20)

#example 4
my.mat <- matrix(runif(25), nrow=5)
dimnames(my.mat) <- list(LETTERS[1:5], letters[11:15])
my.mat # the matrix we want to plot ...
s3d.dat <- data.frame(cols=as.vector(col(my.mat)),
                      rows=as.vector(row(my.mat)),
                      value=as.vector(my.mat))
scatterplot3d(s3d.dat, type="h", lwd=5, pch=" ",
              x.ticklabs=colnames(my.mat), y.ticklabs=rownames(my.mat),
              color=grey(25:1/40), main="scatterplot3d - 4")


#example 5
data(trees)
s3d <- scatterplot3d(trees, type="h", highlight.3d=TRUE,
                     angle=55, scale.y=0.7, pch=16, main="scatterplot3d - 5")
# Now adding some points to the "scatterplot3d"
s3d$points3d(seq(10,20,2), seq(85,60,-5), seq(60,10,-10),
             col="blue", type="h", pch=16)
# Now adding a regression plane to the "scatterplot3d"
attach(trees)
my.lm <- lm(Volume ~ Girth + Height)
s3d$plane3d(my.lm, lty.box = "solid")

## example 6; by Martin Maechler
cubedraw <- function(res3d, min = 0, max = 255, cex = 2, text. = FALSE)
{
  ## Purpose: Draw nice cube with corners
  cube01 <- rbind(c(0,0,1), 0, c(1,0,0), c(1,1,0), 1, c(0,1,1), # < 6 outer
                  c(1,0,1), c(0,1,0)) # <- "inner": fore- & back-ground
  cub <- min + (max-min)* cube01
  ## visibile corners + lines:
  res3d$points3d(cub[c(1:6,1,7,3,7,5) ,], cex = cex, type = 'b', lty = 1)
  ## hidden corner + lines
  res3d$points3d(cub[c(2,8,4,8,6), ], cex = cex, type = 'b', lty = 3)
  if(text.)## debug
    text(res3d$xyz.convert(cub), labels=1:nrow(cub), col='tomato', cex=2)
}
## 6 a) The named colors in R, i.e. colors()
cc <- colors()
crgb <- t(col2rgb(cc))
par(xpd = TRUE)
rr <- scatterplot3d(crgb, color = cc, box = FALSE, angle = 24,
                    xlim = c(-50, 300), ylim = c(-50, 300), zlim = c(-50, 300))
cubedraw(rr)
## 6 b) The rainbow colors from rainbow(201)
rbc <- rainbow(201)
Rrb <- t(col2rgb(rbc))
rR <- scatterplot3d(Rrb, color = rbc, box = FALSE, angle = 24,
                    xlim = c(-50, 300), ylim = c(-50, 300), zlim = c(-50, 300))
cubedraw(rR)
rR$points3d(Rrb, col = rbc, pch = 16)


library(plotly)
mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)
p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="scatter3d/basic")
chart_link

#draw pic with function info
f2 <- function(x, y) x + y
persp3d(f2)

#draw pic with function info
curve_3d <- function(f2, x_range=c(-1, 1), y_range=c(-1, 1), col=1:6 ){ 
  if (!require(rgl) ) {stop("load rgl")}
  xvec <- seq(x_range[1], x_range[2], len=15)
  yvec <- seq(y_range[1], y_range[2], len=15)
  fz <- outer(xvec, yvec, FUN=f2)
  open3d()
  persp3d( xvec, yvec, fz, col=col) }
curve_3d(f2)
snapshot3d("out3dplane.png")

#basic draw
fdejong <- function (x, y) {
  return (x^2 + y^2)
}

x <- seq(-10, 10, length= 30)
y <- x
z <- outer(x, y, fdejong)
z[is.na(z)] <- 1

require(lattice)
wireframe(z, drape=T, col.regions=rainbow(100))