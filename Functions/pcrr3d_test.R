library(imagefx)

## load in the images
data(tux1,tux2)

plot(tux1)
plot(tux2)

## now find the shift vector by using the phase correlation function 
shifts <- pcorr3d(tux1,tux2)

## ---- Plotting Example 1  ----- ##

split.screen(c(1,2))
screen(1)
image(1:nrow(tux1),1:ncol(tux1),tux1,col=gray.colors(200))

## define an example arrow starting and end points based on the shift found
x0 = nrow(tux1)/2
y0 = ncol(tux1)/2
x1 = x0 + shifts$max.shifts[1]
y1 = y0 + shifts$max.shifts[2]

## add arrows indicating how the image shifted
arrows(x0,y0,x1,y1)

## add a point where the arrow is
points(nrow(tux1)/2+shifts$max.shifts[1],ncol(tux1)/2+shifts$max.shifts[2],pch=21,bg='green')

screen(2)
image(1:nrow(tux2),1:ncol(tux2),tux2,col=gray.colors(200))
points(nrow(tux1)/2+shifts$max.shifts[1],ncol(tux1)/2+shifts$max.shifts[2],pch=21,bg='green')

## close the screen
close.screen(all.screens=TRUE)

library(sp)
library(gstat)
data(meuse)
# no trend:
coordinates(meuse) = ~x+y
plot(variogram(log(zinc)~1, meuse))
# residual variogram w.r.t. a linear trend:
variogram(log(zinc)~x+y, meuse)
# directional variogram:
variogram(log(zinc)~x+y, meuse, alpha=c(0,45,90,135))
plot(variogram(log(zinc)~1, meuse, width=20, cutoff=1300))
