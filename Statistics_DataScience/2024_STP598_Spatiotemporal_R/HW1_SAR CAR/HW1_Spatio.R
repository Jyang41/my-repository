# Install and load necessary packages
install.packages("gstat")
install.packages("akima")
install.packages("maps")

library(gstat)
library(akima)
library(maps)

# Load the coalash data
data(coalash, package = "gstat")

### (a)
# Directly plot the sampled sites using x and y planar coordinates and add conotour lines to the plot
plot(coalash$x, coalash$y, main = "Sampled Sites of Coal Ash", xlab = "X Coordinate", ylab = "Y Coordinate", pch = 20, col = "blue")

# Interpolate the data to create a continuous surface
coalash_interp <- interp(coalash$x, coalash$y, coalash$coal, extrap = TRUE)
contour(coalash_interp, add = TRUE)

# Add image plot and contour lines
image(coalash_interp, xlim = range(coalash$x), ylim = range(coalash$y), xlab = "X Coordinate", ylab = "Y Coordinate", main = "Coal Ash Interpolated Surface")
contour(coalash_interp, add = TRUE)

# Generate a 3D surface plot using persp()
persp(coalash_interp$x, coalash_interp$y, coalash_interp$z, 
      theta = -30, phi = 30, col = "lightblue", 
      xlab = "X Coordinate", ylab = "Y Coordinate", zlab = "Coal Ash Content",
      main = "3D Surface Plot of Coal Ash Content")

### (b)
par(mfrow = c(1, 2))

# Descriptive statistics for the coal variable
summary(coalash$coal)

# Plot a histogram
hist(coalash$coal, main = "Histogram of Coal Ash Content", xlab = "Coal Ash Content", col = "lightblue", border = "black")

# Stem-and-leaf plot
stem(coalash$coal)

# Quantiles of coal ash content
quantile(coalash$coal)

# Boxplot
boxplot(coalash$coal, main = "Boxplot of Coal Ash Content", ylab = "Coal Ash Content", col = "lightgreen")

### (c) Geostatistical analysis of coalash data
#install.packages("geoR")  # Install geoR if not already installed
library(geoR)

obj = cbind(coalash$x, coalash$y, coalash$coal)

# Convert the matrix into a geo-referenced object using geoR's as.geodata function
coalash.geo = as.geodata(obj, coords.col = 1:2, data.col = 3)

# Calculate the classical semivariogram using the variog function
coalash.var = variog(coalash.geo, estimator.type = 'classical')

# Calculate the robust semivariogram
coalash.var.robust = variog(coalash.geo, estimator.type = 'modulus')

# Set up the plotting layout for side-by-side semivariograms
par(mfrow = c(1, 2))

# Plot the classical semivariogram
plot(coalash.var, main = 'Classical Semivariogram')

# Plot the robust semivariogram
plot(coalash.var.robust, main = 'Robust Semivariogram')

# Fit an exponential model to the robust semivariogram
coalash.var.fit = variofit(coalash.var.robust, 
                           ini.cov.pars = c(2.0, 0.5), 
                           cov.model = 'exponential', 
                           fix.nugget = FALSE, 
                           nugget = 1.0)
print(coalash.var.fit)
# Exponential covariance model was fitted to the variogram using weighted least squares (WLS)
# parameter estimates:
# tausq: 0.6835, spatial variability at very small distance, probability due to measurement noise or microscale vriability
# sigmasq: 1.2135, partial sill, The larger the partial sill, the more variance is explained by spatial correlation
# phi: 7.1813, range parameter, which controls how quickly the covariance between two points decays as a function of distance. 
#             In an exponential model, the covariance decays rapidly, but spatial dependence is still present beyond this range. 
#             How far apart points can be before they are no longer spatially correlated. 
# Practical Range with cor=0.05 for asymptotic range: 21.51322
#           :the distance at which the spatial correlation becomes negligible (when the correlation drops to ~5%).
#           :samples located within 21.51 units of each other are likely to exhibit some degree of spatial autocorrelation, while those farther apart are not strongly correlated.
# variofit: minimised weighted sum of squares = 236.0256
#         : how well the model fits the observed variogram. The smaller the value, the better the fit.
#         : a reasonable fit, yet still could be improved by adjusting the model or covariance structure.


# Perform likelihood-based fitting for the exponential model using likfit
coalash.lik.fit = likfit(coalash.geo, 
                         ini.cov.pars = c(2.0, 0.5), 
                         cov.model = 'exponential', 
                         trend = 'cte', 
                         fix.nugget = FALSE, 
                         nugget = 1.0, 
                         nospatial = TRUE, 
                         lik.method = 'ML')
print(coalash.lik.fit)
# likfit: estimated model parameters:
#   beta    tausq  sigmasq      phi 
# "9.6769" "1.0350" "0.6896" "7.0211" 
# Practical Range with cor=0.05 for asymptotic range: 21.03324
# 
# likfit: maximised log-likelihood = -321

### Correlograms

# Create correlogram using the coalash data
install.packages("ncf")
library(ncf)

# Define coordinates and data
x <- coalash$x
y <- coalash$y
z <- coalash$coal

# Generate correlogram
coalash.corr <- correlog(x, y, z, increment = 1, resamp = 0)

# Plot the correlogram
plot(coalash.corr)


################ SAR and CAR
### Kriging
# Bayesian kriging provides posterior distributions for the spatial parameters, 
# rather than just point estimates (as in frequentist methods). 
# The advantage is that it incorporates uncertainty in these parameters 
# by giving you a range of possible values (posterior distribution) instead of a single point estimate.
library(geoR)

# Create the geo-referenced object using coalash data
obj = cbind(coalash$x, coalash$y, coalash$coal)
coalash.geo = as.geodata(obj, coords.col = 1:2, data.col = 3)


# Perform Bayesian kriging
coalash.bayes = krige.bayes(coalash.geo, locations = 'no', borders = NULL, 
                            model = model.control(trend.d = 'cte', cov.model = 'exponential'), 
                            prior = prior.control(beta.prior = 'flat', sigmasq.prior = 'reciprocal', 
                                                  tausq.rel.prior = 'uniform', tausq.rel.discrete = seq(0, 1, .01)))
# Larger phi suggest a broader spatial correlation, 
#             meaning that samples farther apart are still correlated.
# Smaller phi imply that spatial correlation decays quickly, 
#             meaning that samples need to be close to each other to exhibit correlation.

# Extract posterior samples
out = coalash.bayes$posterior$sample

# Plot posterior distributions of parameters
par(mfrow = c(2, 2))
hist(out$beta, breaks = 20, freq = FALSE, main = NULL)
lines(density(out$beta), col = 'blue', lwd = 2)
title('Mean')

hist(out$sigmasq, breaks = 20, freq = FALSE, main = NULL)
lines(density(out$sigmasq), col = 'blue', lwd = 2)
title('Sill')

hist(out$phi, breaks = 20, freq = FALSE, main = NULL)
lines(density(out$phi), col = 'blue', lwd = 2)
title('Range')

hist(out$tausq, breaks = 20, freq = FALSE, main = NULL)
lines(density(out$tausq), col = 'blue', lwd = 2)
title('Nugget')

######### Areal Models
## Areal Models
install.packages('spdep')
install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("maps")
install.packages('RColorBrewer')
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spdep)
library(maptools)
library(classInt)
library(RColorBrewer)
library(maps)

usa.state=map(database="state", fill=TRUE, plot=FALSE)
state.ID <- sapply(strsplit(usa.state$names, ":"), function(x) x[1])
usa.poly = map2SpatialPolygons(usa.state, IDs=state.ID)
usa.nb = poly2nb(usa.poly)
usa.adj.mat = nb2mat(usa.nb, style="B")

mn.county = map("county", "minnesota", fill=TRUE, plot=FALSE)
county.ID <- sapply(strsplit(mn.county$names, ","), function(x) x[2])
mn.poly = map2SpatialPolygons(mn.county, IDs=county.ID)
mn.nb = poly2nb(mn.poly)
mn.adj.mat = nb2mat(mn.nb, style="B")

mn.region.id <- attr(mn.nb, "region.id")
winona.neighbors.index = mn.nb[[match("winona", mn.region.id)]]
winona.neighbors = rownames(mn.adj.mat[winona.neighbors.index,])
print(winona.neighbors)

### Moran’s I and Geary’s C
# Load necessary packages
install.packages("spdep")
library(spdep)

# Define coordinates for coalash
coords = cbind(coalash$x, coalash$y)

# Define neighbors using nearest neighbors (k = 5)
coalash.nb = knn2nb(knearneigh(coords, k = 5))

# Create a list of spatial weights
coalash.listw = nb2listw(coalash.nb, style = "W")

# Perform Moran's I test
moran.out = moran.test(coalash$coal, listw = coalash.listw)
moran.I = moran.out$estimate[1]
moran.I.se = sqrt(moran.out$estimate[3])
moran_tab = data.frame(I = moran.I, se = moran.I.se)
print(moran_tab)

# Perform Geary's C test
geary.out = geary.test(coalash$coal, listw = coalash.listw)
geary.C = geary.out$estimate[1]
geary.C.se = sqrt(geary.out$estimate[3])
geary_tab = data.frame(C = geary.C, se = geary.C.se)
print(geary_tab)


### SAR and CAR Model Fitting
library(spatialreg)

# Create nearest neighbor structure for coalash data (k = 5)
coords = cbind(coalash$x, coalash$y)
coalash.nb = knn2nb(knearneigh(coords, k = 5))

# Create spatial weights list
coalash.listw = nb2listw(coalash.nb, style = "W")

# Fit a SAR model
coalash.sar.out = lagsarlm(coalash ~ 1, data = coalash, listw = coalash.listw)
summary(coalash.sar.out)

# Fit a CAR model
coalash.car.out = spautolm(coalash ~ 1, data = coalash, family = "CAR", listw = coalash.listw)
summary(coalash.car.out)

# Add fitted values to the coalash dataset
coalash$fitted.sar = fitted(coalash.sar.out)
coalash$fitted.car = fitted(coalash.car.out)

### Visualization of fitted SAR/CAR models
# Draw map of the original and fitted values (optional)
install.packages("RColorBrewer")
library(RColorBrewer)
library(maps)

# Create breaks and color palette
brks = c(min(coalash$coal), 2, 3, 4, max(coalash$coal))
color.pal = rev(brewer.pal(4, "RdBu"))

# Plot the coalash original and SAR fitted values
par(mfrow = c(2, 1))
plot(coalash$x, coalash$y, col = color.pal[cut(coalash$coal, breaks = brks)], pch = 16, main = "Original Coal Ash Content")
plot(coalash$x, coalash$y, col = color.pal[cut(coalash$fitted.sar, breaks = brks)], pch = 16, main = "Fitted SAR Coal Ash Content")





#############################################################################
################ professor's
### kriging

scallops.bayes = krige.bayes(scallops.geo,locations = 'no',borders = NULL, model = model.control(trend.d='cte',cov.model = 'exponential'),prior = prior.control(beta.prior = 'flat',sigmasq.prior = 'reciprocal',tausq.rel.prior ='uniform', tausq.rel.discrete = seq(0,1,.01) ))
out=scallops.bayes$posterior$sample
par(mfrow=c(2,2))
hist(out$beta,breaks=20,freq=F,main=NULL)
lines(density(out$beta),col='blue',lwd=2)
title('mean')
hist(out$sigmasq,breaks=20,freq=F,main=NULL)
lines(density(out$sigmasq),col='blue',lwd=2)
title('sill')
hist(out$phi,breaks=20,freq=F,main=NULL)
lines(density(out$phi),col='blue',lwd=2)
title('range')
hist(out$tausq,breaks=20,freq=F,main=NULL)
lines(density(out$tausq),col='blue',lwd=2)
title('nugget')



### Moran's I and Geary's C
usa.listw = nb2listw(usa.nb, style="W")
state.sat.scores<-read.table('https://www.counterpointstat.com/uploads/1/1/9/3/119383887/state-sat.dat',header=F)
colnames(state.sat.scores) <- c('STATE','VERBAL','MATH','PERCT')
# remove alaska and hawaii
x = ((state.sat.scores$STATE=="alaska") | (state.sat.scores$STATE=="hawaii") | (state.sat.scores$STATE=="us"))
index = c(1:nrow(state.sat.scores))[x]
state.sat.scores.contig = state.sat.scores[-index,]

moran.out=moran.test(state.sat.scores.contig$VERBAL, listw=usa.listw)
moran.I = moran.out$estimate[1]
moran.I.se = sqrt(moran.out$estimate[3])
moran_tab=data.frame(I=moran.I, se=moran.I.se)
table(moran_tab)

geary.out=geary.test(state.sat.scores.contig$VERBAL, listw=usa.listw)
geary.C = geary.out$estimate[1]
geary.C.se = sqrt(geary.out$estimate[3])
geary_tab=data.frame(C=geary.C, se=geary.C.se)
table(geary_tab)

### SAR and CAR model fitting
nc.sids <- readShapePoly(system.file("shapes/sids.shp", package="spData")[1],ID="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
rn <- sapply(slot(nc.sids, "polygons"), function(x) slot(x, "ID"))
ncCC89.nb <- read.gal(system.file("weights/ncCC89.gal", package="spData")[1], region.id=rn)

nc.sids.rates.FT = sqrt(1000) * (sqrt(nc.sids$SID79/nc.sids$BIR79) + sqrt((nc.sids$SID79 + 1)/nc.sids$BIR79))
nc.sids$rates.FT = nc.sids.rates.FT
nc.sids.nwbir.FT = sqrt(1000) * (sqrt(nc.sids$NWBIR79/nc.sids$BIR79) + sqrt((nc.sids$NWBIR79 + 1)/nc.sids$BIR79))
nc.sids$nwbir.FT = nc.sids.nwbir.FT

# prepare for adjacency matrix
ncCC89.listw = nb2listw(ncCC89.nb, style="B", zero.policy=TRUE) # zero.policy=TRUE because of two islands
# nc.county.id = attr(ncCC89.nb, "region.id")
# nc.no.neighbors = card(ncCC89.nb)
# nc.islands = as.character(nc.sids[card(ncCC89.nb) == 0, ]$NAME)
# print(nc.islands)
# fit SAR model
#nc.sids.sar.out = errorsarlm(rates.FT~ nwbir.FT, data=nc.sids, listw=ncCC89.listw, zero.policy=TRUE)
ls("package:spdep")
exists("spautolm")
install.packages("spatialreg")
library(spatialreg)
nc.sids.sar.out = spautolm(rates.FT~ nwbir.FT, data=nc.sids, family="SAR", listw=ncCC89.listw, zero.policy=TRUE)
summary(nc.sids.sar.out)
nc.sids$fitted.sar = fitted(nc.sids.sar.out)

# fit CAR model
install.packages("spatialreg")
library(spatialreg)
exists("spautolm")
nc.sids.car.out = spautolm(rates.FT~ nwbir.FT, data=nc.sids, family="CAR", listw=ncCC89.listw, zero.policy=TRUE)
summary(nc.sids.car.out)
nc.sids$fitted.car = fitted(nc.sids.car.out)

#Draw the maps using the maps function
library(maps)
library(classInt)
brks = c(0, 2.0, 3.0, 3.5, 6.0)
install.packages("RColorBrewer")
library(RColorBrewer)
color.pallete = rev(brewer.pal(4,"RdBu"))
class.raw = classIntervals(var=nc.sids$rates.FT, n=4, style="fixed", fixedBreaks=brks, dataPrecision=4)
color.code.raw = findColours(class.raw, color.pallete)
class.fitted = classIntervals(var=nc.sids$fitted.sar, n=4, style="fixed", fixedBreaks=brks, dataPrecision=4)
color.code.fitted = findColours(class.fitted, color.pallete)

leg.txt = c("<2.0", "2.0-3.0", "3.0-3.5",">3.5")

par(mfrow=c(2,1), oma = c(0,0,4,0) + 0.1, mar = c(0,0,1,0) + 0.1)
plot(nc.sids, col=color.code.raw)
title("a) Raw Freeman-Tukey transformed SIDS rates" )
legend("bottomleft", legend=leg.txt, cex=1.25, bty="n", horiz = FALSE, fill = color.pallete)
plot(nc.sids, col=color.code.fitted)
title("b) Fitted SIDS rates from SAR model")

install.packages("sf")
install.packages("spData")
library(sf)
library(spData)
#columbus.poly <- readShapePoly(system.file("shapes/columbus.shp", package="spData")[1])
columbus.poly <- st_read(system.file("shapes/columbus.shp", package="spData"))

columbus.coords = coordinates(columbus.poly)
columbus.knn = knearneigh(columbus.coords)
columbus.knn2nb = knn2nb(columbus.knn)
columbus.dist.list = nbdists(columbus.knn2nb, columbus.coords)
columbus.dist.vec = unlist(columbus.dist.list)
columbus.dist.max = max(columbus.dist.vec)
columbus.dnn.nb = dnearneigh(columbus.coords, 0, columbus.dist.max)

columbus.dnn.listw = nb2listw(columbus.dnn.nb, style="B", zero.policy=TRUE)

##SAR model regressing HOUSE_VAL+INCOME using distance-based nearest neighbors
columbus.dnn.sar.out = spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="SAR", listw=columbus.dnn.listw, zero.policy=TRUE)
columbus.dnn.sar.fitted = fitted(columbus.dnn.sar.out)
columbus.poly$fitted.dnn.sar = columbus.dnn.sar.fitted
summary(columbus.dnn.sar.out)

##CAR model regressing HOUSE_VAL+INCOME using distance-based nearest neighbors
columbus.dnn.car.out = spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="CAR", listw=columbus.dnn.listw, zero.policy=TRUE)
columbus.dnn.car.fitted = fitted(columbus.dnn.car.out)
columbus.poly$fitted.dnn.car = columbus.dnn.car.fitted
summary(columbus.dnn.car.out)

