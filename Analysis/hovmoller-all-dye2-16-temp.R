library(ncdf4)
library(raster)
library(rasterVis)
library(maptools)
library(fields)

d.dir <- "/media/mol/MARTINO/RetMIP_data/DMI/"
d.dir <- "/media/mol/MARTINO/RetMIP_data/CFM/"
d.dir <- "/media/mol/MARTINO/RetMIP_data/Uppsala/"
d.dir <- "/media/mol/MARTINO/RetMIP_data/Munneke/"
d.dir <- "/media/mol/MARTINO/RetMIP_data/MeyerHewitt/"

w.dir <- "/home/mol/RetMIP/"
setwd(paste0(w.dir))

input2 <- "RetMIP_DMIHH_Dye-2_16_3hourly_values.nc"
nc <- nc_open(paste(d.dir,input2, sep = ""))
time <- ncvar_get(nc,"time")

input <- "RetMIP_DMIHH_Dye-2_16_3hourly_columns.nc"
input <- "RetMIP_CFM-KM_Dye2-16_3hourly_columns.nc"
input <- "RetMIP_UppsalaUniBucket_DYE2S_3hourly_columns.nc"
input <- "RetMIP_IMAUFDM_Dye-2_16_3hourly_columns.nc"
input <- "RetMIP_MeyerHewitt_Dye-2_16_3hourly_columns.nc"

fld <- "rho"
fld <- "lwc"
fld <- "temp"
      
nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste(fld)); dim(var)
var.rot <- t(var[nrow(var):1,])

#colfunc <- colorRampPalette(c("black", "blue", "red", "white"))
colfunc <- colorRampPalette(c("white", "red","green", "blue", "black"))
#colfunc <- colorRampPalette(c("white","black"))
#colfunc <- colorRampPalette(c("white","yellow","black"))

min <- floor(min(var.rot, na.rm = T)/10)*10 ; min
max <- ceiling(max(var.rot, na.rm = T)/10)*10 ; max
#main = expression(paste(rho,~ ~frac("kg", "m"^3)))
#main <- "mm weq"
main <- "Temperature \n      (K)"

n <- ceiling((max-min)/50)+1
n <- ceiling((max-min))+1
n <- 15
zlim <- c(min,max)


#############################
png(filename=paste0(w.dir,"hovmoeller_dye2-16_columns_all_temp.png"), 
    width=1000, height=1500, pointsize = 18)
layout(matrix(c(1,2,  
                3,2,  
                4,2,  
                5,2,  
                6,2, 
                7,2), 
              nrow=6, byrow = T), width = c(4,1),height = c(1,1,1))
layout.show(n=7)
par(mar = c(3,3,3,2))
d.dir <- "/media/mol/MARTINO/RetMIP_data/DMI/"
input <- "RetMIP_DMIHH_Dye-2_16_3hourly_columns.nc"

nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste0(fld)); dim(var)
var.rot <- t(var[nrow(var):1,])

image(var.rot, axes = F, col = colfunc(n), zlim = zlim,
      xlab = "Months", ylab = "Depth (m)", 
      main = paste(input))#, sub = "[02 May 2016 - 28 Oct 2016]")
axis(1, labels = c("May", "June", "July", "August", "September", "October"), at=seq(15*8, 1440, by=30*8)/1440)
axis(2, labels = c(rev(seq(0,20,by=2))), at=seq(0,1,by=0.1), las=1)
#contour(var.rot, add=T, nlevels = n/5, col = "white", lwd = 3, labcex = 1.2, zlim = zlim)
legend_image <- as.raster(matrix(rev(colfunc(n)), ncol=1))
box()

xl <- 1; yb <- 1; xr <- 1.2; yt <- 2

plot(NA,type="n",ann=T, ylab = '', xlab = '',xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n", 
     main=main, adj = 0, cex.main = 1.5)
rect(xl,
  head(seq(yb,yt,(yt-yb)/n),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/n),-1),
  col=colfunc(n))
mtext(seq(zlim[1],zlim[2],l=n+1)[2:n],side=2,at=(seq(yb,yt,(yt-yb)/n)[2:n])-0,las=2,cex=0.9)
#dev.off()

##################################

d.dir <- "/media/mol/MARTINO/RetMIP_data/CFM/"
input <- "RetMIP_CFM-KM_Dye2-16_3hourly_columns.nc"

nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste(fld)); dim(var)
var.rot <- t(var[nrow(var):1,])

image(var.rot, axes = F, col = colfunc(n), zlim = zlim,
      xlab = "Months", ylab = "Depth (m)", 
      main = paste(input))#, sub = "[02 May 2016 - 28 Oct 2016]")
axis(1, labels = c("May", "June", "July", "August", "September", "October"), at=seq(15*8, 1440, by=30*8)/1440)
axis(2, labels = c(rev(seq(0,20,by=2))), at=seq(0,1,by=0.1), las=1)
#contour(var.rot, add=T, nlevels = n/5, col = "white", lwd = 3, labcex = 1.2, zlim = zlim)
legend_image <- as.raster(matrix(rev(colfunc(n)), ncol=1))
box()


##################################

d.dir <- "/media/mol/MARTINO/RetMIP_data/Uppsala/"
input <- "RetMIP_UppsalaUniBucket_DYE2S_3hourly_columns.nc"

nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste(fld)); dim(var)
var.rot <- t(var[nrow(var):1,])

image(var.rot, axes = F, col = colfunc(n), zlim = zlim,
      xlab = "Months", ylab = "Depth (m)", 
      main = paste(input), sub = "[02 May 2016 - 28 Oct 2016]")
axis(1, labels = c("May", "June", "July", "August", "September", "October"), at=seq(15*8, 1440, by=30*8)/1440)
axis(2, labels = c(rev(seq(0,20,by=2))), at=seq(0,1,by=0.1), las=1)
#contour(var.rot, add=T, nlevels = n/5, col = "white", lwd = 3, labcex = 1.2, zlim = zlim)
legend_image <- as.raster(matrix(rev(colfunc(n)), ncol=1))
box()

##################################

d.dir <- "/media/mol/MARTINO/RetMIP_data/Munneke/"
input <- "RetMIP_IMAUFDM_Dye-2_16_3hourly_columns.nc"

nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste(fld)); dim(var)

# for Munneke IMAUFDM !!!!!!
var.ny <- var[1:1436,]
#var.rot <- var.ny[nrow(var.ny):1,ncol(var.ny):1]
var.rot <- var.ny[,ncol(var.ny):1]

image(var.rot, axes = F, col = colfunc(n), zlim = zlim,
      xlab = "Months", ylab = "Depth (m)", 
      main = paste(input), sub = "[02 May 2016 - 28 Oct 2016]")
axis(1, labels = c("May", "June", "July", "August", "September", "October"), at=seq(15*8, 1440, by=30*8)/1440)
axis(2, labels = c(rev(seq(0,20,by=2))), at=seq(0,1,by=0.1), las=1)
#contour(var.rot, add=T, nlevels = n/5, col = "white", lwd = 3, labcex = 1.2, zlim = zlim)
legend_image <- as.raster(matrix(rev(colfunc(n)), ncol=1))
box()


##################################

d.dir <- "/media/mol/MARTINO/RetMIP_data/MeyerHewitt/"
input <- "RetMIP_MeyerHewitt_Dye-2_16_3hourly_columns.nc"

nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste(fld)); dim(var)
var.rot <- t(var[nrow(var):1,])

image(var.rot, axes = F, col = colfunc(n), zlim = zlim,
      xlab = "Months", ylab = "Depth (m)", 
      main = paste(input), sub = "[02 May 2016 - 28 Oct 2016]")
axis(1, labels = c("May", "June", "July", "August", "September", "October"), at=seq(15*8, 1440, by=30*8)/1440)
axis(2, labels = c(rev(seq(0,20,by=2))), at=seq(0,1,by=0.1), las=1)
#contour(var.rot, add=T, nlevels = n/5, col = "white", lwd = 3, labcex = 1.2, zlim = zlim)
legend_image <- as.raster(matrix(rev(colfunc(n)), ncol=1))
box()


##################################


d.dir <- "/media/mol/MARTINO/RetMIP_data/GEUS/"
input <- "RetMIP_GEUS_Dye-2_16_3hourly_columns.nc"

nc <- nc_open(paste(d.dir,input, sep = ""))
var <- ncvar_get(nc,paste(fld)); dim(var)
var.rot <- t(var[nrow(var):1,])

image(var.rot, axes = F, col = colfunc(n), zlim = zlim,
      xlab = "Months", ylab = "Depth (m)", 
      main = paste(input), sub = "[02 May 2016 - 28 Oct 2016]")
axis(1, labels = c("May", "June", "July", "August", "September", "October"), at=seq(15*8, 1440, by=30*8)/1440)
axis(2, labels = c(rev(seq(0,20,by=2))), at=seq(0,1,by=0.1), las=1)
#contour(var.rot, add=T, nlevels = n/5, col = "white", lwd = 3, labcex = 1.2, zlim = zlim)
legend_image <- as.raster(matrix(rev(colfunc(n)), ncol=1))
box()
dev.off()
