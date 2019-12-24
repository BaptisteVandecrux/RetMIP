library(readxl)
library(lubridate)
library(ncdf4)
d.dir <- "C:\\Users\\bav\\GitHub\\RetMIP\\Data\\cores"
o.dir <- "Plots/"


##### get obs data ####
corelist <- "C:\\Users\\bav\\GitHub\\RetMIP\\Data\\CoreList.xlsx"
corenames <- as.data.frame(read_excel(corelist, range = "B1:B704"))
location <- as.data.frame(read_excel(corelist, range = "C1:C704"))

c.d <- (read_excel(corelist, range = "D1:D704"))
core_date <- as.numeric(as.Date(c.d$DateCored, "%d-%m-%Y"))
#range(core_date)
lat <- as.data.frame(read_excel(corelist, range = "E1:E704"))
lon <- as.data.frame(read_excel(corelist, range = "F1:F704"))
alt <- as.data.frame(read_excel(corelist, range = "G1:G704"))
depth <- as.data.frame(read_excel(corelist, range = "J1:J704"))


loc <- "Dye-2"
t.dye2 <- (seq(as.Date("01-06-1998", "%d-%m-%Y" ),as.Date("02-05-2015", "%d-%m-%Y"), by="days"))
#t.dye2 <- (seq(as.Date("01-06-1998", "%d-%m-%Y" ),as.Date("02-05-2015", "%d-%m-%Y"), by="days"))
t <- as.numeric(t.dye2); range(t)
nr <- which(location==paste0(loc));nr
nr <- nr[-4] # for dye2!

loc <- "KAN-U"
t.kanu <- as.numeric(seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
t <- t.kanu
#range(t)
nr <- which(location==paste0(loc));nr

########### DMI ################
m.dir <- path_model

i_mod <- 1
file_name_col <- paste('RetMIP_',models[i_mod],'_',sites[i_site],'_3hourly_columns.nc',sep = "")
input<-paste(models[i_mod],'/',file_name_col,sep = "")

nc <- nc_open(paste(m.dir,input, sep = ""))
var <- ncvar_get(nc,"rho"); dim(var)
var.dmi <- (var[nrow(var):1,])
dim(var.dmi)

par(mfrow = c(3,5))
    for(cn in nr) {

co <- read.csv(paste0(d.dir,'\\',cn,".csv"), sep = ";")
co[which(co[,2]==-999),2] <- NA

#### MODELDATA: ####
t.m <- which(t==core_date[cn])

rho.dmi <-(rev(var.dmi[,t.m]))

#### plot data ####
n <- length(co[,1])
nmax <- max(co[,1])
nl <- floor(n/100)
nl <- floor(nmax/100)
#nl <- ceiling(n/100)

plot(co[,2],rev(co[,1]), type="s", las=1, axes = F, xlim=c(200,950), #ylim = c(0,max(co[,1])),
          ylab = "Depth (m)", xlab = "Density (kg/m3)", main = paste0(location[cn,]))

par(new=T)
plot((rho.dmi[1:(nl*10)]), seq(nl*10,1,-1), type="s", las=1, axes = F, col=2,xlim=c(200,950),lwd=2, #ylim = c(0,nl),
     ylab = "Depth (m)", xlab = "Density (kg/m3)", main = paste0(location[cn,]))
axis(1);box()
axis(2, labels = c((seq(0,nl,2))), 
     at = c(seq(nl*10,0,-20)),las = 1)

par(new=T)
plot((rho.cfm[1:(nl*10)]), seq(nl*10,1,-1), type="s", las=1, axes = F, col=3,xlim=c(200,950),lwd=2, #ylim = c(0,nl),
     ylab = "", xlab = "")

mtext(side = 3, line = 0, paste0(c.d[cn,]))
legend("bottomleft", c("OBS","DMI", "CFM"), col=c(1,2,3), lty = 1, lwd = 2, bty = "n")
}
dev.off()
