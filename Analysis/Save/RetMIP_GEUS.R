library(ncdf4)
library(ggplot2)
library(reshape)
library(functional)
library(pracma)
library(dplyr)
library(lubridate)
library(gridExtra)

path_model<-'C:/Users/bav/OneDrive - Geological survey of Denmark and Greenland/Data/RetMIP/Model outputs/'
sites<-list('KANU','Dye2-16','Dye2-long','Summit','FA')
models <- list('CFM-Cr','CFM-KM','DMIHH','GEUS','IMAUFDM','MeyerHewitt','SUTRAICE','UppsalaUniBucket','UppsalaUniDeep')

i_mod<-1
i_site<-1
file_name_val <- paste('RetMIP_',models[i_mod],'_',sites[i_site],'_3hourly_values.nc',sep = "")
file_name_col <- paste('RetMIP_',models[i_mod],'_',sites[i_site],'_3hourly_columns.nc',sep = "")

path_file_val<-paste(path_model,models[i_mod],'/',file_name_val,sep = "")
path_file_col<-paste(path_model,models[i_mod],'/',file_name_col,sep = "")

# ==== loading column data
file_nc_col<- nc_open(path_file_col)
rho_mod <- ncvar_get(file_nc_col, varid="rho")
time_mod<-ncvar_get(file_nc_col,varid="time")

date_mod <- chron(time_mod, origin="1900-01-01") 

# some filtering
rho_mod<-rho_mod[apply(rho_mod, 1, Compose(is.finite, all)),]

# rearranging in xyz array
rho_mod<-melt(rho_mod)
tmp <- data.frame(rho_mod)
# ==== Density ====
h_rho_mod<- ggplot(data = data.frame(rho_mod), 
                      aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho geus dye2_16")+  scale_x_continuous(breaks=c(0,500,1000,1500),
                     labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20")) + scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), 
                        type="viridis", breaks=c(300,400,500,600,700,800,900),
                        labels=c("300","400","500","600","700","800","900"))

# ==== Temperature ====                                                                                                                                                                                                                                                                                                                                                                                                    
temp_mod<-ncvar_get(file_nc_col, varid = "temp")
temp_mod<-temp_mod[apply(temp_mod, 1, Compose(is.finite, all)),]
temp_mod<-temp_mod-273.15
temp_mod<-melt(temp_mod)
h_temp_mod <- ggplot(data = data.frame(temp_mod), 
        aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),
        labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),
        expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),
        labels=c("0", "5", "10","15","20"),
        expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [ºC]")), 
        type="viridis")+scale_fill_gradientn(name="Temperature [?C]",
        colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),
        limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))

# ==== LWC ====
lwc_mod<- ncvar_get(file_nc_col, varid = "lwc")
lwc_mod<-lwc_mod[apply(lwc_mod, 1, Compose(is.finite, all)),]
lwc_mod<-melt(lwc_mod)
h_lwc_mod<- ggplot(data = data.frame(lwc_mod), 
      aes(x = X2, y = -X1, fill= value)) + geom_tile() + xlab("Date") + ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),
      labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),
      expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),
      labels=c("0","1","2","3","4", "5","6"),
      limits = c(-60,NA),
      expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",
      colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))

# ==== icecon ====
# icecon_geus<- ncvar_get(file_nc_col,varid="icecon")
# rfrz1_geus <- ncvar_get(file_nc_col, varid = "rfrz")

# ==== Total values =====

# time_mod <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
file_nc_val <- nc_open(path_file_val)
# tfac_mod    <- ncvar_get(file_nc_val, varid="tfac")
# tlwc_mod    <- ncvar_get(file_nc_val, varid = "tlwc")
trfrz_mod   <- ncvar_get(file_nc_val, varid = "trfrz")

time_mod   <- ncvar_get(file_nc_val, varid = "time")
date_mod <- as.POSIXct("1900-01-01") + time_mod*3600*24


#Remove NA values
trfrz_mod <- trfrz_mod[!is.na(trfrz_mod)]
# trfrz_mod <- trfrz_mod[seq(1, length(trfrz_mod), 8)]
# unit harmonization
if (strcmp(models[[i_mod]],"GEUS")) {
  trfrz_mod <- trfrz_mod*10000}

h_refreezing_mod<- ggplot()+geom_line(aes(x=date_mod, y = trfrz_mod))
plot(h_refreezing_mod)


# total runoff
trunoff_mod<- ncvar_get(file_nc_val, varid = "trunoff")
trunoff_mod<-trunoff_mod[!is.na(trunoff_mod)]
#trunoff_mod<- trunoff_mod[seq(1, length(trunoff_mod), 8)]

if (strcmp(models[[i_mod]],"GEUS")) {
  trunoff_mod*1000}
h_runoff_mod<- ggplot()+geom_line(aes(x=date_mod, y = trunoff_mod)) 
plot(h_runoff_mod)

#Yearly totals for refreezing:
year_list <- unique(year(date_mod))
trfrz_year = year_list*NaN
trunoff_year = year_list*NaN

for (i in 1:length(year_list)){
  ind <- year(date_mod)==year_list[i]
  trfrz_year[i]<- sum(trfrz_mod[ind])
  trunoff_year[i]<- sum(trunoff_mod[ind])
}

# ==== plotting ====
windows();
par(mfrow=c(length(year_list),1),
    mar = c(3,5,2,1))
for (i in 1:length(year_list)){
  ind <- year(date_mod)==year_list[i]
  
  plot(date_mod[ind],trfrz_mod[ind],
       xlab = if(i<length(year_list)) "" else "Month",
       main = if(i==1) "Three-hourly refreezing (mm w.eq.)" else "")
}

# ==== plotting ====
windows();
par(mfrow=c(length(year_list),1),
    mar = c(3,5,2,1))
for (i in 1:length(year_list)){
  ind <- year(date_mod)==year_list[i]
  
  plot(date_mod[ind],trunoff_mod[ind],
       xlab = if(i<length(year_list)) "" else "Month",
       main = if(i==1) "Three-hourly refreezing (mm w.eq.)" else "")
}

# ==== plotting ====
p<-list()
for (i in 1:length(year_list)){
  ind <- year(date_mod)==year_list[i]
  
  p[[i]] <- qplot(date_mod[ind],trunoff_mod[ind],
       xlab =  "",
       ylab= "")
}

do.call("grid.arrange", 
        c(p, ncol=1,
          top=sites[i_site], 
          left="Three-hourly runoff (mm w.eq.)",
          bottom="Month"))
