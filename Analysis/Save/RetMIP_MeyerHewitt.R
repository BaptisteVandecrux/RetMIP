library(ncdf4)
library(ggplot2)
library(reshape)
library(functional)

###DYE-2_16 
#Hourly averages
file1_MH<- nc_open("RetMIP_MeyerHewitt_Dye-2_16_3hourly_columns.nc")
rho1_MH <- ncvar_get(file1_MH, varid="rho")
rho1_MH<-rho1_MH[apply(rho1_MH, 1, Compose(is.finite, all)),]
rho1_MH<-melt(rho1_MH)
plot_rho1_MH<- ggplot(data = data.frame(rho1_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho MH dye2_16")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp1_MH<-ncvar_get(file1_MH, varid = "temp")
temp1_MH<-temp1_MH[apply(temp1_MH, 1, Compose(is.finite, all)),]
temp1_MH<-temp1_MH-273.15
temp1_MH<-melt(temp1_MH)
plot_temp1_MH<- ggplot(data = data.frame(temp1_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc1_MH<- ncvar_get(file1_MH, varid = "lwc")
lwc1_MH<-lwc1_MH[apply(lwc1_MH, 1, Compose(is.finite, all)),]
lwc1_MH<-melt(lwc1_MH)
plot_lwc1_MH<- ggplot(data = data.frame(lwc1_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
icecon_MH<- ncvar_get(file1_MH,varid="icecon")
rfrz1_MH <- ncvar_get(file1_MH, varid = "rfrz")
#Total values
t_dye2_16 <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
file2_MH<- nc_open("RetMIP_MeyerHewitt_Dye-2_16_3hourly_values.nc")
tfac_MH <- ncvar_get(file2_MH, varid="tfac")
tlwc_MH<-ncvar_get(file2_MH, varid = "tlwc")
trfrz_MH<- ncvar_get(file2_MH, varid = "trfrz")
#Remove NA values
trfrz_MH<-trfrz_MH[!is.na(trfrz_MH)]
trfrz_MH<- trfrz_MH[seq(1, length(trfrz_MH), 8)]
trunoff_MH<- ncvar_get(file2_MH, varid = "trunoff")
trunoff_MH<- ncvar_get(file2_MH, varid = "trunoff")
trunoff_MH<-trunoff_MH[!is.na(trunoff_MH)]
trunoff_MH<- trunoff_MH[seq(1, length(trunoff_MH), 8)]
plot_refreezing_MH_dye_16<- ggplot()+geom_line(aes(x=t_dye2_16, y = trfrz_MH)) 
###KAN-U
file3_MH<- nc_open("RetMIP_MeyerHewitt_KAN_U_3hourly_columns.nc")
rho2_MH <- ncvar_get(file3_MH, varid="rho")
rho2_MH<-rho2_MH[apply(rho2_MH, 1, Compose(is.finite, all)),]
rho2_MH<-melt(rho2_MH)
plot_rho2_MH<- ggplot(data = data.frame(rho2_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho MH KANU")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp2_MH<-ncvar_get(file3_MH, varid = "temp")
temp2_MH<-temp2_MH[apply(temp2_MH, 1, Compose(is.finite, all)),]
temp2_MH<-temp2_MH-273.15
temp2_MH<-melt(temp2_MH)
plot_temp2_MH<- ggplot(data = data.frame(temp2_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc2_MH<- ncvar_get(file3_MH, varid = "lwc")
lwc2_MH<-lwc2_MH[apply(lwc2_MH, 1, Compose(is.finite, all)),]
lwc2_MH<-melt(lwc2_MH)
plot_lwc2_MH<- ggplot(data = data.frame(lwc2_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
icecon2_MH<- ncvar_get(file3_MH,varid="icecon")
rfrz2_MH <- ncvar_get(file3_MH, varid = "rfrz")
t_kanu<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_kanu)
##Total values
file4_MH<- nc_open("RetMIP_MeyerHewitt_KAN_U_3hourly_values.nc")
tfac_kanu_MH <- ncvar_get(file4_MH, varid="tfac")
tlwc_kanu_MH<-ncvar_get(file4_MH, varid = "tlwc")
trfrz_kanu_MH<- ncvar_get(file4_MH, varid = "trfrz")
#Remove NA values
trfrz_kanu_MH<-trfrz_kanu_MH[!is.na(trfrz_kanu_MH)]
trfrz_kanu_MH<- trfrz_kanu_MH[seq(1, length(trfrz_kanu_MH), 8)]
trunoff_kanu_MH<- ncvar_get(file4_MH, varid = "trunoff")
trunoff_kanu_MH<-trunoff_kanu_MH[!is.na(trunoff_kanu_MH)]
trunoff_kanu_MH<- trunoff_kanu_MH[seq(1, length(trunoff_kanu_MH), 8)]
plot_refreezing_kanu_MH<- ggplot()+geom_line(aes(x=t_kanu, y = trfrz_kanu_MH)) 
#Yearly totals for refreezing:
trfrz_kanu_MH_yearly_total <- trfrz_kanu_MH[seq(1, length(trfrz_kanu_MH), 1)]
trfrz_kanu_MH_yearly_total_1<- sum(trfrz_kanu_MH_yearly_total[1:366])
trfrz_kanu_MH_yearly_total_2<- sum(trfrz_kanu_MH_yearly_total[366:731])
trfrz_kanu_MH_yearly_total_3<- sum(trfrz_kanu_MH_yearly_total[731:1096])
trfrz_kanu_MH_yearly_total_4<- sum(trfrz_kanu_MH_yearly_total[1096:1461])
trfrz_kanu_MH_yearly_total_5<- sum(trfrz_kanu_MH_yearly_total[1461:1706])
yearly_trfrz_kanu_MH<-c(trfrz_kanu_MH_yearly_total_1,trfrz_kanu_MH_yearly_total_2,trfrz_kanu_MH_yearly_total_3,trfrz_kanu_MH_yearly_total_4,trfrz_kanu_MH_yearly_total_5)
t_kanu_yearly<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="years"))
trunoff_kanu_MH_yearly_total <- trunoff_kanu_MH[seq(1, length(trunoff_kanu_MH), 1)]
trunoff_kanu_MH_yearly_total_1<- sum(trunoff_kanu_MH_yearly_total[1:366])
trunoff_kanu_MH_yearly_total_2<- sum(trunoff_kanu_MH_yearly_total[366:731])
trunoff_kanu_MH_yearly_total_3<- sum(trunoff_kanu_MH_yearly_total[731:1096])
trunoff_kanu_MH_yearly_total_4<- sum(trunoff_kanu_MH_yearly_total[1096:1461])
trunoff_kanu_MH_yearly_total_5<- sum(trunoff_kanu_MH_yearly_total[1461:1706])
yearly_trunoff_kanu_MH<-c(trunoff_kanu_MH_yearly_total_1,trunoff_kanu_MH_yearly_total_2,trunoff_kanu_MH_yearly_total_3,trunoff_kanu_MH_yearly_total_4,trunoff_kanu_MH_yearly_total_5)
###SUMMIT
file5_MH<-nc_open("RetMIP_MeyerHewitt_Summit_3hourly_columns.nc")
rho3_MH <- ncvar_get(file5_MH, varid="rho")
temp3_MH<-ncvar_get(file5_MH, varid = "temp")
temp3_MH_2012<- temp3_MH[,33608:36528]
temp3_MH_2012<-temp3_MH_2012[apply(temp3_MH_2012, 1, Compose(is.finite, all)),]
temp3_MH_2012<-temp3_MH_2012-273.15
temp3_MH_2012<-melt(temp3_MH_2012)
plot_temp3_MH_2012<- ggplot(data = data.frame(temp3_MH_2012), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,1000,2000,3000),labels=c("01-Jan-2012", "01-May-2012", "01-Sep-2012","31-Dec-2012"),expand=c(0.01,0.01)) + scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(0,-20,-40,-60),labels=c("0","-20","-40","-60"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-60,0))

lwc3_MH<- ncvar_get(file5_MH, varid = "lwc")
icecon3_MH<- ncvar_get(file5_MH,varid="icecon")
rfrz3_MH <- ncvar_get(file5_MH, varid = "rfrz")
t_summit<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="days"))
as.POSIXct(t_summit)
##Total values
file6_MH<- nc_open("RetMIP_MeyerHewitt_Summit_3hourly_values.nc")
tfac_summit_MH <- ncvar_get(file6_MH, varid="tfac")
tlwc_summit_MH<-ncvar_get(file6_MH, varid = "tlwc")
trfrz_summit_MH<- ncvar_get(file6_MH, varid = "trfrz")
#Remove NA values
trfrz_summit_MH<-trfrz_summit_MH[!is.na(trfrz_summit_MH)]
trfrz_summit_MH<- trfrz_summit_MH[seq(1, length(trfrz_summit_MH), 8)]
trunoff_summit_MH<- ncvar_get(file6_MH, varid = "trunoff")
trunoff_summit_MH<-trunoff_summit_MH[!is.na(trunoff_summit_MH)];dim(trunoff_summit_MH)
trunoff_summit_MH<- trunoff_summit_MH[seq(1, length(trunoff_summit_MH), 8)]
plot_runoff_summit_MH<- ggplot()+geom_line(aes(x=t_summit, y = trunoff_summit_MH)) 
plot_refreezing_summit_MH<- ggplot()+geom_line(aes(x=t_summit, y = trfrz_summit_MH)) 
##Yearly values summit
trfrz_summit_MH_yearly_total <- trfrz_summit_MH[seq(1, length(trfrz_summit_MH), 1)]
trfrz_summit_MH_yearly_total_1<- sum(trfrz_summit_MH_yearly_total[1:357])
trfrz_summit_MH_yearly_total_2<- sum(trfrz_summit_MH_yearly_total[357:714])
trfrz_summit_MH_yearly_total_3<- sum(trfrz_summit_MH_yearly_total[714:1071])
trfrz_summit_MH_yearly_total_4<- sum(trfrz_summit_MH_yearly_total[1071:1428])
trfrz_summit_MH_yearly_total_5<- sum(trfrz_summit_MH_yearly_total[1428:1785])
trfrz_summit_MH_yearly_total_6<- sum(trfrz_summit_MH_yearly_total[1785:2142])
trfrz_summit_MH_yearly_total_7<- sum(trfrz_summit_MH_yearly_total[2142:2499])
trfrz_summit_MH_yearly_total_8<- sum(trfrz_summit_MH_yearly_total[2499:2856])
trfrz_summit_MH_yearly_total_9<- sum(trfrz_summit_MH_yearly_total[2856:3213])
trfrz_summit_MH_yearly_total_10<- sum(trfrz_summit_MH_yearly_total[3213:3570])
trfrz_summit_MH_yearly_total_11<- sum(trfrz_summit_MH_yearly_total[3570:3927])
trfrz_summit_MH_yearly_total_12<- sum(trfrz_summit_MH_yearly_total[3927:4284])
trfrz_summit_MH_yearly_total_13<- sum(trfrz_summit_MH_yearly_total[4284:4641])
trfrz_summit_MH_yearly_total_14<- sum(trfrz_summit_MH_yearly_total[4641:4998])
trfrz_summit_MH_yearly_total_15<- sum(trfrz_summit_MH_yearly_total[4998:5363])
yearly_trfrz_summit_MH<-c(trfrz_summit_MH_yearly_total_1,trfrz_summit_MH_yearly_total_2,trfrz_summit_MH_yearly_total_3,trfrz_summit_MH_yearly_total_4,trfrz_summit_MH_yearly_total_5, trfrz_summit_MH_yearly_total_6, trfrz_summit_MH_yearly_total_7,trfrz_summit_MH_yearly_total_8,trfrz_summit_MH_yearly_total_9,trfrz_summit_MH_yearly_total_10,trfrz_summit_MH_yearly_total_11,trfrz_summit_MH_yearly_total_12,trfrz_summit_MH_yearly_total_13,trfrz_summit_MH_yearly_total_14,trfrz_summit_MH_yearly_total_15)
t_summit_yearly<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="1 year"))


###FA
file7_MH<-nc_open("RetMIP_MeyerHewitt_FA_3hourly_columns.nc")
rho4_MH <- ncvar_get(file7_MH, varid="rho")
rho4_MH<-rho4_MH[apply(rho4_MH, 1, Compose(is.finite, all)),]
rho4_MH<-melt(rho4_MH)
plot_rho4_MH<- ggplot(data = data.frame(rho4_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho MH FA")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp4_MH<-ncvar_get(file7_MH, varid = "temp")
temp4_MH<-temp4_MH[apply(temp4_MH, 1, Compose(is.finite, all)),]
temp4_MH<-temp4_MH-273.15
temp4_MH<-melt(temp4_MH)
plot_temp4_MH<- ggplot(data = data.frame(temp4_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc4_MH<- ncvar_get(file7_MH, varid = "lwc")
lwc4_MH<-lwc4_MH[apply(lwc4_MH, 1, Compose(is.finite, all)),]
lwc4_MH<-melt(lwc4_MH)
plot_lwc4_MH<- ggplot(data = data.frame(lwc4_MH), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,30))
icecon4_MH<- ncvar_get(file7_MH,varid="icecon")
rfrz4_MH <- ncvar_get(file7_MH, varid = "rfrz")
t_FA<- (seq(as.Date("12-04-2014", "%d-%m-%Y" ),as.Date("02-12-2014", "%d-%m-%Y"), by="days"))
as.POSIXct(t_FA)
##Total values
file8_MH<- nc_open("RetMIP_MeyerHewitt_FA_3hourly_values.nc")
tfac_FA_MH <- ncvar_get(file8_MH, varid="tfac")
tlwc_FA_MH<-ncvar_get(file8_MH, varid = "tlwc")
trfrz_FA_MH<- ncvar_get(file8_MH, varid = "trfrz")
#Remove NA values
trfrz_FA_MH<-trfrz_FA_MH[!is.na(trfrz_FA_MH)]
trfrz_FA_MH<- trfrz_FA_MH[seq(1, length(trfrz_FA_MH), 8)]
trunoff_FA_MH<- ncvar_get(file8_MH, varid = "trunoff")
trunoff_FA_MH<-trunoff_FA_MH[!is.na(trunoff_FA_MH)]
trunoff_FA_MH<- trunoff_FA_MH[seq(1, length(trunoff_FA_MH), 8)]
plot_refreezing_FA_MH<- ggplot()+geom_line(aes(x=t_FA, y = trfrz_FA_MH)) 

