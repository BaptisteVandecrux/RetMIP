library(ncdf4)
library(plot.matrix)
library(ggplot2)
###DYE-2_16 
#Hourly averages
file1<- nc_open("RetMIP_IMAUFDM_Dye-2_16_3hourly_columns.nc")
rho1 <- ncvar_get(file1, varid="rho"); dim(rho1)
rho1<-rho1[apply(rho1, 1, Compose(is.finite, all)),]
rho1<-melt(rho1)
plot_rho1_IMAUFDM<- ggplot(data = data.frame(rho1), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho IMAUFDM dye2_16")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")),type="viridis")
temp1<-ncvar_get(file1, varid = "temp"); dim(temp1)
temp1<-temp1[apply(temp1, 1, Compose(is.finite, all)),]
temp1<-temp1-273.15
temp1<-melt(temp1)
plot_temp1_IMAUFDM<- ggplot(data = data.frame(temp1), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc1<- ncvar_get(file1, varid = "lwc"); dim(lwc1)
lwc1<-lwc1[apply(lwc1, 1, Compose(is.finite, all)),]
lwc1<-melt(lwc1)
plot_lwc1_IMAUFDM<- ggplot(data = data.frame(lwc1), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
depth1<- ncvar_get(file1,varid="depth"); dim(depth1)
dz1<- ncvar_get(file1, varid = "dz");dim(dz1)
rfrz1 <- ncvar_get(file1, varid = "rfrz"); dim(rfrz1)
#Total values
t_dye2_16 <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
t <- as.numeric(t_dye2_16)
file2<- nc_open("RetMIP_IMAUFDM_Dye-2_16_3hourly_values.nc")
tfac <- ncvar_get(file2, varid="tfac"); dim(tfac)
tlwc<-ncvar_get(file2, varid = "tlwc"); dim(tlwc)
trfrz<- ncvar_get(file2, varid = "trfrz"); dim(trfrz)
#Remove NA values
trfrz<-trfrz[!is.na(trfrz)]
trfrz<- trfrz[seq(1, length(trfrz), 8)]
trunoff<- ncvar_get(file2, varid = "trunoff"); dim(trunoff)
trunoff<-trunoff[!is.na(trunoff)]
trunoff<- trunoff[seq(1, length(trunoff), 8)]
plot_refreezing<- ggplot()+geom_line(aes(x=t_dye2_16, y = trfrz)) 
###KAN-U
file3<- nc_open("RetMIP_IMAUFDM_KAN-U_3hourly_columns.nc")
rho2 <- ncvar_get(file3, varid="rho")
rho2<-rho2[apply(rho2, 1, Compose(is.finite, all)),]
rho2<-melt(rho2)
plot_rho2_IMAUFDM<- ggplot(data = data.frame(rho2), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho IMAUFDM KANU")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp2<-ncvar_get(file3, varid = "temp")
temp2<-temp2[apply(temp2, 1, Compose(is.finite, all)),]
temp2<-temp2-273.15
temp2<-melt(temp2)
plot_temp2_IMAUFDM<- ggplot(data = data.frame(temp2), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc2<- ncvar_get(file3, varid = "lwc")
lwc2<-lwc2[apply(lwc2, 1, Compose(is.finite, all)),]
lwc2<-melt(lwc2)
plot_lwc2_IMAUFDM<- ggplot(data = data.frame(lwc2), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
depth2<- ncvar_get(file3,varid="depth")
dz2<- ncvar_get(file3, varid = "dz")
rfrz2 <- ncvar_get(file3, varid = "rfrz")
t_kanu<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_kanu)
##Total values
file4<- nc_open("RetMIP_IMAUFDM_KAN-U_3hourly_values.nc")
tfac_kanu <- ncvar_get(file4, varid="tfac")
tlwc_kanu<-ncvar_get(file4, varid = "tlwc")
trfrz_kanu<- ncvar_get(file4, varid = "trfrz");dim(trfrz_kanu)
#Remove NA values
trfrz_kanu<-trfrz_kanu[!is.na(trfrz_kanu)];dim(trfrz_kanu)
trfrz_kanu<- trfrz_kanu[seq(1, length(trfrz_kanu), 8)];dim(trfrz_kanu)
trunoff_kanu<- ncvar_get(file4, varid = "trunoff")
trunoff_kanu<-trunoff_kanu[!is.na(trunoff_kanu)]
trunoff_kanu<- trunoff_kanu[seq(1, length(trunoff_kanu), 8)]
plot_refreezing_kanu<- ggplot()+geom_line(aes(x=t_kanu, y = trfrz_kanu)) 
#Yearly totals for refreezing:
trfrz_kanu_IMAUFDM_yearly_total <- trfrz_kanu[seq(1, length(trfrz_kanu), 1)]
trfrz_kanu_IMAUFDM_yearly_total_1<- sum(trfrz_kanu_IMAUFDM_yearly_total[1:366])
trfrz_kanu_IMAUFDM_yearly_total_2<- sum(trfrz_kanu_IMAUFDM_yearly_total[366:731])
trfrz_kanu_IMAUFDM_yearly_total_3<- sum(trfrz_kanu_IMAUFDM_yearly_total[731:1096])
trfrz_kanu_IMAUFDM_yearly_total_4<- sum(trfrz_kanu_IMAUFDM_yearly_total[1096:1461])
trfrz_kanu_IMAUFDM_yearly_total_5<- sum(trfrz_kanu_IMAUFDM_yearly_total[1461:1706])
yearly_trfrz_kanu_IMAUFDM<-c(trfrz_kanu_IMAUFDM_yearly_total_1,trfrz_kanu_IMAUFDM_yearly_total_2,trfrz_kanu_IMAUFDM_yearly_total_3,trfrz_kanu_IMAUFDM_yearly_total_4,trfrz_kanu_IMAUFDM_yearly_total_5)
t_kanu_yearly<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="years"))
#Yearly totals for runoff
trunoff_kanu_IMAUFDM_yearly_total <- trunoff_kanu[seq(1, length(trunoff_kanu), 1)]
trunoff_kanu_IMAUFDM_yearly_total_1<- sum(trunoff_kanu_IMAUFDM_yearly_total[1:366])
trunoff_kanu_IMAUFDM_yearly_total_2<- sum(trunoff_kanu_IMAUFDM_yearly_total[366:731])
trunoff_kanu_IMAUFDM_yearly_total_3<- sum(trunoff_kanu_IMAUFDM_yearly_total[731:1096])
trunoff_kanu_IMAUFDM_yearly_total_4<- sum(trunoff_kanu_IMAUFDM_yearly_total[1096:1461])
trunoff_kanu_IMAUFDM_yearly_total_5<- sum(trunoff_kanu_IMAUFDM_yearly_total[1461:1706])
yearly_trunoff_kanu_IMAUFDM<-c(trunoff_kanu_IMAUFDM_yearly_total_1,trunoff_kanu_IMAUFDM_yearly_total_2,trunoff_kanu_IMAUFDM_yearly_total_3,trunoff_kanu_IMAUFDM_yearly_total_4,trunoff_kanu_IMAUFDM_yearly_total_5)
###SUMMIT
file5<-nc_open("RetMIP_IMAUFDM_Summit_3hourly_columns.nc")
rho3 <- ncvar_get(file5, varid="rho")
temp3<-ncvar_get(file5, varid = "temp")
temp3_2012<- temp3[33608:36528,]
temp3_2012<-temp3_2012[apply(temp3_2012, 1, Compose(is.finite, all)),]
temp3_2012<-temp3_2012-273.15
temp3_2012<-melt(temp3_2012)
plot_temp3_IMAUFDM_2012<- ggplot(data = data.frame(temp3_2012), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,1000,2000,3000),labels=c("01-Jan-2012", "01-May-2012", "01-Sep-2012","31-Dec-2012"),expand=c(0.01,0.01)) + scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(0,-20,-40,-60),labels=c("0","-20","-40","-60"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-60,0))

lwc3<- ncvar_get(file5, varid = "lwc")
lwc3<-lwc3[apply(lwc3, 1, Compose(is.finite, all)),]
lwc3<-melt(lwc3)
plot_lwc3_IMAUFDM<- ggplot(data = data.frame(lwc3), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("lwc IMAUFDM Summit")#+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,50,100,150,200),labels=c("0", "5", "10","15","20"))
depth3<- ncvar_get(file5,varid="depth")
dz3<- ncvar_get(file5, varid = "dz")
rfrz3 <- ncvar_get(file5, varid = "rfrz")
t_summit<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="days"))
as.POSIXct(t_summit)
##Total values
file6<- nc_open("RetMIP_IMAUFDM_Summit_3hourly_values.nc")
tfac_summit_IMAUFDM <- ncvar_get(file6, varid="tfac")
tlwc_summit_IMAUFDM<-ncvar_get(file6, varid = "tlwc")
trfrz_summit_IMAUFDM<- ncvar_get(file6, varid = "trfrz")
#Remove NA values
trfrz_summit_IMAUFDM<-trfrz_summit_IMAUFDM[!is.na(trfrz_summit_IMAUFDM)]
trfrz_summit_IMAUFDM<- trfrz_summit_IMAUFDM[seq(1, length(trfrz_summit_IMAUFDM), 8)]
trunoff_summit_IMAUFDM<- ncvar_get(file6, varid = "trunoff");dim(trunoff_summit_IMAUFDM)
trunoff_summit_IMAUFDM<-trunoff_summit_IMAUFDM[!is.na(trunoff_summit_IMAUFDM)];dim(trunoff_summit_IMAUFDM)
trunoff_summit_IMAUFDM<- trunoff_summit_IMAUFDM[seq(1, length(trunoff_summit_IMAUFDM), 8)]
plot_runoff_summit_IMAUFDM<- ggplot()+geom_line(aes(x=t_summit, y = trunoff_summit_IMAUFDM)) 
##Yearly values summit
trfrz_summit_IMAUFDM_yearly_total <- trfrz_summit_IMAUFDM[seq(1, length(trfrz_summit_IMAUFDM), 1)]
trfrz_summit_IMAUFDM_yearly_total_1<- sum(trfrz_summit_IMAUFDM_yearly_total[1:357])
trfrz_summit_IMAUFDM_yearly_total_2<- sum(trfrz_summit_IMAUFDM_yearly_total[357:714])
trfrz_summit_IMAUFDM_yearly_total_3<- sum(trfrz_summit_IMAUFDM_yearly_total[714:1071])
trfrz_summit_IMAUFDM_yearly_total_4<- sum(trfrz_summit_IMAUFDM_yearly_total[1071:1428])
trfrz_summit_IMAUFDM_yearly_total_5<- sum(trfrz_summit_IMAUFDM_yearly_total[1428:1785])
trfrz_summit_IMAUFDM_yearly_total_6<- sum(trfrz_summit_IMAUFDM_yearly_total[1785:2142])
trfrz_summit_IMAUFDM_yearly_total_7<- sum(trfrz_summit_IMAUFDM_yearly_total[2142:2499])
trfrz_summit_IMAUFDM_yearly_total_8<- sum(trfrz_summit_IMAUFDM_yearly_total[2499:2856])
trfrz_summit_IMAUFDM_yearly_total_9<- sum(trfrz_summit_IMAUFDM_yearly_total[2856:3213])
trfrz_summit_IMAUFDM_yearly_total_10<- sum(trfrz_summit_IMAUFDM_yearly_total[3213:3570])
trfrz_summit_IMAUFDM_yearly_total_11<- sum(trfrz_summit_IMAUFDM_yearly_total[3570:3927])
trfrz_summit_IMAUFDM_yearly_total_12<- sum(trfrz_summit_IMAUFDM_yearly_total[3927:4284])
trfrz_summit_IMAUFDM_yearly_total_13<- sum(trfrz_summit_IMAUFDM_yearly_total[4284:4641])
trfrz_summit_IMAUFDM_yearly_total_14<- sum(trfrz_summit_IMAUFDM_yearly_total[4641:4998])
trfrz_summit_IMAUFDM_yearly_total_15<- sum(trfrz_summit_IMAUFDM_yearly_total[4998:5363])
yearly_trfrz_summit_IMAUFDM<-c(trfrz_summit_IMAUFDM_yearly_total_1,trfrz_summit_IMAUFDM_yearly_total_2,trfrz_summit_IMAUFDM_yearly_total_3,trfrz_summit_IMAUFDM_yearly_total_4,trfrz_summit_IMAUFDM_yearly_total_5, trfrz_summit_IMAUFDM_yearly_total_6, trfrz_summit_IMAUFDM_yearly_total_7,trfrz_summit_IMAUFDM_yearly_total_8,trfrz_summit_IMAUFDM_yearly_total_9,trfrz_summit_IMAUFDM_yearly_total_10,trfrz_summit_IMAUFDM_yearly_total_11,trfrz_summit_IMAUFDM_yearly_total_12,trfrz_summit_IMAUFDM_yearly_total_13,trfrz_summit_IMAUFDM_yearly_total_14,trfrz_summit_IMAUFDM_yearly_total_15)
t_summit_yearly<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="1 year"))


###FA
file7<-nc_open("RetMIP_IMAUFDM_FA_3hourly_columns.nc")
rho4 <- ncvar_get(file7, varid="rho")
rho4<-rho4[apply(rho4, 1, Compose(is.finite, all)),]
rho4<-melt(rho4)
plot_rho4_IMAUFDM<- ggplot(data = data.frame(rho4), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho IMAUFDM FA")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp4<-ncvar_get(file7, varid = "temp")
temp4<-temp4[apply(temp4, 1, Compose(is.finite, all)),]
temp4<-temp4-273.15
temp4<-melt(temp4)
plot_temp4_IMAUDFM<- ggplot(data = data.frame(temp4), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc4<- ncvar_get(file7, varid = "lwc")
lwc4<-lwc4[apply(lwc4, 1, Compose(is.finite, all)),]
lwc4<-melt(lwc4)
plot_lwc4_IMAUFDM<- ggplot(data = data.frame(lwc4), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,30))
depth4<- ncvar_get(file7,varid="depth")
dz4<- ncvar_get(file7, varid = "dz")
rfrz4 <- ncvar_get(file7, varid = "rfrz")
t_FA<- (seq(as.Date("12-04-2014", "%d-%m-%Y" ),as.Date("02-12-2014", "%d-%m-%Y"), by="days"))
as.POSIXct(t_FA)
##Total values
file8<- nc_open("RetMIP_IMAUFDM_FA_3hourly_values.nc")
tfac_FA_IMAUFDM <- ncvar_get(file8, varid="tfac")
tlwc_FA_IMAUFDM<-ncvar_get(file8, varid = "tlwc")
trfrz_FA_IMAUFDM<- ncvar_get(file8, varid = "trfrz")
#Remove NA values
trfrz_FA_IMAUFDM<-trfrz_FA_IMAUFDM[!is.na(trfrz_FA_IMAUFDM)]
trfrz_FA_IMAUFDM<- trfrz_FA_IMAUFDM[seq(1, length(trfrz_FA_IMAUFDM), 8)]
trunoff_FA_IMAUFDM<- ncvar_get(file8, varid = "trunoff")
trunoff_FA_IMAUFDM<-trunoff_FA_IMAUFDM[!is.na(trunoff_FA_IMAUFDM)]
trunoff_FA_IMAUFDM<- trunoff_FA_IMAUFDM[seq(1, length(trunoff_FA_IMAUFDM), 8)]
plot_refreezing_FA_IMAUFDM<- ggplot()+geom_line(aes(x=t_FA, y = trfrz_FA_IMAUFDM)) 



