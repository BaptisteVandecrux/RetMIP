###DYE-2_16S
#Hourly averages
library(ncdf4)

file1_UniBucket<- nc_open("RetMIP_UppsalaUniBucket_DYE2S_3hourly_columns.nc")
rho1_UniBucket <- ncvar_get(file1_UniBucket, varid="rho")
rho1_UniBucket<-rho1_UniBucket[apply(rho1_UniBucket, 1, Compose(is.finite, all)),]
rho1_UniBucket<-melt(rho1_UniBucket)
plot_rho1_UniBucket<- ggplot(data = data.frame(rho1_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho UniBucket dye2_16") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp1_UniBucket<-ncvar_get(file1_UniBucket, varid = "temp")
temp1_UniBucket<-temp1_UniBucket[apply(temp1_UniBucket, 1, Compose(is.finite, all)),]
temp1_UniBucket<-temp1_UniBucket-273.15
temp1_UniBucket<-melt(temp1_UniBucket)
plot_temp1_UniBucket<- ggplot(data = data.frame(temp1_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"),expand=c(0.01,0.01))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc1_UniBucket<- ncvar_get(file1_UniBucket, varid = "lwc")
lwc1_UniBucket<-lwc1_UniBucket[apply(lwc1_UniBucket, 1, Compose(is.finite, all)),]
lwc1_UniBucket<-melt(lwc1_UniBucket)
plot_lwc1_UniBucket<- ggplot(data = data.frame(lwc1_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))

icecon_UniBucket<- ncvar_get(file1_UniBucket,varid="icecon")
rfrz1_UniBucket <- ncvar_get(file1_UniBucket, varid = "rfrz")
#Total values
t_dye2_16 <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_dye2_16)
file2_UniBucket<- nc_open("RetMIP_UppsalaUniBucket_DYE2S_3hourly_values.nc")
tfac_UniBucket <- ncvar_get(file2_UniBucket, varid="tfac")
tlwc_UniBucket<-ncvar_get(file2_UniBucket, varid = "tlwc")
trfrz_UniBucket<- ncvar_get(file2_UniBucket, varid = "trfrz")
#Remove NA values
trfrz_UniBucket<-trfrz_UniBucket[!is.na(trfrz_UniBucket)]
trfrz_UniBucket<- trfrz_UniBucket[seq(1, length(trfrz_UniBucket), 8)]
trunoff_UniBucket<- ncvar_get(file2_UniBucket, varid = "trunoff")
trunoff_UniBucket<- ncvar_get(file2_UniBucket, varid = "trunoff")
trunoff_UniBucket<-trunoff_UniBucket[!is.na(trunoff_UniBucket)]
trunoff_UniBucket<- trunoff_UniBucket[seq(1, length(trunoff_UniBucket), 8)]
plot_refreezing_UniBucket_dye_16_S<- ggplot()+geom_line(aes(x=t_dye2_16, y = trfrz_UniBucket)) 


###KAN-U
file3_UniBucket<- nc_open("RetMIP_UppsalaUniBucket_KANU_3hourly_columns.nc")
rho2_UniBucket <- ncvar_get(file3_UniBucket, varid="rho")
rho2_UniBucket<-rho2_UniBucket[apply(rho2_UniBucket, 1, Compose(is.finite, all)),]
rho2_UniBucket<-melt(rho2_UniBucket)
plot_rho2_UniBucket<- ggplot(data = data.frame(rho2_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho UniBucket KANU")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp2_UniBucket<-ncvar_get(file3_UniBucket, varid = "temp")
temp2_UniBucket<-temp2_UniBucket[apply(temp2_UniBucket, 1, Compose(is.finite, all)),]
temp2_UniBucket<-temp2_UniBucket-273.15
temp2_UniBucket<-melt(temp2_UniBucket)
plot_temp2_UniBucket<- ggplot(data = data.frame(temp2_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc2_UniBucket<- ncvar_get(file3_UniBucket, varid = "lwc")
lwc2_UniBucket<-lwc2_UniBucket[apply(lwc2_UniBucket, 1, Compose(is.finite, all)),]
lwc2_UniBucket<-melt(lwc2_UniBucket)
plot_lwc2_UniBucket<- ggplot(data = data.frame(lwc2_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
icecon3_UniBucket<- ncvar_get(file3_UniBucket,varid="icecon")
rfrz3_UniBucket <- ncvar_get(file3_UniBucket, varid = "rfrz")
t_kanu<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_kanu)
##Total values
file4_UniBucket<- nc_open("RetMIP_UppsalaUniBucket_KANU_3hourly_values.nc")
tfac_kanu_UniBucket <- ncvar_get(file4_UniBucket, varid="tfac")
tlwc_kanu_UniBucket<-ncvar_get(file4_UniBucket, varid = "tlwc")
trfrz_kanu_UniBucket<- ncvar_get(file4_UniBucket, varid = "trfrz")
#Remove NA values
trfrz_kanu_UniBucket<-trfrz_kanu_UniBucket[!is.na(trfrz_kanu_UniBucket)]
trfrz_kanu_UniBucket<- trfrz_kanu_UniBucket[seq(1, length(trfrz_kanu_UniBucket), 8)]
trunoff_kanu_UniBucket<- ncvar_get(file4_UniBucket, varid = "trunoff")
trunoff_kanu_UniBucket<-trunoff_kanu_UniBucket[!is.na(trunoff_kanu_UniBucket)]
trunoff_kanu_UniBucket<- trunoff_kanu_UniBucket[seq(1, length(trunoff_kanu_UniBucket), 8)]
plot_refreezing_kanu_UniBucket<- ggplot()+geom_line(aes(x=t_kanu, y = trfrz_kanu_UniBucket)) 
#Yearly totals for refreezing:
trfrz_kanu_UniBucket_yearly_total <- trfrz_kanu_UniBucket[seq(1, length(trfrz_kanu_UniBucket), 1)]
trfrz_kanu_UniBucket_yearly_total_1<- sum(trfrz_kanu_UniBucket_yearly_total[1:366])
trfrz_kanu_UniBucket_yearly_total_2<- sum(trfrz_kanu_UniBucket_yearly_total[366:731])
trfrz_kanu_UniBucket_yearly_total_3<- sum(trfrz_kanu_UniBucket_yearly_total[731:1096])
trfrz_kanu_UniBucket_yearly_total_4<- sum(trfrz_kanu_UniBucket_yearly_total[1096:1461])
trfrz_kanu_UniBucket_yearly_total_5<- sum(trfrz_kanu_UniBucket_yearly_total[1461:1706])
yearly_trfrz_kanu_UniBucket<-c(trfrz_kanu_UniBucket_yearly_total_1,trfrz_kanu_UniBucket_yearly_total_2,trfrz_kanu_UniBucket_yearly_total_3,trfrz_kanu_UniBucket_yearly_total_4,trfrz_kanu_UniBucket_yearly_total_5)
t_kanu_yearly<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="years"))
#Yearly totals for runoff
trunoff_kanu_UniBucket_yearly_total <- trunoff_kanu_UniBucket[seq(1, length(trunoff_kanu_UniBucket), 1)]
trunoff_kanu_UniBucket_yearly_total_1<- sum(trunoff_kanu_UniBucket_yearly_total[1:366])
trunoff_kanu_UniBucket_yearly_total_2<- sum(trunoff_kanu_UniBucket_yearly_total[366:731])
trunoff_kanu_UniBucket_yearly_total_3<- sum(trunoff_kanu_UniBucket_yearly_total[731:1096])
trunoff_kanu_UniBucket_yearly_total_4<- sum(trunoff_kanu_UniBucket_yearly_total[1096:1461])
trunoff_kanu_UniBucket_yearly_total_5<- sum(trunoff_kanu_UniBucket_yearly_total[1461:1706])
yearly_trunoff_kanu_UniBucket<-c(trunoff_kanu_UniBucket_yearly_total_1,trunoff_kanu_UniBucket_yearly_total_2,trunoff_kanu_UniBucket_yearly_total_3,trunoff_kanu_UniBucket_yearly_total_4,trunoff_kanu_UniBucket_yearly_total_5)

###SUMMIT
file5_UniBucket<-nc_open("RetMIP_UppsalaUniBucket_SUMMIT_3hourly_columns.nc")
rho3_UniBucket <- ncvar_get(file5_UniBucket, varid="rho")
temp3_UniBucket<-ncvar_get(file5_UniBucket, varid = "temp")
temp3_UniBucket_2012<- temp3_UniBucket[,33608:36528]
temp3_UniBucket_2012<-temp3_UniBucket_2012[apply(temp3_UniBucket_2012, 1, Compose(is.finite, all)),]
temp3_UniBucket_2012<-temp3_UniBucket_2012-273.15
temp3_UniBucket_2012<-melt(temp3_UniBucket_2012)
plot_temp3_UniBucket_2012<- ggplot(data = data.frame(temp3_UniBucket_2012), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,1000,2000,3000),labels=c("01-Jan-2012", "01-May-2012", "01-Sep-2012","31-Dec-2012"),expand=c(0.01,0.01)) + scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(0,-20,-40,-60),labels=c("0","-20","-40","-60"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-60,0))

lwc3_UniBucket<- ncvar_get(file5_UniBucket, varid = "lwc")
icecon3_UniBucket<- ncvar_get(file5_UniBucket,varid="icecon")
rfrz3_UniBucket <- ncvar_get(file5_UniBucket, varid = "rfrz")
t_summit<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="days"))
as.POSIXct(t_summit)
##Total values
file6_UniBucket<- nc_open("RetMIP_UppsalaUniBucket_SUMMIT_3hourly_values.nc")
tfac_summit_UniBucket <- ncvar_get(file6_UniBucket, varid="tfac")
tlwc_summit_UniBucket<-ncvar_get(file6_UniBucket, varid = "tlwc")
trfrz_summit_UniBucket<- ncvar_get(file6_UniBucket, varid = "trfrz")
#Remove NA values
trfrz_summit_UniBucket<-trfrz_summit_UniBucket[!is.na(trfrz_summit_UniBucket)]
trfrz_summit_UniBucket<- trfrz_summit_UniBucket[seq(1, length(trfrz_summit_UniBucket), 8)]
trunoff_summit_UniBucket<- ncvar_get(file6_UniBucket, varid = "trunoff")
trunoff_summit_UniBucket<-trunoff_summit_UniBucket[!is.na(trunoff_summit_UniBucket)];dim(trunoff_summit_UniBucket)
trunoff_summit_UniBucket<- trunoff_summit_UniBucket[seq(1, length(trunoff_summit_UniBucket), 8)]
plot_runoff_summit_UniBucket<- ggplot()+geom_line(aes(x=t_summit, y = trunoff_summit_UniBucket)) 
plot_refreezing_summit_UniBucket<- ggplot()+geom_line(aes(x=t_summit, y = trfrz_summit_UniBucket)) 
##Yearly values summit
trfrz_summit_UniBucket_yearly_total <- trfrz_summit_UniBucket[seq(1, length(trfrz_summit_UniBucket), 1)]
trfrz_summit_UniBucket_yearly_total_1<- sum(trfrz_summit_UniBucket_yearly_total[1:357])
trfrz_summit_UniBucket_yearly_total_2<- sum(trfrz_summit_UniBucket_yearly_total[357:714])
trfrz_summit_UniBucket_yearly_total_3<- sum(trfrz_summit_UniBucket_yearly_total[714:1071])
trfrz_summit_UniBucket_yearly_total_4<- sum(trfrz_summit_UniBucket_yearly_total[1071:1428])
trfrz_summit_UniBucket_yearly_total_5<- sum(trfrz_summit_UniBucket_yearly_total[1428:1785])
trfrz_summit_UniBucket_yearly_total_6<- sum(trfrz_summit_UniBucket_yearly_total[1785:2142])
trfrz_summit_UniBucket_yearly_total_7<- sum(trfrz_summit_UniBucket_yearly_total[2142:2499])
trfrz_summit_UniBucket_yearly_total_8<- sum(trfrz_summit_UniBucket_yearly_total[2499:2856])
trfrz_summit_UniBucket_yearly_total_9<- sum(trfrz_summit_UniBucket_yearly_total[2856:3213])
trfrz_summit_UniBucket_yearly_total_10<- sum(trfrz_summit_UniBucket_yearly_total[3213:3570])
trfrz_summit_UniBucket_yearly_total_11<- sum(trfrz_summit_UniBucket_yearly_total[3570:3927])
trfrz_summit_UniBucket_yearly_total_12<- sum(trfrz_summit_UniBucket_yearly_total[3927:4284])
trfrz_summit_UniBucket_yearly_total_13<- sum(trfrz_summit_UniBucket_yearly_total[4284:4641])
trfrz_summit_UniBucket_yearly_total_14<- sum(trfrz_summit_UniBucket_yearly_total[4641:4998])
trfrz_summit_UniBucket_yearly_total_15<- sum(trfrz_summit_UniBucket_yearly_total[4998:5363])
yearly_trfrz_summit_UniBucket<-c(trfrz_summit_UniBucket_yearly_total_1,trfrz_summit_UniBucket_yearly_total_2,trfrz_summit_UniBucket_yearly_total_3,trfrz_summit_UniBucket_yearly_total_4,trfrz_summit_UniBucket_yearly_total_5, trfrz_summit_UniBucket_yearly_total_6, trfrz_summit_UniBucket_yearly_total_7,trfrz_summit_UniBucket_yearly_total_8,trfrz_summit_UniBucket_yearly_total_9,trfrz_summit_UniBucket_yearly_total_10,trfrz_summit_UniBucket_yearly_total_11,trfrz_summit_UniBucket_yearly_total_12,trfrz_summit_UniBucket_yearly_total_13,trfrz_summit_UniBucket_yearly_total_14,trfrz_summit_UniBucket_yearly_total_15)
t_summit_yearly<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="1 year"))

###FA
file7_UniBucket<-nc_open("RetMIP_UppsalaUniBucket_FA_3hourly_columns.nc")
rho4_UniBucket <- ncvar_get(file7_UniBucket, varid="rho")
rho4_UniBucket<-rho4_UniBucket[apply(rho4_UniBucket, 1, Compose(is.finite, all)),]
rho4_UniBucket<-melt(rho4_UniBucket)
plot_rho4_UniBucket<- ggplot(data = data.frame(rho4_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho UniBucket FA")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp4_UniBucket<-ncvar_get(file7_UniBucket, varid = "temp")
temp4_UniBucket<-temp4_UniBucket[apply(temp4_UniBucket, 1, Compose(is.finite, all)),]
temp4_UniBucket<-temp4_UniBucket-273.15
temp4_UniBucket<-melt(temp4_UniBucket)
plot_temp4_UniBucket<- ggplot(data = data.frame(temp4_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc4_UniBucket<- ncvar_get(file7_UniBucket, varid = "lwc")
lwc4_UniBucket<-lwc4_UniBucket[apply(lwc4_UniBucket, 1, Compose(is.finite, all)),]
lwc4_UniBucket<-melt(lwc4_UniBucket)
plot_lwc4_UniBucket<- ggplot(data = data.frame(lwc4_UniBucket), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,30))
icecon4_UniBucket<- ncvar_get(file7_UniBucket,varid="icecon")
rfrz4_UniBucket <- ncvar_get(file7_UniBucket, varid = "rfrz")
t_FA<- (seq(as.Date("12-04-2014", "%d-%m-%Y" ),as.Date("02-12-2014", "%d-%m-%Y"), by="days"))
as.POSIXct(t_FA)
##Total values
file8_UniBucket<- nc_open("RetMIP_UppsalaUniBucket_FA_3hourly_values.nc")
tfac_FA_UniBucket <- ncvar_get(file8_UniBucket, varid="tfac")
tlwc_FA_UniBucket<-ncvar_get(file8_UniBucket, varid = "tlwc")
trfrz_FA_UniBucket<- ncvar_get(file8_UniBucket, varid = "trfrz")
#Remove NA values
trfrz_FA_UniBucket<-trfrz_FA_UniBucket[!is.na(trfrz_FA_UniBucket)]
trfrz_FA_UniBucket<- trfrz_FA_UniBucket[seq(1, length(trfrz_FA_UniBucket), 8)]
trunoff_FA_UniBucket<- ncvar_get(file8_UniBucket, varid = "trunoff")
trunoff_FA_UniBucket<-trunoff_FA_UniBucket[!is.na(trunoff_FA_UniBucket)]
trunoff_FA_UniBucket<- trunoff_FA_UniBucket[seq(1, length(trunoff_FA_UniBucket), 8)]
plot_refreezing_FA_UniBucket<- ggplot()+geom_line(aes(x=t_FA, y = trfrz_FA_UniBucket)) 

