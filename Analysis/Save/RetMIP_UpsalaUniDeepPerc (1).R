library(ncdf4)
library(ggplot2)
library(functional)
library(reshape)
###DYE-2_16S
#Hourly averages
file1_UniDeep<- nc_open("RetMIP_UppsalaUniDeepPerc_DYE2S_3hourly_columns.nc")
rho1_UniDeep <- ncvar_get(file1_UniDeep, varid="rho")
rho1_UniDeep<-rho1_UniDeep[apply(rho1_UniDeep, 1, Compose(is.finite, all)),]
rho1_UniDeep<-melt(rho1_UniDeep)
plot_rho1_UniDeep<- ggplot(data = data.frame(rho1_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho UniDeep dye2_16")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp1_UniDeep<-ncvar_get(file1_UniDeep, varid = "temp");dim(temp1_UniDeep)
temp1_UniDeep<-temp1_UniDeep[apply(temp1_UniDeep, 1, Compose(is.finite, all)),]
temp1_UniDeep<-temp1_UniDeep-273.15
temp1_UniDeep<-melt(temp1_UniDeep)
unideep_unibucket<-temp1_UniDeep$value-temp1_UniBucket$value
unideep_unibucket_T<-temp1_UniDeep
unideep_unibucket_T$value<-unideep_unibucket
plot_temp1_UniDeep<- ggplot(data = data.frame(temp1_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis")+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
plot_dif_T_UniDeep_Unibucket<- ggplot(data = data.frame(unideep_unibucket_T), aes(x = X2, y = -X1, fill= abs(value))) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature diff [?C]")), type="viridis")+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-10,20))
unideep_geus<-temp1_UniDeep$value-temp1_geus$value
unideep_geus_T<-temp1_UniDeep
unideep_geus_T$value<-unideep_geus
plot_dif_T_UniDeep_geus<- ggplot(data = data.frame(unideep_geus_T), aes(x = X2, y = -X1, fill= abs(value))) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature diff [?C]")), type="viridis")+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-10,20))
unideep_MH<-temp1_UniDeep$value[1:287000]-temp1_MH$value
unideep_MH_T<-temp1_MH
unideep_MH_T$value<-unideep_MH
plot_dif_T_UniDeep_MH<- ggplot(data = data.frame(unideep_MH_T), aes(x = X2, y = -X1, fill= abs(value))) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature diff [?C]")), type="viridis")+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-10,20))
unideep_IMAU<-temp1_UniDeep$value-temp1$value
unideep_IMAU_T<-temp1_UniDeep
unideep_IMAU_T$value<-unideep_IMAU
plot_dif_T_UniDeep_IMAU<- ggplot(data = data.frame(unideep_IMAU_T), aes(x = X2, y = -X1, fill= abs(value))) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature diff [?C]")), type="viridis")+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-10,20))


lwc1_UniDeep<- ncvar_get(file1_UniDeep, varid = "lwc")
lwc1_UniDeep<-lwc1_UniDeep[apply(lwc1_UniDeep, 1, Compose(is.finite, all)),]
lwc1_UniDeep<-melt(lwc1_UniDeep)
plot_lwc1_UniDeep<- ggplot(data = data.frame(lwc1_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))

icecon_UniDeep<- ncvar_get(file1_UniDeep,varid="icecon")
rfrz1_UniDeep <- ncvar_get(file1_UniDeep, varid = "rfrz")
#Total values
t_dye2_16 <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
file2_UniDeep<- nc_open("RetMIP_UppsalaUniDeepPerc_DYE2S_3hourly_values.nc")
tfac_UniDeep <- ncvar_get(file2_UniDeep, varid="tfac")
tlwc_UniDeep<-ncvar_get(file2_UniDeep, varid = "tlwc")
trfrz_UniDeep<- ncvar_get(file2_UniDeep, varid = "trfrz")
#Remove NA values
trfrz_UniDeep<-trfrz_UniDeep[!is.na(trfrz_UniDeep)]
trfrz_UniDeep<- trfrz_UniDeep[seq(1, length(trfrz_UniDeep), 8)]
trunoff_UniDeep<- ncvar_get(file2_UniDeep, varid = "trunoff")

trunoff_UniDeep<-trunoff_UniDeep[!is.na(trunoff_UniDeep)]
trunoff_UniDeep<- trunoff_UniDeep[seq(1, length(trunoff_UniDeep), 8)]
plot_runoff_UniDeep_dye_16<- ggplot()+geom_line(aes(x=t_dye2_16, y = trunoff_UniDeep)) 
plot_refreezing_UniDeep_dye_16_S<- ggplot()+geom_line(aes(x=t_dye2_16, y = trfrz_UniDeep)) 

###KAN-U
file3_UniDeep<- nc_open("RetMIP_UppsalaUniDeepPerc_KANU_3hourly_columns.nc")
rho2_UniDeep <- ncvar_get(file3_UniDeep, varid="rho")
rho2_UniDeep<-rho2_UniDeep[apply(rho2_UniDeep, 1, Compose(is.finite, all)),]
rho2_UniDeep<-melt(rho2_UniDeep)
plot_rho2_UniDeep<- ggplot(data = data.frame(rho2_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho UniDeep KANU")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp2_UniDeep<-ncvar_get(file3_UniDeep, varid = "temp")
temp2_UniDeep<-temp2_UniDeep[apply(temp2_UniDeep, 1, Compose(is.finite, all)),]
temp2_UniDeep<-temp2_UniDeep-273.15
temp2_UniDeep<-melt(temp2_UniDeep)
plot_temp2_UniDeep<- ggplot(data = data.frame(temp2_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc2_UniDeep<- ncvar_get(file3_UniDeep, varid = "lwc")
lwc2_UniDeep<-lwc2_UniDeep[apply(lwc2_UniDeep, 1, Compose(is.finite, all)),]
lwc2_UniDeep<-melt(lwc2_UniDeep)
plot_lwc2_UniDeep<- ggplot(data = data.frame(lwc2_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
icecon3_UniDeep<- ncvar_get(file3_UniDeep,varid="icecon")
rfrz3_UniDeep <- ncvar_get(file3_UniDeep, varid = "rfrz")
t_kanu<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_kanu)
##Total values
file4_UniDeep<- nc_open("RetMIP_UppsalaUniDeepPerc_KANU_3hourly_values.nc")
tfac_kanu_UniDeep <- ncvar_get(file4_UniDeep, varid="tfac")
tlwc_kanu_UniDeep<-ncvar_get(file4_UniDeep, varid = "tlwc")
trfrz_kanu_UniDeep<- ncvar_get(file4_UniDeep, varid = "trfrz")
#Remove NA values
trfrz_kanu_UniDeep<-trfrz_kanu_UniDeep[!is.na(trfrz_kanu_UniDeep)]
trfrz_kanu_UniDeep<- trfrz_kanu_UniDeep[seq(1, length(trfrz_kanu_UniDeep), 8)]
trunoff_kanu_UniDeep<- ncvar_get(file4_UniDeep, varid = "trunoff")
trunoff_kanu_UniDeep<-trunoff_kanu_UniDeep[!is.na(trunoff_kanu_UniDeep)]
trunoff_kanu_UniDeep<- trunoff_kanu_UniDeep[seq(1, length(trunoff_kanu_UniDeep), 8)]
plot_refreezing_kanu_UniDeep<- ggplot()+geom_line(aes(x=t_kanu, y = trfrz_kanu_UniDeep)) 
#Yearly totals for refreezing:
trfrz_kanu_UniDeep_yearly_total <- trfrz_kanu_UniDeep[seq(1, length(trfrz_kanu_UniDeep), 1)]
trfrz_kanu_UniDeep_yearly_total_1<- sum(trfrz_kanu_UniDeep_yearly_total[1:366])
trfrz_kanu_UniDeep_yearly_total_2<- sum(trfrz_kanu_UniDeep_yearly_total[366:731])
trfrz_kanu_UniDeep_yearly_total_3<- sum(trfrz_kanu_UniDeep_yearly_total[731:1096])
trfrz_kanu_UniDeep_yearly_total_4<- sum(trfrz_kanu_UniDeep_yearly_total[1096:1461])
trfrz_kanu_UniDeep_yearly_total_5<- sum(trfrz_kanu_UniDeep_yearly_total[1461:1706])
yearly_trfrz_kanu_UniDeep<-c(trfrz_kanu_UniDeep_yearly_total_1,trfrz_kanu_UniDeep_yearly_total_2,trfrz_kanu_UniDeep_yearly_total_3,trfrz_kanu_UniDeep_yearly_total_4,trfrz_kanu_UniDeep_yearly_total_5)
t_kanu_yearly<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="years"))
#Yearly totals for runoff
trunoff_kanu_UniDeep_yearly_total <- trunoff_kanu_UniDeep[seq(1, length(trunoff_kanu_UniDeep), 1)]
trunoff_kanu_UniDeep_yearly_total_1<- sum(trunoff_kanu_UniDeep_yearly_total[1:366])
trunoff_kanu_UniDeep_yearly_total_2<- sum(trunoff_kanu_UniDeep_yearly_total[366:731])
trunoff_kanu_UniDeep_yearly_total_3<- sum(trunoff_kanu_UniDeep_yearly_total[731:1096])
trunoff_kanu_UniDeep_yearly_total_4<- sum(trunoff_kanu_UniDeep_yearly_total[1096:1461])
trunoff_kanu_UniDeep_yearly_total_5<- sum(trunoff_kanu_UniDeep_yearly_total[1461:1706])
yearly_trunoff_kanu_UniDeep<-c(trunoff_kanu_UniDeep_yearly_total_1,trunoff_kanu_UniDeep_yearly_total_2,trunoff_kanu_UniDeep_yearly_total_3,trunoff_kanu_UniDeep_yearly_total_4,trunoff_kanu_UniDeep_yearly_total_5)
###SUMMIT
file5_UniDeep<-nc_open("RetMIP_UppsalaUniDeepPerc_SUMMIT_3hourly_columns.nc")
rho3_UniDeep <- ncvar_get(file5_UniDeep, varid="rho")
temp3_UniDeep<-ncvar_get(file5_UniDeep, varid = "temp")
temp3_UniDeep_2012<- temp3_UniDeep[,33608:36528]
temp3_UniDeep_2012<-temp3_UniDeep_2012[apply(temp3_UniDeep_2012, 1, Compose(is.finite, all)),]
temp3_UniDeep_2012<-temp3_UniDeep_2012-273.15
temp3_UniDeep_2012<-melt(temp3_UniDeep_2012)
plot_temp3_UniDeep_2012<- ggplot(data = data.frame(temp3_UniDeep_2012), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,1000,2000,3000),labels=c("01-Jan-2012", "01-May-2012", "01-Sep-2012","31-Dec-2012"),expand=c(0.01,0.01)) + scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(0,-20,-40,-60),labels=c("0","-20","-40","-60"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-60,0))

lwc3_UniDeep<- ncvar_get(file5_UniDeep, varid = "lwc")
icecon3_UniDeep<- ncvar_get(file5_UniDeep,varid="icecon")
rfrz3_UniDeep <- ncvar_get(file5_UniDeep, varid = "rfrz")
t_summit<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="days"))
as.POSIXct(t_summit)
##Total values
file6_UniDeep<- nc_open("RetMIP_UppsalaUniDeepPerc_SUMMIT_3hourly_values.nc")
tfac_summit_UniDeep <- ncvar_get(file6_UniDeep, varid="tfac")
tlwc_summit_UniDeep<-ncvar_get(file6_UniDeep, varid = "tlwc")
trfrz_summit_UniDeep<- ncvar_get(file6_UniDeep, varid = "trfrz")
#Remove NA values
trfrz_summit_UniDeep<-trfrz_summit_UniDeep[!is.na(trfrz_summit_UniDeep)]
trfrz_summit_UniDeep<- trfrz_summit_UniDeep[seq(1, length(trfrz_summit_UniDeep), 8)]
trunoff_summit_UniDeep<- ncvar_get(file6_UniDeep, varid = "trunoff")
trunoff_summit_UniDeep<-trunoff_summit_UniDeep[!is.na(trunoff_summit_UniDeep)];dim(trunoff_summit_UniDeep)
trunoff_summit_UniDeep<- trunoff_summit_UniDeep[seq(1, length(trunoff_summit_UniDeep), 8)]
plot_runoff_summit_UniDeep<- ggplot()+geom_line(aes(x=t_summit, y = trunoff_summit_UniDeep)) 
plot_refreezing_summit_UniDeep<- ggplot()+geom_line(aes(x=t_summit, y = trfrz_summit_UniDeep)) 
##Yearly values summit
trfrz_summit_UniDeep_yearly_total <- trfrz_summit_UniDeep[seq(1, length(trfrz_summit_UniDeep), 1)]
trfrz_summit_UniDeep_yearly_total_1<- sum(trfrz_summit_UniDeep_yearly_total[1:357])
trfrz_summit_UniDeep_yearly_total_2<- sum(trfrz_summit_UniDeep_yearly_total[357:714])
trfrz_summit_UniDeep_yearly_total_3<- sum(trfrz_summit_UniDeep_yearly_total[714:1071])
trfrz_summit_UniDeep_yearly_total_4<- sum(trfrz_summit_UniDeep_yearly_total[1071:1428])
trfrz_summit_UniDeep_yearly_total_5<- sum(trfrz_summit_UniDeep_yearly_total[1428:1785])
trfrz_summit_UniDeep_yearly_total_6<- sum(trfrz_summit_UniDeep_yearly_total[1785:2142])
trfrz_summit_UniDeep_yearly_total_7<- sum(trfrz_summit_UniDeep_yearly_total[2142:2499])
trfrz_summit_UniDeep_yearly_total_8<- sum(trfrz_summit_UniDeep_yearly_total[2499:2856])
trfrz_summit_UniDeep_yearly_total_9<- sum(trfrz_summit_UniDeep_yearly_total[2856:3213])
trfrz_summit_UniDeep_yearly_total_10<- sum(trfrz_summit_UniDeep_yearly_total[3213:3570])
trfrz_summit_UniDeep_yearly_total_11<- sum(trfrz_summit_UniDeep_yearly_total[3570:3927])
trfrz_summit_UniDeep_yearly_total_12<- sum(trfrz_summit_UniDeep_yearly_total[3927:4284])
trfrz_summit_UniDeep_yearly_total_13<- sum(trfrz_summit_UniDeep_yearly_total[4284:4641])
trfrz_summit_UniDeep_yearly_total_14<- sum(trfrz_summit_UniDeep_yearly_total[4641:4998])
trfrz_summit_UniDeep_yearly_total_15<- sum(trfrz_summit_UniDeep_yearly_total[4998:5363])
yearly_trfrz_summit_UniDeep<-c(trfrz_summit_UniDeep_yearly_total_1,trfrz_summit_UniDeep_yearly_total_2,trfrz_summit_UniDeep_yearly_total_3,trfrz_summit_UniDeep_yearly_total_4,trfrz_summit_UniDeep_yearly_total_5, trfrz_summit_UniDeep_yearly_total_6, trfrz_summit_UniDeep_yearly_total_7,trfrz_summit_UniDeep_yearly_total_8,trfrz_summit_UniDeep_yearly_total_9,trfrz_summit_UniDeep_yearly_total_10,trfrz_summit_UniDeep_yearly_total_11,trfrz_summit_UniDeep_yearly_total_12,trfrz_summit_UniDeep_yearly_total_13,trfrz_summit_UniDeep_yearly_total_14,trfrz_summit_UniDeep_yearly_total_15)
t_summit_yearly<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="1 year"))

###FA
file7_UniDeep<-nc_open("RetMIP_UppsalaUniDeepPerc_FA_3hourly_columns.nc")
rho4_UniDeep <- ncvar_get(file7_UniDeep, varid="rho")
rho4_UniDeep<-rho4_UniDeep[apply(rho4_UniDeep, 1, Compose(is.finite, all)),]
rho4_UniDeep<-melt(rho4_UniDeep)
plot_rho4_UniDeep<- ggplot(data = data.frame(rho4_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho UniDeep FA")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp4_UniDeep<-ncvar_get(file7_UniDeep, varid = "temp")
temp4_UniDeep<-temp4_UniDeep[apply(temp4_UniDeep, 1, Compose(is.finite, all)),]
temp4_UniDeep<-temp4_UniDeep-273.15
temp4_UniDeep<-melt(temp4_UniDeep)
plot_temp4_UniDeep<- ggplot(data = data.frame(temp4_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc4_UniDeep<- ncvar_get(file7_UniDeep, varid = "lwc")
lwc4_UniDeep<-lwc4_UniDeep[apply(lwc4_UniDeep, 1, Compose(is.finite, all)),]
lwc4_UniDeep<-melt(lwc4_UniDeep)
plot_lwc4_UniDeep<- ggplot(data = data.frame(lwc4_UniDeep), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,30))
icecon4_UniDeep<- ncvar_get(file7_UniDeep,varid="icecon")
rfrz4_UniDeep <- ncvar_get(file7_UniDeep, varid = "rfrz")
t_FA<- (seq(as.Date("12-04-2014", "%d-%m-%Y" ),as.Date("02-12-2014", "%d-%m-%Y"), by="days"))
as.POSIXct(t_FA)
##Total values
file8_UniDeep<- nc_open("RetMIP_UppsalaUniDeepPerc_FA_3hourly_values.nc")
tfac_FA_UniDeep <- ncvar_get(file8_UniDeep, varid="tfac")
tlwc_FA_UniDeep<-ncvar_get(file8_UniDeep, varid = "tlwc")
trfrz_FA_UniDeep<- ncvar_get(file8_UniDeep, varid = "trfrz")
#Remove NA values
trfrz_FA_UniDeep<-trfrz_FA_UniDeep[!is.na(trfrz_FA_UniDeep)]
trfrz_FA_UniDeep<- trfrz_FA_UniDeep[seq(1, length(trfrz_FA_UniDeep), 8)]
trunoff_FA_UniDeep<- ncvar_get(file8_UniDeep, varid = "trunoff")
trunoff_FA_UniDeep<-trunoff_FA_UniDeep[!is.na(trunoff_FA_UniDeep)]
trunoff_FA_UniDeep<- trunoff_FA_UniDeep[seq(1, length(trunoff_FA_UniDeep), 8)]
plot_refreezing_FA_UniDeep<- ggplot()+geom_line(aes(x=t_FA, y = trfrz_FA_UniDeep))

