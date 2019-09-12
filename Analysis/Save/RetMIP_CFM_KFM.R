library(ncdf4)
#DYE 2_16
file1_CFM_KFM<- nc_open("RetMIP_CFM-KM_Dye2-16_3hourly_columns.nc")
rho1_CFM_KFM <- ncvar_get(file1_CFM_KFM, varid="rho")
rho1_CFM_KFM<-rho1_CFM_KFM[apply(rho1_CFM_KFM, 1, Compose(is.finite, all)),]
rho1_CFM_KFM<-melt(rho1_CFM_KFM)
plot_rho1_CFM_KFM<- ggplot(data = data.frame(rho1_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho CFM_KFM dye2_16") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp1_CFM_KFM<-ncvar_get(file1_CFM_KFM, varid = "temp");dim(temp1_CFM_KFM)
temp1_CFM_KFM<-temp1_CFM_KFM[apply(temp1_CFM_KFM, 1, Compose(is.finite, all)),]
temp1_CFM_KFM<-temp1_CFM_KFM-273.15
temp1_CFM_KFM<-melt(temp1_CFM_KFM)
plot_temp1_CFM_KFM<- ggplot(data = data.frame(temp1_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [ÂºC]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name= "Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc1_CFM_KFM<- ncvar_get(file1_CFM_KFM, varid = "lwc")
lwc1_CFM_KFM<-lwc1_CFM_KFM[apply(lwc1_CFM_KFM, 1, Compose(is.finite, all)),]
lwc1_CFM_KFM<-1000*lwc1_CFM_KFM
lwc1_CFM_KFM<-melt(lwc1_CFM_KFM)
plot_lwc1_CFM_KFM<- ggplot(data = data.frame(lwc1_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))

icecon_CFM_KFM<- ncvar_get(file1_CFM_KFM,varid="icecon")
rfrz1_CFM_KFM <- ncvar_get(file1_CFM_KFM, varid = "rfrz")
#Total values
t_dye2_16 <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_dye2_16)
file2_CFM_KFM<- nc_open("RetMIP_CFM-KM_Dye2-16_3hourly_values.nc")
tfac_CFM_KFM <- ncvar_get(file2_CFM_KFM, varid="tfac")
tlwc_CFM_KFM<-ncvar_get(file2_CFM_KFM, varid = "tlwc")
trfrz_CFM_KFM<- ncvar_get(file2_CFM_KFM, varid = "trfrz")
#Remove NA values
trfrz_CFM_KFM<-trfrz_CFM_KFM[!is.na(trfrz_CFM_KFM)]
trfrz_CFM_KFM<- trfrz_CFM_KFM[seq(1, length(trfrz_CFM_KFM), 8)]
trfrz_CFM_KFM<-1000*trfrz_CFM_KFM
trunoff_CFM_KFM<- ncvar_get(file2_CFM_KFM, varid = "trunoff")
trunoff_CFM_KFM<- ncvar_get(file2_CFM_KFM, varid = "trunoff")
trunoff_CFM_KFM<-trunoff_CFM_KFM[!is.na(trunoff_CFM_KFM)]
trunoff_CFM_KFM<- trunoff_CFM_KFM[seq(1, length(trunoff_CFM_KFM), 8)]
trunoff_CFM_KFM<-trunoff_CFM_KFM*1000
plot_refreezing_CFM_KFM_dye_16_S<- ggplot()+geom_line(aes(x=t_dye2_16, y = trfrz_CFM_KFM)) +theme_bw()


###KAN-U
file3_CFM_KFM<- nc_open("RetMIP_CFM-KM_KANU_3hourly_columns.nc")
rho2_CFM_KFM <- ncvar_get(file3_CFM_KFM, varid="rho")
rho2_CFM_KFM<-rho2_CFM_KFM[apply(rho2_CFM_KFM, 1, Compose(is.finite, all)),]
rho2_CFM_KFM<-melt(rho2_CFM_KFM)
plot_rho2_CFM_KFM<- ggplot(data = data.frame(rho2_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho CFM_KFM KANU")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp2_CFM_KFM<-ncvar_get(file3_CFM_KFM, varid = "temp")
temp2_CFM_KFM<-temp2_CFM_KFM[apply(temp2_CFM_KFM, 1, Compose(is.finite, all)),]
temp2_CFM_KFM<-temp2_CFM_KFM-273.15
temp2_CFM_KFM<-melt(temp2_CFM_KFM)
plot_temp2_CFM_KFM<- ggplot(data = data.frame(temp2_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc2_CFM_KFM<- ncvar_get(file3_CFM_KFM, varid = "lwc")
lwc2_CFM_KFM<-1000*lwc2_CFM_KFM
lwc2_CFM_KFM<-lwc2_CFM_KFM[apply(lwc2_CFM_KFM, 1, Compose(is.finite, all)),]
lwc2_CFM_KFM<-melt(lwc2_CFM_KFM)
plot_lwc2_CFM_KFM<- ggplot(data = data.frame(lwc2_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
icecon3_CFM_KFM<- ncvar_get(file3_CFM_KFM,varid="icecon")
rfrz3_CFM_KFM <- ncvar_get(file3_CFM_KFM, varid = "rfrz")
t_kanu<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_kanu)
##Total values
file4_CFM_KFM<- nc_open("RetMIP_CFM-KM_KANU_3hourly_values.nc")
tfac_kanu_CFM_KFM <- ncvar_get(file4_CFM_KFM, varid="tfac")
tlwc_kanu_CFM_KFM<-ncvar_get(file4_CFM_KFM, varid = "tlwc")
trfrz_kanu_CFM_KFM<- ncvar_get(file4_CFM_KFM, varid = "trfrz")
#Remove NA values
trfrz_kanu_CFM_KFM<-trfrz_kanu_CFM_KFM[!is.na(trfrz_kanu_CFM_KFM)]
trfrz_kanu_CFM_KFM<- trfrz_kanu_CFM_KFM[seq(1, length(trfrz_kanu_CFM_KFM), 8)]
trfrz_kanu_CFM_KFM<-trfrz_kanu_CFM_KFM*1000
trunoff_kanu_CFM_KFM<- ncvar_get(file4_CFM_KFM, varid = "trunoff")
trunoff_kanu_CFM_KFM<-trunoff_kanu_CFM_KFM[!is.na(trunoff_kanu_CFM_KFM)]
trunoff_kanu_CFM_KFM<- trunoff_kanu_CFM_KFM[seq(1, length(trunoff_kanu_CFM_KFM), 8)]
trunoff_kanu_CFM_KFM<-trunoff_kanu_CFM_KFM*1000
plot_refreezing_kanu_CFM_KFM<- ggplot()+geom_line(aes(x=t_kanu, y = trfrz_kanu_CFM_KFM)) 
#Yearly totals for refreezing:
trfrz_kanu_CFM_KFM_yearly_total <- trfrz_kanu_CFM_KFM[seq(1, length(trfrz_kanu_CFM_KFM), 1)]
trfrz_kanu_CFM_KFM_yearly_total_1<- sum(trfrz_kanu_CFM_KFM_yearly_total[1:366])
trfrz_kanu_CFM_KFM_yearly_total_2<- sum(trfrz_kanu_CFM_KFM_yearly_total[366:731])
trfrz_kanu_CFM_KFM_yearly_total_3<- sum(trfrz_kanu_CFM_KFM_yearly_total[731:1096])
trfrz_kanu_CFM_KFM_yearly_total_4<- sum(trfrz_kanu_CFM_KFM_yearly_total[1096:1461])
trfrz_kanu_CFM_KFM_yearly_total_5<- sum(trfrz_kanu_CFM_KFM_yearly_total[1461:1706])
yearly_trfrz_kanu_CFM_KFM<-c(trfrz_kanu_CFM_KFM_yearly_total_1,trfrz_kanu_CFM_KFM_yearly_total_2,trfrz_kanu_CFM_KFM_yearly_total_3,trfrz_kanu_CFM_KFM_yearly_total_4,trfrz_kanu_CFM_KFM_yearly_total_5)
t_kanu_yearly<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="years"))
#Yearly totals for runoff
trunoff_kanu_CFM_KFM_yearly_total <- trunoff_kanu_CFM_KFM[seq(1, length(trunoff_kanu_CFM_KFM), 1)]
trunoff_kanu_CFM_KFM_yearly_total_1<- sum(trunoff_kanu_CFM_KFM_yearly_total[1:366])
trunoff_kanu_CFM_KFM_yearly_total_2<- sum(trunoff_kanu_CFM_KFM_yearly_total[366:731])
trunoff_kanu_CFM_KFM_yearly_total_3<- sum(trunoff_kanu_CFM_KFM_yearly_total[731:1096])
trunoff_kanu_CFM_KFM_yearly_total_4<- sum(trunoff_kanu_CFM_KFM_yearly_total[1096:1461])
trunoff_kanu_CFM_KFM_yearly_total_5<- sum(trunoff_kanu_CFM_KFM_yearly_total[1461:1706])
yearly_trunoff_kanu_CFM_KFM<-c(trunoff_kanu_CFM_KFM_yearly_total_1,trunoff_kanu_CFM_KFM_yearly_total_2,trunoff_kanu_CFM_KFM_yearly_total_3,trunoff_kanu_CFM_KFM_yearly_total_4,trunoff_kanu_CFM_KFM_yearly_total_5)

###SUMMIT
file5_CFM_KFM<-nc_open("RetMIP_CFM-KM_Summit_3hourly_columns.nc")
rho3_CFM_KFM <- ncvar_get(file5_CFM_KFM, varid="rho")
temp3_CFM_KFM<-ncvar_get(file5_CFM_KFM, varid = "temp")
temp3_CFM_KFM_2012<- temp3_CFM_KFM[,33608:36528]
temp3_CFM_KFM_2012<-temp3_CFM_KFM_2012[apply(temp3_CFM_KFM_2012, 1, Compose(is.finite, all)),]
temp3_CFM_KFM_2012<-temp3_CFM_KFM_2012-273.15
temp3_CFM_KFM_2012<-melt(temp3_CFM_KFM_2012)
plot_temp3_CFM_KFM_2012<- ggplot(data = data.frame(temp3_CFM_KFM_2012), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,1000,2000,3000),labels=c("01-Jan-2012", "01-May-2012", "01-Sep-2012","31-Dec-2012"),expand=c(0.01,0.01)) + scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(0,-20,-40,-60),labels=c("0","-20","-40","-60"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(-60,0))

lwc3_CFM_KFM<- ncvar_get(file5_CFM_KFM, varid = "lwc")
icecon3_CFM_KFM<- ncvar_get(file5_CFM_KFM,varid="icecon")
rfrz3_CFM_KFM <- ncvar_get(file5_CFM_KFM, varid = "rfrz")
t_summit<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="days"))
as.POSIXct(t_summit)
##Total values
file6_CFM_KFM<- nc_open("RetMIP_CFM-KM_Summit_3hourly_values.nc")
tfac_summit_CFM_KFM <- ncvar_get(file6_CFM_KFM, varid="tfac")
tlwc_summit_CFM_KFM<-ncvar_get(file6_CFM_KFM, varid = "tlwc")
trfrz_summit_CFM_KFM<- ncvar_get(file6_CFM_KFM, varid = "trfrz")
#Remove NA values
trfrz_summit_CFM_KFM<-trfrz_summit_CFM_KFM[!is.na(trfrz_summit_CFM_KFM)]
trfrz_summit_CFM_KFM<- trfrz_summit_CFM_KFM[seq(1, length(trfrz_summit_CFM_KFM), 8)]
trfrz_summit_CFM_KFM<-trfrz_summit_CFM_KFM*1000
trunoff_summit_CFM_KFM<- ncvar_get(file6_CFM_KFM, varid = "trunoff")
trunoff_summit_CFM_KFM<-trunoff_summit_CFM_KFM[!is.na(trunoff_summit_CFM_KFM)];dim(trunoff_summit_CFM_KFM)
trunoff_summit_CFM_KFM<- trunoff_summit_CFM_KFM[seq(1, length(trunoff_summit_CFM_KFM), 8)]
trunoff_summit_CFM_KFM<-trunoff_summit_CFM_KFM*1000
plot_runoff_summit_CFM_KFM<- ggplot()+geom_line(aes(x=t_summit, y = trunoff_summit_CFM_KFM)) 
plot_refreezing_summit_CFM_KFM<- ggplot()+geom_line(aes(x=t_summit, y = trfrz_summit_CFM_KFM)) 
##Yearly values summit
trfrz_summit_CFM_KFM_yearly_total <- trfrz_summit_CFM_KFM[seq(1, length(trfrz_summit_CFM_KFM), 1)]
trfrz_summit_CFM_KFM_yearly_total_1<- sum(trfrz_summit_CFM_KFM_yearly_total[1:357])
trfrz_summit_CFM_KFM_yearly_total_2<- sum(trfrz_summit_CFM_KFM_yearly_total[357:714])
trfrz_summit_CFM_KFM_yearly_total_3<- sum(trfrz_summit_CFM_KFM_yearly_total[714:1071])
trfrz_summit_CFM_KFM_yearly_total_4<- sum(trfrz_summit_CFM_KFM_yearly_total[1071:1428])
trfrz_summit_CFM_KFM_yearly_total_5<- sum(trfrz_summit_CFM_KFM_yearly_total[1428:1785])
trfrz_summit_CFM_KFM_yearly_total_6<- sum(trfrz_summit_CFM_KFM_yearly_total[1785:2142])
trfrz_summit_CFM_KFM_yearly_total_7<- sum(trfrz_summit_CFM_KFM_yearly_total[2142:2499])
trfrz_summit_CFM_KFM_yearly_total_8<- sum(trfrz_summit_CFM_KFM_yearly_total[2499:2856])
trfrz_summit_CFM_KFM_yearly_total_9<- sum(trfrz_summit_CFM_KFM_yearly_total[2856:3213])
trfrz_summit_CFM_KFM_yearly_total_10<- sum(trfrz_summit_CFM_KFM_yearly_total[3213:3570])
trfrz_summit_CFM_KFM_yearly_total_11<- sum(trfrz_summit_CFM_KFM_yearly_total[3570:3927])
trfrz_summit_CFM_KFM_yearly_total_12<- sum(trfrz_summit_CFM_KFM_yearly_total[3927:4284])
trfrz_summit_CFM_KFM_yearly_total_13<- sum(trfrz_summit_CFM_KFM_yearly_total[4284:4641])
trfrz_summit_CFM_KFM_yearly_total_14<- sum(trfrz_summit_CFM_KFM_yearly_total[4641:4998])
trfrz_summit_CFM_KFM_yearly_total_15<- sum(trfrz_summit_CFM_KFM_yearly_total[4998:5363])
yearly_trfrz_summit_CFM_KFM<-c(trfrz_summit_CFM_KFM_yearly_total_1,trfrz_summit_CFM_KFM_yearly_total_2,trfrz_summit_CFM_KFM_yearly_total_3,trfrz_summit_CFM_KFM_yearly_total_4,trfrz_summit_CFM_KFM_yearly_total_5, trfrz_summit_CFM_KFM_yearly_total_6, trfrz_summit_CFM_KFM_yearly_total_7,trfrz_summit_CFM_KFM_yearly_total_8,trfrz_summit_CFM_KFM_yearly_total_9,trfrz_summit_CFM_KFM_yearly_total_10,trfrz_summit_CFM_KFM_yearly_total_11,trfrz_summit_CFM_KFM_yearly_total_12,trfrz_summit_CFM_KFM_yearly_total_13,trfrz_summit_CFM_KFM_yearly_total_14,trfrz_summit_CFM_KFM_yearly_total_15)
t_summit_yearly<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="1 year"))

###FA
file7_CFM_KFM<-nc_open("RetMIP_CFM-KM_FA_3hourly_columns.nc")
rho4_CFM_KFM <- ncvar_get(file7_CFM_KFM, varid="rho")
rho4_CFM_KFM<-rho4_CFM_KFM[apply(rho4_CFM_KFM, 1, Compose(is.finite, all)),]
rho4_CFM_KFM<-melt(rho4_CFM_KFM)
plot_rho4_CFM_KFM<- ggplot(data = data.frame(rho4_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho CFM_KFM FA")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp4_CFM_KFM<-ncvar_get(file7_CFM_KFM, varid = "temp")
temp4_CFM_KFM<-temp4_CFM_KFM[apply(temp4_CFM_KFM, 1, Compose(is.finite, all)),]
temp4_CFM_KFM<-temp4_CFM_KFM-273.15
temp4_CFM_KFM<-melt(temp4_CFM_KFM)
plot_temp4_CFM_KFM<- ggplot(data = data.frame(temp4_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc4_CFM_KFM<- ncvar_get(file7_CFM_KFM, varid = "lwc")
lwc4_CFM_KFM<-1000*lwc4_CFM_KFM
lwc4_CFM_KFM<-lwc4_CFM_KFM[apply(lwc4_CFM_KFM, 1, Compose(is.finite, all)),]
lwc4_CFM_KFM<-melt(lwc4_CFM_KFM)
plot_lwc4_CFM_KFM<- ggplot(data = data.frame(lwc4_CFM_KFM), aes(x = X2, y = -X1, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,30))
icecon4_CFM_KFM<- ncvar_get(file7_CFM_KFM,varid="icecon")
rfrz4_CFM_KFM <- ncvar_get(file7_CFM_KFM, varid = "rfrz")
t_FA<- (seq(as.Date("12-04-2014", "%d-%m-%Y" ),as.Date("02-12-2014", "%d-%m-%Y"), by="days"))
as.POSIXct(t_FA)
##Total values
file8_CFM_KFM<- nc_open("RetMIP_CFM-KM_FA_3hourly_values.nc")
tfac_FA_CFM_KFM <- ncvar_get(file8_CFM_KFM, varid="tfac")
tlwc_FA_CFM_KFM<-ncvar_get(file8_CFM_KFM, varid = "tlwc")
trfrz_FA_CFM_KFM<- ncvar_get(file8_CFM_KFM, varid = "trfrz");dim(trfrz_FA_CFM_KFM)
#Remove NA values
trfrz_FA_CFM_KFM<-trfrz_FA_CFM_KFM[!is.na(trfrz_FA_CFM_KFM)];dim(trfrz_FA_CFM_KFM)
trfrz_FA_CFM_KFM<- trfrz_FA_CFM_KFM[seq(1, length(trfrz_FA_CFM_KFM), 8)];dim(trfrz_FA_CFM_KFM)
trfrz_FA_CFM_KFM<-trfrz_FA_CFM_KFM*1000
trunoff_FA_CFM_KFM<- ncvar_get(file8_CFM_KFM, varid = "trunoff")
trunoff_FA_CFM_KFM<-trunoff_FA_CFM_KFM[!is.na(trunoff_FA_CFM_KFM)]
trunoff_FA_CFM_KFM<- trunoff_FA_CFM_KFM[seq(1, length(trunoff_FA_CFM_KFM), 8)]
trunoff_FA_CFM_KFM<-trunoff_FA_CFM_KFM*1000
plot_refreezing_FA_CFM_KFM<- ggplot()+geom_line(aes(x=t_FA, y = trfrz_FA_CFM_KFM)) 
