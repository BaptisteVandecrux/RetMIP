file1_DTU<- nc_open("RetMIP_DTU_Dye-2_16_3hourly_columns.nc")
rho1_DTU <- ncvar_get(file1_DTU, varid="rho")
rho1_DTU<-rho1_DTU[apply(rho1_DTU, 1, Compose(is.finite, all)),]
rho1_DTU<-melt(rho1_DTU)
plot_rho1_DTU<- ggplot(data = data.frame(rho1_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho DTU dye2_16") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp1_DTU<-ncvar_get(file1_DTU, varid = "temp");dim(temp1_DTU)
temp1_DTU<-temp1_DTU[apply(temp1_DTU, 1, Compose(is.finite, all)),]
temp1_DTU<-temp1_DTU-273.15
temp1_DTU<-melt(temp1_DTU)
plot_temp1_DTU<- ggplot(data = data.frame(temp1_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc1_DTU<- ncvar_get(file1_DTU, varid = "lwc")
lwc1_DTU<-1000*lwc1_DTU
lwc1_DTU<-lwc1_DTU[apply(lwc1_DTU, 1, Compose(is.finite, all)),]
lwc1_DTU<-melt(lwc1_DTU)
plot_lwc1_DTU<- ggplot(data = data.frame(lwc1_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1436),labels=c("02-May-2016", "02-Jul-2016", "02-Sep-2016","28-Oct-2016"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))

icecon_DTU<- ncvar_get(file1_DTU,varid="icecon")
rfrz1_DTU <- ncvar_get(file1_DTU, varid = "rfrz")
#Total values
t_dye2_16 <- (seq(as.Date("02-05-2016", "%d-%m-%Y" ),as.Date("28-10-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_dye2_16)
file2_DTU<- nc_open("RetMIP_DTU_Dye-2_16_3hourly_values.nc")
tfac_DTU <- ncvar_get(file2_DTU, varid="tfac")
tlwc_DTU<-ncvar_get(file2_DTU, varid = "tlwc")
trfrz_DTU<- ncvar_get(file2_DTU, varid = "trfrz")

#Remove NA values
trfrz_DTU<-trfrz_DTU[!is.na(trfrz_DTU)]
trfrz_DTU<- trfrz_DTU[seq(1, length(trfrz_DTU), 8)]
trfrz_DTU<-trfrz_DTU*1000
trunoff_DTU<- ncvar_get(file2_DTU, varid = "trunoff")
trunoff_DTU<- ncvar_get(file2_DTU, varid = "trunoff")
trunoff_DTU<-trunoff_DTU[!is.na(trunoff_DTU)]
trunoff_DTU<- trunoff_DTU[seq(1, length(trunoff_DTU), 8)]
trunoff_DTU<-trunoff_DTU*1000
plot_refreezing_DTU_dye_16_S<- ggplot()+geom_line(aes(x=t_dye2_16, y = trfrz_DTU)) 


###KAN-U
file3_DTU<- nc_open("RetMIP_DTU_KAN-U_3hourly_columns.nc")
rho2_DTU <- ncvar_get(file3_DTU, varid="rho")
rho2_DTU<-rho2_DTU[apply(rho2_DTU, 1, Compose(is.finite, all)),]
rho2_DTU<-melt(rho2_DTU)
plot_rho2_DTU<- ggplot(data = data.frame(rho2_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho DTU KANU")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp2_DTU<-ncvar_get(file3_DTU, varid = "temp")
temp2_DTU<-temp2_DTU[apply(temp2_DTU, 1, Compose(is.finite, all)),]
temp2_DTU<-temp2_DTU-273.15
temp2_DTU<-melt(temp2_DTU)
plot_temp2_DTU<- ggplot(data = data.frame(temp2_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc2_DTU<- ncvar_get(file3_DTU, varid = "lwc")
lwc2_DTU<-1000*lwc2_DTU
lwc2_DTU<-lwc2_DTU[apply(lwc2_DTU, 1, Compose(is.finite, all)),]
lwc2_DTU<-melt(lwc2_DTU)
plot_lwc2_DTU<- ggplot(data = data.frame(lwc2_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")+ scale_x_continuous(breaks=c(0,5000,10000),labels=c("01-May-2012", "28-Dec-2013", "01-May-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-10,-20,-30,-40,-50,-60),labels=c("0","1","2","3","4", "5","6"),limits = c(-60,NA),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,5))
icecon3_DTU<- ncvar_get(file3_DTU,varid="icecon")
rfrz3_DTU <- ncvar_get(file3_DTU, varid = "rfrz")
t_kanu<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="days"))
as.POSIXct(t_kanu)
##Total values
file4_DTU<- nc_open("RetMIP_DTU_KAN-U_3hourly_values.nc")
tfac_kanu_DTU <- ncvar_get(file4_DTU, varid="tfac")
tlwc_kanu_DTU<-ncvar_get(file4_DTU, varid = "tlwc")
trfrz_kanu_DTU<- ncvar_get(file4_DTU, varid = "trfrz");dim(trfrz_kanu_DTU)
#Remove NA values
trfrz_kanu_DTU<-trfrz_kanu_DTU[!is.na(trfrz_kanu_DTU)];dim(trfrz_kanu_DTU)
trfrz_kanu_DTU<- trfrz_kanu_DTU[seq(1, length(trfrz_kanu_DTU), 8)];dim(trfrz_kanu_DTU)
trfrz_kanu_DTU<-1000*trfrz_kanu_DTU
trunoff_kanu_DTU<- ncvar_get(file4_DTU, varid = "trunoff")
trunoff_kanu_DTU<-trunoff_kanu_DTU[!is.na(trunoff_kanu_DTU)]
trunoff_kanu_DTU<- trunoff_kanu_DTU[seq(1, length(trunoff_kanu_DTU), 8)]
trunoff_kanu_DTU<-1000*trunoff_kanu_DTU
plot_refreezing_kanu_DTU<- ggplot()+geom_line(aes(x=t_kanu, y = trfrz_kanu_DTU)) 
#Yearly totals for refreezing:
trfrz_kanu_DTU_yearly_total <- trfrz_kanu_DTU[seq(1, length(trfrz_kanu_DTU), 1)]
trfrz_kanu_DTU_yearly_total_1<- sum(trfrz_kanu_DTU_yearly_total[1:366])
trfrz_kanu_DTU_yearly_total_2<- sum(trfrz_kanu_DTU_yearly_total[366:731])
trfrz_kanu_DTU_yearly_total_3<- sum(trfrz_kanu_DTU_yearly_total[731:1096])
trfrz_kanu_DTU_yearly_total_4<- sum(trfrz_kanu_DTU_yearly_total[1096:1461])
trfrz_kanu_DTU_yearly_total_5<- sum(trfrz_kanu_DTU_yearly_total[1461:1706])
yearly_trfrz_kanu_DTU<-c(trfrz_kanu_DTU_yearly_total_1,trfrz_kanu_DTU_yearly_total_2,trfrz_kanu_DTU_yearly_total_3,trfrz_kanu_DTU_yearly_total_4,trfrz_kanu_DTU_yearly_total_5)
t_kanu_yearly<- (seq(as.Date("01-05-2012", "%d-%m-%Y" ),as.Date("31-12-2016", "%d-%m-%Y"), by="years"))
#Yearly totals for runoff
trunoff_kanu_DTU_yearly_total <- trunoff_kanu_DTU[seq(1, length(trunoff_kanu_DTU), 1)]
trunoff_kanu_DTU_yearly_total_1<- sum(trunoff_kanu_DTU_yearly_total[1:366])
trunoff_kanu_DTU_yearly_total_2<- sum(trunoff_kanu_DTU_yearly_total[366:731])
trunoff_kanu_DTU_yearly_total_3<- sum(trunoff_kanu_DTU_yearly_total[731:1096])
trunoff_kanu_DTU_yearly_total_4<- sum(trunoff_kanu_DTU_yearly_total[1096:1461])
trunoff_kanu_DTU_yearly_total_5<- sum(trunoff_kanu_DTU_yearly_total[1461:1706])
yearly_trunoff_kanu_DTU<-c(trunoff_kanu_DTU_yearly_total_1,trunoff_kanu_DTU_yearly_total_2,trunoff_kanu_DTU_yearly_total_3,trunoff_kanu_DTU_yearly_total_4,trunoff_kanu_DTU_yearly_total_5)

###SUMMIT
file5_DTU<-nc_open("RetMIP_DTU_Summit_3hourly_columns.nc")
rho3_DTU <- ncvar_get(file5_DTU, varid="rho")
temp3_DTU<-ncvar_get(file5_DTU, varid = "temp")
temp3_DTU_2012<- temp3_DTU[33608:36528,]
temp3_DTU_2012<-temp3_DTU_2012[apply(temp3_DTU_2012, 1, Compose(is.finite, all)),]
temp3_DTU_2012<-temp3_DTU_2012-273.15
temp3_DTU_2012<-melt(temp3_DTU_2012)
plot_temp3_DTU_2012<- ggplot(data = data.frame(temp3_DTU_2012), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)")  + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))

lwc3_DTU<- ncvar_get(file5_DTU, varid = "lwc")
icecon3_DTU<- ncvar_get(file5_DTU,varid="icecon")
rfrz3_DTU <- ncvar_get(file5_DTU, varid = "rfrz")
t_summit<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="days"))
as.POSIXct(t_summit)
##Total values
file6_DTU<- nc_open("RetMIP_DTU_Summit_3hourly_values.nc")
tfac_summit_DTU <- ncvar_get(file6_DTU, varid="tfac")
tlwc_summit_DTU<-ncvar_get(file6_DTU, varid = "tlwc")
trfrz_summit_DTU<- ncvar_get(file6_DTU, varid = "trfrz")
#Remove NA values
trfrz_summit_DTU<-trfrz_summit_DTU[!is.na(trfrz_summit_DTU)]
trfrz_summit_DTU<- trfrz_summit_DTU[seq(1, length(trfrz_summit_DTU), 8)]
trfrz_summit_DTU<-trfrz_summit_DTU*1000
trunoff_summit_DTU<- ncvar_get(file6_DTU, varid = "trunoff")
trunoff_summit_DTU<-trunoff_summit_DTU[!is.na(trunoff_summit_DTU)];dim(trunoff_summit_DTU)
trunoff_summit_DTU<- trunoff_summit_DTU[seq(1, length(trunoff_summit_DTU), 8)]
trunoff_summit_DTU<-1000*trunoff_summit_DTU
plot_runoff_summit_DTU<- ggplot()+geom_line(aes(x=t_summit, y = trunoff_summit_DTU)) 
plot_refreezing_summit_DTU<- ggplot()+geom_line(aes(x=t_summit, y = trfrz_summit_DTU)) 
##Yearly values summit
trfrz_summit_DTU_yearly_total <- trfrz_summit_DTU[seq(1, length(trfrz_summit_DTU), 1)]
trfrz_summit_DTU_yearly_total_1<- sum(trfrz_summit_DTU_yearly_total[1:357])
trfrz_summit_DTU_yearly_total_2<- sum(trfrz_summit_DTU_yearly_total[357:714])
trfrz_summit_DTU_yearly_total_3<- sum(trfrz_summit_DTU_yearly_total[714:1071])
trfrz_summit_DTU_yearly_total_4<- sum(trfrz_summit_DTU_yearly_total[1071:1428])
trfrz_summit_DTU_yearly_total_5<- sum(trfrz_summit_DTU_yearly_total[1428:1785])
trfrz_summit_DTU_yearly_total_6<- sum(trfrz_summit_DTU_yearly_total[1785:2142])
trfrz_summit_DTU_yearly_total_7<- sum(trfrz_summit_DTU_yearly_total[2142:2499])
trfrz_summit_DTU_yearly_total_8<- sum(trfrz_summit_DTU_yearly_total[2499:2856])
trfrz_summit_DTU_yearly_total_9<- sum(trfrz_summit_DTU_yearly_total[2856:3213])
trfrz_summit_DTU_yearly_total_10<- sum(trfrz_summit_DTU_yearly_total[3213:3570])
trfrz_summit_DTU_yearly_total_11<- sum(trfrz_summit_DTU_yearly_total[3570:3927])
trfrz_summit_DTU_yearly_total_12<- sum(trfrz_summit_DTU_yearly_total[3927:4284])
trfrz_summit_DTU_yearly_total_13<- sum(trfrz_summit_DTU_yearly_total[4284:4641])
trfrz_summit_DTU_yearly_total_14<- sum(trfrz_summit_DTU_yearly_total[4641:4998])
trfrz_summit_DTU_yearly_total_15<- sum(trfrz_summit_DTU_yearly_total[4998:5363])
yearly_trfrz_summit_DTU<-c(trfrz_summit_DTU_yearly_total_1,trfrz_summit_DTU_yearly_total_2,trfrz_summit_DTU_yearly_total_3,trfrz_summit_DTU_yearly_total_4,trfrz_summit_DTU_yearly_total_5, trfrz_summit_DTU_yearly_total_6, trfrz_summit_DTU_yearly_total_7,trfrz_summit_DTU_yearly_total_8,trfrz_summit_DTU_yearly_total_9,trfrz_summit_DTU_yearly_total_10,trfrz_summit_DTU_yearly_total_11,trfrz_summit_DTU_yearly_total_12,trfrz_summit_DTU_yearly_total_13,trfrz_summit_DTU_yearly_total_14,trfrz_summit_DTU_yearly_total_15)
t_summit_yearly<- (seq(as.Date("02-07-2000", "%d-%m-%Y" ),as.Date("08-03-2015", "%d-%m-%Y"), by="1 year"))

###FA
file7_DTU<-nc_open("RetMIP_DTU_FA_3hourly_columns.nc")
rho4_DTU <- ncvar_get(file7_DTU, varid="rho")
rho4_DTU<-rho4_DTU[apply(rho4_DTU, 1, Compose(is.finite, all)),]
rho4_DTU<-melt(rho4_DTU)
plot_rho4_DTU<- ggplot(data = data.frame(rho4_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + ggtitle("Rho DTU FA")+ scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"))+ scale_fill_continuous(name= as.expression(bquote("Density [kg/"~m^3~ "]")), type="viridis", breaks=c(300,400,500,600,700,800,900),labels=c("300","400","500","600","700","800","900"))
temp4_DTU<-ncvar_get(file7_DTU, varid = "temp")
temp4_DTU<-temp4_DTU[apply(temp4_DTU, 1, Compose(is.finite, all)),]
temp4_DTU<-temp4_DTU-273.15
temp4_DTU<-melt(temp4_DTU)
plot_temp4_DTU<- ggplot(data = data.frame(temp4_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.01,0.01))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.01,0.01))+ scale_fill_continuous(name= as.expression(bquote("Temperature [?C]")), type="viridis",breaks=c(-0.5,-10,-20,-30,-40,-50),labels=c("0","-10","-20","-30","-40","-50"))+scale_fill_gradientn(name="Temperature [ºC]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(min(temp1_CFM_KFM$value),max(temp1_CFM_KFM$value)))
lwc4_DTU<- ncvar_get(file7_DTU, varid = "lwc")
lwc4_DTU<-1000*lwc4_DTU
lwc4_DTU<-lwc4_DTU[apply(lwc4_DTU, 1, Compose(is.finite, all)),]
lwc4_DTU<-melt(lwc4_DTU)
plot_lwc4_DTU<- ggplot(data = data.frame(lwc4_DTU), aes(x = X1, y = -X2, fill= value)) + geom_tile()+xlab("Date")+ylab("Depth (m)") + scale_x_continuous(breaks=c(0,500,1000,1500),labels=c("12-Apr-2014", "12-Jun-2014", "12-Sep-2014","12-Nov-2014"),expand=c(0.02,0.02))+ scale_y_continuous(breaks=c(0,-50,-100,-150,-200),labels=c("0", "5", "10","15","20"),expand=c(0.02,0.02))+scale_fill_gradientn(name="lwc [mm weq]",colours = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B"),limits=range(0,30))
icecon4_DTU<- ncvar_get(file7_DTU,varid="icecon")
rfrz4_DTU <- ncvar_get(file7_DTU, varid = "rfrz")
t_FA<- (seq(as.Date("12-04-2014", "%d-%m-%Y" ),as.Date("02-12-2014", "%d-%m-%Y"), by="days"));length(t_FA)
as.POSIXct(t_FA)
##Total values
file8_DTU<- nc_open("RetMIP_DTU_FA_3hourly_values.nc")
tfac_FA_DTU <- ncvar_get(file8_DTU, varid="tfac")
tlwc_FA_DTU<-ncvar_get(file8_DTU, varid = "tlwc")
trfrz_FA_DTU<- ncvar_get(file8_DTU, varid = "trfrz");dim(trfrz_FA_DTU)
#Remove NA values
trfrz_FA_DTU<-trfrz_FA_DTU[!is.na(trfrz_FA_DTU)];dim(trfrz_FA_DTU)
trfrz_FA_DTU<- trfrz_FA_DTU[seq(1, length(trfrz_FA_DTU), 8)];dim(trfrz_FA_DTU)
trfrz_FA_DTU<-trfrz_FA_DTU*1000
trunoff_FA_DTU<- ncvar_get(file8_DTU, varid = "trunoff")
trunoff_FA_DTU<-trunoff_FA_DTU[!is.na(trunoff_FA_DTU)]
trunoff_FA_DTU<- trunoff_FA_DTU[seq(1, length(trunoff_FA_DTU), 8)]
trunoff_FA_DTU<-1000*trunoff_FA_DTU
plot_refreezing_FA_DTU<- ggplot()+geom_line(aes(x=t_FA, y = trfrz_FA_DTU)) 

