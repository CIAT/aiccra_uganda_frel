require(terra)
require(data.table)
require(stringr)
require(dplyr)
require(RColorBrewer)
require(exactextractr)



data_dir<-"datasets"
save_dir<-"results"
if(!dir.exists(save_dir)){
  dir.create(save_dir,recursive = T)
}
# Load Uganda admin ####
# Load Uganda admin from global humanitarian exchange

# Data can be downloaded from:
# https://data.humdata.org/dataset/cod-ab-uga
# uga_admbnda_ubos_20200824_SHP.zipSHP (32.7M)
# Modified: 28 August 2020
# Uganda administrative level 0-4 boundary shapefiles

uga_admin2<-terra::vect(paste0(data_dir,"/uga_admbnda_ubos_20200824_shp/uga_admbnda_adm2_ubos_20200824.shp"))

# Focal areas
admin_focal<-c("Agago", "Amuru", "Kitgum", "Lamwo", "Nwoya", "Pader", "Abim", "Karenga", "Arua", "Koboko", "Maracha", "Moyo")

# Check all focal area represented in administrative polygons
length(admin_focal[admin_focal %in% uga_admin2$ADM2_EN]) == length(admin_focal)

# Subset admin areas to focal areas
uga_admin_focal<-uga_admin2[uga_admin2$ADM2_EN %in% admin_focal,]

# Plot admin areas
plot(uga_admin_focal)
text(uga_admin_focal,labels = uga_admin_focal$ADM2_EN)

# LULC Data ####
# Read in LULC metadata this includes the emissions factors take from the FREL (Tables 10 and 11)
# The FREL can be found in "./documents/Final - Uganda Forest Reference Emission Level Document -February 2018.pdf"
metadata<-fread(paste0(data_dir,"/uganda_lulc/metadata.csv"))[1:14]
metadata[is.na(`Emission Factor`),`Emission Factor`:=0][,Class:=as.numeric(Class)]

# Create reclassification that simplifies the LULC class into forest classes
# These data are downloaded from the FAO Map Catslog and meta-data can be found in the "./datasets/uganda_lulc" folder
reclass_matrix_forest<-as.matrix(metadata[,list(Class,Category_Code)])
forest_labs<-data.frame(unique(metadata[,list(Category_Code,Category)]))
colnames(forest_labs)<-c("value","label")

forest_coltab<-data.frame(values=c(1,2,3,4,5), cols=c("brown","darkgreen","palegreen","grey95","skyblue"))

# Create reclassification that turns the LULC class into an emissions factor (EF)
reclass_matrix_ef<-as.matrix(unique(metadata[,list(Category_Code,`Emission Factor`)]))

# Load LULC data ####
# unzip files 
if(!file.exists(paste0(data_dir,"/uganda_lulc/m_UG_LC_2000.tif"))){
  unzip(paste0(data_dir,"/uganda_lulc/m_UG_LC_2000.zip"),exdir=paste0(data_dir,"/uganda_lulc"))
}

if(!file.exists(paste0(data_dir,"/uganda_lulc/m_UG_LC_2005.tif"))){
  unzip(paste0(data_dir,"/uganda_lulc/m_UG_LC_2005.zip"),exdir=paste0(data_dir,"/uganda_lulc"))
}

if(!file.exists(paste0(data_dir,"/uganda_lulc/m_UG_LC_2015.tif"))){
  unzip(paste0(data_dir,"/uganda_lulc/m_UG_LC_2015.zip"),exdir=paste0(data_dir,"/uganda_lulc"))
}

# Load files
lulc_2000<-terra::rast(paste0(data_dir,"/uganda_lulc/m_UG_LC_2000.tif"))
lulc_2005<-terra::rast(paste0(data_dir,"/uganda_lulc/m_UG_LC_2005.tif"))
lulc_2015<-terra::rast(paste0(data_dir,"/uganda_lulc/m_UG_LC_2015.tif"))
lulc_2017<-terra::rast(paste0(data_dir,"/uganda_lulc/LULC2017.tif"))

# Reproject focal area ####
uga_admin_focal_rp<-terra::project(uga_admin_focal,lulc_2017)
uga_admin2_rp<-terra::project(uga_admin2,lulc_2017)

# Convert admin vector to raster
uga_admin_focal_rp$ID<-1:nrow(uga_admin_focal_rp)
uga_admin_focal_rast<-terra::rasterize(uga_admin_focal_rp,lulc_2017,field="ID")
# calculate area of admin regions
uga_admin_focal_rp$area_ha<-expanse(uga_admin_focal_rp,unit="ha")

# Crop & mask to focal area ####
lulc_2000<-terra::mask(terra::crop(lulc_2000,uga_admin_focal_rp),uga_admin_focal_rp)
(lulc_2000_vals<-table(values(lulc_2000)))

lulc_2005<-terra::mask(terra::crop(lulc_2005,uga_admin_focal_rp),uga_admin_focal_rp)
lulc_2015<-terra::mask(terra::crop(lulc_2015,uga_admin_focal_rp),uga_admin_focal_rp)

lulc_2017<-terra::mask(terra::crop(lulc_2017,uga_admin_focal_rp),uga_admin_focal_rp)
(lulc_2017_vals<-table(values(lulc_2017)))

# Reclassify LULC to forest types ####
lulc_2000_rc<-terra::classify(lulc_2000,reclass_matrix_forest)
lulc_2005_rc<-terra::classify(lulc_2005,reclass_matrix_forest)
lulc_2015_rc<-terra::classify(lulc_2015,reclass_matrix_forest)
lulc_2017_rc<-terra::classify(lulc_2017,reclass_matrix_forest)

levels(lulc_2000_rc) <- forest_labs
levels(lulc_2005_rc) <- forest_labs
levels(lulc_2015_rc) <- forest_labs
levels(lulc_2017_rc) <- forest_labs

coltab(lulc_2000_rc) <- forest_coltab
coltab(lulc_2005_rc) <- forest_coltab
coltab(lulc_2015_rc) <- forest_coltab
coltab(lulc_2017_rc) <- forest_coltab

lulc_stack_rc<-c(lulc_2000_rc,lulc_2005_rc,lulc_2015_rc,lulc_2017_rc)
plot(lulc_stack_rc)

# Consistency checks ####

lulc_2005_rc_cc<-lulc_2005_rc*10
lulc_2015_rc_cc<-lulc_2015_rc*100
lulc_2017_rc_cc<-lulc_2017_rc*1000

lulc_stack_rc<-app(c(lulc_2000_rc,lulc_2005_rc_cc,lulc_2015_rc_cc,lulc_2017_rc_cc),sum)
lulc_stack_rc_vals<-values(lulc_stack_rc)

lulc_stack_rc_valtab<-data.table(value=unique(lulc_stack_rc_vals))

lulc_stack_rc_valtab[,count_plantation:=stringr::str_count(value.sum,"1")
                     ][,count_forest:=stringr::str_count(value.sum,"2")
                       ][,count_woodland:=stringr::str_count(value.sum,"3")
                         ][, count_nonforest:=stringr::str_count(value.sum,"4")
                           ][,count_water:=stringr::str_count(value.sum,"5")
                             ][,max_count:=max(c(count_plantation,count_forest,count_woodland,count_nonforest,count_water)),by=value.sum
                               ][,fwp_1:=any(c(count_plantation,count_forest,count_woodland) %in% 1),by=value.sum
                                 ][,fwp_3:=any(c(count_plantation,count_forest,count_woodland) %in% 3),by=value.sum]

#The automatic consistency check served to eliminate unrealistic change trajectories
#that were not dealt with in the manual review and revision. Most of these unrealistic
#change trajectories covered very small areas, with 386 out of the 431 class combinations
#present in the map covering just 1% of the map area. An example of an unrealistic
#change trajectory would be “THF – WL – THF – THF” because a conversion from THF
#to woodland and back is very unlikely. In this case, the trajectory was changed to “THF
#– THF – THF – THF”.
#The following principles were applied in the automatic consistency check:
#  • Areas of ‘No data’ were replaced with the previous epoch’s LULC label except
#for epoch 2000, where ‘No data’ was replaced with the label from epoch 2005.

#• If water was detected in any epoch, the class label was applied to all other
#epochs unless the area was classified as forest in at least 3 epochs, in which case
#the area was classified as forest.


# Reclassify instances where water vals only occur once & forest values occur 3 times
lulc_stack_rc_valtab[count_water==1 & count_plantation==3] # no values
lulc_stack_rc_valtab[count_water==1 & count_forest==3] # no values

(X<-lulc_stack_rc_valtab[count_water==1 & count_woodland==3])
(XN<-which(lulc_stack_rc_vals %in% X$value.sum))
lulc_2000_rc[XN]<-3
lulc_2005_rc[XN]<-3
lulc_2015_rc[XN]<-3
lulc_2017_rc[XN]<-3

# Reclassify instances where water vals occur 2-3 times
(X<-lulc_stack_rc_valtab[count_water %in% c(2,3),value.sum])
(XN<-which(lulc_stack_rc_vals %in% X))
lulc_2000_rc[XN]<-5
lulc_2005_rc[XN]<-5
lulc_2015_rc[XN]<-5
lulc_2017_rc[XN]<-5

# There is one value of forest in y2005 compared to all other years - fix this
lulc_stack_rc_valtab[count_forest==1]

# Forest in 2005
X<-which(lulc_2005_rc[]==2)
Y<-unique(lulc_stack_rc_vals[X])

X<-lulc_stack_rc_valtab[value.sum %in% Y]
X[,v2000:=substr(value.sum,4,4)][,v2015:=substr(value.sum,2,2)]
X[,reclass:=3][v2000==4 & v2015==4,reclass:=4]

# Reclass forest to non-forest
XN<-which(lulc_stack_rc_vals %in% X[reclass==4,value.sum])
lulc_2005_rc[XN]<-4

# Reclass forest to woodland
XN<-which(lulc_stack_rc_vals %in% X[reclass==3,value.sum])
lulc_2005_rc[XN]<-3

#• Areas exhibiting a single-epoch change in class label then reverting to the
#previously designated class label were made consistent by re-labelling the ‘odd’
#epoch to match the majority (i.e. THF – WL –THF becomes THF – THF – THF).

# Find instance where there 1 instance of one category and 3 instances of another
X<-lulc_stack_rc_valtab[fwp_1 & fwp_3
                        ][count_plantation==3,replace:=1
                          ][count_forest ==3,replace:=2
                            ][count_woodland ==3,replace:=3]

# Replacement with plantation
(XN<-which(lulc_stack_rc_vals %in% X[replace==1,value.sum]))
lulc_2000_rc[XN]<-1
lulc_2005_rc[XN]<-1
lulc_2015_rc[XN]<-1
lulc_2017_rc[XN]<-1

# Replacement with woodland
(XN<-which(lulc_stack_rc_vals %in% X[replace==3,value.sum]))
lulc_2000_rc[XN]<-3
lulc_2005_rc[XN]<-3
lulc_2015_rc[XN]<-3
lulc_2017_rc[XN]<-3

# Areas where natural forest was detected after an epoch mapped as nonforest, also the nonforest epoch was reclassified to natural forest. 
# This was not applied to plantations

# Plantation then forest
X<-c(1222,1122,1112) 
(XN<-which(lulc_stack_rc_vals %in% X)) # no values

# Plantation then woodland
target_vals<-c(1333,1133,1113) 
(XN<-which(lulc_stack_rc_vals %in% X)) # no values

# Plot reclassified data ####
lulc_stack_rc<-c(lulc_2000_rc,lulc_2005_rc,lulc_2015_rc,lulc_2017_rc)
names(lulc_stack_rc)<-c("y2000","y2005","y2015","y2017")
lulc_stack_rc_plot<-terra::project(lulc_stack_rc,uga_admin_focal)

uga_admin_focal$ADM2_EN_2c<-substr(uga_admin_focal$ADM2_EN,1,2)

# Function to add admin boundaries and names to map
add_fun<-function(){
  terra::plot(uga_admin_focal,add=T)
  text(uga_admin_focal,labels="ADM2_EN_2c")
}

plot(lulc_stack_rc_plot,legend = "topleft",fun=add_fun)

# calculate Forest Change Stats ####
unique(lulc_2000_rc[])
unique(lulc_2017_rc[])

lulc_2017_rcx10<-lulc_2017_rc*10
lulc_change_0_17<-lulc_2000_rc+lulc_2017_rcx10

vals<-unique(lulc_change_0_17[])
vals<-vals[!is.na(vals)]
change_class_table<-data.table(vals=vals,past=substr(unique(vals),2,2),future=substr(unique(vals),1,1))
change_class_table$past_des<-metadata$Category[match(change_class_table$past,metadata$Category_Code)]
change_class_table$future_des<-metadata$Category[match(change_class_table$future,metadata$Category_Code)]
change_class_table[,change_des:=paste(c(past_des,future_des),collapse = "->"),by=vals][past_des==future_des,change_des:=past_des]
change_class_table<-change_class_table[,list(vals,change_des)]

# Set suspect class to NA (e.g. water to forest)
lulc_change_0_17<-classify(lulc_change_0_17,matrix(c(54,45,35,53,rep(NA,4)),ncol=2))
change_class_table<-change_class_table[!vals %in% c(54,45,35,53)]

#Set colours
change_class_table[change_des=="Non-forest",cols:="grey90"]
change_class_table[change_des=="Woodland->Non-forest",cols:="firebrick1"]
change_class_table[change_des=="Non-forest->Woodland",cols:="green4"]
change_class_table[change_des=="Woodland",cols:="palegreen"]
change_class_table[change_des=="Water",cols:="dodgerblue"]
change_class_table[change_des=="Woodland->Plantation",cols:="orange"]
change_class_table[change_des=="Non-forest->Plantation",cols:="mediumpurple"]
change_class_table[change_des=="Plantation->Non-forest",cols:="yellow"]
change_class_table[change_des=="Plantation",cols:="thistle"]
change_class_table[change_des=="Plantation->Woodland",cols:="olivedrab"]
change_class_table[change_des=="Non-forest->Forest",cols:="darkgreen"]

levels(lulc_change_0_17)<-change_class_table[,list(vals,change_des)]
coltab(lulc_change_0_17)<-change_class_table[,list(vals,cols)]
plot(lulc_change_0_17,plg=list(x="topleft",ncol=2),main="LULC change 2000-2017")
plot(project(uga_admin_focal,lulc_change_0_17),add=T)

# Tabulate data

system.time(lulc_change_0_17_ext<-data.table(terra::extract(lulc_change_0_17,uga_admin_focal_rp))) # Extract data for administrative areas.
# Takes 67sec

# exactextractr::exact_extract a much faster function for data extraction
# system.time(lulc_change_0_17_ext2<-exactextractr::exact_extract(lulc_change_0_17,sf::st_as_sf(uga_admin_focal_rp))) # Extract data for administrative areas.
# Takes 2-6.1 sec - but needs converting to format of terra extract
# lulc_change_0_17_ext2<-rbindlist(lapply(1:length(lulc_change_0_17_ext2),FUN=function(i){
  X<- lulc_change_0_17_ext2[[i]]
  X$ID<-i
  X
}))

lulc_change_0_17_tab<-merge(lulc_change_0_17_ext,uga_admin_focal_rp[,c("ID","ADM2_EN","area_ha")])[,!"ID"]
lulc_change_0_17_tab<-dcast(lulc_change_0_17_tab,ADM2_EN+area_ha~change_des,fun=length)

# Mean pixel size in ha
cs_ha_mean<-mean(terra::cellSize(lulc_change_0_17,unit="ha")[],na.rm=T)

# Convert pixels to ha
lulc_change_0_17_tab<-data.table(lulc_change_0_17_tab[,1:2],lulc_change_0_17_tab[,-c(1,2)]*cs_ha_mean)
lulc_change_0_17_tab<-dplyr::mutate_if(lulc_change_0_17_tab,is.numeric,~round(.,0))

totals<-colSums(lulc_change_0_17_tab[,2:14])

totals_perc<-round(100*totals[2:13]/totals[1],2)

totals<-data.frame(matrix(totals,ncol=length(totals)))
colnames(totals)<-colnames(lulc_change_0_17_tab[,2:14])
totals$ADM2_EN <-"Total"

lulc_change_0_17_tab<-rbind(lulc_change_0_17_tab,totals,use.names=T)

# Calculate woodland change stats
lulc_change_0_17_tab[,woodland_net:=`Non-forest->Woodland` - `Woodland->Non-forest`
                     ][,woodland_00:=`Woodland->Non-forest` + Woodland
                       ][,woodland_17:=woodland_00+woodland_net
                       ][,woodland_change_perc:=round(100*((woodland_17/woodland_00)-1),1)
                       ][,compound_rate:=(woodland_17/woodland_00)^(1/17)-1
                         ][,compound_rate_perc:=round(100*(((woodland_17/woodland_00)^(1/17))-1),1)
                           ][,woodland_emissions_tot_mt:=-round(woodland_net*metadata[Name=="Woodland",`Emission Factor`]/10^6,3)
                             ][,woodland_change_18:=round(woodland_17*compound_rate,3)
                               ][,woodland_emissions_18_t:=-round(woodland_change_18*metadata[Name=="Woodland",`Emission Factor`],0)
                                 ][,woodland_change_18_ha:=round(woodland_change_18/area_ha,4)
                                   ][,woodland_emissions_18_t_ha:=round(woodland_emissions_18_t/area_ha,4)
                                     ][,compound_rate:=NULL]

write.table(lulc_change_0_17_tab[,-c(2:14,20:24)],"clipboard",row.names = F,sep="\t")
write.table(lulc_change_0_17_tab[,-c(2:19)],"clipboard",row.names = F,sep="\t")

fwrite(lulc_change_0_17_tab,file=paste0(save_dir,"/woodland_change_00_17.csv"))

# Make sure datasets are ordered correctly
uga_admin_focal_rp$ADM2_EN == lulc_change_0_17_tab$ADM2_EN

uga_admin_focal_rp$woodland_17_ha<-lulc_change_0_17_tab$woodland_17
uga_admin_focal_rp$woodland_change_perc<-lulc_change_0_17_tab$woodland_change_perc
uga_admin_focal_rp$compound_rate<-lulc_change_0_17_tab$compound_rate
uga_admin_focal_rp$woodland_emissions_tot_mt<-lulc_change_0_17_tab$woodland_emissions_tot_mt
uga_admin_focal_rp$woodland_change_18_ha<-lulc_change_0_17_tab$woodland_change_18
uga_admin_focal_rp$woodland_emissions_18_t<-lulc_change_0_17_tab$woodland_emissions_18_t

plot(uga_admin_focal_rp,
     c("woodland_17_ha","woodland_change_perc","compound_rate","woodland_emissions_tot_mt","woodland_change_18_ha","woodland_emissions_18_t"),
     type="continuous",
     col=rev(grDevices::terrain.colors(50)))

# Reclass to emission factors ####
ef_2000<-terra::classify(lulc_2000_rc,rcl=reclass_matrix_ef)
ef_2005<-terra::classify(lulc_2005_rc,rcl=reclass_matrix_ef)
ef_2015<-terra::classify(lulc_2015_rc,rcl=reclass_matrix_ef)
ef_2017<-terra::classify(lulc_2017_rc,rcl=reclass_matrix_ef)

# Stack emission factor data
ef<-c(ef_2000,ef_2005,ef_2015,ef_2017)
names(ef)<-c("y2000","y2005","y2015","y2017")

ef_plot<-terra::project(ef,uga_admin_focal)
plot(ef_plot,legend = "topleft",fun=add_fun)

# To do loss rate of different classes between times and % change of total. Work out annual emissions on assumed % loss rates and declining total coverage. ####

# Work out differences in emissions factors #### 
ef_00_17<-(ef_2000-ef_2017)
ef_05_17<-(ef_2005-ef_2017)
ef_00_15<-(ef_2000-ef_2015)
ef_05_15<-(ef_2005-ef_2015)

ef_diff<-c(ef_00_17,ef_05_17,ef_00_15,ef_05_15)
names(ef_diff)<-c("y2000-17","y2005-17","y2000-15","y2005-15")

# Plot emissions per ha
ef_diff_plot<-terra::project(ef_diff,uga_admin_focal)
ef_diff_plot<-terra::classify(ef_diff_plot,matrix(c(0,NA),nrow=1))

plot(ef_diff_plot,
     legend = "topleft",
     fun=add_fun,
     range=c(-300,300))


# Convert to emissions/pixel ####
area_ha<-terra::cellSize (ef,unit="ha")
ef_diff_pix<-ef_diff*area_ha # gives tons per pixel for summation

# Summarize data by admin zones #####

# I think zonal is faster than extract
gain<-data.table(terra::zonal(ef_diff_pix,uga_admin_focal_rast,fun=function(x){-sum(x[x<0],na.rm=T)}))
loss<-data.table(terra::zonal(ef_diff_pix,uga_admin_focal_rast,fun=function(x){sum(x[x>0],na.rm=T)}))

gain<-melt(gain,id.vars="ID",variable.name = "period")[,variable:="gain"]
loss<-melt(loss,id.vars="ID",variable.name = "period")[,variable:="loss"]

diff_tab<-rbind(gain,loss)
diff_tab<-dcast(diff_tab,ID+period~variable)
diff_tab[,period:=gsub("ef_","",period)]
diff_tab[,admin:=uga_admin_focal_rp$ADM2_EN[match(diff_tab$ID,uga_admin_focal_rp$ID)]
         ][,area_ha:=uga_admin_focal_rp$area_ha[match(diff_tab$ID,uga_admin_focal_rp$ID)]
           ][,ID:=NULL
           ][,emissions_tot:=loss-gain
             ][,years:=as.numeric(substr(period,4,5))-as.numeric(substr(period,1,2))
               ][,emissions_annual:=round(emissions_tot/years,2)
                 ][,emissions_annual_ha:=round(emissions_annual/area_ha,2)
                   ][,period:=gsub("_","-",period)]

diff_tab<-diff_tab %>% 
  relocate(admin, .before = period) %>% 
  relocate(area_ha, .before = period) %>%
  relocate(years, .after = period) 

diff_tab<-diff_tab[order(period,admin)]


fwrite(diff_tab,file=paste0(save_dir,"/FREL_emissions_by_admin.csv"))

