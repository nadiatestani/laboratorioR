#Trabajo final

#Cargo librerias
require(ncdf4)
require(fields)
require(mapdata)
require(ggplot2)
require(maps)
require(RColorBrewer)
require(scales)
require(gridExtra)
require(gdata)
require(reshape2)
require(MASS)

#cargo datos
ruta<-"/home/nadia/Documentos/na/trabajofinal/"
archivo<-"precip.mon.mean.nc"
datos<-nc_open(paste(ruta,archivo,sep=""))


#a

#nombre de archivo:
nombre_archivo<-datos$filename 
#cantidad de variables:
variables_cantidad<-length(datos$var)
#nombre de las variables:
variables_nombres<-names(datos$var) 
#cantidad de datos:
#genero un array en donde cada conteiner tenga los datos de cada dimension 
#(esto es para ahorrarme cargar todos los datos)
nombres_dimensiones<-names(datos$dim)
datos_dimensiones <- 0
for (i in 1:length(nombres_dimensiones)) {
  datos_dimensiones[i] <- list(ncvar_get(datos,nombres_dimensiones[i]))  
}
names(datos_dimensiones)<-nombres_dimensiones 
longitudes_cantidad<-length(datos_dimensiones$lon) 
latitudes_cantidad<-length(datos_dimensiones$lat)
tiempos_cantidad<-length(datos_dimensiones$time)
datos_cantidad<-longitudes_cantidad*latitudes_cantidad*tiempos_cantidad

#período:
#convierto los datos de fecha en fechas entendibles:
ncatt_get(datos,"time","units") #leo lo que hay en time: "hours since 1-1-1 00:00:0.0"
tiempos<-as.Date((datos_dimensiones$time-48)/24 ,origin="1-01-01") 
#divido por 24 porque mi unidad es horas y yo quiero los dias
#Obs: esta dos dias desfasado del primero de cada mes entonces le resto dos dias
periodo<-range(tiempos)
periodo_unico_elemento<-paste(periodo[1],periodo[2], sep=",") 

#región:
longitudes<-range(datos_dimensiones$lon)
latitudes<-range(datos_dimensiones$lat)
region<-paste(as.character(longitudes[1]),"E-",as.character(longitudes[2]),
              "E, ",as.character(latitudes[1]),"N-",as.character(latitudes[2]),"N",sep="")

# atributos:
variables_unidad<-datos$var$precip$units
variables_que_es<-datos$var$precip$longname
resolucion<-c((longitudes[2]-longitudes[1])/(longitudes_cantidad-1),((latitudes[2]-latitudes[1])/(latitudes_cantidad-1)))
resolucion_unico_elemento<-paste(as.character(resolucion[1]),"X",as.character(resolucion[2]),sep=" ")
modelo<-"NCEP"
version<-"V1201"

#Guardo la descripcion en un ascii

nombres<-c("nombre del archivo:","cantidad de variables:","nombre de variables:","descripcion de variables:",
           "unidad de la variable:","resolucion:","cantidad de datos:","periodo:","region:","modelo:","version:")
info<-c(nombre_archivo,variables_cantidad,variables_nombres,variables_que_es,variables_unidad,resolucion_unico_elemento,
        datos_cantidad,periodo_unico_elemento,region,modelo,version)
a<-cbind(nombres,info)
write.table(a,file="trabajo_final_a.txt",quote=FALSE,col.names = FALSE) 

###########################################################################################################################

#b

#Voy a eliminar el primer enero y febrero, porque quiero el trimestre dic enero y feb y me falta el primer diciembre 
#Del mismo modo elimino los ultimos ocho meses para que me queden años enteros

tiempos_b<- tiempos[which(tiempos=="1979-03-01"):which(tiempos=="2011-02-01")]
precipitacion_b<-ncvar_get(datos,variables_nombres,start=c(1,1,3),count=c(-1,-1,length(tiempos_b)))

#Acomodo la informacion en arrays
#Uno por meses
precipitacion_pormeses<-array(precipitacion_b,dim=c(longitudes_cantidad ,latitudes_cantidad,12,32)) 

#Otro por estacion
precipitacion_porestacion<-array(precipitacion_b,dim=c(longitudes_cantidad, latitudes_cantidad,3,4,32)) 

#Calculo el promedio estacional y mensual
promedio_por_estacion<-apply(precipitacion_porestacion,c(1,2,4),mean,rm.na=TRUE)
promedio_por_mes<-apply(precipitacion_pormeses,c(1,2,3),mean,rm.na=TRUE)

#Calculo el desvio estandar estacional y mensual
desvio_por_estacion<-apply(precipitacion_porestacion,c(1,2,4),sd)
desvio_por_mes<-apply(precipitacion_pormeses,c(1,2,3),sd) 

#Defino la region de sudamerica
latitudes_valores<-rev(datos_dimensiones$lat)
longitudes_valores<-datos_dimensiones$lon
latitudes_sudamerica<-latitudes_valores[latitudes_valores>-60&latitudes_valores<15]
longitudes_sudamerica<-longitudes_valores[longitudes_valores>270&longitudes_valores<330]

#Grafico promedios por trimestre:
trimestre<-c("MAM","JJA","SON","DEF")
p<-list(0,length=4)
for (i in 1:4) {
  #voy a convertir los promedios estacionales en data frames
  promedio_por_estacion_data_frame<-data.frame(x = rep(longitudes_valores, length(latitudes_valores)),
                                                y = rep(latitudes_valores, each = length(longitudes_valores)), 
                                                z = matrix(promedio_por_estacion[,,i], length(longitudes_valores) * length(latitudes_valores))) 
  map.world <- map_data("world2")
  my_fill <- scale_fill_gradientn(name = expression("mmdia"),
                                  colours = rev(brewer.pal(11, "RdYlBu")), breaks = seq(0,20,2), limits = c(0, 20)) 
  p[[i]]<- ggplot(promedio_por_estacion_data_frame, aes(x = x, y = y)) +
    geom_tile(aes(fill = z)) +
    my_fill +
    ggtitle(trimestre[i])+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_map(dat=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F) +
    coord_quickmap(xlim = range(longitudes_sudamerica), ylim = range(latitudes_sudamerica))
}

promedio_trimestres_plot<-grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],nrow=2,top="Promedio de precipitacion por dia en un trimestre")
ggsave("promedio_trimestres_plot.png",promedio_trimestres_plot,width=12.5, height=7.25, dpi=72)

#Grafico desvios por trimestre:

ds<-list(0,length=4)
for (i in 1:4) {
  desvio_por_estacion_data_frame <-data.frame(x = rep(longitudes_valores, length(latitudes_valores)),y = rep(latitudes_valores, each = length(longitudes_valores)), z = matrix(desvio_por_estacion[,,i], length(longitudes_valores) * length(latitudes_valores)))
  map.world <- map_data("world2")
  my_fill <- scale_fill_gradientn(name = expression("mmdia"),
                                  colours = rev(brewer.pal(11, "RdYlBu")), breaks = seq(0,6,1), limits = c(0, 6)) 
  ds[[i]]<- ggplot(desvio_por_estacion_data_frame, aes(x = x, y = y)) +
    geom_tile(aes(fill = z)) +
    my_fill +
    ggtitle(trimestre[i])+
    theme(plot.title = element_text(hjust = 0.5),text = element_text(size=12))+
    geom_map(dat=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F) +
    coord_quickmap(xlim = range(longitudes_sudamerica), ylim = range(latitudes_sudamerica))
}
desvio_trimestres_plot<-grid.arrange(ds[[1]],ds[[2]],ds[[3]],ds[[4]],nrow=2,top="Desvio de precipitacion por dia en un trimestre")

ggsave("desvio_trimestres_plot.png",plot=desvio_trimestres_plot,width=12.5, height=7.25, dpi=72)

#Grafico promedios por mes:

meses<-c("Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre","Enero","Febrero")
pm<-list(0,length=12)
for (i in 1:12) {
  promedio_por_mes_data_frame <-data.frame(x = rep(longitudes_valores, length(latitudes_valores)),y = rep(latitudes_valores, each = length(longitudes_valores)), z = matrix(promedio_por_mes[,,i], length(longitudes_valores) * length(latitudes_valores))) 
  map.world <- map_data("world2")
  my_fill <- scale_fill_gradientn(name = expression("mmdia"),
                                  colours = rev(brewer.pal(11, "RdYlBu")), breaks = seq(0,20,2), limits = c(0, 20)) #estos limites estan ok para pp en todos los trimestres porque abarca el maximo maximo (19 y pico para la segunda estacion)
  pm[[i]]<- ggplot(promedio_por_mes_data_frame, aes(x = x, y = y)) +
    geom_tile(aes(fill = z)) +
    my_fill +
    ggtitle(meses[i])+
    theme(plot.title = element_text(hjust = 0.5),text = element_text(size=12))+
    geom_map(dat=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F) +
    coord_quickmap(xlim = range(longitudes_sudamerica), ylim = range(latitudes_sudamerica))
}

promedio_meses_plot_MAM<-grid.arrange(pm[[1]],pm[[2]],pm[[3]],nrow=1,top="Promedio de precipitacion por dia en un mes")
promedio_meses_plot_JJA<-grid.arrange(pm[[4]],pm[[5]],pm[[6]],nrow=1,top="Promedio de precipitacion por dia en un mes")
promedio_meses_plot_SON<-grid.arrange(pm[[7]],pm[[8]],pm[[9]],nrow=1,top="Promedio de precipitacion por dia en un mes")
promedio_meses_plot_DEF<-grid.arrange(pm[[10]],pm[[11]],pm[[12]],nrow=1,top="Promedio de precipitacion por dia en un mes")

ggsave("promedio_meses_plot_MAM.png",promedio_meses_plot_MAM,width=9.5, height=5.25, dpi=72)
ggsave("promedio_meses_plot_JJA.png",promedio_meses_plot_JJA,width=9.5, height=5.25, dpi=72)
ggsave("promedio_meses_plot_SON.png",promedio_meses_plot_SON,width=9.5, height=5.25, dpi=72)
ggsave("promedio_meses_plot_DEF.png",promedio_meses_plot_DEF,width=9.5, height=5.25, dpi=72)


#Grafico desvios por mes

dsm<-list(0,length=12)
for (i in 1:12) {
  desvio_por_mes_data_frame <-data.frame(x = rep(longitudes_valores, length(latitudes_valores)),y = rep(latitudes_valores, each = length(longitudes_valores)), z = matrix(desvio_por_mes[,,i], length(longitudes_valores) * length(latitudes_valores)))
  map.world <- map_data("world2")
  my_fill <- scale_fill_gradientn(name = expression("mmdia"),
                                  colours = rev(brewer.pal(11, "RdYlBu")), breaks = seq(0,6,1), limits = c(0, 6)) 
  dsm[[i]]<- ggplot(desvio_por_mes_data_frame, aes(x = x, y = y)) +
    geom_tile(aes(fill = z)) +
    my_fill +
    ggtitle(meses[i])+
    theme(plot.title = element_text(hjust = 0.5),text = element_text(size=12))+
    geom_map(dat=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F) +
    coord_quickmap(xlim = range(longitudes_sudamerica), ylim = range(latitudes_sudamerica))
}

desvio_meses_plot_MAM<-grid.arrange(dsm[[1]],dsm[[2]],dsm[[3]],nrow=1,top="Desvio de la precipitacion por dia en un mes")
desvio_meses_plot_JJA<-grid.arrange(dsm[[4]],dsm[[5]],dsm[[6]],nrow=1,top="Desvio de la precipitacion por dia en un mes")
desvio_meses_plot_SON<-grid.arrange(dsm[[7]],dsm[[8]],dsm[[9]],nrow=1,top="Desvio de la precipitacion por dia en un mes")
desvio_meses_plot_DEF<-grid.arrange(dsm[[10]],dsm[[11]],dsm[[12]],nrow=1,top="Desvio de la precipitacion por dia en un mes")

ggsave("desvio_meses_plot_MAM.png", desvio_meses_plot_MAM,width=9.5, height=5.25, dpi=72)
ggsave("desvio_meses_plot_JJA.png", desvio_meses_plot_JJA,width=9.5, height=5.25, dpi=72)
ggsave("desvio_meses_plot_SON.png", desvio_meses_plot_SON,width=9.5, height=5.25, dpi=72)
ggsave("desvio_meses_plot_DEF.png", desvio_meses_plot_DEF,width=9.5, height=5.25, dpi=72)

##############################################################################################################################
#c
#defino
nombres_regiones<-c("Amazonas","SAM","Noreste de Brasil","SACZ","Cuenca de La Plata")
numero_meses<-c(3:12,1,2) #porque el primer mes es marzo
dias_por_mes<-c(31,30,31,30,31,31,30,31,30,31,31,28) #los ordeno desde marzo hasta febrero

latitudes_sur<-c(-13.75,-16.25,-16.25,-26.25,-38.75)
latitudes_norte<-c(1.25,-3.75,-1.25,-16.25,-23.75)
longitudes_oeste<-c(291.25,301.25,313.75,308.75,296.25)
longitudes_este<-c(303.75,316.25,326.25,321.25,303.75)

regiones<-data.frame(nombres_regiones,latitudes_sur,latitudes_norte,longitudes_oeste,longitudes_este)

#calculo la media mensual 
precipitacion_region<-list(0,length=5)
media_mensual_region<-list(0,length=5)
plot_region<-list(0,length=5)
for(i in 1:5) {
  #extraigo los datos de la region i:
  longitudes_region<-which((longitudes_valores<=regiones[i,5])&(longitudes_valores>=regiones[i,4]))
  latitudes_region<-which((latitudes_valores<=regiones[i,3])&(latitudes_valores>=regiones[i,2]))
  precipitacion_region[[i]]<-ncvar_get(datos,variables_nombres,start=c((longitudes_region[1]),(latitudes_region[1]),3),
                                       count=c(length(longitudes_region),length(latitudes_region),length(tiempos_b)))  
  #acomodo la informacion mensualmente
  precipitacion_region[[i]]<-array(precipitacion_region[[i]],dim=c(length(longitudes_region),length(latitudes_region),12,32)) 
  #calculo el promedio areal
  promedio_espacial_region<-apply(precipitacion_region[[i]],c(3,4),mean,na.rm=TRUE)
  #calculo la media mensual sobre la region
  media_mensual_region[[i]]<-apply(promedio_espacial_region,1,mean,rm.na=TRUE)
}

#grafico
#paso a data.frame
for(i in 1:5) 
  local({
    i = i
    DF<-data.frame(x=numero_meses,y=media_mensual_region[[i]]*dias_por_mes[i]) #lo paso a datos mensuales 
    plot_region[[i]]<<-ggplot(DF, aes(x=x, y=y)) +  
      geom_col(stat =  "identity", colour="coral4",fill="coral3")+
      scale_x_continuous(breaks=seq(1,12,1))+
      ylab("PP(mm/mes)")+
      xlab("meses")+
      theme_bw()+
      ggtitle (regiones[[i,1]])+
      theme(plot.title = element_text(hjust = 0.5))
  })
plot_c<-grid.arrange(plot_region[[1]],plot_region[[2]],plot_region[[3]],plot_region[[4]],plot_region[[5]],
                     nrow=2,top="Marcha anual promedio en cada region")
ggsave("grafico_c.png",plot_c,width=11.5, height=5.25, dpi=72)


##########################################################################################################
#d

#periodo en comun:
#enero 1990 hasta noviembre 2011 pero voy a tomar hasta diciembre 2010 para tener los años completos

datos_campo<-read.xls(paste(ruta,"abuelo_datos.xlsx",sep=""), sheet=1)
latitud_campo<-datos_campo[1,2]
longitud_campo<-datos_campo[1,3]+360
precipitacion_campo<-read.xls(paste(ruta,"abuelo_datos.xlsx",sep=""),sheet=2) #esto es mensual
precipitacion_campo_periodo_comun<-precipitacion_campo[c(1:21),c(2:13)]

#busco los cuatro puntos mas cercanos:

#creo una matriz de latitud y otra de longitud
latitudes_matriz<-matrix(rep(latitudes_valores,each=length(longitudes_valores)),
                         byrow=TRUE,ncol=length(longitudes_valores))
longitudes_matriz<-matrix(rep(longitudes_valores,each=length(latitudes_valores)),
                          byrow=FALSE,nrow=length(latitudes_valores))

#resto en cada matriz los valores del campo y tomo la distancia al punto
distancia_al_campo<-sqrt((latitudes_matriz-latitud_campo)^2+(longitudes_matriz-longitud_campo)^2)

#tomo los cuatro mas cercanos y los guardo en una lista "puntos"
puntos<-list(0,length=4)
for(i in 1:4){
  posicion_en_matrices<-arrayInd(which(distancia_al_campo==(sort(distancia_al_campo)[i])),.dim=dim(latitudes_matriz))
  puntos[[i]]<-c(latitudes_matriz[posicion_en_matrices[1]],longitudes_matriz[1,posicion_en_matrices[2]])
}

tiempos_periodo_comun<- tiempos[which(tiempos=="1990-01-01"):which(tiempos=="2010-12-01")]
latitudes_cercanas<-c(puntos[[1]][1],puntos[[2]][1],puntos[[3]][1],puntos[[4]][1])
longitudes_cercanas<-c(puntos[[1]][2],puntos[[2]][2],puntos[[3]][2],puntos[[4]][2])
#cargo los datos de precipitacion en los tiempos del periodo en comun,
#y en los seis puntos entre el maximo y el minimo de latitudes_cercanas 
#y entre el maximo y minimo de longitudes_cercanas
precipitacion_netcdf_periodo_comun<-ncvar_get(datos,
                                              variables_nombres,
                                              start=c(which(longitudes_valores==min(longitudes_cercanas)),
                                                      which(latitudes_valores==min(latitudes_cercanas)),
                                                      which(tiempos=="1990-01-01")),
                                              count=c(3,2,length(tiempos_periodo_comun))) 

#aca la region queda restringida en seis puntos entonces tengo que sacar los dos puntos 
#que no se corresponden con puntos cercanos (4), estos son los de latitud -36.25 y longitud 298.75 y 303.75, 
#osea la latitud 2 y las longitudes 1 y 3 

#elimino los dos puntos que no van
precipitacion_netcdf_periodo_comun<-precipitacion_netcdf_periodo_comun[
  -c(precipitacion_netcdf_periodo_comun[1,2,],precipitacion_netcdf_periodo_comun[3,2,])]

#lo acomodo en un array por meses
precipitacion_netcdf_periodo_comun<-array(precipitacion_netcdf_periodo_comun,dim=c(3,2,12,21))

#para poder comparar con los datos de Necochea, multiplico por la cantidad de dias de cada mes 
dias_por_mes<-c(31,28,31,30,31,30,31,31,30,31,30,31)
for (i in 1:12){
  precipitacion_netcdf_periodo_comun[,,i,]<-precipitacion_netcdf_periodo_comun[,,i,]*dias_por_mes[i]
}

#calculo medias mensuales de el campo, las convierto en data.frame y grafico
medias_mensuales_campo<-apply(precipitacion_campo_periodo_comun,2,mean,na.rm=TRUE)

medias_mensuales_campo_df<-data.frame(x=c(1:12),y=medias_mensuales_campo)

plot_medias_mensuales_campo<<-ggplot(medias_mensuales_campo_df, aes(x=x, y=y)) +  
  geom_bar(stat =  "identity", colour="blue",fill="lightblue",position="dodge")+
  scale_x_continuous(breaks=seq(1,12,1))+
  ylim(c(0,220))+
  ylab("PP(mm/mes)")+
  xlab("meses")+
  theme_bw()+
  ggtitle ("media mensual campo")+
  theme(plot.title = element_text(hjust = 0.5))

#Hago lo mismo con los datos del  NCEP (promediando sobre los cuatro puntos)
medias_mensuales_netcdf<-apply(precipitacion_netcdf_periodo_comun,3,mean,na.rm=TRUE)
medias_mensuales_netcdf_df<-data.frame(x=c(1:12),y=medias_mensuales_netcdf)
plot_medias_mensuales_netcdf<-ggplot(medias_mensuales_netcdf_df, aes(x=x, y=y)) +  
  geom_bar(stat =  "identity", colour="red",fill="pink")+
  scale_x_continuous(breaks=seq(1,12,1))+
  ylim(c(0,220))+
  ylab("PP(mm/mes)")+
  xlab("meses")+
  theme_bw()+
  ggtitle ("media mensual modelo")+
  theme(plot.title = element_text(hjust = 0.5))

#Pongo los graficos en una misma hoja
plot_comparo_medias<-grid.arrange(plot_medias_mensuales_campo,plot_medias_mensuales_netcdf, nrow=1)
ggsave("medias_d.png",plot_comparo_medias,width=9.5, height=5.25, dpi=72)

#valores extremos para cada mes, los presento en una tabla
maximos_por_mes_campo<-apply(precipitacion_campo_periodo_comun,2,max)
minimos_por_mes_campo<-apply(precipitacion_campo_periodo_comun,2,min)
maximos_por_mes_netcdf<-apply(precipitacion_netcdf_periodo_comun,3,max,rm.na=TRUE)
minimos_por_mes_netcdf<-apply(precipitacion_netcdf_periodo_comun,3,min,rm.na=TRUE)

extremos_d<-as.data.frame(rbind(maximos_por_mes_campo,maximos_por_mes_netcdf,minimos_por_mes_campo,minimos_por_mes_netcdf))
write.matrix(extremos_d,"extremos_d")

#Calculo desvios estandares mensuales para los datos del campo, los convierto en data.frame y grafico
desvio_campo<-apply(precipitacion_campo_periodo_comun,2,sd)
desvio_campo_df<-data.frame(x=c(1:12),y=desvio_campo)
plot_desvios_mensuales_campo<-ggplot(desvio_campo_df, aes(x=x, y=y)) +  
  geom_bar(stat =  "identity", colour="blue",fill="lightblue",position="dodge")+
  scale_x_continuous(breaks=seq(1,12,1))+
  ylim(c(0,100))+
  ylab("PP(mm/mes)")+
  xlab("meses")+
  theme_bw()+
  ggtitle ("Desvio mensual campo")+
  theme(plot.title = element_text(hjust = 0.5))

#Hago lo mismo para los datos del NCEP
desvio_comparo<-apply(precipitacion_netcdf_periodo_comun,3,sd)
desvio_comparo_df<-data.frame(x=c(1:12),y=desvio_comparo)
plot_desvios_mensuales_netcdf<-ggplot(desvio_comparo_df, aes(x=x, y=y)) +  
  geom_bar(stat =  "identity", colour="red",fill="pink")+
  scale_x_continuous(breaks=seq(1,12,1))+
  ylim(c(0,100))+
  ylab("PP(mm/mes)")+
  xlab("meses")+
  theme_bw()+
  ggtitle ("Desvio mensual modelo")+
  theme(plot.title = element_text(hjust = 0.5))

plot_comparo_desvios<-grid.arrange(plot_desvios_mensuales_campo, plot_desvios_mensuales_netcdf, nrow=1)
ggsave("desvios_d.png",plot_comparo_desvios,width=9.5, height=5.25, dpi=72)


#calculo el acumulado anual en el campo, lo convierto a data.frame y lo grafico
acumulado_campo<-apply(precipitacion_campo_periodo_comun,1,sum,rm.na=TRUE)
acumulado_campo_df<-data.frame(x=c(1990:2010),y=acumulado_campo)
plot_acumulado_campo<-ggplot(acumulado_campo_df, aes(x=x, y=y)) +  
  geom_bar(stat =  "identity", colour="blue",fill="lightblue",position="dodge")+
  scale_x_continuous(breaks=seq(1990,2010,2))+
  ylim(c(0,2500))+
  ylab("PP(mm/año)")+
  xlab("años")+
  theme_bw()+
  ggtitle ("Precipitacion acumulada anual campo")+
  theme(plot.title = element_text(hjust = 0.5))

#Hago lo mismo para los datos del NCEP
#primero promedio las cuatro estaciones cercanas
promedio_precipitacion_comparo<-apply(precipitacion_netcdf_periodo_comun,c(3,4),mean) 
acumulado_campo_comparo<-apply(promedio_precipitacion_comparo,2,sum,rm.na=TRUE)
acumulado_netcdf_df<-data.frame(x=c(1990:2010),y=acumulado_campo_comparo)
plot_acumulado_netcdf<-ggplot(acumulado_netcdf_df, aes(x=x, y=y)) +  
  geom_bar(stat =  "identity", colour="red",fill="pink")+
  scale_x_continuous(breaks=seq(1990,2010,2))+
  ylim(c(0,2500))+
  ylab("PP(mm/año)")+
  xlab("años")+
  theme_bw()+
  ggtitle ("Precipitacion acumulada anual modelo")+
  theme(plot.title = element_text(hjust = 0.5))
plot_comparo_acumulado<-grid.arrange(plot_acumulado_campo,plot_acumulado_netcdf, nrow=1)
ggsave("acumulado_d.png",plot_comparo_acumulado,width=9.5, height=5.25, dpi=72) 





