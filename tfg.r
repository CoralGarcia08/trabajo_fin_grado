# librerias
library(idealista18) #de donde voy a sacar los datos
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)

library(sp) # paquete de referencia de datos espaciales
library(spdep) # econometría espacial



library(sphet) # modelos de regresión espacial con heterocedasticidad
library(spatialreg) # para otros métodos de regresión espacial
library(tmap)
library(tidyverse)


library(stargazer)


# datos
df_raw <- Madrid_Sale
polygons <- Madrid_Polygons
madrid_pois <- Madrid_POIS



#####################################################
############### ANALISIS EXPLORATORIO ###############
#####################################################

###Exploracion inicial: estructura y variables

names(df_raw)

# #ASSETID: Un identificador único para cada activo o propiedad.
# PERIOD: El período al que pertenecen los datos, posiblemente la fecha de registro o publicación.
# PRICE: El precio de venta de la propiedad.
# UNITPRICE: El precio por unidad de área (por ejemplo, precio por metro cuadrado).
# CONSTRUCTEDAREA: El área de construcción de la propiedad.
# ROOMNUMBER: El número de habitaciones en la propiedad.
# BATHNUMBER: El número de baños en la propiedad.
# HASTERRACE: Indicador de si la propiedad tiene terraza.
# HASLIFT: Indicador de si la propiedad tiene ascensor.
# HASAIRCONDITIONING: Indicador de si la propiedad tiene aire acondicionado.
# AMENITYID: Un identificador relacionado con comodidades o características especiales de la propiedad.
# HASPARKINGSPACE: Indicador de si la propiedad tiene espacio de estacionamiento.
# ISPARKINGSPACEINCLUDEDINPRICE: Indicador de si el espacio de estacionamiento está incluido en el precio.
# PARKINGSPACEPRICE: El precio del espacio de estacionamiento.
# HASNORTHORIENTATION: Indicador de si la propiedad tiene orientación al norte.
# HASSOUTHORIENTATION: Indicador de si la propiedad tiene orientación al sur.
# HASEASTORIENTATION: Indicador de si la propiedad tiene orientación al este.
# HASWESTORIENTATION: Indicador de si la propiedad tiene orientación al oeste.
# HASBOXROOM: Indicador de si la propiedad tiene trastero.
# HASWARDROBE: Indicador de si la propiedad tiene armario empotrado.
# HASSWIMMINGPOOL: Indicador de si la propiedad tiene piscina.
# HASDOORMAN: Indicador de si la propiedad cuenta con portero o conserje.
# HASGARDEN: Indicador de si la propiedad tiene jardín.
# ISDUPLEX: Indicador de si la propiedad es un dúplex.
# ISSTUDIO: Indicador de si la propiedad es un estudio.
# ISINTOPFLOOR: Indicador de si la propiedad está en la planta superior.
# CONSTRUCTIONYEAR: Año de construcción de la propiedad.
# FLOORCLEAN: Limpiador de suelos de la propiedad. 
# FLATLOCATIONID: Identificador de la ubicación del piso.
# CADCONSTRUCTIONYEAR: Año de construcción según el catastro.
# CADMAXBUILDINGFLOOR: Máxima planta del edificio según el catastro. segun kike fuera
# CADDWELLINGCOUNT: Número de viviendas en el edificio según el catastro.
# CADASTRALQUALITYID: Identificador de la calidad catastral. 
# BUILTTYPEID_1, BUILTTYPEID_2, BUILTTYPEID_3: Identificadores relacionados con el tipo de construcción.
# DISTANCE_TO_CITY_CENTER, DISTANCE_TO_METRO, DISTANCE_TO_CASTELLANA: Distancia de la propiedad al centro de la ciudad, al metro y a la Castellana.

#amenityid???

summary(df_raw)
str(df_raw)


### valores faltantes
sum(is.na(df_raw))

missing_values <- colSums(is.na(df_raw))
print(missing_values)


df <- subset(df_raw, select = -c(ASSETID, PERIOD, ISPARKINGSPACEINCLUDEDINPRICE, PARKINGSPACEPRICE,
                                 FLATLOCATIONID, CONSTRUCTIONYEAR, FLOORCLEAN, CADASTRALQUALITYID,
                                 BUILTTYPEID_1, BUILTTYPEID_2, BUILTTYPEID_3))

################## visualizaciones

hist(df$PRICE)

df_sample = df %>% sample_n(20000, replace=FALSE)

options(scipen = 999)

ggplot(df, aes(x = PRICE)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  labs(x = "Precio", y = "Frecuencia")

ggplot(df_sample, aes(x = CONSTRUCTEDAREA, y = PRICE)) + 
  geom_point() + 
  labs(x = "Área construida", y = "Precio")


ggplot(df, aes(x = PRICE , y = DISTANCE_TO_CITY_CENTER)) + 
  geom_boxplot() + 
  labs(x = "Distancia al centro de la ciudad", y = "Precio")

##################




m <- leaflet(data=df[sample(nrow(df),200),]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(lng=~LONGITUDE,
                    lat=~LATITUDE,
                    icon=list(),
                    popup=~paste0(PRICE, " euros")) %>%
  addMiniMap()

m

#######
#######


datos_map <-df[sample(nrow(df),368),]
precios<-datos_map$PRICE

getColor <- function(precios_nor) {
  sapply(scale(precios_nor), function(tip) {
    if(tip <= -0.5) {
      "green"
    } else if(tip <= 0.75) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(log(precios))
)

leaflet(datos_map)%>%
  addTiles() %>%
  addAwesomeMarkers(lng=~LONGITUDE,
                    lat=~LATITUDE,
                    icon=icons,
                    label=~paste0(PRICE, " euros"),
                    popup=~paste0(PRICE, " euros"))


####################
####################



# Paso el data frame a un objeto espacial sf para poder aplicar todas las funciones de sf: 
                                                                                                                
#df_sf <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs("+proj=longlat +datum=WGS84"))

##########################################################
#### ANALISIS EXPLORATORIO DE DATOS ESPACIALES (AEDE) ####
##########################################################

####

library(caTools)
set.seed(123)
df_sample=df[sample(nrow(df),20000),]
split = sample.split(df_sample$PRICE, SplitRatio = 0.7)  #house.price es la variable dependiente
train = subset(df_sample, split==TRUE)
test = subset(df_sample, split==FALSE)






tmap_arrange(
  tm_shape(train) +
    tm_symbols(palette ="YlGn",col = "PRICE",style = "quantile")+
    tmap_options(check.and.fix = TRUE),
  tm_shape(train) +
    tm_symbols(palette ="Reds",col = "DISTANCE_TO_CITY_CENTER",style = "quantile")+
    tmap_options(check.and.fix = TRUE),
  nrow=1
)
cor(train$PRICE,train$DISTANCE_TO_CITY_CENTER)













