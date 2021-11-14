library(ggplot2)

data <- read.csv(file = '/home/laura/Desktop/univ/univ2021-2/Proba/proyecto/Muertes_Accidentes_Transito.csv')


# TABLA ANÁLISIS POR EDAD 
max_edad <- as.numeric(max(data[,4]))
min_edad <- as.numeric(min(data[,4]))
media <- mean(data[,4])
mediana <- median(data[,4])
dev_est <- sd(data[,4])
varianza <- var(data[,4], na.rm = FALSE)

sum(data[,7])


# GRÁFICA ANÁLISIS POR EL GÉNERO
M <- 0
Fem <- 0

for(i in data[,3])
  if(i == "Masculino")
    M <- M+1

for(i in data[,3])
  if(i == "Femenino")
    Fem <- Fem+1

daTa <- data.frame(
  Género=c("Masculino","Femenino") ,
  Cantidad=c(M,Fem)
)

ggplot(daTa,aes(x=Género, y=Cantidad)) + 
  geom_bar(stat = "identity",color="blue",fill="blue")


# GRÁFICA ANÁLISIS POR RANGOS DE EDAD
Edad <- data[,4] 
hist(Edad,main="Histograma de edad",col="lightblue",border="black",xlab = "Edad",ylab="Cantidad")


boxplot(data[,4],
        main = "Boxplot de edad",
        xlab = "Edad",
        ylab = "",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)


# GRÁFICA ANÁLISIS POR EL TIPO DE ACCIDENTE
Caida <- 0  
Ciclista <- 0
Colision <- 0
ColisionFija <- 0
ColisionAnimales <- 0
ColisionMovil <- 0
PeatonAtro <- 0
SinDatos <- 0
Volcamiento <- 0

for (i in data[,2])
  if (i == "Caída de Ocupante" ){
    Caida <- Caida + 1
  }else if(i == "Ciclista Atropellado"){
    Ciclista <- Ciclista + 1 
  }else if (i == "Colisión"){
    Colision <- Colision + 1
  }else if (i == "Colisión con Objeto Fijo" || i == "Colisión"){
    ColisionFija <- ColisionFija + 1
  }else if (i == "Colisión con animales"){
    ColisionAnimales <- ColisionAnimales + 1
  }else if (i == "Colisión con objeto movil" ||i == "Colisión con Objeto Móvil"){
    ColisionMovil <- ColisionMovil + 1
  }else if (i == "Peatón Atropellado"){
    PeatonAtro <- PeatonAtro + 1
  }else if(i == "Sin Dato"){
    SinDatos <- SinDatos + 1    
  }else{
    Volcamiento <- Volcamiento + 1
  }

library(ggplot2)
graficoCausa <- data.frame(
  Causa=c("Caída Ocupante","Ciclista Atropellado","Colisión","Colisión con Objeto fijo","Colisión con animales","Colisión con objeto móvil",
          "Peatón Atropellado","Sin Datos","Volcamiento") ,  
  val=c(Caida,Ciclista,Colision,ColisionFija,ColisionAnimales,ColisionMovil,PeatonAtro,SinDatos,Volcamiento)
)

ggplot(graficoCausa, aes(x=Causa, y=val)) + 
  geom_bar(stat = "identity",color="green",fill="lightgreen") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# GRAFICA ANÁLISIS DE LAS CARACTERÍSTICAS DE LA VÍCTIMA
ConVehiculo <- 0 # Conductores de auto 
Ciclista <- 0
Motociclista <- 0
Peatones <- 0
Pasajeros <- 0 # Pasajeros ya sean de un auto o moto 
sinDatos <- 0
otro <- 0 # Datos no considerados/otros 



for (i in data[,5])
  if (i == "Conductor Veh?culo" || i == "Conductor Vehículo" ){
    ConVehiculo <- ConVehiculo + 1
  }else if(i == "Ciclista"){
    Ciclista <- Ciclista + 1 
  }else if (i == "Conductor Moto"){
    Motociclista <- Motociclista + 1
  }else if (i == "Peat?n"||i == "Peaton"||i == "Peatón"){
    Peatones <- Peatones + 1
  }else if (i == "Pasajero Moto" ||i == "Pasajero Vehiculo" ||i == "Pasajero Vehículo"){
    Pasajeros <- Pasajeros + 1
  }else if(i == "Sin Dato"){
    sinDatos <- sinDatos + 1    
  }else{
    otro <- otro + 1
  }

library(ggplot2)
graficoVictimas <- data.frame(
  Victimas=c("Conductor","Ciclistas","Motociclistas","Peatones","Pasajeros","Sin Datos","Otros") ,  
  val=c(ConVehiculo,Ciclista,Motociclista,Peatones,Pasajeros,sinDatos,otro)
)

ggplot(graficoVictimas,aes(x=Victimas, y=val)) + 
  geom_bar(stat = "identity",color="green",fill="lightgreen")


