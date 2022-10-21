library(readxl)
library(gtools)
library(dplyr)
library(DataExplorer)
library(data.table)
library(ggplot2)
library(magrittr)


# LECTURA DE BASE DE DATOS -------------------------------------------------

base_mayo <- read_excel("C:/Users/danie/Desktop/Estadistica/ProgramaVirrey/VIRREY/data/BAA.xlsx",
                        sheet = "MAYO")

base_junio <- read_excel("C:/Users/danie/Desktop/Estadistica/ProgramaVirrey/VIRREY/data/BAA.xlsx", 
                         sheet = "JUNIO")

base_julio <- read_excel("C:/Users/danie/Desktop/Estadistica/ProgramaVirrey/VIRREY/data/BAA.xlsx", 
                         sheet = "JULIO")

base_agosto <- read_excel("C:/Users/danie/Desktop/Estadistica/ProgramaVirrey/VIRREY/data/BAA.xlsx", 
                          sheet = "AGOSTO")

base_septiembre <- read_excel("C:/Users/danie/Desktop/Estadistica/ProgramaVirrey/VIRREY/data/BAA.xlsx", 
                              sheet = "SEPTIEMBRE")
base<-list(base_mayo,base_junio,base_julio,base_agosto,base_septiembre)


# FUNCIONES ---------------------------------------------------------------

barras_general<-function(x){plot_bar(x,order_bar = T, nrow = 2, ncol = 2)}
hist_general<-function(x){plot_histogram(x)}
tablas<-function(x){
  for (y in 1:ncol(x)) {
    t<-table(x[, y])
    print(t) %>% head()
  }
}
resumen<-function(x){
  barras_general(x)
  hist_general(x)
  tablas(x)
}

# ENTRADAS ----------------------------------------------------------------

mes<-"agosto"
cancelacion<-"NO"
especialidad<-"CIRUGIA GENERAL"
especialista<-"ESCOBAR DIAZ ALEJANDRO"
procedimiento<-"HERNIORRAFIA UMBILICAL VIA ABIERTA"
anestesiologo<-"TORRES MARTINEZ ELIANA LISEL"
anestesia<-"REGIONAL"

# FILTRO POR MES-----------------------------------------------------------

m = switch (mes,                #Asignar valor de posicion al mes
            "mayo" = 1,
            "junio" = 2,
            "julio" = 3,
            "agosto" = 4,
            "septiembre" = 5
)

basem<-as.data.frame(base[m])  #Define una tabla de datos por el mes seleccionado

# PURGA DE BASE DE DATOS --------------------------------------------------

basem$COMPLICACIONES[is.na(basem$COMPLICACIONES)] <- "NO"
basem$MORTALIDAD[is.na(basem$MORTALIDAD)] <- "NO"
basem$PATOLOGIA[is.na(basem$PATOLOGIA)] <- "NO"
basem$PROFILAXIS.ANTIBIOTICA[is.na(basem$PROFILAXIS.ANTIBIOTICA)] <- "NO"
basem$TIPO.DE.ANESTESIA[basem$TIPO.DE.ANESTESIA=="REGIONAL : SEDACION"] <- "REGIONAL + SEDACION"
basem$FECHA.DE.CIRUGIA<-as.character.Date(basem$FECHA.DE.CIRUGIA)
names(basem)[25]<-"CAUSA.DE.CANCELACION"
variable.names(basem)

# ELIMINACION DE COLUMNAS -------------------------------------------------

basem<- basem %>% 
  select (FECHA.DE.CIRUGIA, EDAD, GENERO, PROCEDIMIENTO.1, PROCEDIMIENTO.2, 
         (PROCEDIMIENTO.3:TIPO.DE.ANESTESIA), PROFILAXIS.ANTIBIOTICA, 
         (CANCELACION:TIPO.DE.COMPLICACION), (MORTALIDAD:CULTIVO))
  
resumen(basem %>% select(-OBSERVACIONES.CANCELACION) %>% head())
#colnames(basem)
# FILTROS -----------------------------------------------------------------

switch (cancelacion,           #Filtro de cancelación
        "SI" = 
          {basec<-basem %>% 
            subset (basem$CANCELACION == "SI") %>% 
            select((FECHA.DE.CIRUGIA:PROCEDIMIENTO.1), ESPECIALIDAD.TRATANTE, 
                   CIRUJANO.TRATANTE, CAUSA.DE.CANCELACION, OBSERVACIONES.CANCELACION)
          },
        "NO" = 
          {basec<-basem %>% 
            subset (basem$CANCELACION != "SI") %>% 
            select(-(CANCELACION:OBSERVACIONES.CANCELACION))
           }
)
resumen(basec)

                  #Filtro de especialidad

basee <- basec %>% subset(basec$ESPECIALIDAD.TRATANTE==especialidad) 
basee <- basee %>% select(-ESPECIALIDAD.TRATANTE) %>% resumen

                  #filtro de especialista

basesp <- basee %>% subset(basee$CIRUJANO.TRATANTE==especialista) 
basesp <- basesp %>% select(-CIRUJANO.TRATANTE) %>% resumen

                  #Filtro de procedimiento

basep <- basesp %>% subset(basesp$PROCEDIMIENTO.1==procedimiento) 
basep <- basep %>% select(-PROCEDIMIENTO.1)%>% resumen

                  #Filtro de anestesiólogo

basea <- basep %>% subset(basep$ANESTESIOLOGO==anestesiologo)  
basea <- basea %>% select(-ANESTESIOLOGO)  %>% resumen

                  #Filtro de tipo de anestesia

basean <- basea %>% subset(basea$TIPO.DE.ANESTESIA==anestesia)  
basean <- basean %>% select(-TIPO.DE.ANESTESIA) %>% resumen
