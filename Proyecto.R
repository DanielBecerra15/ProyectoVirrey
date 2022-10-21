library(readxl)
library(gtools)
library(dplyr)

base_mayo <- read_excel("C:/Users/danie/Downloads/BAA.xlsx", 
                        sheet = "MAYO")
compm<-matrix(data = NA, nrow = 886-nrow(base_mayo), ncol = 45)
colnames(compm)<-colnames(base_mayo)
base_mayof<-rbind(base_mayo,compm)

base_junio <- read_excel("C:/Users/danie/Downloads/BAA.xlsx", 
                        sheet = "JUNIO")
compjn<-matrix(data = NA, nrow = 886-nrow(base_junio), ncol = 45)
colnames(compjn)<-colnames(base_junio)
base_juniof<-rbind(base_junio,compjn)

base_julio <- read_excel("C:/Users/danie/Downloads/BAA.xlsx", 
                        sheet = "JULIO")
compjl<-matrix(data = NA, nrow = 886-nrow(base_julio), ncol = 45)
colnames(compjl)<-colnames(base_julio)
base_juliof<-rbind(base_julio,compjl)

base_agosto <- read_excel("C:/Users/danie/Downloads/BAA.xlsx", 
                        sheet = "AGOSTO")
compa<-matrix(data = NA, nrow = 886-nrow(base_agosto), ncol = 45)
colnames(compa)<-colnames(base_agosto)
base_agostof<-rbind(base_agosto,compa)

base_septiembre <- read_excel("C:/Users/danie/Downloads/BAA.xlsx", 
                          sheet = "SEPTIEMBRE")
base_septiembref <- base_septiembre

dat<-list(1:886)
varia<-list(colnames(base_agosto))
meses<-list(c("mayo","junio","julio","agosto","septiembre"))

base<-array(data = c(as.matrix(base_mayof),as.matrix(base_juniof), as.matrix(base_juliof), 
                     as.matrix(base_agostof), as.matrix(base_septiembref)),
            dim = c(886,45,5),
            dimnames = c(dat,varia,meses) )

mes<-"agosto"
cancelacion<-"SI"
especialidad<-"UROLOGIA"
especialista<-""

m = switch (mes,
  "mayo" = 1,
  "junio" = 2,
  "julio" = 3,
  "agosto" = 4,
  "septiembre" = 5
)

c = switch (cancelacion,
  "SI" = basec<-subset(base[,,m],subset = base[,24,m]=="SI"),
  "NO" = basec<-subset(base[,,m],subset = base[,24,m]!="SI")
)

e = switch (especialidad,
  "CIRUGIA GENERAL" = basee<-subset(basec, subset = basec[,12]=="CIRUGIA GENERAL"),
  "GINECOLOGIA" = basee<-subset(basec, subset = basec[,12]=="GINECOLOGIA"),
  "ORTOPEDIA" = basee<-subset(basec, subset = basec[,12]=="ORTOPEDIA"),
  "CIRUGIAPLASTICA Y RECONSTRUCTIVA" = basee<-subset(basec, subset = basec[,12]=="CIRUGIA PLASTICA Y RECONSTRUCTIVA"),
  "ODONTOLOGIA" = basee<-subset(basec, subset = basec[,12]=="ODONTOLOGIA"),
  "UROLOGIA" = basee<-subset(basec, subset = basec[,12]=="UROLOGIA"),
  "DERMATOLOGIA" = basee<-subset(basec, subset = basec[,12]=="DERMATOLOGIA"),
  "OFTALMOLOGIA" = basee<-subset(basec, subset = basec[,12]=="OFTALMOLOGIA")
)

sp = switch (especialista,
             ARIZA MAESTRE SHIRLEY DAYANA
             CABRERA DIAZ RONALD FRANCISCO 
             CABRERA QUECANO ANDRES GUILLERMO
             CASTELLANOS BECERRA ABRAHAM 
             ESCOBAR DIAZ ALEJANDRO
             ESCOBAR MARLES LINA PATRICIA 
             GODOY FORERO OMAR ANDRES 
             GRANADOS URIBE NELSON ADOLFO
             GUERRERO PRECIADO ROMAN ALBERTO 
             MIKAN LOZANO ANGELICA MARIA
             MONTOYA PALACIO GLORIA MARIA 
             MORALES CASTRO GULILLERMO ADOLFO
             MORENO SHETT KAREN 
             ORJUELA ESPINOSA NATHALIA
             PASTRAN ALFONSO ASTRID MARITZA 
             PEREA CASTELLANOS JORGE ARMANDO JUNIOR
             PINEDA ALZATE CARLOS ALBERTO 
             PINZON ALVAREZ GERMAN
             POLANCO ZULETA ANDRES FELIPE 
             ROJAS LOPEZ SUSANA
             SANTAMARIA RAMIREZ ALIDA 
             VILLADIEGO ROZO PEDRO LEON
             VILLAMIZAR FIGUEROA PEDRO 
             
)