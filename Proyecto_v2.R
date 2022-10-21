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
cancelacion<-"NO"
especialidad<-"CIRUGIA GENERAL"
especialista<-"ESCOBAR DIAZ ALEJANDRO"
procedimiento<-"HERNIORRAFIA UMBILICAL VIA ABIERTA"
anestesiologo<-"TORRES MARTINEZ ELIANA LISEL"
anestesia<-"REGIONAL"
m = switch (mes,
  "mayo" = 1,
  "junio" = 2,
  "julio" = 3,
  "agosto" = 4,
  "septiembre" = 5
)

switch (cancelacion,
  "SI" = basec<-subset(base[,,m],subset = base[,24,m]=="SI"),
  "NO" = basec<-subset(base[,,m],subset = base[,24,m]!="SI")
)

basee<-subset(basec, subset = basec[,12]==especialidad)

basesp<-subset(basee,subset = basee[,13]==especialista)

basep<-subset(basesp, subset = basesp[,7]==procedimiento)

basea<-subset(basep, subset = basep[,17]==anestesiologo)

basean<-subset(basea, subset = basea[,18]==anestesia)

View(basea)
