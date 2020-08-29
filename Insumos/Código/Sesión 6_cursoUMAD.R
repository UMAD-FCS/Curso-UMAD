# ---------------------------------------------------------------------------- #
#                                  SESION VI
#                              ACTIVIDAD PRACTICA
#          *****   Respondiendo preguntas sobre desigualdad con ECH   *****
#                     
#                        CURSO EDUCACION PERMANENTE
#                     Unidad de Metodos y Acceso a datos
#
#     Recursos, usos y estrategias para el tratamiento sistemetico de datos
#                  en proyectos y trabajos de estudiantes 
#
#  
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#
#  ** CONSIGNA DEL EJERCICIO **
#  
#  Se entregan los microdatos de la Encuesta Continua de Hogares 2019. A partir 
#  de la misma responda las siguientes preguntas de investigaci?n 
#  (recuerde ponderar):
#    
#   1) ?Qu? proporci?n de adolescentes de 14 a 17 a?os percibe ingresos propios?
#  
#   2) ?Hay diferencias en la proporci?n de adolescentes perceptores entre 
#       aquellos que residen en el quintil 1 y el quintil 5?
#    
#   3) ?Hay diferencias en la media de ingresos percibidos entre aquellos que 
#       perciben ingreso en el quintil 1 y quintil 5?
#    
#   4) ?Hay diferencias en la media de ingreso per-c?pita del hogar en los hogares
#      en que hay adolescentes perceptores en comparaci?n con aquellos en que no?
#
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#  0) Preparacicon del ambito de trabajo
# ---------------------------------------------------------------------------- #

#install.packages("rio")
#install.packages("tidyverse")

library(rio)
library(tidyverse)

setwd("C:/Users/Usuario/Dropbox/1. Unidad de M?todos y Acceso a Datos/4. Cursos/Educaci?n permanente_Recursos/Microdatos")
ech2019 <- rio::import ("ECH_2019.dta") 

# ---------------------------------------------------------------------------- #
#   1) ?Qu? proporcion de adolescentes de 14 a 17 a?os percibe ingresos propios?
# ---------------------------------------------------------------------------- #


filt_adolescentes  <- filter(ech2019, e27 >= 14 & e27<=17)
pobadolescente <- sum(filt_adolescentes$pesoano)

filt_yadolescentes  <- filter(ech2019, e27 >= 14 & e27<=17 & PT1>0)
yadolescentes  <- sum(filt_yadolescentes$pesoano)

prop <- yadolescentes/pobadolescente
prop


# ---------------------------------------------------------------------------- #
#   2) ?Hay diferencias en la proporci?n de adolescentes perceptores entre 
#       aquellos que residen en el quintil 1 y el quintil 5?
# ---------------------------------------------------------------------------- #

# Construccion de la variable quintil

ech2019 <- mutate(ech2019, ypc = HT11/ ht19)


dq <- unique(ech2019[,c(1, 557)])
q <- quantile(dq$ypc, prob=seq(0,1,1/5))

quintil <- numeric()
for(i in 1:nrow(dq)){
  if(dq$ypc[i] >= q[1] & dq$ypc[i] <= q[2]) { quintil[i] <- 1 }
  if(dq$ypc[i] >  q[2] & dq$ypc[i] <= q[3]) { quintil[i] <- 2 }
  if(dq$ypc[i] >  q[3] & dq$ypc[i] <= q[4]) { quintil[i] <- 3 }
  if(dq$ypc[i] >  q[4] & dq$ypc[i] <= q[5]) { quintil[i] <- 4 }
  if(dq$ypc[i] >  q[5]                    ) { quintil[i] <- 5 }
  
}

dq$quintil <- quintil
ech2019 <- merge(ech2019, dq[, c(1, 3)], by = "numero")


# Calculo de proporciones

quintiles <- split(ech2019, ech2019$quintil)

m=5
n=1
adol_q   = matrix(0,m,n)
yadol_q  = matrix(0,m,n)


for(i in 1:m){
  
  filt_adolescentes  <- filter(quintiles[[i]], e27 >= 14 & e27<=17)
  pobadolescente <- sum(filt_adolescentes$pesoano)
  
  filt_yadolescentes  <- filter(quintiles[[i]], e27 >= 14 & e27<=17 & PT1>0)
  yadolescentes  <- sum(filt_yadolescentes$pesoano)
  
  adol_q [i,1]  = pobadolescente
  yadol_q[i,1] = yadolescentes

}

prop_q = yadol_q/adol_q

tabla_yadol_q = cbind (adol_q,yadol_q, prop_q)
colnames(tabla_yadol_q) <- c('Adolescentes', 'Adolescentes y propio', 'Proporcion')
rownames(tabla_yadol_q) <- c('Q1', 'Q2', 'Q3','Q4','Q5')
tabla_yadol_q

# ---------------------------------------------------------------------------- #
#   3) ?Hay diferencias en la media de ingresos percibidos entre aquellos que 
#       perciben ingreso en el quintil 1 y quintil 5?
# ---------------------------------------------------------------------------- #


ech2019 <- mutate(ech2019, my_exp = PT1 * pesoano)
quintiles <- split(ech2019, ech2019$quintil)

myadol_q  = matrix(0,m,n)

for(i in 1:m){
  
  filt_yadolescentes  <- filter(quintiles[[i]], e27 >= 14 & e27<=17 & PT1>0)
  myadol_exp     <- sum(filt_yadolescentes$my_exp)
  yadolescentes  <- sum(filt_yadolescentes$pesoano)
  
  myadol_q [i,1]  = myadol_exp/yadolescentes*100

}

tabla_yadol_q = cbind (tabla_yadol_q,myadol_q)
colnames(tabla_yadol_q) <- c('Adolescentes', 'Adolescentes y propio', 'Proporcion', 'Media y')
rownames(tabla_yadol_q) <- c('Q1', 'Q2', 'Q3','Q4','Q5')
tabla_yadol_q


# ---------------------------------------------------------------------------- #
#   4) ?Hay diferencias en la media de ingreso per-c?pita del hogar en los hogares
#      en que hay adolescentes perceptores en comparaci?n con aquellos en que no?
# ---------------------------------------------------------------------------- #

ech2019 <- ech2019 %>% mutate(
  adolescente  = case_when(
    
    e27 >= 14 & e27 <= 17      ~ 1,
    e27 <  14 | e27 >  17      ~ 2,
  )
)

h_adolescente <- ech2019 %>%
                 group_by(numero) %>%
                 summarize(h_adolescente   = min(adolescente))

ech2019 = merge(ech2019, h_adolescente, by = "numero")

ech2019 <- mutate(ech2019, ypc_exp = ypc * pesoano)
ech2019 <- mutate(ech2019, uno = 1)

hogares <- unique(ech2019[,c(1, 17, 557, 559, 560, 561)])

h_adol <- split(hogares, hogares$h_adolescente)

mypc_adol  = matrix(0,2,1)
colnames(mypc_adol) <- c('Media ypc')
rownames(mypc_adol) <- c('Sin adolescentes', 'Con adolescentes')


for(i in 1:2){

tot_hogares    <- sum(h_adol[[i]]$pesoano)
ytot_hogares   <- sum(h_adol[[i]]$ypc_exp)

mypc_adol[i,1] = ytot_hogares/tot_hogares

}

mypc_adol

