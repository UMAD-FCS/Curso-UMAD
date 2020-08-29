# ---------------------------------------------------------------------------- #
#                                SESION V
#        Microdatos, metadatos, formulario y diccionario de variables. 
#           Un ejemplo desde las encuestas de hogares en Uruguay  
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
#  Se entregan los microdatos de la Encuesta continua de Hogares 2019 (INE).
#  A partir de esta informaci?n realice las siguientes activiades
# 
#  1) Construya una variabe que indique la condici?n de activiad de las personas
#     con las siguientes categor?a. Para su construcci?n consulte el diccionario 
#     de variables, el formulario, el manual de encuestador y los metadatos del
#     Instituto Nacional de Estad?stica. 
#     1. Menores de 14 a?os
#     2. Ocupados
#     3. Desocupados
#     4. Inactivos
#   
# 
#  2) Calcule las tasas de actividad, empleo y desempleo para el total pa?s
# 
#  3) A partir de la revisi?n de los metadatos del INE analice las posibilidades
#     an?lisis geogr?fico que habilita a encuesta. ?Para qu? unidades de an?lisis
#     es representativa? ?Qu? variables de agrupaci?n territorial son provistas
#     por el INE en los microdatos publicados?
# 
#  4) Calcule las tasas de actividad, empleo y desempleo para Montevideo, las 
#     ciudades del interior con m?s de 5.000 habitantes y las ciudades
#     del interior con menos de 5.000 habitantes. Compare los resultados. 
# 
#  5) Analice las tasas de actividad, empleo y desempleo en Montevideo y su 
#     ?rea metropolitana. ?Hay diferencias en estos indicadores de empleo 
#     entre el ?rea que corresponde a Canelones y San Jos?? ?Hay diferencias
#     entre municipios?
#  
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#  0) Preparacion del ambito de trabajo
# ---------------------------------------------------------------------------- #

#install.packages("rio")
#install.packages("tidyverse")

library(rio)
library(tidyverse)

setwd("C:/Users/Usuario/Dropbox/1. Unidad de M?todos y Acceso a Datos/4. Cursos/Educaci?n permanente_Recursos/Microdatos")
ech2019 <- rio::import ("ECH2019.csv") 


# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#  1) Construya una variabe que indique la condici?n de activiad de las personas
#     con las siguientes categor?a. Para su construcci?n consulte el diccionario 
#     de variables, el formulario, el manual de encuestador y los metadatos del
#     Instituto Nacional de Estad?stica. 
#     1. Menores de 14 a?os
#     2. Ocupados
#     3. Desocupados
#     4. Inactivos
# ---------------------------------------------------------------------------- #


#~~~~~~~~~~~#
# OCUPADOS 
# 
# Para identificar la poblaci?n ocupada tenga en cuenta que:
# 
# Se entiene por poblaci?n ocupada a quienes ha trabajado como m?nimo una hora 
# en una actividad econ?mica en la semana anterior (semana de referencia a la 
# aplicaci?n de la encuesta). 
#
# Bajo este entendido revise las siguientes variables de la encuesta dado que 
# identifican al conjunto de situaciones posibles para definir si la persona 
# se encuentra ocupada o no:
# 
# (f269 == 1) Trabaj? la semana pasada .
# (f270 == 1) La semana pasada, al menos una hora, colabor? en un negocio familiar 
#             o llev? a cabo alg?n trabajo puntual y concreto 
# (f271 == 1) Recibi? o recibir? un sueldo o pago
# (f272 == 1) Trabaj? para un negocio del hogar
# (f273 == 1) Aunque no trabaj? la semana pasada, tiene alg?n trabajo, negocio 
#             o actividad por la que percibe habitualmente un pago
# (f69 == 1)  No trabaj? por estar de licencia
# ((f69 > 1 & f69 < 9) & (f274 = 1)) No trabaj? la semana pasada pero sigui? 
#             recibiendo sueldos o ganancias
# (f275 < 3)  No trabaj? la semana pasada, pero volver? en menos de 12 semanas
# (f276 == 1) Realiz? tareas agr?colas para producir alimentos para el consumo 
#             del hogar
# (f277 == 1) M?s de la mitad de lo que produce para consumo del hogar en el a?o, 
#             lo destina a la venta
# (f107 == 1) Busc? trabajo en la ?ltima semana
# (f108 == 2 | f108 = 3) Tiene trabajo que comenzar? en los pr?ximos 30 d?as o 
#             est? esperando el resultado de gestiones ya emprendidas
#~~~~~~~~~~~#

#~~~~~~~~~~~#
# DESEMPLEADOS
# 
# Para identificar la poblaci?n desocupada tenga en cuenta que:
# 
# Se entiene por poblaci?n desocupada a quienes se refiere a personas que, 
# no teniendo ocupaci?n, est?n buscando activamente trabajo. 
# 
# Bajo este entendido revise las siguientes variables de la encuesta dado que 
# identifican al conjunto de situaciones posibles para definir si la persona 
# se encuentra desocupada o no:
# 
# (f109 == 1) Busc? trabajo en las ?ltimas cuatro semanas 
# (f110  < 7) Llev? acciones concretas de b?squeda 
# (f116 == 1) Trabaj? alguna vez en su vida
# (f116 == 2) No trabaj? nunca en su vida
# (f69  == 9) Se encuentra en seguro de desempleo (F.1)
# (f117 == 1) Se encuentra en seguro de desempleo (F.6)
#~~~~~~~~~~~#

#~~~~~~~~~~~#
# INACTIVOS
# 
# Para identificar la poblaci?n desocupada tenga en cuenta que:
# 
# Se considera poblaci?n inactiva a las personas mayores de 14 a?os que no
# tienen trabajo ni lo buscan activamente. 
#~~~~~~~~~~~#


ech2019 <- ech2019 %>% mutate(
           menores  = case_when(
           e27 <  14              ~ 1,
           TRUE                   ~ 0
           )
)           

ech2019 <- ech2019 %>% mutate(
           ocupados  = case_when(
           
          ((e27   >=  14)  &            
          
           (f269 == 1 | f270 == 1 & f271 == 1) |
             
           (f269 == 2 & f270 == 1 & f271 == 2 & f272 == 1) |
             
           (f269 == 1 & f271 == 2 & f272 == 2 & f273 == 1  & f69 == 1) |

           (f269 == 2 & f270 == 1 & f271 == 2 & f272 == 1) |	
          
           (f269 == 1 & f271 == 2 & f272 == 2 & f273 == 1  & f69 == 1)  |

           (f269 == 1 & f271 == 2 & f272 == 2 & f273 == 1  & f69 != 1 & 
            f69 != 9 & f69 != 0 & f274 == 1) | 
             
           (f274 == 2 &(f275 == 1 | f275 == 2) |
                
           (f269 == 2 & f270 == 1 & f271 == 2 & f272 == 2 & f273 == 1 & f69 == 1) |
                
           (f269 == 1 & f271 == 2 & f272 == 1) |
                
           (f269 == 2 & f270 == 2 & f273 == 1 & f69 == 1) |
                
           ((f269 == 2 & f270 == 2 & f273 == 1 & 
            (f69 == 2 | f69 == 3 | f69 == 5 | f69 == 6 | f69 == 7 | f69== 8) & 
             f274 == 1) |
                   
           ((f269 == 2 & f270 == 2 & f273 == 1 & 
            (f69 == 2 | f69 == 3 | f69 == 5 | f69 == 6 | f69 == 7 | f69== 8) & 
             f274 == 2 & (f275 == 1 | f275 == 2)) |
                      
           (f276 == 1 & f277 == 1)))))  ~ 1,             

           TRUE                         ~ 0
           )
)

ech2019 <- ech2019 %>% mutate(
           desempleados  = case_when(
    
          ((e27        >=  14)  & 
           (ocupados   !=  1)   & 
          
          (((f106 == 1 & (f107 == 1 | f109 == 1)) & (f110 > 0  &  f110<= 6)) | 
            (f106 == 1 & (f108 == 2 | f108 == 3))))  ~ 1, 

          TRUE                         ~ 0
           )
)

ech2019 <- ech2019 %>% mutate(
           condicion  = case_when(
           menores     == 1                                     ~ 1,
           ocupados    == 1                                     ~ 2,
           desempleados == 1                                    ~ 3,
           (menores == 0 | (ocupados == 0 & desempleados == 0)) ~ 4,
           )
)
    
    
sum(is.na(ech2019$condicion))
table (ech2019$condicion)

             
# ---------------------------------------------------------------------------- #
#  2) Calcule las tasas de actividad, empleo y desempleo para el total pa?s
# ---------------------------------------------------------------------------- #

#~~~~~~~~~~~#
# TASA DE ACTIVIDAD: Cociente entre la poblaci?n activa (ocupados y desempleados)
#                     y la poblaci?n en edad de trabajar (mayores de 14 a?os).
#~~~~~~~~~~~#

pea <- filter(ech2019, (ocupados == 1 | desempleados == 1))
pea <- sum(pea$pesoano)

pet <- filter(ech2019, e27>=14)
pet <- sum(pet$pesoano)

TA <- pea/pet*100
TA

#~~~~~~~~~~~#
# TASA DE EMPLEO: Cociente entre la poblaci?n empleada y la poblaci?n en edad 
#                 de trabajar (mayores de 14 a?os).
#~~~~~~~~~~~#

pobocupada <- filter(ech2019, ocupados == 1)
pobocupada <- sum(pobocupada$pesoano)

TE <- pobocupada/pet*100
TE

#~~~~~~~~~~~#
# TASA DE DESEMPLEO: Cociente entre la poblaci?n desempleada y la poblaci?n 
#                    econ?micamente activa.
#~~~~~~~~~~~#

pobdesocupada <- filter(ech2019, desempleados == 1)
pobdesocupada <- sum(pobdesocupada$pesoano)

TD <-pobdesocupada/pea*100
TD

tasas_totalpais <- matrix(c (TA, TE, TD), nrow = 1, ncol = 3)
colnames(tasas_totalpais) <- c('TA', 'TE', 'TD')
tasas_totalpais


# ---------------------------------------------------------------------------- #
#  3) A partir de la revisi?n de los metadatos del INE analice las posibilidades
#     an?lisis geogr?fico que habilita a encuesta. ?Para qu? unidades de an?lisis
#     es representativa? ?Qu? variables de agrupaci?n territorial son provistas
#     por el INE en los microdatos publicados?
# ---------------------------------------------------------------------------- #

#~ Revision de metadatos

# ---------------------------------------------------------------------------- #
#  4) Calcule las tasas de actividad, empleo y desempleo para Montevideo, las 
#     ciudades del interior con m?s de 5.000 habitantes y las ciudades
#     del interior con menos de 5.000 habitantes. Compare los resultados. 
# ---------------------------------------------------------------------------- #

regiones <- split(ech2019, ech2019$region_3)

m = 3
n = 3
tasas_r = matrix(0, m, n)
colnames(tasas_r) <- c('TA', 'TE', 'TD')
rownames(tasas_r) <- c('Montevideo', 'Interior mas de 5.000', 'Interior menos de 5.000')


for(i in 1:m){
  
  pea_r  <- filter(regiones[[i]] , (ocupados == 1 | desempleados == 1))
  pea_r  <- sum(pea_r$pesoano)
  
  pet_r  <- filter(regiones[[i]], e27>=14)
  pet_r  <- sum(pet_r$pesoano)
  
  pobocupada_r <- filter(regiones[[i]], ocupados == 1)
  pobocupada_r <- sum(pobocupada_r$pesoano)
  
  pobdesocupada_r  <- filter(regiones[[i]], desempleados == 1)
  pobdesocupada_r  <- sum(pobdesocupada_r$pesoano)
  
  tasas_r[i,1] = pea_r/pet_r*100
  tasas_r[i,2] = pobocupada_r/pet_r*100
  tasas_r[i,3] = pobdesocupada_r/pea_r*100
  
}
tasas_r  




# ---------------------------------------------------------------------------- #
#  5) Analice las tasas de actividad, empleo y desempleo en Montevideo y su 
#     ?rea metropolitana. ?Hay diferencias en estos indicadores de empleo 
#     entre el ?rea que corresponde a Canelones y San Jos?? ?Hay diferencias
#     entre municipios? 
# ---------------------------------------------------------------------------- #

# Area metropolitana

unique(ech2019$dpto)
unique(ech2019$nomdpto)
unique(ech2019$estred13)

ech2019 <- ech2019 %>% mutate(
           ametropolitana  = case_when(
           dpto == 1                      ~ 1,
           dpto == 3  & estred13 ==6      ~ 2,
           dpto == 16 & estred13 == 6     ~ 3
           )
)
              
table(ech2019$ametropolitana)

regiones_amet<- split(ech2019, ech2019$ametropolitana)

m = 3
n = 3
tasas_amet = matrix(0, m ,n)
colnames(tasas_amet) <- c('TA', 'TE', 'TD')
rownames(tasas_amet) <- c('Montevideo', 'Canelones', 'San Jose')


for(i in 1:m){
  
  pea_amet  <- filter(regiones_amet[[i]] , (ocupados == 1 | desempleados == 1))
  pea_amet  <- sum(pea_amet$pesoano)
  
  pet_amet  <- filter(regiones_amet[[i]], e27>=14)
  pet_amet  <- sum(pet_amet$pesoano)
  
  pobocupada_amet <- filter(regiones_amet[[i]], ocupados == 1)
  pobocupada_amet <- sum(pobocupada_amet$pesoano)
  
  pobdesocupada_amet  <- filter(regiones_amet[[i]], desempleados == 1)
  pobdesocupada_amet  <- sum(pobdesocupada_amet$pesoano)
  
  tasas_amet[i,1] = pea_amet/pet_amet*100
  tasas_amet[i,2] = pobocupada_amet/pet_amet*100
  tasas_amet[i,3] = pobdesocupada_amet/pea_amet*100
  
}
tasas_amet


# Municipios

table(ech2019$ccz)

ech2019 <- ech2019 %>% mutate(
           municipios  = case_when(
    dpto == 1 & (ccz == 18 | ccz == 17 | ccz == 14)                ~ 1,
    dpto == 1 & (ccz == 1  | ccz == 2)                             ~ 2,
    dpto == 1 & (ccz == 3  | ccz == 15 | ccz == 16)                ~ 3,
    dpto == 1 & (ccz == 10 | ccz == 11)                            ~ 4,
    dpto == 1 & (ccz == 6  | ccz == 7  | ccz == 8)                 ~ 5,
    dpto == 1 & (ccz == 9)                                         ~ 6,
    dpto == 1 & (ccz == 12 | ccz == 13)                            ~ 7,
    dpto == 1 & (ccz == 4  | ccz == 54)                            ~ 8
  )
)

table(ech2019$municipios)

regiones_mun<- split(ech2019, ech2019$municipios)

m = 8
n = 3
tasas_mun = matrix(0, m, n)
colnames(tasas_mun) <- c('TA', 'TE', 'TD')
rownames(tasas_mun) <- paste("Municipio", 1:8)
  

for(i in 1:m){
  
  pea_mun  <- filter(regiones_mun[[i]] , (ocupados == 1 | desempleados == 1))
  pea_mun  <- sum(pea_mun$pesoano)
  
  pet_mun  <- filter(regiones_mun[[i]], e27>=14)
  pet_mun  <- sum(pet_mun$pesoano)
  
  pobocupada_mun <- filter(regiones_mun[[i]], ocupados == 1)
  pobocupada_mun <- sum(pobocupada_mun$pesoano)
  
  pobdesocupada_mun  <- filter(regiones_mun[[i]], desempleados == 1)
  pobdesocupada_mun  <- sum(pobdesocupada_mun$pesoano)
  
  
  tasas_mun[i,1] = pea_mun/pet_mun*100
  tasas_mun[i,2] = pobocupada_mun/pea_mun*100
  tasas_mun[i,3] = pobdesocupada_mun/pea_mun*100
  
}
tasas_mun
