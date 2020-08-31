# ---------------------------------------------------------------------------- #
#                               SESION III y IV
#  Estructura basica de la informacion y elementos claves de la estad?stica 
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
#  Se entrega una fragmento de la Encuesta Uso del Tiempo y el trabajo no
#  remunerdado (EUT) de 2013 con las siguientes variables:
#  
#  numero:      codigo identificador del hogar
#  pesoeut:     ponderador de la base de datos
#  hstr_sem:    horas de trabajo remunerado semanales
#  hstnr_sem:   horas de trabajo no remunerado semanales
#  sexo:        sexo
# 
#  A partir de esta informaci?n:
#  
#  1) Construya una curva de distribucion de frecuencias simples del trabajo  
#     remunerado. Establezca su media, moda, mediana, varianza y desvio estandar.
#  
#  2) Construya una curva de distribucion de frecuencias simples del trabajo no 
#     remunerado. Establezca su media, moda, mediana, varianza y desvio estandar.
#  
#  3) Generar un grafico de tipo scatter y la regresion lineal que surge de 
#     combinar estas dos variables.
#  
#  4) Comparar las medias de ambas variables a partir de la variable sexo.
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
eut2013 <- rio::import (here::here("Scripts_curso", "EUT_2013_reduc.xls")) 

#Expansi?n de base de datos

data0 <- unique(eut2013[,c(1, 2)])
data2 <- split(data0, data0$numero)

out <- list()
for(i in 1:length(data2)){
  
    out[[i]] <- data.frame(numero  = rep(data2[[i]]$numero, data2[[i]]$peso_eut),
                new_num = as.numeric(paste0(data2[[i]]$numero, 1:data2[[i]]$peso_eut))) 
}
out[[1]]

out2 <- do.call(rbind, out)
eut2013exp = merge(out2, eut2013, by = "numero")

# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#  1) Construya una curva de distribuci?n de frecuencias simples del trabajo  
#     remunerado. Establezca su media, moda, mediana, varianza y desvio estandar.
# ---------------------------------------------------------------------------- #


# Filtro: Personas que realizan trabajo remunerado
hace_tr <- filter(eut2013exp, hstr_sem > 0)

# ~~~~~~~~~~~~~~~~~~
# Media
# ~~~~~~~~~~~~~~~~~~
mean  (hace_tr$hstr_sem, na.rm = FALSE)

# media manual
sum(hace_tr$hstr_sem) / length(hace_tr$hstr_sem)

# ~~~~~~~~~~~~~~~~~~
# Mediana
# ~~~~~~~~~~~~~~~~~~
median(hace_tr$hstr_sem, na.rm = FALSE)

orden     <- sort(hace_tr$hstr_sem, decreasing = TRUE)
ubicacion <- ceiling(length(hace_tr$hstr_sem) / 2) # ceiling por si es numero impar el largo
mediana   <- orden[ubicacion] 
mediana

# ~~~~~~~~~~~~~~~~~~
# Moda
# ~~~~~~~~~~~~~~~~~~
tabla_tr <- table(hace_tr$hstr_sem)
names(sort(tabla_tr, decreasing = TRUE)[1])

# ~~~~~~~~~~~~~~~~~~
# Varianza
# ~~~~~~~~~~~~~~~~~~
var(hace_tr$hstr_sem, na.rm = FALSE)

# varianza manual (prmedio de desviaciones al cuadrado)
sum((hace_tr$hstr_sem - mean(hace_tr$hstr_sem))^2) / (length(hace_tr$hstr_sem) - 1)

# por partes
distancia <- hace_tr$hstr_sem - mean(hace_tr$hstr_sem)
cuadrado  <- distancia ^ 2
suma_dis2 <- sum(cuadrado)
varianza  <- suma_dis2 / (length(hace_tr$hstr_sem) - 1)
varianza

# ~~~~~~~~~~~~~~~~~~
# Desvio estandar
# ~~~~~~~~~~~~~~~~~~
sd(hace_tr$hstr_sem, na.rm = FALSE)

# manual
sqrt(var(hace_tr$hstr_sem, na.rm = FALSE))
sqrt(varianza)


# Histograma
hist(hace_tr$hstr_sem, freq = TRUE) 
hist(hace_tr$hstr_sem, freq = FALSE)

# Histograma con l?nea de densidad
hist(hace_tr$hstr_sem, freq = FALSE, ylim = c(0, 0.2))
lines(density(hace_tr$hstr_sem), col = "red",  lwd = 2)
abline(v = mean(hace_tr$hstr_sem),lty=3   , col = "red")
abline(v = median(hace_tr$hstr_sem),lty=3, col = "blue")


# ---------------------------------------------------------------------------- #
#  2) Construya una curva de distribucion de frecuencias simples del trabajo no 
#     remunerado. Establezca su media, moda, mediana, varianza y desvio estandar.
# ---------------------------------------------------------------------------- #


# Filtro: Personas que realizan trabajo no remunerado
hace_tnr <- filter(eut2013exp, hstnr_sem > 0)

# Media
mean  (hace_tnr$hstnr_sem, na.rm = FALSE)

# Mediana
median(hace_tnr$hstnr_sem, na.rm = FALSE)

# Moda
tabla_tnr <- table(hace_tnr$hstnr_sem)
sort(tabla_tnr, decreasing = TRUE)

# Desv?o est?ndar
sd (hace_tnr$hstnr_sem, na.rm = FALSE)

# Varianza
var(hace_tnr$hstnr_sem, na.rm = FALSE)

# Histograma
hist(hace_tnr$hstnr_sem, freq = TRUE) 
hist(hace_tnr$hstnr_sem, freq = FALSE)

# Histograma con l?nea de densidad
hist(hace_tnr$hstnr_sem, freq = FALSE, ylim = c(0, 0.05))
lines(density(hace_tnr$hstnr_sem), col = "red",  lwd = 2)
abline(v = mean(hace_tnr$hstnr_sem)  ,lty=3, col = "red")
abline(v = median(hace_tnr$hstnr_sem),lty=3, col = "blue")





# ---------------------------------------------------------------------------- #
#  3) Generar un grafico de tipo scatter y la regresion lineal que surge de 
#     combinar estas dos variables.
# ---------------------------------------------------------------------------- #

plot (eut2013exp$hstr_sem, eut2013exp$hstnr_sem)

modelo <- lm(hstr_sem ~ hstnr_sem, data = eut2013exp)

str    (modelo)
print  (modelo)
class  (modelo)
summary(modelo) 

modelo.sum <- summary(modelo)
modelo.sum$r.squared ()	

plot(modelo)


# ---------------------------------------------------------------------------- #
#  4) Comparar las medias de ambas variables a partir de la variable sexo.
# ---------------------------------------------------------------------------- #

# Medias

tabla_sexo <- summarise(group_by(eut2013exp, sexo), 
                    TR  = mean(hstr_sem), 
                    TNR = mean(hstnr_sem)
                    )
tabla_sexo

# Filtros

mujeres <- filter(eut2013exp, sexo == "mujer" , hstr_sem>0, hstnr_sem>0)
varones <- filter(eut2013exp, sexo == "hombre", hstr_sem>0, hstnr_sem>0)


# Grafico: Trabajo remunerado segun sexo

plot (density(mujeres$hstr_sem), col = "red" , ylim = c(0, 0.2)); 
abline(v=mean(mujeres$hstr_sem),lty=3, col = "red")
lines(density(varones$hstr_sem), col = "green")
abline(v=mean(varones$hstr_sem),lty=3, col = "green")
legend("topright", c("Mujeres", "Varones"),
       lty = 1, col = c("red", "green")
       )

# Gr?fico: Trabajo no remunerado segun sexo

plot (density(mujeres$hstnr_sem), col = "red" , ylim = c(0, 0.07)); 
abline(v=mean(mujeres$hstnr_sem),lty=3, col = "red")
lines(density(varones$hstnr_sem), col = "green")
abline(v=mean(varones$hstnr_sem),lty=3, col = "green")
legend("topright", c("Mujeres", "Varones"),
       lty = 1, col = c("red", "green")
       )
