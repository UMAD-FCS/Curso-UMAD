
# library(rio)
library(tidyverse)
library(googledrive)


#----------------------------------------------------------
# Dos  opciones para bajar las bases.
#  1) instalar el paquete googledrive y bajarlo directamente del link.
#     Recuerden para instalar: install.packages('googledrive')

#  2) descargar cada arhivo en su computadora
#     usando los links y depsupues utilizar la funcion
#     import del paquete rio
#     el link a la carpeta drive es
#     https://drive.google.com/drive/folders/1P7juNaQiN70Y4xogiFPvXPDhcIzwuwgH?usp=sharing

# En este script utilizamos la primera opcion

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


drive_download("https://drive.google.com/file/d/1388jI6nENyMVAyrsHdGngBPaSZSZnp5l/view?usp=sharing", overwrite = TRUE)
load(file = "bases_latino.RData") # el archivo ya tiene las bases pre cargadas

# uno las variables de cada base en una unica variabale. 


pais <- bind_rows(pais = c(
  lat95$pais, lat96$pais, lat97$idenpa, lat98$idenpa, lat00$IDENPA,
  lat01$idenpa, lat02$idenpa, lat03$idenpa, lat04$idenpa, lat05$idenpa,
  lat06$idenpa, lat07$idenpa, lat08$idenpa, lat09$idenpa, lat11$IDENPA,
  lat13$idenpa, lat10$IDENPA, lat15$idenpa, lat16$idenpa, lat17$idenpa,
  lat18$IDENPA
))

pais$id <- 1:nrow(pais)

# recodifico la varibale pais
pais$pais <- factor(pais$pais,
  levels = c(
    32, 68, 76, 152, 170, 188, 214, 218, 222, 320, 340, 484, 558, 591,
    600, 604, 724, 858, 862
  ),
  labels = c(
    "Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Costa Rica",
    "Rep. Dominicana", "Ecuador", "El Salvador", "Guatemala",
    "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Perú",
    "España", "Uruguay", "Venezuela"
  )
)



anio <- bind_rows(anio = c(
  lat95$numero, lat96$numero, lat97$numinves, lat98$numinves, lat00$NUMINVES,
  lat01$numinves, lat02$numinves, lat03$numinves, lat04$numinves, lat05$numinves, 
  lat06$numinves, lat07$numinves, lat08$numinves, lat09$numinves, lat10$NUMINVES,
  lat11$NUMINVES, lat13$numinves, lat15$numinves, lat16$numinves, lat17$numinves,
  lat18$NUMINVES
))

anio <- anio %>% mutate(anio = case_when(
  anio == 16 ~ 2011,
  anio == 17 ~ 2013,
  anio == 18 ~ 2015,
  TRUE ~ anio
))

nivel_edu <- bind_rows(nivel_edu = c(
  lat95$s17, lat96$s14, lat97$s10, lat98$s11, lat00$S6,
  lat01$s6, lat02$s6, lat03$s6, lat04$s6, lat05$s11,
  lat06$s11, lat07$s15, lat08$s15, lat09$s12,
  lat10$S14, lat11$S21, lat13$S17, lat15$S19, lat16$S13,
  lat17$S14, lat18$S10
))


sexo <- bind_rows(sexo = c(
  lat95$s1, lat96$s1, lat97$s1, lat98$s1, lat00$S1,
  lat01$s1, lat02$s1, lat03$s1, lat04$s1, lat05$s6,
  lat06$s6, lat07$s10, lat08$s8, lat09$s5,
  lat10$S7, lat11$S16, lat13$S10, lat15$S12, lat16$sexo,
  lat17$sexo, lat18$SEXO
))


edad <- bind_rows(edad = c(
  lat95$s2, lat96$s2, lat97$s2, lat98$s2, lat00$S2,
  lat01$s2, lat02$s1, lat03$s2, lat04$s2, lat05$s7,
  lat06$s7, lat07$s11, lat08$s9, lat09$s6,
  lat10$S8, lat11$S17, lat13$S11, lat15$S13, lat16$edad,
  lat17$edad, lat18$EDAD
))


ap.dem <- bind_rows(ap.dem = c(
  lat95$p20, lat96$p19, lat97$sp31, lat98$sp28, lat00$P29ST, lat01$p46st,
  lat02$p32st, lat03$p14st, lat04$p13st, lat05$p16st, lat06$p17st, lat07$p9st,
  lat08$p13st, lat09$p10st, lat10$P10ST, lat11$P13ST,
  lat13$P12STGBS, lat15$P11STGBS, lat16$P8STGBS,
  lat17$P8STGBS, lat18$P12STGBS
))


sat.dem <- bind_rows(sat.dem = c(
  lat95$p21, lat96$p20, lat97$sp32, lat98$sp29, lat00$P30ST, lat01$p45st,
  lat02$p33st, lat03$p15st, lat04$p14st, lat05$p18st, lat06$p21st, lat07$p21st,
  lat08$p22st.a, lat09$p12st.a, lat10$P11ST.A, lat11$P14ST.A, lat13$P13TGB.A,
  lat15$P12TG.A, lat16$P9STGBSA, lat17$P9STGBSC.A, lat18$P13STGBS.A
))

idio <- bind_rows(idio = c(
  lat95$p31, lat96$p38, lat97$sp56, lat98$sp52, lat00$P52ST, lat01$p54st,
  lat02$p64st, lat03$p60st, lat04$p87st, lat05$p34st, lat06$p47st, lat07$p67st,
  lat08$p56st, lat09$p69st, lat10$P60ST, lat11$P76ST, lat13$P41ST, lat15$P27ST,
  lat16$P17ST, lat17$P19STC, lat18$P22ST
))

pond <- bind_rows(pond = c(
  lat95$wt, lat96$wt, lat97$wt, lat01$wt, lat98$pondera, lat00$WT, lat02$wt,
  lat03$wt, lat04$wt, lat05$wt, lat06$wt, lat07$wt, lat08$wt, lat09$wt,
  lat10$wt, lat11$WT, lat13$wt, lat15$wt, lat16$wt, lat17$wt, lat18$WT
))


# unifica todas las variables en una unica base de datos

data_sel <- bind_cols(
  pais, anio, nivel_edu, sexo, edad, ap.dem, sat.dem, idio,
  pond
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAFICOS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

olas<- unique(data_sel$anio)

# Variación en el apoyo a la democracia
data_sel %>%
  filter(ap.dem == 1:3, pais != "España") %>%
  drop_na(ap.dem) %>%
  ggplot(aes(y = pond, x = anio, fill = as.factor(ap.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(breaks = olas)+
  labs(
    x = "Años",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())



# Satisfacción con la democracia

data_sel %>%
  filter(sat.dem == 1:4, pais != "España") %>%
  drop_na(sat.dem) %>%
  ggplot(aes(y = pond, x = anio, fill = as.factor(sat.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(breaks = olas)+
  labs(
    x = "Años",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Variación en el apoyo a la democracia por país

data_sel %>%
  filter(ap.dem == 1:3, pais != "España") %>%
  ggplot(aes(y = pond, x = anio, fill = as.factor(ap.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(breaks = olas)+
  labs(
    x = "Años",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  facet_wrap(~pais) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


# Satisfacción con la democracia por país

data_sel %>%
  filter(sat.dem == 1:4, pais != "España") %>%
  drop_na(sat.dem) %>%
  ggplot(aes(y = pond, x = anio, fill = as.factor(sat.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  scale_x_continuous(breaks = olas)+
  labs(
    x = "Años",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  facet_wrap(~pais) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )



# Satisfacción con la democracia por sexo

data_sel %>%
  filter(sat.dem == 1:4, pais != "España") %>%
  drop_na(sexo, sat.dem) %>%
  ggplot(aes(y = pond, x = sexo, fill = as.factor(sat.dem), label = pond)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "Sexo",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Satisfacción con la democracia por edad
data_sel %>%
  filter(sat.dem == 1:4, edad > 14, pais != "España") %>%
  drop_na(edad, sat.dem) %>%
  ggplot(aes(y = pond, x = edad, fill = as.factor(sat.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "edad",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Apoyo a la democracia por sexo


data_sel %>%
  filter(ap.dem == 1:3, pais != "España") %>%
  drop_na(sexo, ap.dem) %>%
  ggplot(aes(y = pond, x = sexo, fill = as.factor(ap.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "Sexo",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


# Apoyo a la democracia por edad


data_sel %>%
  filter(ap.dem == 1:3, edad > 14, pais != "España") %>%
  drop_na(edad, ap.dem) %>%
  ggplot(aes(y = pond, x = edad, fill = as.factor(ap.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "edad",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


# Apoyo a la democracia por autoidentificación ideológica
data_sel %>%
  filter(ap.dem == 1:3, idio > -1, pais != "España") %>%
  drop_na(idio, ap.dem) %>%
  ggplot(aes(y = pond, x = as.factor(idio), fill = as.factor(ap.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "autoidentificación ideológica",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Apoyo a la democracia por autoidentificación por nivel educativo
data_sel %>%
  filter(ap.dem == 1:3, nivel_edu > 0, pais != "España") %>%
  drop_na(nivel_edu, ap.dem) %>%
  ggplot(aes(y = pond, x = as.factor(nivel_edu), fill = as.factor(ap.dem))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "Años de educación",
    y = "% porcenatje",
    title = "",
    caption = "Unidad de Métodos y Acceso a Datos (UMAD)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())