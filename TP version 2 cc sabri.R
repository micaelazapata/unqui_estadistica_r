# Llamamos libreria

library(tidyverse)
library(skimr)
library("lubridate")
library(nlme)
library(dplyr)

# Llamamos base
# covid <- read_csv("Covid19Casos.csv")

# Indagamos base
# 
# head(covid)
# names(covid)
# 
# class(covid$sepi_apertura)
# class(covid$fecha_diagnostico)
# sample(covid$fecha_diagnostico, 10)
# 
# unique(covid_confirmados$sepi_apertura)

# Transformamos para achicar base

# covid_confirmados <- covid %>% 
#   filter(clasificacion_resumen == "Confirmado",
#          residencia_provincia_nombre %in% c("Buenos Aires", "CABA"))

# guardo base achicada para trabajar directamente sobre esa, remuevo la pesada

# readr::write_csv(covid_confirmados,
#                  'covid_confirmados.csv')
# rm(covid)

#Crear la base solo con AMBA ---------------------

# covid_confirmados <- read_csv("E:\\TODO\\Cursos\\Cursos ARICHULA\\Introduccion a R Estadistica UNQ\\Trabajo final\\Covid19Casos.csv")

#apertura base sabri 
covid_confirmados <- read_csv("covid_confirmados.csv")

# PASO 1 : Filtrando la base para trabajar solo con el AMBA----------
confirmados_AMBA_filtro <- covid_confirmados %>% 
  filter(residencia_departamento_nombre  %in% c('Almirante Brown', 
                                                'Avellaneda', 'Berazatagui'
                                                , 'Berisso', 'Brandsen', 
                                                'Campana', 'Ca?uelas', 'Ensenada'
                                                , 'Escobar', 'Esteban Echeverr?a',
                                                'Exaltaci?n de la Cruz', 
                                                'Ezeiza', 'Florencio Varela',
                                                'General Las Heras', 
                                                'General Rodr?guez', 
                                                'General San Mart?n', 
                                                'Hurlingham', 'Ituzaing?', 
                                                'Jos? C. Paz', 'La Matanza', 
                                                'Lan?s', 'La Plata', 
                                                'Lomas de Zamora', 
                                                'Luj?n', 'Marcos Paz',
                                                'Malvinas Argentinas',
                                                'Moreno', 'Merlo', 'Mor?n',
                                                'Pilar', 'Presidente Per?n',
                                                'Quilmes', 'San Fernando', 
                                                'San Isidro', 
                                                'San Miguel', 'San Vicente', 
                                                'Tigre', 'Tres de Febrero', 
                                                'Vicente L?pez','Z?rate',
                                                'COMUNA 01', 'COMUNA 02',  
                                                'COMUNA 03' ,'COMUNA 04',
                                                'COMUNA 05','COMUNA 06','COMUNA 07', 
                                                'COMUNA 08', 'COMUNA 09', 'COMUNA 10'
                                                ,'COMUNA 11', 'COMUNA 12', 'COMUNA 13','COMUNA 14', 'COMUNA 15'))

confirmados_AMBA_filtro %>%     
  filter(!residencia_departamento_nombre == "SIN ESPECIFICAR")     

# Cambiamos la comuna por caba para que nos quede caba como una sola localidad
confirmados_para_trabajar <-confirmados_AMBA_filtro%>%
  mutate(localidad= case_when(residencia_departamento_nombre %in% c('COMUNA 01', 'COMUNA 02',  
                              'COMUNA 03' ,'COMUNA 04',
                              'COMUNA 05','COMUNA 06','COMUNA 07', 
                              'COMUNA 08', 'COMUNA 09', 'COMUNA 10',
                              'COMUNA 11', 'COMUNA 12', 'COMUNA 13','COMUNA 14', 'COMUNA 15') ~ "CABA",
                              TRUE~ as.character(residencia_departamento_nombre)))

#armamos la base solo para que nos queden las variables que vamos a usar
confirmados_para_trabajar <- confirmados_para_trabajar %>%
  select(localidad,residencia_departamento_nombre, edad, sexo, origen_financiamiento,
         asistencia_respiratoria_mecanica, cuidado_intensivo, fecha_internacion,
         sepi_apertura, fallecido,origen_financiamiento,fecha_apertura)
                              
rm(covid_confirmados)
rm(confirmados_AMBA_filtro)

# PASO 2: explorando la base --------------------------
summary(confirmados_para_trabajar)
str(confirmados_para_trabajar)
skim(confirmados_para_trabajar)

#eliminar na y outlires de edad (seleccionar solo las que estan entre 0 y 100):
confirmados_para_trabajar <-confirmados_para_trabajar %>%
  filter (edad<105 & edad>-1)%>%
  drop_na(edad) %>%
  filter (sexo != 'NR')

#sacamos los NA de fecha de internacion


#queda poner 0 en los NA en la fecha de internacion, y 1 cuando tiene fecha

#PASO 3: analizando las variables de la base -------

#Analisis contagios y fallecimientos por edad, sexo y localidad ----------------


distribucion_fallecido_edad<-confirmados_para_trabajar%>%
  filter(fallecido=="SI")%>%
  ggplot(aes(x=edad))+
  geom_histogram(binwiht=30)
distribucion_fallecido_edad

Fallecidos_agrupadosxedad<-confirmados_para_trabajar%>%
  filter(fallecido=="SI")%>%
  group_by(edad)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(30)

distribucion_contagiados_edad<-confirmados_para_trabajar%>%
  ggplot(aes(x=edad))+
  geom_histogram(binwiht=30)
distribucion_contagiados_edad

confirmados_agrupadosxedad<-confirmados_para_trabajar%>%
  group_by(edad)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)

#la edad de mayor contafio se da entre los 26 y 35 años, dadonde la mayor cantidad de contagios
#en los 29 y 30 años, con más de 34 mil contagiados.
#sin embargo, los más afectados por la enfermedad son aquellos de mayor  edad, dandose la
#mayor cantidad de fallecidos entre los 70 y 80 años, con el mayor pico en los 77 años con 969 fallecidos
#siguiendole muy de cerca la edad de 73 años con 656 fallecidos

confirmados_agrupadosxlocalidad<-confirmados_para_trabajar%>%
  group_by(localidad)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)

#las localidades con mayor tasa de contagios son CABA y la matanza,
# seguidos mucho mas de lejos por La Plata y Quilmes.
#estan coinciden con ser las localidades de mayor población. 

Fallecidos_agrupadosxlocalidad<-confirmados_para_trabajar%>%
  filter(fallecido=="SI")%>%
  group_by(localidad)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(10)

#las localidades con mayor tasa de fallecidos, conciden con las de mayores casos, 
#pero el ranking es diferente, siendo la Matanza la localidad con mayor fallecidos, seguidos por CABA,
#la plata y quilmes. Cabe destacar que el salto que se produce entre las dos primeras es mucho
#menos que el que se observa en los contagiados. Mientras que la brecha entre el 3er puesto y el primero
# en numero de contagiados es del 30%, entre las mismas localidades 
# la relacion entre CABA y Quilmes es del 56%, lo que muestra una tasa de letalidad diferente los localidad
# es por ello que en el trabajo vamos a analizar los factores que explican
#estan diferentes tasas de letalidad entre las localidades



# PASO 4: Analizando las olas de contagio -------------------------

#semana epidemiologica para ver olas de contagio

confirmados_para_trabajar <-confirmados_para_trabajar %>% 
    mutate(anio = year(fecha_apertura)) %>% 
    mutate(sepi_anio = paste(anio,sepi_apertura, sep = "_"))

#¿cuales fueron las olas de mayores contagiados y fallecidos en el AMBA?

confirmados_para_trabajar %>%
   group_by(sepi_anio, fallecido)%>%
    arrange(desc(sepi_anio))%>%
ggplot(aes(x=sepi_anio, y=fallecido, fill=fallecido))+
  geom_col()+
  labs(title = "Casos confirmados de Covid-19",
       x = "Semana epidemiologica",
       y = "cantidad de casos")

confirmados_xsemanaepi<-confirmados_para_trabajar%>%
  group_by(sepi_anio)%>%
    summarise(n=n())%>%
  arrange(desc (n))%>%
  head(10)


#las semanas de mayor contagio se dieron en el 2021, en particular las semanas 14,15 y 16
#esto se diuo entre el 28 de marzo y el 10 de abril

confirmados_para_trabajar %>%
  filter (fallecido=="SI")
  group_by(sepi_anio, fallecido)%>%
  arrange(desc(sepi_anio))%>%
  ggplot(aes(x=sepi_anio, y=fallecido, fill=fallecido))+
  geom_col()+
  labs(title = "Casos confirmados de Covid-19",
       x = "Semana epidemiologica",
       y = "cantidad de casos")
  
  fallecidos_xsemanaepi<-confirmados_para_trabajar%>%
    filter(fallecido=="SI")%>%
    group_by(sepi_anio)%>%
    summarise(n=n())%>%
    arrange(desc (n))%>%
    head(10)
#la semana epidemiologia de mayor fallecimiento se dan entre 15 y 16 con más de 1300 fallecidos
# esto se dio hacia principios de abril de este año
  
  
# PASO 5: analisis de letalidad ----------------
# Calculamos Tasa de letalidad = fallecidos/nofallecidos*100
  
  confirmados_AMBA_xdepto <- confirmados_para_trabajar %>%
    group_by(localidad, fallecido) %>%
    summarise (residencia=n()) %>% 
    pivot_wider(names_from = fallecido, values_from = residencia) %>% 
    mutate(ratio_fall_nofall = round(SI / NO,5)*100) %>% 
    select(-SI, -NO)
  
  #¿cuales son los municipios del que han registrado la mayor tasa de letalidad?----------------
  
  confirmados_AMBA_xdepto %>%
  arrange(desc(ratio_fall_nofall))%>%
  head(10)

  
  
 #PASO 6: ARMANDO LA REGRESION ---------------------
  
  covid_confirmados_pararegresion <- confirmados_para_trabajar %>%
  group_by(localidad)%>%
    summarise(edad_promedio=mean(edad,na.rm=TRUE),
                                 Q_M=sum(sexo=="F")/n(),
                                 Q_H=sum(sexo=="M")/n(),
                                 Q_respirador=sum(asistencia_respiratoria_mecanica=="SI"),
                                 Q_fin_privado=sum(origen_financiamiento=="Privado"), 
                                 Q_fin_publico=sum(origen_financiamiento=="Público"),
                                 #Q_internacion=sum(fecha_internacion!=NA),
                                 Q_cuidadoIntensivo=sum(cuidado_intensivo=="SI"))
                              
                                                          
  
base_regresion_final <-covid_confirmados_pararegresion%>%
left_join(confirmados_AMBA_xdepto, by = "localidad")


modelo_lineal <- base_regresion_final %>%
  lm(ratio_fall_nofall ~ edad_promedio+Q_M+Q_H+ Q_fin_privado + Q_fin_publico + Q_respirador + Q_cuidadoIntensivo , .)

summary(modelo_lineal)
base_regresion_final %>%
  ggplot(aes(x=edad_promedio, y=ratio_fall_nofall))+
geom_point()

base_regresion_final %>%
  ggplot(aes(x=Q_M, y=ratio_fall_nofall))+
  geom_point() +
geom_smooth(method = lm)

base_regresion_final %>%
  ggplot(aes(x=Q_H, y=ratio_fall_nofall))+
  geom_point() +
  geom_smooth(method = lm)


# la tasa de letalidad por municipio esta asociada a la edad de las personas congagiadas
# pero no guarda relación con el resto de las variables
#los resultados indican que la tasa de letalidad por municipio aumenta en 0,37 puntos cuando aumenta en un año la edad del infectad
# retomando lo visto anteriormente, los municipios con mayor tasa de letalidad seran aquellos que presenten 
#una población de infectados mas envegecida 

# se probará el analisis interactuando la edad y el sexo de lso
modelo_lineal_2 <- base_regresion_final %>%
  lm(ratio_fall_nofall ~ edad_promedio + Q_M + Q_H +Q_respirador+Q_cuidadoIntensivo + 
                          Q_M * edad_promedio +  Q_H * edad_promedio, .)
     
summary(modelo_lineal_2)  
#los resultados no cambian cuando el analisis se hace interactuando
# la edad con el sexo de los infectados. Con lo cual se asume que la tasa de letalidad
# no esta vinculada con las personas mayores de diferente genero

modelo_lineal_3 <- base_regresion_final %>%
  lm(ratio_fall_nofall ~ edad_promedio + Q_M + Q_H +Q_respirador+Q_cuidadoIntensivo + 
       Q_cuidadoIntensivo * edad_promedio +  Q_cuidadoIntensivo * edad_promedio, .)

summary(modelo_lineal_3)
# la interacción entre edad haber estado en cuidados intensivos, no es significativa
# es decir la tasa de letalidad por municipio no cambia si en el municipio hay más personas 
# internadas en cuidados intensivos.

modelo_lineal_4 <-base_regresion_final%>%
  lm(ratio_fall_nofall ~ edad_promedio + Q_H + Q_M +Q_respirador+Q_cuidadoIntensivo + 
      +  Q_cuidadoIntensivo * Q_H   +  Q_cuidadoIntensivo * Q_M, .)

summary(modelo_lineal_4)
#tampoco ser hombre o mujer afecta la tasa de letalidad, el hecho de haber pasado por cuidado intensivo

modelo_lineal_5 <-base_regresion_final%>%
  lm(ratio_fall_nofall ~ edad_promedio + Q_H + Q_M +Q_respirador+Q_cuidadoIntensivo + 
       +  Q_cuidadoIntensivo * Q_respirador, .)

summary(modelo_lineal_5)

#el estar en cuidados intensivos y tener asitencia respiratoria mecanica
#reduce la tasa de mortalidad en 0,00000079 puntos. Y tambien hace reducir la incidencia
# edad promedio, pasando a 0,307 puntos por un el aumento en un 1% de la letalidad
#vamos a comparar los 4 modelos analizados

anova(modelo_lineal, modelo_lineal_2, modelo_lineal_3, modelo_lineal_4, modelo_lineal_5)

#¿cómo se entiende esto?

# conclusiones: la tasa de letalidad solo depende positivamente de la edad de las personas, y negativamente de haber estado internado en cuidados intensivos con respirador
# las internaciones sin asistencia respiratoria no cambia la tasa de letalidad de los municipios, al igual que no
# hay diferencia entre hombres y mujeres.
# tampoco se distingue diferencias entre aquellos que con ingresos más altos pueden acceder al sistema de salud privado

# por lo tanto, la edad es el factor clave que explica la tasa de letalidad de los municipios, y la internacion con asistencia repiratoria mecanica
#tiende a reducir en una pequeña medida dicha tasa de letalidad. 