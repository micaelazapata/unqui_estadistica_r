# Llamamos libreria

library(tidyverse)

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

covid_confirmados <- read_csv("covid_confirmados.csv")

conf_sem_depto <- covid_confirmados %>% 
  group_by(sepi_apertura, 
           # residencia_provincia_nombre,
           residencia_departamento_id, 
           residencia_departamento_nombre, 
           edad, 
           sexo) %>% 
  summarise(cantidad = n()) 

conf_sem_depto <- conf_sem_depto %>% 
  filter(!residencia_departamento_nombre == "SIN ESPECIFICAR")

names(conf_sem_depto)

conf_depto <- conf_sem_depto %>% 
  group_by(residencia_departamento_id, sepi_apertura) %>% 
  summarise(n = n())

#lubridate es para manejar fechas
#filtro los municipios del AMBA
#se que hay una forma de hacer un filtro.if para poner como condicional
#para que filtre solo si en provincia dice buenos aires pero no me sali?

library("lubridate")

confirmados_AMBA_filtro <- covid_confirmados %>% 
  filter(year(fecha_diagnostico)!= 2019) %>% 
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
                                                ,'COMUNA 11', 'COMUNA 12', 'COMUNA 13',
                                                'COMUNA 14', 'COMUNA 15'))


confirmados_AMBA <- confirmados_AMBA_filtro %>% 
  group_by(residencia_provincia_nombre,fecha_diagnostico) %>%
  summarise(cantidad = n())

#Comparacion de CABA y AMBA por fechas

confirmados_AMBA %>%
  ggplot() +
  aes(x = fecha_diagnostico, y = cantidad, color = residencia_provincia_nombre)+
  geom_line() +
  scale_x_date(date_labels = "%B/%y") +
  ylab("casos")+
  xlab("meses")+
  labs(title = "Casos mensuales en CABA y municipios del AMBA",
       caption = "Elaborado en base a datos de Argentina.gob.ar")




#hago un grafico por departamento de GBA

#filtro CABA 
confirmados_AMBA_DEPTO <- confirmados_AMBA_filtro %>%
  filter(residencia_provincia_nombre != 'CABA') %>%
  group_by(residencia_departamento_nombre) %>%
  summarise(cantidad = n())


#armo un grafico de lineas por departamento
#no incluye CABA


confirmados_AMBA_DEPTO %>%
  ggplot(aes(x = residencia_departamento_nombre, y = cantidad ,
             fill = cantidad)) + geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Casos confirmados de Covid-19 por Departamento del GBA",
       x = "Cantidad de casos",
       y = "Departamento")

confirmados_AMBA_filtro %>%     
  filter(!residencia_departamento_nombre == "SIN ESPECIFICAR")                                        

#explorando la base --------------------------
summary(confirmados_AMBA_filtro)
str(confirmados_AMBA_filtro)
skim(confirmados_AMBA_filtro)

#valores outliers en edad
confirmados_AMBA_filtro %>%
  ggplot(aes(x=edad))+
  geom_histogram(bins=10)+
  theme_minimal()

#eliminar na y outlires de edad (seleccionar solo las que estan entre 0 y 100):
confirmados_AMBA_filtro2 <-confirmados_AMBA_filtro %>%
  filter (edad<105 & edad>-1)%>%
  drop_na(edad)

summary(confirmados_AMBA_filtro2$edad)

# Internacion, tenemos muchos NA, que asumimos es porque no se han internado
# ¿Como hacer para ponerle 1 a si fue internado?, 
# cuando hay un NA cuando no estuvo internado. llevar a 0 los NA y a 1 cuando tiene fecha de internacion
# mutate(internacion2=case_when(fecha_internacion==1))%>% 

#Analizando las olas de contagio -------------------------

#semana epidemiologica para ver olas de contagio

confirmados_AMBA_filtro2 <-confirmados_AMBA_filtro2 %>% 
  mutate(anio = year(fecha_diagnostico)) %>% 
  mutate(sepi_anio = paste(sepi_apertura, anio, sep = "_"))

confirmados_AMBA_filtro2 %>%
  summary()

#¿cuales fueron las olas de mayores contagiados y fallecidos en el AMBA?

confirmados_AMBA_filtro2 %>%
  ggplot(aes(x=sepi_anio, y=fallecido, fill=fallecido))+
  geom_col()


#¿cuales son las fechas que definen las olas y variaciones por departamento?

confirmados_AMBA_graficoxdpto <- confirmados_AMBA_filtro2 %>% 
  group_by(residencia_provincia_nombre,sepi_anio) %>%
  summarise(cantidad = n())


confirmados_AMBA_graficoxdpto %>%
  ggplot() +
  aes(x = sepi_anio, y = cantidad, color = residencia_provincia_nombre)+
  geom_line() +
  scale_x_date(date_labels = "%B/%y") +
  ylab("casos")+
  xlab("meses")+
  labs(title = "Casos mensuales en CABA y municipios del AMBA",
       caption = "Elaborado en base a datos de Argentina.gob.ar")


#hago un grafico por departamento de GBA

#filtro CABA 
confirmados_AMBA_DEPTO <- confirmados_AMBA_filtro %>%
  filter(residencia_provincia_nombre != 'CABA') %>%
  group_by(residencia_departamento_nombre) %>%
  summarise(cantidad = n())


#armo un grafico de lineas por departamento

confirmados_AMBA_DEPTO %>%
  ggplot(aes(x = residencia_departamento_nombre, y = cantidad ,
             fill = cantidad)) + geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Casos confirmados de Covid-19 por Departamento del GBA",
       x = "Cantidad de casos",
       y = "Departamento")

#analizar por departamento los casos y fallecidos por sexo, edad promedio e internación


#creando las variables para incluir en la regresion -----------------------

#regresion seria: 
# fallecidoxdepto/totalcasosxdpto= sexo + edad + financiamiento+ cuidado intensico + internacion + asistencia respiratoria
#aplicado a nuestra base

# tasadefallecidosxdpto=cantsexoF + cantSexoM + PromedioEdad + CantidadInternados + Cantidadasistenciarespiratoria + cantidadcuidadosintensivos + cantidad financiamiento privado + cantidad financiamiento publico
# Idem pero con interacion entre edad promedio y sexo y otra con edad promedio e internacion
#saque tiempo de detección de la enferemedad porque habia muchos missing e ibamos a perder muchos casos

#Tiempo de deteccion de la enfermedad
#problema que la fecha de inicio de sintomas tiene 940.671 misisng (total de confirmados 2.497.319)
covid_confirmados <-covid_confirmados %>%
  mutate(Tiempo_deteccion=fecha_inicio_sintomas-fecha_diagnostico)
skim(covid_confirmados$Tiempo_deteccion)
#nos queda solo el 54% con valores. vamos a perder muchas observaciones 



#a)Llevar a Binario

covid_confirmados_pararegresion <- confirmados_AMBA_filtro%>%
  mutate(fallecido2=case_when(fallecido=="NO"~1, fallecido=="SI"~0))%>%
  mutate(sexo2=case_when(sexo=="F"~0, sexo=="M"~1))%>%
  mutate(cuidado_intensivo2=case_when(cuidado_intensivo=="NO"~0, cuidado_intensivo=="SI"~1))%>%
  mutate(financiamiento=case_when(origen_financiamiento=="Público" ~1, origen_financiamiento=="Privado" ~0))%>%	
  mutate(asitencia_respiratoria2=case_when(asistencia_respiratoria_mecanica=="NO"~0,asistencia_respiratoria_mecanica=="SI"~1))

#binaria si estuvo en internacion. 

#b) calcular la tasa de fallecidos por departamento para sacar la variable dependiente

#armar la base para la regresion

# con estract separar las variables sexo, fallecidos en dos, par que nos quede por cantidad de femeninos y cantidad de masculinos
#hacer lo mismo con cada variable binaria, para poner solo en la regresion la cantidad de positivos







