# Llamamos libreria

library(tidyverse)

# Llamamos base

covid <- read_csv("Covid19Casos.csv")

# Indagamos base

head(covid)
names(covid)

class(covid$sepi_apertura)
class(covid$fecha_diagnostico)
sample(covid$fecha_diagnostico, 10)

unique(covid_confirmados$sepi_apertura)

# Transformamos para achicar base

covid_confirmados <- covid %>% 
  filter(clasificacion_resumen == "Confirmado",
         residencia_provincia_nombre %in% c("Buenos Aires", "CABA"))

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
