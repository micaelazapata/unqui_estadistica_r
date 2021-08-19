library(tidyverse)

covid <- read_csv("Covid19Casos.csv")


head(covid)
names(covid)

class(covid$sepi_apertura)
class(covid$fecha_diagnostico)
sample(covid$fecha_diagnostico, 10)

unique(covid_confirmados$sepi_apertura)

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