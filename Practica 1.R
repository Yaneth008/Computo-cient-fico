install.packages(c("dplyr", "ggplot2", "visdat", "skimr", "insuranceData"))
library(dplyr)
library(ggplot2)
library(visdat)
library(skimr)
library(insuranceData)
data(dataCar)
str(dataCar)
summary(dataCar)

# Resumen detallado de como se distr. los datos
skim(dataCar)

# Primeras obs.
head(dataCar)

# Dimensión (filas, columnas)
dim(dataCar)

# Caracteristicas del dataframe, conocer el tipo de dato
glimpse(dataCar)
?dataCar

# Conocer el nombre de las columnas (2 opciones)
colnames(dataCar)
names(dataCar)

# Verificar si hay datos faltantes, si sale TRUE, con wich se muestra el dato faltante
miss <- any(is.na(dataCar))
miss

# Visualizar los datos, arroja gráficas que muestran si faltan datos
vis_dat(dataCar)
vis_miss(dataCar)

# Conocer la longitud de la col
pol <- length(dataCar$numclaims)

# Longitud de las reclamaciones >= 1
claims <- sum(dataCar$numclaims>=1)
# Porcentaje
(claims/pol)*100

# To 5 de vehiculos con mayor no. de reclamaciones
top <- dataCar %>%
  group_by(veh_body) %>%
  summarise(top_claims =sum(numclaims)) %>%
  arrange(desc(top_claims))

# Muestra los primeros 5
head(top,5)

# No. de polizas por tipo de vehiculo
pol_tip <- dataCar %>%
  group_by(veh_body) %>%
  summarise(po_t = n()) %>%
  arrange(desc(po_t))
pol_tip

# To 5 de vehiculos con mayor no. de  monto de reclamaciones
top_monto <- dataCar %>%
  group_by(veh_body) %>%
  summarise(top_mclaims =sum(claimcst0)) %>%
  arrange(desc(top_mclaims))
head(top_monto,10)


# Quien hace un no. mayor de reclamaciones: Hombres o Mujeres
# Número total de reclamaciones por género
claims_by_gender <- dataCar %>%
  group_by(gender, veh_body, numclaims) %>%
  summarise(total_claims = sum(claimcst0)) %>%
  arrange(desc(total_claims))

# Mostrar los resultados
print(claims_by_gender)

# Gráfico de barras para visualizar el número de reclamaciones por género
ggplot(claims_by_gender, aes(x = gender, y = total_claims, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Reclamaciones por Género", x = "Género", y = "Total de Reclamaciones") +
  theme_minimal()


# Siniestralidad por edad y genero
claims_by_age <- dataCar %>%
  group_by(gender, agecat, veh_body) %>%
  summarise(tol_claims = sum(numclaims)) %>%
  arrange(desc(tol_claims))
print(claims_by_age)

# Hacer un gráfico de dispersión 
ggplot(dataCar, aes(x=exposure, y=veh_value)) +
  geom_point() + labs(title = "Gráfico dispersión Exposición vs Valor del vehiculo",
                      x= "Exposición", y="Valor del vehiculo") + theme_minimal()



# Valor máximo de vehiculo
max(dataCar$veh_value)

max_veh <- dataCar %>%
  group_by(veh_body, claimcst0, exposure)%>%
  summarise(veh = max(veh_value))%>%
  arrange(desc(veh))
print(max_veh)

# Valor minimo de vehiculo
min_veh <- dataCar %>%
  group_by(veh_body, claimcst0, exposure)%>%
  summarise(veh = min(veh_value))%>%
  arrange(sort(veh))
print(min_veh)

# Gráfico de barras considerando el género
ggplot(claims_by_gender, aes(x=reorder(veh_body, -total_claims), y=total_claims, fill=gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tipos de vehiculos más reclamados por género",
       x="Tipo de vehiculo",
       y="Número de reclamaciones")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
