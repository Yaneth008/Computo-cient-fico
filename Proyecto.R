install.packages(c("dplyr", "janitor"))
install.packages("ggplot2")
library(ggplot2)
# Abrir bases y filtrar datos
mortalidad <- read.csv("01_Defunciones_1950_2070.csv")
mort <- mortalidad %>% 
  filter(ENTIDAD=="República Mexicana" & (AÑO <= 2025 & AÑO >= 1970))

natalidad <- read.csv("04_Tasas_Especificas_Fecundidad_proyecciones.csv")
nat <- natalidad %>% 
  filter(ENTIDAD=="República Mexicana" & (AÑO <= 2025 & AÑO >= 1970))

# Descripción de las variables
glimpse(mort)
glimpse(natalidad)

# Eliminar columnas 
mort <- mort %>%  select(-c(RENGLON, ENTIDAD, CVE_GEO))
nat <- nat %>%  select(-c(RENGLON, ENTIDAD, CVE_GEO))

# V erificar NA
sum(is.na(mort))
sum(is.na(nat))

# Verificar duplicados
mort %>% 
  janitor::get_dupes()
nat %>% 
  janitor::get_dupes()

# Resumen de las tablas 
summary(mort)
summary(nat)

# Determinar la media de la variable sexo
mean_d <- mort %>%
  group_by(SEXO) %>%
  summarize(mean_1 = mean(sum(DEFUNCIONES))); mean_d
# Determinar la media de la variable gpo_edad
mean_n <- nat %>%
  group_by(GPO_EDAD) %>%
  summarize(mean_2 = mean(sum(NACIMIENTOS))); mean_n


# Agrupar edad de tabla de mortalidad y asignar un no.

mort$Grupo_Edad <- ifelse(mort$EDAD >= 80, max(floor(80 / 5) + 1), floor(mort$EDAD / 5) + 1)

# 0-4 > 1 # 5-9 > 2 # 10-14 > 3 # 15-19 > 4 # 20-24 > 5 # 25-29 > 6
# 30-34 > 7 # 35-39 > 8# 40-44 > 9 # 45-49 > 10 # 50-54 > 11 # 55-59 > 12
# 60-64 > 13 # 65-69 > 14 # 70-74 > 15 # 75-79 > 16 # 80-+ > 17

############ Edad, defunción, género ############
edg <- mort %>% 
  group_by(Grupo_Edad, SEXO) %>% 
  summarise(def = sum(DEFUNCIONES)); edg

# Generales
edg$def_mod <- ifelse(edg$SEXO == "Hombres", -edg$def, edg$def)

ggplot(edg, aes(x = Grupo_Edad, y = def_mod, fill = SEXO)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Hombres" = "#81B1D6", "Mujeres" = "#D09EBF")) +
  labs(title = "Total de defunciones por género y grupo de edad",
       x = "Grupo de edad",
       y = "No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = abs)  

# Mujeres
edg_m <- mort %>% 
  filter(SEXO=="Mujeres") %>% 
  group_by(Grupo_Edad, SEXO) %>% 
  summarise(def_m = sum(DEFUNCIONES)); edg_m

ggplot(edg_m, aes(x=Grupo_Edad, y=def_m, fill=SEXO)) +
  scale_fill_manual(values = c("Mujeres" = "#D09EBF")) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Total de defunciones de mujeres por grupo edad",
       x="Grupo de edad",
       y="No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Hombres
edg_h <- mort %>% 
  filter(SEXO=="Hombres") %>% 
  group_by(Grupo_Edad, SEXO) %>% 
  summarise(def_h = sum(DEFUNCIONES)); edg_h

ggplot(edg_h, aes(x=Grupo_Edad, y=def_h, fill=SEXO)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = c("Hombres" = "#81B1D6")) +
  labs(title="Total de defunciones de hombres por  grupo de edad",
       x="Grupo de edad",
       y="No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

############ Edad, nacimiento, género ############
eng <- nat %>% 
  group_by(GPO_EDAD) %>% 
  summarise(nac = sum(NACIMIENTOS)); eng

ggplot(eng, aes(x=GPO_EDAD, y=nac)) +
  geom_bar(stat="identity", position="dodge", fill="#76BD6A") +
  labs(title="Total de nacimientos según la edad de las madres",
       x="Edad de la madre",
       y="No. de nacimientos") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

############ Top. 5 años con mayor no. de defunciones ############

top_def <- mort %>% 
  group_by(AÑO) %>%  
  summarise(top = sum(DEFUNCIONES)) %>% 
  arrange(desc(top)) %>% 
  slice_head(n = 5);top_def

ggplot(top_def, aes(x = factor(AÑO), y = top)) +  
  geom_bar(stat = "identity", fill = "#76BD6A") +
  labs(title = "Top 5 años con mayor no. de defunciones",
       x = "Año", y = "No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############ Top. 5 años con mayor no. de defunciones del género masculino ############
top_def_h <- mort %>% 
  filter(SEXO=="Hombres") %>%
  group_by(AÑO) %>% 
  summarise(top_h = sum(DEFUNCIONES)) %>% 
  arrange(desc(top_h)) %>% 
  slice_head(n = 5) ; top_def_h


ggplot(top_def_h, aes(x=factor(AÑO), y=top_h)) +
  geom_bar(stat="identity", fill="#81B1D6") +
  labs(title="Top 5 años con mayor no. de defunciones de hombres",
       x="Año",
       y="No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

############ Top. 5 años con mayor no. de defunciones del género femenino ############
top_def_m <- mort %>% 
  filter(SEXO=="Mujeres") %>%
  group_by(AÑO) %>% 
  summarise(top_m = sum(DEFUNCIONES)) %>% 
  arrange(desc(top_m)) %>% 
  slice_head(n = 5) ; top_def_m

ggplot(top_def_m, aes(x=factor(AÑO), y=top_m)) +
  geom_bar(stat="identity", fill="#D09EBF") +
  labs(title="Top 5 años con mayor no. de defunciones de mujeres",
       x="Año",
       y="No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

############ Comparativa de top de mujeres y hombres ############
top_def_g <- mort %>% 
  group_by(AÑO, SEXO) %>%  
  summarise(topg = sum(DEFUNCIONES)) %>% 
  arrange(SEXO, desc(topg)) %>%  
  group_by(SEXO) %>%           
  slice_head(n = 5); top_def_g

ggplot(top_def_g, aes(x = factor(AÑO), y = topg, fill = SEXO)) +  
  geom_bar(stat = "identity", position = "dodge") +  
  coord_flip() +
  scale_fill_manual(values = c("Hombres" = "#81B1D6", "Mujeres" = "#D09EBF")) +
  labs(title = "Top 5 años con mayor no. de defunciones por sexo (comparativa)",
       x = "Año", y = "No. de defunciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = abs)

############ Top. 5 años con mayor no. de nacimientos ############
top_nac <- nat %>% 
  group_by(AÑO) %>% 
  summarise(top1 = sum(NACIMIENTOS)) %>% 
  arrange(desc(top1)) %>% 
  slice_head(n = 5) ; top_nac

ggplot(top_nac, aes(x=AÑO, y=top1)) +
  geom_bar(stat="identity", fill="#76BD6A") +
  labs(title="Top 5 años con mayor no. de nacimientos",
       x="Año",
       y="No. de nacimientos") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

############# Top. 5 grupos de edad con mayor no. de nacimientos ############
top_nacg <- nat %>% 
  group_by(GPO_EDAD) %>% 
  summarise(topg = sum(NACIMIENTOS)) %>% 
  arrange(desc(topg)) %>% 
  slice_head(n = 5) ; top_nacg

ggplot(top_nacg, aes(x=GPO_EDAD, y=topg)) +
  geom_bar(stat="identity", fill="#76BD6A") +
  
  labs(title="Top 5 grupos de edad con mayor no. de nacimientos",
       x="Grupo de edad",
       y="No. de nacimientos") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


############# Gráfico de dispersión de las tasas de natalidad por género ############
ggplot(tasas, aes(x = AÑO, y = TASAS)) +
  geom_point(size = 3, alpha = 0.7, color = "#76BD6A") + 
  
  labs(title = "Tasa de Natalidad por grupo de edad de la madre",
       x = "Año",
       y = "Tasa de natalidad") +
  theme_minimal() +
  facet_grid(GPO_EDAD~.)  



