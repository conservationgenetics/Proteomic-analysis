library(tidyverse)
library(readxl)
library(ggpubr)
library(ggthemes)
library(alluvial)

df <- read_excel("../data/database_proteomic_EDITADA_FGC_bis.xlsx")%>% # carga la base de datos 
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%  # Separa una columna en varias
  mutate(presencia= rep(1, nrow(.)))%>% # Añade una columna con valor de 1
  filter(Descripción!= "RECORD REMOVED") %>%  # remueve registros especificos
  filter(Tejido == "leaf")%>% # filtra registros especificos
  separate(Tratamientos, c("Tratamientos","Tiempo"), sep = " ", remove = F) %>% # Separa una columna
  filter(Tiempo != "NA")%>%  # filtra registros especificos
  filter(!is.na(Tiempo)) # remueve NA


ggplot(df %>% filter(Tejido == "leaf")%>% filter(Tiempo != "4h")%>%
         filter(`Up/downb` != "NA"),
       aes(y = presencia,
           axis2 = Tratamientos, axis1 =`Categoria funcional`, axis3 =`Up/downb`)) +
  geom_alluvium(aes(fill =`Up/downb`),
                width = .3, knot.pos = .3, reverse = T) +
  geom_stratum(width = .25, reverse = T) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = T) +
  scale_x_continuous(breaks = 1:3, labels = c("Category fuctional", "Treatment", "Direction"))+
  scale_fill_manual(values = c("red", "blue"), name = "")


