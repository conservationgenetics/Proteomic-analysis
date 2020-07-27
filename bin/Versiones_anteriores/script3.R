# analisis 1 # 
library(tidyverse)
library(readxl)
library(VennDiagram)

setwd("~/google-drive/history_AW/history_FGC/bin/")

rm(list = ls())

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")

df<-distinct(df[,-1])
names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # Total de proteinas con Accession (1263)

library(VennDiagram)
#Make the plot
names(df)

dfa<-df %>% select(Tejido,Accession)
names(dfa)
dfa<-distinct(dfa[,])
names(dfa)

## ven diagram ## 

str(dfa)

venn.diagram(
  x = list(
    dfa %>% filter(Tejido== "leaf")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Tejido== "root")%>% select(`Accession`) %>% unlist()
  ),
  category.names = c("leaf" , "root"),
  filename = '../figures/Ven_Tejidos.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3)))


###### obtener los exclusivos por tejido 

### extraer las palbras eclusivas y comportatidas por articulo 
algo<-VennDiagram::get.venn.partitions(list(roots= dfa %>% filter(Tejido== "leaf")%>% select(`Accession`) %>% unlist() , 
                                            leaf= dfa %>% filter(Tejido== "root")%>% select(`Accession`) %>% unlist()))

algo

#algo$..values..
algo$..count..

library(stringi)

dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por articulo 
dfcos

dfcos %>% select(V1)%>%rename(Accession_compartidos= V1)%>%
  filter(Accession_compartidos != "NA")%>%
  write.csv("../data/accession_tissue/compartidos_tissue.csv")

dfcos %>% select(V2)%>%rename(Accession_roots= V2)%>%
  filter(Accession_roots != "NA")%>%
  write.csv("../data/accession_tissue/exclusivos_roots.csv")

dfcos %>% select(V3)%>%rename(Accession_leaf= V3)%>%
  filter(Accession_leaf != "NA")%>%
  write.csv("../data/accession_tissue/exclusivos_leaf.csv")


# tabla resumen # 
tablares<-dfa %>% group_by(Tejido) %>% 
  summarise(Num_Proteinas_reportadas= length(unique(Accession)))%>%
  mutate(Unicas= c(785, 231), Compartidas= c(247,247))

tab1<-tablares %>% select(-c(Unicas, Compartidas))%>%gather(Categoria, Valor,2)

tab2<-tablares %>% select(-Num_Proteinas_reportadas)%>%gather(Categoria, Valor, 2:3)


tab3<-rbind(tab1,tab2)

tab3

write.csv(tab3,"../data/tablas_resumen/tabla_resumen_tejidos.csv")

names(tab3)

ggplot(tab3, aes(fill=Tejido, y=Valor, x=Tejido)) + 
  geom_bar(stat="identity") +
  xlab("")+theme_few()+
  labs(y= "Número de parámetros", x = "Herramientas")+
  scale_fill_manual(values = c("#f03b20","#ffeda0","#feb24c", 
                               "#4575b4", "#abd9e9", "#74add1",
                               "#bdbdbd", "#525252"))+
  facet_grid(vars(Categoria), scales = "free")+
  theme_clean()+
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.background = element_rect(colour="red", fill="#CCCCFF"))







