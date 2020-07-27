
# objetivo : figura 1 compuesta por diagramas de venn con a) raices b) hojas c) todos 

# scrip# figura 1 # compuesta por venn de roots, leaf y todos 
library(tidyverse)
library(readxl)
library(VennDiagram)

setwd("~/google-drive/history_AW/history_FGC/bin/")

### roots ####

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")%>%filter(Tejido=="root")

df<-distinct(df[,-1])
names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # 478 exclusivas de raiz (1263 Totales)

library(VennDiagram)
#Make the plot
names(df)

dfa<-df %>% dplyr::select(Art,Accession)
names(dfa)
dfa<-distinct(dfa[,])
names(dfa)
dfa

summary(as.factor(dfa$Art))

venn.diagram(
  x = list(
    dfa %>% filter(Art== "Chen et al., 2016")%>% dplyr::select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Li et al., 2015")%>% dplyr::select(`Accession`) %>% unlist()),
  category.names = c("Chen (2016)" , "Li (2015)"),
  filename = '../figures/roots/Ven_Art_root.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", 'blue'),
  fill = c(alpha("#440154ff",0.3), alpha('blue',0.3)))

### extraer las palbras eclusivas y comportatidas por articulo 

algo<-VennDiagram::get.venn.partitions(list(chen= dfa %>% filter(Art== "Chen et al., 2016")%>% dplyr::select(`Accession`) %>% unlist(), 
                                            li=dfa %>% filter(Art== "Li et al., 2015")%>% dplyr::select(`Accession`) %>% unlist()))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por articulo 

dfcos

dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/roots/compartidos.csv")

dfcos %>% dplyr::select(V2)%>%dplyr::rename(Accession= V2)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/roots/unicos_Li_root.csv")

dfcos %>% select(V3)%>%rename(Accession= V3)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/roots/unicos_Chen_root.csv")

#### leaf ####

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")%>%filter(Tejido=="leaf")

df<-distinct(df[,-1])
names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # 1032 exclusivas de raiz (1263 Totales)

library(VennDiagram)
#Make the plot
names(df)

dfa<-df %>% select(Art,Accession)
names(dfa)
dfa<-distinct(dfa[,])
names(dfa)
dfa

summary(as.factor(dfa$Art))

venn.diagram(
  x = list(
    dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist(),
    dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist()),
  category.names = c("Chen (2016)" , "Gong (2017)", "Peng (2018)"),
  filename = '../figures/leaf/Ven_Art_leaf.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff', "red"),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('red',0.3)))

### extraer las palbras eclusivas y comportatidas por articulo 

algo<-VennDiagram::get.venn.partitions(
  list(chen= dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist(),
       gong= dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist(),
       peng= dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist()
      ))

algo$..values..
algo$..count..
algo

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por articulo 

names(dfcos)

dfcos %>% select(V1)%>%rename(Accession= V1)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/compartidos_todos.csv")

dfcos %>% select(V2)%>%rename(Accession= V2)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/compartidos_Peng-Gong.csv")

dfcos %>% select(V3)%>%rename(Accession= V3)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/compartidos_Peng-Chen.csv")

dfcos %>% select(V4)%>%rename(Accession= V4)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/unicos_Peng.csv")

dfcos %>% select(V5)%>%rename(Accession= V5)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/compartidos_Chen-Gong.csv")

dfcos %>% select(V6)%>%rename(Accession= V6)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/unicos_Gong.csv")

dfcos %>% select(V7)%>%rename(Accession= V7)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/leaf/unicos_Chen.csv")

#### totales ####

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")

df<-distinct(df[,-1])
names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # 1263 exclusivas de raiz (1263 Totales)

library(VennDiagram)
#Make the plot
names(df)

dfa<-df %>% select(Art,Accession)
names(dfa)
dfa<-distinct(dfa[,])
names(dfa)
dfa

summary(as.factor(dfa$Art))

venn.diagram(
  x = list(
    dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist(),
    dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist(),
    dfa %>% filter(Art== "Li et al., 2015")%>% select(`Accession`) %>% unlist()),
  category.names = c("Chen (2016)" , "Gong (2017)", "Peng (2018)", "Li (2015)"),
  filename = '../figures/total/Ven_Art_total.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff', "red", "blue"),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('red',0.3), alpha('blue',0.3)))

### extraer las palbras eclusivas y comportatidas por articulo 

algo<-VennDiagram::get.venn.partitions(
  list(chen= dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist(),
       gong= dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist(),
       peng= dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist(),
       li= dfa %>% filter(Art== "Li et al., 2015")%>% select(`Accession`) %>% unlist()
  ))

algo$..values..
algo$..count..
algo

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por articulo 

names(dfcos)

dfcos %>% select(V1)%>%rename(Accession= V1)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_todos.csv")

dfcos %>% select(V2)%>%rename(Accession= V2)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Gong-Li-Peng.csv")

dfcos %>% select(V3)%>%rename(Accession= V3)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Chen-Peng-Li.csv")

dfcos %>% select(V4)%>%rename(Accession= V4)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Peng-Li.csv")

dfcos %>% select(V5)%>%rename(Accession= V5)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Chen-Li-Gong.csv")

dfcos %>% select(V6)%>%rename(Accession= V6)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Li-Gong.csv")

dfcos %>% select(V7)%>%rename(Accession= V7)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Li-Chen.csv")

dfcos %>% select(V8)%>%rename(Accession= V8)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/unicos_Li.csv")

dfcos %>% select(V9)%>%rename(Accession= V9)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Chen-Peng-Gong.csv")

dfcos %>% select(V10)%>%rename(Accession= V10)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Peng-Gong.csv")

dfcos %>% select(V11)%>%rename(Accession= V11)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Chen-Peng.csv")

dfcos %>% select(V12)%>%rename(Accession= V12)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/unicos_Peng.csv")

dfcos %>% select(V13)%>%rename(Accession= V13)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/compartidos_Chen-Gong.csv")

dfcos %>% select(V14)%>%rename(Accession= V14)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/unicos_Gong.csv")

dfcos %>% select(V15)%>%rename(Accession= V15)%>%
  filter(Accession != "NA")%>%
  write.csv("../data/total/unicos_Chen.csv")

# end # 

# extraer los compartidos por roots, leaf, totales by Art

# Extraer compartidos: ROOTS
algo$..count..
comp<- algo$..values..[2]

dfcos<-t(stri_list2matrix(algo$..values..[2], byrow=TRUE)) %>% as.data.frame()

dfcos
class(dfcos)


nrow(comp)


############### intentar ponerlo en ggplot #####
df.venn <- data.frame(x = c(0, 0.866, -0.866, 1,2,3),
                      y = c(1, -0.5, -0.5,1,2.3,3),
                      labels = c('A', 'B', 'C', 'd', 'e', 'f'))
df.venn

library(limma)
library(tidyverse)
library(ggforce)


ggplot(df.venn, aes(x0 = x, y0 = y, r = 1.5, fill = labels)) +
  geom_circle(alpha = .3, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void()



