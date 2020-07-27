# analisis con los compartidos 
library(tidyverse)
library(readxl)
library(plyr)
library(readr)
library(RColorBrewer)
library(dplyr)

#setwd("~/google-drive/history_AW/history_FGC/data")
#####  1) extraer los compartidos de raices ####

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")%>%filter(Tejido == "root")

df
#df<-distinct(df[,-1])
#3names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # 478 unicas

raiz<-read.csv("../data/roots/compartidos.csv")
raiz$Accession<-as.character(raiz$Accession)
str(df)

comp_raiz<-df %>% 
  filter(Accession %in% raiz$Accession) %>% # extraer todos los datos de la BaseGeneral
  filter(`Up/downb` != "NA")

unique(comp_raiz$Accession)
names(comp_raiz)
dim(comp_raiz)

ggplot(comp_raiz, aes(Accession,`Code COG` , fill= `Up/downb`)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = c("#a5b064",
                               "#c153bc"))+
  ggtitle("Compartidos en raiz")+theme_bw()+ 
  guides(fill=guide_legend(title="Direction"))+facet_grid(~Art)

ggsave("../figures/roots/Compartidos_barras_art.png", dpi = 300, height = 6, width = 9)

# compartidos en una sola grafica (Encimados)
ggplot(comp_raiz, aes(Accession,`Code COG` , fill= `Up/downb`)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = c("#a5b064",
                               "#c153bc"))+
  ggtitle("Compartidos en raiz")+theme_bw()+ 
  guides(fill=guide_legend(title="Direction"))

ggsave("../figures/roots/Compartidos_barras_art_juntas.png", dpi = 300, height = 6, width = 9)

# grafica por funcion (cat_new #)
pal<-c("#95caff",
       "#35b300",
       "#8901c8",
       "#ff9632",
       "#015297",
       "#ff47b2",
       "#3a2e00",
       "#e3a0ff")

ggplot(comp_raiz, aes(Accession,`Code COG` , fill= Cat_new)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal)+
  ggtitle("Compartidos en raiz")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))

ggsave("../figures/roots/Compartidos_barras_juntas_function.png", dpi = 300, height = 6, width = 9)


#####  2) extraer los compartidos de hojas ####
df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")%>%filter(Tejido == "leaf")

dim(df)
#df<-distinct(df[,-1])
#dim(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

dim(df)
length(unique(df$Accession)) # 1032 unicas de hoja 

# extraer los compartidos en hojas # 
myfiles = list.files(path="~/google-drive/history_AW/history_FGC/data/leaf/", pattern="compartidos_*", full.names=TRUE)
myfiles
hojas<-ldply(myfiles, read_csv) %>% dplyr::select(Accession)
hojas$Accession<-as.character(hojas$Accession)
hojas

comp_hojas<-df %>% 
  filter(Accession %in% hojas$Accession)#%>% # extraer todos los datos de la BaseGeneral
  #filter(`Up/downb` != "NA")

dim(comp_hojas)
unique(comp_hojas$Accession)

names(comp_hojas)

ggplot(comp_hojas, aes(Accession,`Code COG` , fill= `Up/downb`)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = c("#a5b064",
                               "#c153bc", "black"))+
  ggtitle("Compartidos en hoja")+theme_bw()+ 
  guides(fill=guide_legend(title="Direction"))+facet_grid(~`Art`)

ggsave("../figures/leaf/Compartidos_barras_art_NAs.png", dpi = 300, height = 7, width = 14)

###
ggplot(comp_hojas, aes(Accession,`Code COG` , fill= Cat_new)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal)+
  ggtitle("Compartidos en hoja")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))+facet_grid(~`Art`)

ggsave("../figures/leaf/Compartidos_barras_juntas_function_Art.png",
       dpi = 300, height = 7, width =14)

###
ggplot(comp_hojas, aes(Accession,`Code COG` , fill= Cat_new)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal)+
  ggtitle("Compartidos en hoja")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))

ggsave("../figures/leaf/Compartidos_barras_juntas_function.png",
       dpi = 300, height = 7, width =14)

###
pal1<-c(rep("#e7c349",11), 
       rep("#0059b3", 1),
       rep("#e7c349", 3),
       rep("#0059b3", 1),
       rep("#e7c349", 2),
       rep("#0059b3",1),
       rep("#e7c349", 3),
       rep("#0059b3",1),
       rep("#e7c349", 38))
pal1

ggplot(comp_hojas, aes(Accession,`Code COG` , fill= Accession )) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal1)+
  ggtitle("Compartidos en hoja")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))+theme(legend.position = "none")

ggsave("../figures/leaf/Compartidos_COG_Art_4.png",
       dpi = 300, height = 7, width =14)


####  3) extraer los compartidos en hojas y raices  ####

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")

#df<-distinct(df[,-1])
#3names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # 1263 unicas

# extraer los compartidos entre todos # 
myfiles = list.files(path="~/google-drive/history_AW/history_FGC/data/total/", pattern="compartidos_*", full.names=TRUE)
myfiles
dat_csv<-ldply(myfiles, read_csv) %>% dplyr::select(Accession)
dat_csv$Accession<-as.character(dat_csv$Accession)
dat_csv

comp_todos<-df %>% 
  filter(Accession %in% dat_csv$Accession)# %>% # extraer todos los datos de la BaseGeneral
  #filter(`Up/downb` != "NA")

ggplot(comp_todos, aes(Accession,`Code COG` , fill= `Up/downb`)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = c("#a5b064",
                               "#c153bc", "black"))+
  ggtitle("Compartidos en total")+
  facet_grid(~Art)+theme_bw()+ 
  guides(fill=guide_legend(title="Direction"))

ggsave("../figures/total/Compartidos_barras_art.png", dpi = 300, height = 12, width = 16)

# cat new # 
pal<-c("#95caff",
       "#35b300",
       "#8901c8",
       "#ff9632",
       "#015297",
       "#ff47b2",
       "#3a2e00",
       "#e3a0ff", "red")

ggplot(comp_todos, aes(Accession,`Code COG` , fill= Cat_new)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal)+
  ggtitle("Compartidos en total")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))+facet_grid(~`Art`)

ggsave("../figures/total/Compartidos_barras_juntas_function_Art.png",
       dpi = 300, height = 11, width =17)

# function todos juntos # 
ggplot(comp_todos, aes(Accession,`Code COG` , fill= Cat_new)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal)+
  ggtitle("Compartidos en total")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))

ggsave("../figures/total/Compartidos_barras_juntas_function.png",
       dpi = 300, height = 10, width = 14)

######### compartidos entre todos (2) #####
df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")

#df<-distinct(df[,-1])
#3names(df)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))%>% 
  filter(`Code COG`!= "-")%>% 
  filter(`Code COG`!= "NA")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")

length(unique(df$Accession)) # 1263 unicas

comp_todos<-read.csv("../data/total/compartidos_todos.csv")
comp_todos$Accession<-as.character(comp_todos$Accession)

comp_2<-df %>% 
  filter(Accession %in% comp_todos$Accession) %>% # extraer todos los datos de la BaseGeneral
  filter(`Up/downb` != "NA")

unique(comp_2$Accession)

# graficas #  
ggplot(comp_2, aes(Accession,`Code COG` , fill= `Up/downb`)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = c("#a5b064",
                               "#c153bc", "black"))+
  ggtitle("Compartidos en hoja")+theme_bw()+ 
  guides(fill=guide_legend(title="Direction"))+facet_grid(~`Art`)

ggsave("../figures/total/Compartidos_barras_art_2.png", dpi = 300, 
       height = 7, width = 14)

###
ggplot(comp_2, aes(Accession,`Code COG` , fill= Cat_new)) + 
  geom_tile()+coord_flip()+
  scale_fill_manual(values = pal)+
  ggtitle("Compartidos en hoja")+theme_bw()+ 
  guides(fill=guide_legend(title="Fuction"))+facet_grid(~`Art`)

ggsave("../figures/total/Compartidos_barras_juntas_function_2.png",
       dpi = 300, height = 7, width =14)


##### Trato de ponerlos todos en un mismos plot ####
library(ggpubr)
ggarrange(bxp, dp, bp + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
















