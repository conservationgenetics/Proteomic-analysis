  ## diagramas sobre proteomica de G. hirsutum 
library(tidyverse)
library(readxl)
library(VennDiagram)

df<-read_csv("../data/accession_art/compartidos.csv")

df


history(inf)

df<-df%>% 
  mutate(Presencia = rep(1, nrow(df)))

# venn diamgram bettween papers

names(df)
summary(as.factor(df$Art))

library(ggpubr)
pal<-c("#3c2550",
       "#6cda47",
       "#b54ade",
       "#dedb40",
       "#6045ca",
       "#a4d048",
       "#cc43b2",
       "#5edb85",
       "#522b80",
       "#d7a73c",
       "#657ace",
       "#de4330",
       "#65dcd6",
       "#d94479",
       "#4e9038",
       "#c782d0",
       "#8fdea1",
       "#8f3c6c",
       "#d5d385",
       "#2b2f34",
       "#d37836",
       "#61aec8",
       "#90322a",
       "#419874",
       "#d7736c",
       "#394d27",
       "#b4b1da",
       "#898b3a",
       "#576886",
       "#8c632d",
       "#c2ddc5",
       "#552526",
       "#d8b39a",
       "#77917a",
       "#cf8fa8",
       "#8a695e")

# numero de procesos #

names(df)
ggplot() + 
  geom_bar(aes(x = as.factor(Art), y= Presencia, fill = Proceso_clas_sim), data= df,
           stat="identity")+
  labs(x="Articulos", y = "Procesos identificados")+ 
  scale_fill_manual(values = pal)+ 
  theme(axis.text.x = element_text(angle = 0))

ggsave("../figures/Procesos_barras.png", height = 6, width = 15)


# numero de procesos porcentaje
ggplot() + 
  geom_bar(aes(x = as.factor(Art), y= Presencia, fill = Proceso_clas_sim), data= df,
           stat="identity", position = "fill")+
  labs(x="Articulos", y = "Procesos identificados")+ 
  scale_fill_manual(values = pal)+ 
  theme(axis.text.x = element_text(angle = 0))


ggsave("../figures/Procesos_barras_porcentaje.png", height = 6, width = 15)


# numeor de procesos por tejido

names(df)
ggplot() + 
  geom_bar(aes(x = Tejido, y= Presencia, fill = Proceso_clas_sim), data= df,
           stat="identity")+
  labs(x="Articulos", y = "Procesos identificados")+ 
  scale_fill_manual(values = pal)+ 
  theme(axis.text.x = element_text(angle = 0))

ggsave("../figures/Tejido_Procesos_barras.png", height = 6, width = 15)

ggplot() + 
  geom_bar(aes(x = Tejido, y= Presencia, fill = Proceso_clas_sim), data= df,
           stat="identity", position = "fill")+
  labs(x="Articulos", y = "Procesos identificados")+ 
  scale_fill_manual(values = pal)+ 
  theme(axis.text.x = element_text(angle = 0))

ggsave("../figures/Tejido_Procesos_barras_porcentaje.png", height = 6, width = 15)

############ ven diagram between tissues #####

# library
library(VennDiagram)
names(df)
#Make the plot
venn.diagram(
  x = list(
    df %>% filter(Tejido=="leaf") %>% select(Proceso_clas_sim) %>% unlist() , 
    df %>% filter(Tejido=="roots") %>% select(Proceso_clas_sim) %>% unlist()
  ),
  category.names = c("leaf" , "roots"),
  filename = '../figures/Ven_tissues.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3)))

# obtener las palabras unicas # 


roots<-df %>% filter(Tejido=="roots") %>% select(Proceso_clas_sim)
leaf<-df %>% filter(Tejido=="leaf") %>% select(Proceso_clas_sim)

unique(merge(x=roots,y=leaf,by="Proceso_clas_sim",all=F)) # compartidos

unique(merge(x=roots,y=leaf,by="Proceso_clas_sim",all.x=T)) # roots
unique(merge(x=roots,y=leaf,by="Proceso_clas_sim",all.y=T)) # leaf



### up / down ####
names(df)
ggplot() + 
  geom_bar(aes(x = Tejido, y= Presencia, fill = `Up/downb`), data= df,
           stat="identity")+
  labs(x="Articulos", y = "Procesos identificados")+ 
  scale_fill_manual(values = pal)+ 
  theme(axis.text.x = element_text(angle = 0))

ggsave("../figures/Tejido_Cascada.png", height = 6, width = 15)

###
ggplot() + 
  geom_bar(aes(x = Tejido, y= Presencia, fill = `Up/downb`), data= df,
           stat="identity", position = "fill")+
  labs(x="Articulos", y = "Procesos identificados")+ 
  scale_fill_manual(values = pal)+ 
  theme(axis.text.x = element_text(angle = 0))

ggsave("../figures/Tejido_Cascada_porcentaje.png", height = 6, width = 15)




