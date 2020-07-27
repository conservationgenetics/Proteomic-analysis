library(tidyverse)
library(readxl)

df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")
df

View(df)

names(df)
dim(df)

levels(as.factor(df$Art))
names(df)

library(ggthemes)
col<-c("#56b7e5",
       "#e94c48",
       "#68d55e",
       "#a269e3",
       "#b6ca44",
       "#6a7eeb",
       "#d2b93e",
       "#d074df",
       "#9ed465",
       "#d859b6",
       "#71e098",
       "#e24f8a",
       "#569c41",
       "#9f75be",
       "#dc9835",
       "#497fca",
       "#db6c30",
       "#699ce9",
       "#899345",
       "#e29ddb",
       "#4ba47a",
       "#d96166",
       "#77e2c0",
       "#d57d9a",
       "#4bd0d7",
       "#bb7149",
       "#5092c3",
       "#ec9675",
       "#6e7fb4",
       "#d5cd85",
       "#adabeb",
       "#ae8a49")

levels(as.factor(df$Art))
levels(as.factor(df$Cat_new))


######## OBJETIVO 1 #####
##### Cat new vs Articulos #####

df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(Cat_new))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  ggplot(aes(fill=Cat_new, y=presencia, x=Art)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Abundancia relativa", title = "Articulo Vs Cat_New")+ 
  theme(axis.text.x = element_text(angle = 0))+ coord_flip()

ggsave("../figures/New_figures/CatNew_Articulos.png", dpi = 300, height =  5.43,  width = 7.31)


#### Cat new vs Tratamientos #####
names(df)
levels(as.factor(df$Tratamientos))

df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(Cat_new))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante"))%>% 
  ggplot(aes(fill=Cat_new, y=presencia, x=Tratamientos)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Abundancia relativa", title = "Tratamientos Vs Cat_new")+ 
  theme(axis.text.x = element_text(angle = 0))+ coord_flip()

ggsave("../figures/New_figures/CatNew_Tratamientos.png", dpi = 300, height =  5.43,  width = 7.31)


##### COG vs Articulos #####

names(df)
col<-c("#ce4d2b",
       "#5b65df",
       "#61b248",
       "#9747be",
       "#afac38",
       "#d965cc",
       "#42763a",
       "#cc368b",
       "#5aaf8b",
       "#b683dd",
       "#de8f2e",
       "#5b5fb6",
       "#9c5e1f",
       "#7c96e1",
       "#7a7a33",
       "#894b91",
       "#c89c65",
       "#5d669d",
       "#e38265",
       "#4ea7cd",
       "#98583f",
       "#d489b7",
       "#a24978")


df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(`Funcion (COG)`))%>%
  filter(`Funcion (COG)` != "-" )%>%
  filter(!Art %in% c("Cui et al., 2015", "Deeba et al., 2012"))%>% 
  ggplot(aes(fill=`Funcion (COG)`, y=presencia, x=Art)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Abundancia relativa", title = "Articulos Vs COG")+ 
  theme(axis.text.x = element_text(angle = 0), legend.position = "none")+ coord_flip()

ggsave("../figures/New_figures/COG_Articulos.png")


##### COG vs Tratamientos #####

names(df)
col<-c("#ce4d2b",
       "#5b65df",
       "#61b248",
       "#9747be",
       "#afac38",
       "#d965cc",
       "#42763a",
       "#cc368b",
       "#5aaf8b",
       "#b683dd",
       "#de8f2e",
       "#5b5fb6",
       "#9c5e1f",
       "#7c96e1",
       "#7a7a33",
       "#894b91",
       "#c89c65",
       "#5d669d",
       "#e38265",
       "#4ea7cd",
       "#98583f",
       "#d489b7",
       "#a24978")


df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(`Funcion (COG)`))%>%
  filter(`Funcion (COG)` != "-" )%>%
  filter(!is.na(Tratamientos))%>%
  filter(!Art %in% c("Cui et al., 2015", "Deeba et al., 2012"))%>% 
  ggplot(aes(fill=`Funcion (COG)`, y=presencia, x=Tratamientos)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Abundancia relativa", title = "COG VS TRATAMIENTOS")+ 
  theme(axis.text.x = element_text(angle = 0), legend.position = "none")+ coord_flip()

ggsave("../figures/New_figures/COG_Tratamientos.png", dpi = 300, height =  5.43,  width = 7.31)


###### OBJETIVO 2 ######

#####Compartidas entre articulos, tejidos y tratamientos # 
names(df)
levels(as.factor(df$Tejido))


library(venn)
### Tejido Accession  ####
names(df)
levels(as.factor(df))

png(filename="../figures/New_figures/Venn_Tejido_Accesion.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Tejido== "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(), 
              df %>% filter(Tejido== "root")%>% dplyr::select(Accession)%>% filter(!is.na(Accession)) %>% unlist()), 
     ilabels = F, zcolor = c( "#66A61E", "#7570B3"),
     snames = "leaf, root", box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

dev.off()

##### Tratamientos ####
levels(as.factor(df$Tratamientos))
names(df)

png(filename="../figures/New_figures/Venn_Tratamiento_Accesion.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Tratamientos == "Sensitive 24h") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
     df %>% filter(Tratamientos == "Sensitive 4h") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
     df %>% filter(Tratamientos == "Tolerant 1 week") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
     df %>% filter(Tratamientos == "Tolerant 4h")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628'),
     snames = "Sensitive 24h, Sensitive 4h, Tolerant 1 week, Tolerant 4h", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

dev.off()

#### Type #####

png(filename="../figures/New_figures/Venn_Type_Accesion.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Type == "No Tolerante")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(), 
              df %>% filter(Type == "Sensible")%>% dplyr::select(Accession)%>% filter(!is.na(Accession)) %>% unlist(), 
              df %>% filter(Type == "Tolerante")%>% dplyr::select(Accession)%>% filter(!is.na(Accession)) %>% unlist()), 
     ilabels = F, zcolor = c( "#66A61E", "#7570B3", "red"),
     snames = "No tolerante, Sensible, Tolerante", box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

dev.off()


##### Articulos  ####
levels(as.factor(df$Art))
names(df)

png(filename="../figures/New_figures/Venn_Articulos_Accesion.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Art == "Chen et al., 2016") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(), 
              df %>% filter(Art == "Gong et al., 2017") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Li et al., 2015") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Peng et al., 2018")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Cui et al., 2015")%>%  dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e41a1c','#377eb8','#4daf4a','#984ea3', "black"),
     snames = "Chen (2016), Gong (2017),Li (2015), Peng (2018), Cui (2015) ", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

dev.off()


###### CORRRECIONES FLOR #####

names(df)

df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(Cat_new))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante"))%>% 
  ggplot(aes(fill=Cat_new, y=presencia, x=Tratamientos)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Proportion", title = "Tratamientos Vs Articulos")+ 
  theme(axis.text.x = element_text(angle = 0))+ coord_flip()+ facet_grid(~Art, scales = "free")

ggsave("../figures/New_figures/CatNew_Tratamientos_Vs_Articulos.png", dpi = 300, height =  6,  width = 12.31)


# tartamientos vs Tejido #

names(df)
df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(Cat_new))%>%
  filter(!is.na(Tejido))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!Art %in% c("Deeba et al., 2012")) %>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante")) %>% 
  ggplot(aes(fill=Cat_new, y=presencia, x=Tratamientos)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Proportion", title = "Tratamientos Vs Tejido")+ 
  theme(axis.text.x = element_text(angle = 0))+ coord_flip()+ facet_grid(~Tejido, scales = "free")


ggsave("../figures/New_figures/CatNew_Tratamientos_Vs_Tejidos.png", dpi = 300, height =  5.43,  width = 10.31)


##### con cog  ?? #####
df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(Cat_new))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante"))%>% 
  ggplot(aes(fill=Cat_new, y=presencia, x=Tratamientos)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Proportion", title = "Tratamientos Vs Articulos")+ 
  theme(axis.text.x = element_text(angle = 0))+ coord_flip()+ facet_grid(~Art, scales = "free")

ggsave("../figures/New_figures/CatNew_Tratamientos_Vs_Articulos.png", dpi = 300, height =  5.43,  width = 10.31)


# tartamientos vs Tejido #

names(df)
df %>% mutate(presencia= rep(1, nrow(df)))%>%
  filter(!is.na(Cat_new))%>%
  filter(!is.na(Tejido))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!Art %in% c("Deeba et al., 2012")) %>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante")) %>% 
  ggplot(aes(fill=Cat_new, y=presencia, x=Tratamientos)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Proportion", title = "Tratamientos Vs Tejido")+ 
  theme(axis.text.x = element_text(angle = 0))+ coord_flip()+ facet_grid(~Tejido, scales = "free")


##### correcion (VENN) #####
levels(as.factor(df$Tejido))
######## ven de solo plantas: Articulos##### 
levels(as.factor(df$Art))

png(filename="../figures/New_figures/Venn_Articulos_SOLO_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Art == "Chen et al., 2016") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(), 
              df %>% filter(Art == "Gong et al., 2017") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Li et al., 2015") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Peng et al., 2018")%>%filter(Tejido == "leaf")%>%  dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Cui et al., 2015")%>%filter(Tejido == "leaf")%>%  dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e41a1c','#377eb8','#4daf4a','#984ea3', "black"),
     snames = "Chen (2016), Gong (2017),Li (2015), Peng (2018) , Cui (2015)", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.1)

text(2, 2, "Solo Plantas", adj = c(-1.5,0))

dev.off()


######## ven de solo plantas: Tratamientos ##### 

png(filename="../figures/New_figures/Venn_Tratamientos_SOLO_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Tratamientos == "Sensitive 24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Tratamientos == "Sensitive 4h") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Tratamientos == "Tolerant 24h")%>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Tratamientos == "Tolerant 4h")%>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628'),
     snames = "Sensitive 24h, Sensitive 4h, Tolerant 24h, Tolerant 4h", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)


text(2, 2, "Solo Hojas", adj = c(-1.5,0))

dev.off()

### seleccionar por sensible y tolerante y otro por 
# horas de exposicion 

df1<- df %>% mutate(TIPO = Tratamientos) %>% 
  separate(TIPO, c("TIPO","Tiempo"), sep = " ")

df1$Tiempo[df1$Tiempo == 1] <- "1 week"
levels(as.factor(df1$TIPO))
df1


png(filename="../figures/New_figures/Venn_TIPO_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df1 %>% filter(TIPO == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df1 %>% filter(TIPO == "Tolerant")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#ffff33','#a65628'),
     snames = "Sensitive , Tolerant", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

text(2, 2, "Solo Plantas", adj = c(-1.5,0))


dev.off()


### ven tiempo ####
png(filename="../figures/New_figures/Venn_Tiempo_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df1 %>% filter(Tiempo == "1 week")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df1 %>% filter(Tiempo == "4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df1 %>% filter(Tiempo == "24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#377eb8','#4daf4a','#ffff33'),
     snames = "1 week , 4h , 24h", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)
text(2, 2, "Solo Plantas", adj = c(-1.5,0))


dev.off()


########## piramides #####
names(df)
levels(as.factor(df$`Up/downb`))

df1<- df %>% mutate(presencia= rep(1, nrow(df)))%>%
  mutate(TIPO = Tratamientos) %>% 
  separate(TIPO, c("TIPO","Tiempo"), sep = " ")


##### Up/down articulos  #####
df2<-df1 %>% group_by(Art, `Up/downb`) %>% summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA" )
df2

col <- c("#72cb87",
         "#749ee3")

ggplot(df2, aes(x = Art, y = Total * ((-1)^(`Up/downb` == "down")), 
                             fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(Art, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.5, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(Art, -Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = -.5, size=4)+
  scale_fill_manual(values = col)+ theme_minimal()+theme(legend.position = "top")

ggsave("../figures/New_figures/Up_Down_Articulos.png",height = 8, width = 7)


##### Up/down tratamientos #####
col <- c("#72cb87",
         "#749ee3")

df2<-df1 %>% group_by(Tratamientos, `Up/downb`) %>% 
  summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA")%>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante")) %>%
  filter(!is.na(Tratamientos))
df2

ggplot(df2, aes(x = Tratamientos, y = Total * ((-1)^(`Up/downb` == "down")), 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(Tratamientos, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.9, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(Tratamientos, -Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 2, size=4)+
  scale_fill_manual(values = col)+ theme_minimal()+theme(legend.position = "top")

ggsave("../figures/New_figures/Up_Down_Tratamientos.png",height = 6, width = 7)


df2<-df1 %>% group_by(Tiempo, `Up/downb`) %>% 
  summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA" )%>% 
  filter(Tiempo != "NA" ) %>% 
  filter(Tiempo != "Tolerante" ) 

df2$Tiempo[df2$Tiempo == 1] <- "1 week"
df2

ggplot(df2, aes(x = Tiempo, y = Total * ((-1)^(`Up/downb` == "down")), 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(Tiempo, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.6, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(Tiempo, -Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 0, size=4)+
  scale_fill_manual(values = col)+ theme_minimal()+theme(legend.position = "top")

ggsave("../figures/New_figures/Up_Down_Tiempo.png",width = 6)

######### Up/down funciones vs tiempo #######

df2<-df1 %>% group_by(Tiempo, Cat_new, `Up/downb`) %>% 
  summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA" )%>% 
  filter(Tiempo != "NA" ) %>% 
  filter(Tiempo != "Tolerante" ) 

df2


df2$Tiempo[df2$Tiempo == 1] <- "1 week"

df2 <- df2 %>% filter(Cat_new != "NA")

ggplot(df2, aes(x = Tiempo, y = Total, 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(Tiempo, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.6, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(Tiempo, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 0, size=4)+
  scale_fill_manual(values = col)+theme(legend.position = "top")+
    facet_grid(`Up/downb`~Cat_new, scales = "free",space = "free")+
  theme(axis.text.x = element_text(angle = 90))

ggsave("../figures/New_figures/Up_Down_Tiempo_VS_funcion.png",height = 8 , width = 14)

######### Up/down funciones vs tratamiento #######

df2<-df1 %>% group_by(Tratamientos, Cat_new, `Up/downb`) %>% 
  summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA" )%>% 
  filter(Tratamientos != "NA" ) %>% 
  filter(Tratamientos != "Tolerante")%>% filter(Cat_new != "NA")
 
df2

ggplot(df2, aes(x = Tratamientos, y = Total, 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(Tratamientos, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.6, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(Tratamientos, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 0, size=4)+
  scale_fill_manual(values = col)+theme(legend.position = "top")+
  facet_grid(`Up/downb`~Cat_new, scales = "free",space = "free")+
  theme(axis.text.x = element_text(angle = 90))

ggsave("../figures/New_figures/Up_Down_Tratamiento_VS_funcion.png",height = 8 , width = 14)


############ Up/down TIPO VS Funciones ######

df1<- df %>% mutate(presencia= rep(1, nrow(df)))%>%
  mutate(TIPO = Tratamientos) %>% 
  separate(TIPO, c("TIPO","Tiempo"), sep = " ")%>% 
  group_by(TIPO, Cat_new, `Up/downb`) %>% 
  summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA" )%>% 
  filter(TIPO %in% c("Sensitive", "Tolerant" ))%>%
  filter(!is.na(`Up/downb`))%>%
  filter(!is.na(TIPO))%>%
  filter(!is.na(Cat_new))

names(df1)

ggplot(df1, aes(x = TIPO, y = Total, 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df1, `Up/downb`  == "up"), 
            aes(TIPO, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.6, size=4)+
  geom_text(data = subset(df1, `Up/downb`  == "down"), 
            aes(TIPO, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 0, size=4)+
  scale_fill_manual(values = col)+theme(legend.position = "top")+
  facet_grid(`Up/downb`~Cat_new, scales = "free",space = "free")+
  theme(axis.text.x = element_text(angle = 90))

ggsave("../figures/New_figures/Up_Down_TIPO_VS_funcion.png",height = 8 , width = 14)




