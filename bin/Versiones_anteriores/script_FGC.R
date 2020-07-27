library(tidyverse)
library(readxl)

df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))

df

names(df)
levels(as.factor(df$Art))

###### Peng  #####
peng<-df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(Art %in% c("Peng et al., 2018")) %>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante"))%>%
  ggplot(aes(y=presencia, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="Peng et al., 2018", y= "Abundance", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))
peng

###### Gong  #####
gong<-df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(Art %in% c("Gong et al., 2017")) %>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante"))%>%
  ggplot(aes(y=presencia, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="Gong et al., 2017", y= "", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))

gong

###### chen hojas  #####

names(df)
chen_hoja<-df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(Art %in% c("Chen et al., 2016"))%>%
  filter(Tejido == "leaf")%>%
  ggplot(aes(y=presencia, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="Chen et al., 2016", y= "", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))

chen_hoja

###### chen roots  #####

names(df)
chen_root<-df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(Art %in% c("Chen et al., 2016"))%>%
  filter(Tejido == "root")%>%
  ggplot(aes(y=presencia, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="Chen et al., 2016", y= "", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))

chen_root


###### Li  #####
Li<- df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(Art %in% c("Li et al., 2015"))%>%
  ggplot(aes(y=presencia, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="Li et al., 2015", y= "", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))
Li

###### Cui  #####
Cui<- df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(Art %in% c("Cui et al., 2015"))%>% 
  filter(!Tratamientos %in% c("Tolerante"))%>%
  ggplot(aes(y=presencia, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="Cui et al., 2015", y= "", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))
Cui

library(ggpubr)
ggarrange(peng, gong,chen_hoja, Cui, chen_root, Li, ncol = 6, 
          widths=c(0.7,0.7, 0.3,0.4, 0.3, 0.3),
          common.legend = TRUE)

ggsave("../figures/New_figures/Todos_juntos.png", dpi = 300,height = 7, width = 15)


df %>% group_by(Art, Tejido, Tratamientos, `Categoria funcional`) %>%
  summarise(Total=sum(presencia)) %>% arrange(Tejido) %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>%
  filter(!Art %in% c("Deeba et al., 2012"))%>% 
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante"))%>%
  ggplot(aes(y=Total, x=Tratamientos, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values = col)+
  labs(x="", y= "", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))+ 
  facet_grid(Tejido~Art, scales = "free", space = "free")





levels()

####### veen diagram #####

png(filename="../figures/New_figures/Venn_Tratamientos_SOLO_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Art %in% c("Peng et al., 2018")) %>% filter(Tratamientos == "Sensitive 24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Peng et al., 2018")) %>% filter(Tratamientos == "Sensitive 4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Gong et al., 2017")) %>% filter(Tratamientos == "Sensitive 24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Gong et al., 2017")) %>% filter(Tratamientos == "Sensitive 4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Cui et al., 2015")) %>% filter(Tratamientos == "Sensitive 24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Peng et al., 2018")) %>% filter(Tratamientos == "Tolerant 24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Peng et al., 2018")) %>% filter(Tratamientos == "Tolerant 4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Gong et al., 2017")) %>% filter(Tratamientos == "Tolerant 24h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art %in% c("Gong et al., 2017")) %>% filter(Tratamientos == "Tolerant 4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628', "black", "blue","pink", "green"),
     snames = "Peng_Sensitive 24h, Peng_Sensitive 4h, Gong_Sensitive 24h, Gon_Sensitive 4h, 
     Cui_Sensitive 24h, Peng_Tolerant 24h, Peng_Tolerant 4h, Gong_Tolerant 24h, Gon_Tolerant 4h", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)


text(2, 2, "Solo Plantas", adj = c(-1.5,0))

dev.off()

######### Fig 1 ######
df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>%
  filter(!is.na(`Categoria funcional`))

col<-c("#4b9761",
       "#cc59c0",
       "#6dc45b",
       "#896ddd",
       "#c3c34e",
       "#6f88d1",
       "#d19030",
       "#4bb1d4",
       "#d75d36",
       "#63d5b3",
       "#de4366",
       "#868f41",
       "#cc8fce",
       "#c7945f",
       "#cd5f93",
       "#d0746d","red")

df

names(df)

df %>% group_by(Tiempo, Tratamientos, Art) %>% summarise(Total= sum(presencia)) %>% 
  filter(Art != "Deeba et al., 2012")%>% arrange(Tratamientos)

df$`Categoria funcional` = factor(df$`Categoria funcional`, levels=c("CELLULAR PROCESSES AND SIGNALING",
                                            "Cell division, differentiation and fate",
                                             "Wall structure, signaling and interactions",
                                             "Membrane and transport",
                                             "Cytoskeleton",
                                             "Signaling",
                                             "Protein folding and transport",
                                             "Protein modification",
                                             "Proteolysis",
                                             "Stress and defense",
                                            "INFORMATION STORAGE AND PROCESSING",
                                             "Transcription related",
                                             "DNA structure and dynamics",
                                             "Translation and ribosome structure",
                                            "METABOLISM",
                                             "Energy production and conversion",
                                             "Photosynthesis",
                                             "Carbohydrate and energy metabolism",
                                             "Other metabolism",
                                            "POORLY CHARACTERIZED",
                                             "Unknown",
                                             "Miscellaneous"))


ggplot(df %>% filter(Tratamientos != "NA") %>% 
         filter(Tejido == "leaf") , 
       aes(fill=factor(`Categoria funcional`, levels=c("CELLULAR PROCESSES AND SIGNALING",
                                                       "Cell division, differentiation and fate",
                                                       "Wall structure, signaling and interactions",
                                                       "Membrane and transport",
                                                       "Cytoskeleton",
                                                       "Signaling",
                                                       "Protein folding and transport",
                                                       "Protein modification",
                                                       "Proteolysis",
                                                       "Stress and defense",
                                                       "INFORMATION STORAGE AND PROCESSING",
                                                       "Transcription related",
                                                       "DNA structure and dynamics",
                                                       "Translation and ribosome structure",
                                                       "METABOLISM",
                                                       "Energy production and conversion",
                                                       "Photosynthesis",
                                                       "Carbohydrate and energy metabolism",
                                                       "Other metabolism",
                                                       "POORLY CHARACTERIZED",
                                                       "Unknown",
                                                       "Miscellaneous")), y=presencia, x=Tiempo)) + 
  geom_bar(stat="identity", position = "fill") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = c(col),name = "")+
  facet_wrap(vars(Tratamientos), 
             strip.position = "top", ncol = 1,
             scales="free")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Time")+ggtitle("Expresion en hojas tolerante y sensibles")+
  facet_grid(~Tratamientos, scales = "free")


ggsave("../figures/New_figures/Fig_1_Chen.png", dpi = 300,height = 8, width = 10)


##### Fig 1: sin cheen ####
names(df)

ggplot(df %>% filter(Art != "Chen et al., 2016") %>% filter(Tratamientos != "NA") %>% 
         filter(Tejido == "leaf") , 
       aes(fill=factor(`Categoria funcional`, levels=c("CELLULAR PROCESSES AND SIGNALING",
                                                       "Cell division, differentiation and fate",
                                                       "Wall structure, signaling and interactions",
                                                       "Membrane and transport",
                                                       "Cytoskeleton",
                                                       "Signaling",
                                                       "Protein folding and transport",
                                                       "Protein modification",
                                                       "Proteolysis",
                                                       "Stress and defense",
                                                       "INFORMATION STORAGE AND PROCESSING",
                                                       "Transcription related",
                                                       "DNA structure and dynamics",
                                                       "Translation and ribosome structure",
                                                       "METABOLISM",
                                                       "Energy production and conversion",
                                                       "Photosynthesis",
                                                       "Carbohydrate and energy metabolism",
                                                       "Other metabolism",
                                                       "POORLY CHARACTERIZED",
                                                       "Unknown",
                                                       "Miscellaneous")), y=presencia, x=Tiempo)) + 
  geom_bar(stat="identity", position = "fill") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = c(col),name = "")+
  facet_wrap(vars(Tratamientos), 
             strip.position = "top", ncol = 1,
             scales="free")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Time")+ggtitle("Expresion en hojas tolerante y sensibles")+
  facet_grid(Tratamientos~Art, scales = "free")

ggsave("../figures/New_figures/Fig_1.png", dpi = 300,height = 8, width = 10)


###### Fig 2 #######
# 2 Diagramas de Venn Solo sensibles y tolerancia, 4 y 24 todo hoja
library(venn)

levels(as.factor(df$Tratamientos))

df1<-df %>% 
  unite("Var_TT",  c(Tratamientos, Tiempo), remove = FALSE)

##### Tratamientos####
png(filename="../figures/New_figures/Venn_Tratamientos_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Tratamientos == "Tolerant") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#4daf4a','#984ea3'),
     snames = "Sensitive, Tolerant", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

dev.off()

names(df)
levels(as.factor(df$Tiempo))

##### Tiempo ####
png(filename="../figures/New_figures/Venn_Tiempo_PLANTAS.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Tiempo == "4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Tiempo == "24h") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c("#e34a33", "#2c7fb8"),
     snames = "4 h, 24 h", 
     box = F, ellipse = F,  ilcs = 1, sncs = 1.2)

dev.off()


#### shared by Tratamientos #####

algo<-VennDiagram::get.venn.partitions(list(Sensibles= df %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            Tolerantes=df %>% filter(Tratamientos == "Tolerant") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Comp_Sensibles_Tolerantes.csv")

dfcos %>% dplyr::select(V2)%>% dplyr::rename(Accession= V2)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Unicos_Tolerantes.csv")

dfcos %>% dplyr::select(V3)%>% dplyr::rename(Accession= V3)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Unicos_Sensibles.csv")


#### shared by Tiempo #####

algo<-VennDiagram::get.venn.partitions(list(T_4h=df %>% filter(Tiempo == "4h")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            T_24h=df %>% filter(Tiempo == "24h") %>% filter(Tejido == "leaf")%>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Comp_4h_24h.csv")

dfcos %>% dplyr::select(V2)%>% dplyr::rename(Accession= V2)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Unicos_24h.csv")

dfcos %>% dplyr::select(V3)%>% dplyr::rename(Accession= V3)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Unicos_4h.csv")


#### shared by chen/Li #####

algo<-VennDiagram::get.venn.partitions(list(Chen= df %>% filter(Art == "Chen et al., 2016") %>% filter(Tejido == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            Li= df %>% filter(Art == "Li et al., 2015") %>% filter(Tejido == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos chen y li 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Comp_Chen_Li.csv")

dfcos %>% dplyr::select(V2)%>% dplyr::rename(Accession= V2)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Unicos_Li.csv")

dfcos %>% dplyr::select(V3)%>% dplyr::rename(Accession= V3)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")%>%
  write.csv("../data/Compartidos/Unicos_Chen.csv")



##### Figura 2 bis #####
# 2 diagramas: 1 tolerantes(tratamientos) ; 2 sensibles (tratamientos)


png(filename="../figures/New_figures/Venn_Tolerantes_Tiempo.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Art == "Cui et al., 2015") %>% filter(Tiempo == "24h") %>% filter(Tratamientos == "Tolerant")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Gong et al., 2017") %>% filter(Tiempo == "24h") %>% filter(Tratamientos == "Tolerant")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Peng et al., 2018") %>% filter(Tiempo == "24h") %>% filter(Tratamientos == "Tolerant")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Gong et al., 2017") %>% filter(Tiempo == "4h") %>% filter(Tratamientos == "Tolerant")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Peng et al., 2018") %>% filter(Tiempo == "4h") %>% filter(Tratamientos == "Tolerant")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c("#57d193",
                              "#b371da",
                              "#7aca4f",
                              "#c5b741",
                              "#df703a"),
     snames = "Cui_T-24, Gong_T-24, Peng_T-24, Gong_T-4, Peng_T-4", 
     box = F, ellipse = T, ilcs = 1, sncs = 1.2)

text(5, 2, "Tolerantes", adj = c(0,0))
dev.off()

### Sensibles (tratamientos)##

png(filename="../figures/New_figures/Venn_Sensibles_Tiempo.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Art == "Cui et al., 2015") %>% filter(Tiempo == "24h") %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Gong et al., 2017") %>% filter(Tiempo == "24h") %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Peng et al., 2018") %>% filter(Tiempo == "24h") %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Gong et al., 2017") %>% filter(Tiempo == "4h") %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Peng et al., 2018") %>% filter(Tiempo == "4h") %>% filter(Tratamientos == "Sensitive")%>% filter(Tejido == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c("#57d193",
                              "#b371da",
                              "#7aca4f",
                              "#c5b741",
                              "#df703a"),
     snames = "Cui_S-24, Gong_S-24, Peng_S-24, Gong_S-4, Peng_S-4", 
     box = F, ellipse = T, ilcs = 1, sncs = 1.2)

text(5, 2, "Sensitive", adj = c(0,0))
dev.off()


## chen vs li ###
levels(as.factor(df$Art))

png(filename="../figures/New_figures/Venn_Raiz.png", res = 300, height = 1500, width = 1500)

venn(x = list(df %>% filter(Art == "Chen et al., 2016") %>% filter(Tejido == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Art == "Li et al., 2015") %>% filter(Tejido == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c("#57d193",
                              "#b371da"),
     snames = "Chen, Li", 
     box = F, ellipse = F, ilcs = 1, sncs = 1.2)

text(5, 2, "Root", adj = c(0,0))
dev.off()

### solo chen y li ####
df %>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(Tejido))%>% 
  filter(Art %in% c("Chen et al., 2016", "Li et al., 2015"))%>%
  filter(Tejido == "root")%>%
  ggplot(aes(y=presencia, x=Art, fill=`Categoria funcional`)) + 
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = col)+ theme_tufte()+
  labs(x="", y= "Percentage", title = "")+ 
  theme(axis.text.x = element_text(angle = 0))

ggsave("../figures/New_figures/Chen_Li.png", dpi = 300)


##### Figura 3 #######

# 3 Up y Down, siguiendo la misma estrategia que la figura 1 pero sin separar artículos, ordenar los colores por categorías grandes.

ggplot(df %>% filter(Art != "Chen et al., 2016")%>% filter(`Up/downb` != "NA") %>% filter(Tratamientos != "NA") %>% 
         filter(Tejido == "leaf") , 
       aes(fill=factor(`Categoria funcional`, levels=c("CELLULAR PROCESSES AND SIGNALING",
                                                       "Cell division, differentiation and fate",
                                                       "Wall structure, signaling and interactions",
                                                       "Membrane and transport",
                                                       "Cytoskeleton",
                                                       "Signaling",
                                                       "Protein folding and transport",
                                                       "Protein modification",
                                                       "Proteolysis",
                                                       "Stress and defense",
                                                       "INFORMATION STORAGE AND PROCESSING",
                                                       "Transcription related",
                                                       "DNA structure and dynamics",
                                                       "Translation and ribosome structure",
                                                       "METABOLISM",
                                                       "Energy production and conversion",
                                                       "Photosynthesis",
                                                       "Carbohydrate and energy metabolism",
                                                       "Other metabolism",
                                                       "POORLY CHARACTERIZED",
                                                       "Unknown",
                                                       "Miscellaneous")), y=presencia, x=Tiempo)) + 
  geom_bar(stat="identity", position = "fill") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = c(col),name = "")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Time")+ggtitle("Expresion en hojas tolerante y sensibles")+
  facet_grid(Tratamientos~`Up/downb`, scales = "free")

ggsave("../figures/New_figures/Fig_1.png", dpi = 300,height = 8, width = 10)

####### opcion 3.1 #######

df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>%
  filter(!is.na(`Categoria funcional`))

names(df)

ggplot(df %>% filter(Art != "Chen et al., 2016") %>% filter(`Up/downb` != "NA") %>%
         filter(Tratamientos != "NA") %>% 
         filter(Tejido == "leaf"), aes(x = Tiempo, y = presencia, 
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
  scale_fill_manual(values = col)+theme(legend.position = "top")+facet_wrap(~`Categoria funcional`)


#### opcion 2 #### 
names(df)

ggplot(df %>% filter(!is.na(`Up/downb`))%>% filter( `Up/downb`!= "NA") %>% 
         filter(Tejido == "leaf")%>% 
         filter(Tiempo != "1 week") , 
       aes(fill=`Up/downb`, y=presencia, x= reorder(Tiempo, desc(`Up/downb`)))) + 
  geom_bar(stat="identity") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = c(col),name = "")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Time")+ggtitle("Expresion en hojas tolerante y sensibles")+scale_x_discrete(limits=c("4h","24h"))+
  facet_grid(Tratamientos~`Categoria funcional`, scales = "free")

ggsave("../figures/New_figures/Grafica_que_flor_me_dijo_NO.png", height = 7, width = 11)



#### op. 3 BUENA  #####

names(df2)

col<-c("#57d193",
      "#b371da",
      "#7aca4f",
      "#e55485",
      "#c5b741",
      "#df703a")

df2<-df %>% group_by(Tratamientos, `Up/downb`, Tiempo, `Categoria funcional`) %>% 
  summarise(Total= sum(presencia)) %>% filter(`Up/downb` != "NA")%>%
  filter(!Tratamientos %in% c("Tolerante", "No Tolerante")) %>%
  filter(!is.na(Tiempo))%>% filter(!is.na(`Up/downb`))%>% 
  filter( `Up/downb`!= "NA") %>% filter(Tiempo != "1 week")

df2


df2$`Categoria funcional` = factor(df2$`Categoria funcional`, levels=c("CELLULAR PROCESSES AND SIGNALING",
                                      "Cell division, differentiation and fate",
                                      "Wall structure, signaling and interactions",
                                      "Membrane and transport",
                                      "Cytoskeleton",
                                      "Signaling",
                                      "Protein folding and transport",
                                      "Protein modification",
                                      "Proteolysis",
                                      "Stress and defense",
                                      "INFORMATION STORAGE AND PROCESSING",
                                      "Transcription related",
                                      "DNA structure and dynamics",
                                      "Translation and ribosome structure",
                                      "METABOLISM",
                                      "Energy production and conversion",
                                      "Photosynthesis",
                                      "Carbohydrate and energy metabolism",
                                      "Other metabolism",
                                      "POORLY CHARACTERIZED",
                                      "Unknown",
                                      "Miscellaneous"), labels=c("CELLULAR PROCESSES AND SIGNALING",
                                                                 "Cell division, differentiation and fate",
                                                                 "Wall structure, signaling and interactions",
                                                                 "Membrane and transport",
                                                                 "Cytoskeleton",
                                                                 "Signaling",
                                                                 "Protein folding and transport",
                                                                 "Protein modification",
                                                                 "Proteolysis",
                                                                 "Stress and defense",
                                                                 "INFORMATION STORAGE AND PROCESSING",
                                                                 "Transcription related",
                                                                 "DNA structure and dynamics",
                                                                 "Translation and ribosome structure",
                                                                 "METABOLISM",
                                                                 "Energy production and conversion",
                                                                 "Photosynthesis",
                                                                 "Carbohydrate and energy metabolism",
                                                                 "Other metabolism",
                                                                 "POORLY CHARACTERIZED",
                                                                 "Unknown",
                                                                 "Miscellaneous")) 

library(ggpubr)
ggplot(df2, aes(x = Tiempo, y = Total * ((-1)^(`Up/downb` == "down")), 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(Tiempo, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.5, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(Tiempo, -Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = -.5, size=4)+
  scale_fill_manual(values = col)+ theme_minimal()+theme(legend.position = "top")+scale_x_discrete(limits=c("4h","24h"))+
  facet_grid(Tratamientos~`Categoria funcional`, scales = "free")+theme_pubclean()+ 
  geom_hline(yintercept=0, color = "grey")


##### OP. 3 iNVERTIDA #####

df2$Tiempo <- factor(df2$Tiempo, levels=c('4h','24h'))


p1<-ggplot(df2, aes(x = `Categoria funcional`, y = Total * ((-1)^(`Up/downb` == "down")), 
                fill = `Up/downb`)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Num de proteinas")+ 
  geom_text(data = subset(df2, `Up/downb`  == "up"), 
            aes(`Categoria funcional`, Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = 1.5, size=4)+
  geom_text(data = subset(df2, `Up/downb`  == "down"), 
            aes(`Categoria funcional`, -Total, group=`Up/downb`, label=Total),
            position = position_dodge(width=0.9), vjust = -.5, size=4)+
  scale_fill_manual(values = c("#2171b5", "#6baed6"))+ theme_minimal()+theme(legend.position = "top")+
  facet_grid(Tratamientos~Tiempo, scales = "free")+theme_pubclean()+ 
  geom_hline(yintercept=0, color = "grey")+ theme(axis.text.x = element_text(angle = 0))+coord_flip()


p1






g <- ggplot_gtable(ggplot_build(p1))
stripr <- which(grepl('strip-r', g$layout$name))
fills <- c("#c7e9c0","#df65b0")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

grid.draw(g)


#############  fig 4 #########
library(ggpubr)
library(data.table)
library(vegan)
library(ade4)
library(adegraphics)
library(tidyverse)
library(iNEXT)
library(data.table)
library(vegan)
library(ade4)
library(adegraphics)

#### PCO Tiempo #### 
df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>%
  filter(!is.na(Art))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(`Up/downb`)) %>%
  reshape2::dcast(Art+Tratamientos+ `Up/downb` + Tiempo~ Accession,
                  value.var = "presencia", fun= sum) %>% 
  unite(Time_Trat, c(Tratamientos, Tiempo), sep = " ", remove = FALSE)%>%
  filter(Tiempo != "1 week")


View(df)
names(df)

dat_sp<-df[,7:length(df)] # spp
dat_env<-df[,1:6]

pcos1<-vegdist(dat_sp, method= "bray", binary=F)
pcos1
plot(hclust(pcos1,"ward.D2"))
PCOpol<-dudi.pco(sqrt(pcos1),scannf=F, nf=3) # sqrt para ponerlo en un espacio euclidiano, el cluster no lo crea en el espacio
scatter(PCOpol)# lo ubica en espacio euclidiano

df_ali<-cbind(PCOpol$li, dat_env$Time_Trat) %>% dplyr::rename(Time_Trat= "dat_env$Time_Trat")
df_ali

qplot(data = df_ali , x = A1, y = A2, colour = Time_Trat) + 
  stat_ellipse(geom = "polygon", alpha = .3, aes(fill = Time_Trat))+
  theme_minimal()+
  labs(x= "NMDS1", y = "NMDS2")+
  theme(
    legend.position = "top",
    plot.title = element_text(color="black", size=10))+ ggtitle("")+ 
  annotate("text", x = .8, y = .5, label = "p > 0.2")+
  scale_fill_manual(values = c('#d7191c','#e66101','#1a9641','#2b83ba'),name = "")+
  scale_color_manual(values = c('#d7191c','#e66101','#1a9641','#2b83ba'),name = "")

ggsave("../figures/New_figures/PCoA_Tiempo.png", width = 7, height = 5)

library(pairwiseAdonis)
pairwise.adonis(sqrt(pcos1),df_ali$Time_Trat, perm = 9999)


#### PCO TRATAmientos ####
df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>%
  filter(!is.na(Art))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(`Up/downb`)) %>%
  reshape2::dcast(Art+Tratamientos+ `Up/downb` + Tiempo~ Accession,
                  value.var = "presencia", fun= sum)

dat_sp<-df[,6:length(df)] # spp
dat_env<-df[,1:5]

pcos1<-vegdist(dat_sp, method= "bray", binary=F)
pcos1
plot(hclust(pcos1,"ward.D2"))
PCOpol<-dudi.pco(sqrt(pcos1),scannf=F, nf=3) # sqrt para ponerlo en un espacio euclidiano, el cluster no lo crea en el espacio
scatter(PCOpol)# lo ubica en espacio euclidiano

df_ali<-cbind(PCOpol$li, dat_env$Tratamientos) %>% dplyr::rename(Tratamientos= "dat_env$Tratamientos")
df_ali

qplot(data = df_ali %>% filter(Tratamientos != "1 week"), x = A1, y = A2, colour = Tratamientos) + 
  stat_ellipse(geom = "polygon", alpha = .3, aes(fill = Tratamientos))+
  theme_minimal()+scale_fill_brewer(palette="Dark2")+ scale_color_brewer(palette="Dark2")+
  labs(x= "NMDS1", y = "NMDS2")+
  theme(
    legend.position = "top",
    plot.title = element_text(color="black", size=10))+ ggtitle("")+ 
  annotate("text", x = .8, y = .3, label = "p > 0.5")

ggsave("../figures/New_figures/PCoA_Tratamientos.png", width = 7, height = 5)

library(pairwiseAdonis)
pairwise.adonis(sqrt(pcos1),df_ali$Tratamientos, perm = 9999)


#### PCO Articulos ####
df <- read_excel("../data/database_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>%
  filter(!is.na(Art))%>%
  filter(!is.na(Tratamientos))%>%
  filter(!is.na(`Categoria funcional`))%>%
  filter(!is.na(`Up/downb`)) %>%
  reshape2::dcast(Art+Tratamientos+ `Up/downb` + Tiempo~ Accession,
                  value.var = "presencia", fun= sum)

dat_sp<-df[,6:length(df)] # spp
dat_env<-df[,1:5]

pcos1<-vegdist(dat_sp, method= "bray", binary=F)
pcos1
plot(hclust(pcos1,"ward.D2"))
PCOpol<-dudi.pco(sqrt(pcos1),scannf=F, nf=3) # sqrt para ponerlo en un espacio euclidiano, el cluster no lo crea en el espacio
scatter(PCOpol)# lo ubica en espacio euclidiano

df_ali<-cbind(PCOpol$li, dat_env$Art) %>% dplyr::rename(Art= "dat_env$Art")
df_ali

qplot(data = df_ali %>% filter(Art != "1 week"), x = A1, y = A2, colour = Art) + 
  stat_ellipse(geom = "polygon", alpha = .3, aes(fill = Art))+
  theme_minimal()+scale_fill_brewer(palette="Dark2")+ scale_color_brewer(palette="Dark2")+
  labs(x= "NMDS1", y = "NMDS2")+
  theme(
    legend.position = "top",
    plot.title = element_text(color="black", size=10))+ ggtitle("")+ 
  annotate("text", x = .7, y = .6, label = "p > 0.5")

ggsave("../figures/New_figures/PCoA_Art.png", width = 7, height = 5)

library(pairwiseAdonis)
pairwise.adonis(sqrt(pcos1),df_ali$Art, perm = 9999)



