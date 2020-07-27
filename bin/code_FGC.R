#### Scripts used in the article: ####
# Title: 

# Authors: Florencia García-Campusano;Valeria Alavez1*; Javier Pérez-López1*; Denise Arroyo-Lambaer1; Ana Wegier1**

# 1 Laboratorio de Genética de la Conservación, Jardín Botánico, Instituto de Biología, Universidad Nacional Autónoma de México, Ciudad de México, México.
# * Posgrado en Ciencias Biológicas, Instituto de Biología, Universidad Nacional Autónoma de México, Ciudad de México, México.
# ** Correspondence: awegier@ib.unam.mx

# script con base corroborada con flor. 
#setwd("~/google-drive/history_AW/history_FGC/bin/")

library(tidyverse)
library(readxl)
library(venn)
library(ggthemes)


##### LEAF  ######## 
col<-c("#58b745",
       "#825eca",
       "#abb834",
       "#cb57b3",
       "#6cad59",
       "#d34360",
       "#53bb94",
       "#c7532b",
       "#45aecf",
       "#d79b39",
       "#7782ca",
       "#a0a152",
       "#ba668c",
       "#42763c",
       "#d68669",
       "#87682a")

#### Figure 1a #####
df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

df$Treatment <- factor(df$Treatment, levels=c('4h','24h', '1 week'))

ggplot(df %>% 
         filter(Tissue == "leaf")%>%
         filter(!is.na(`Functional category`)), 
       aes(fill=factor(`Functional category`, levels=c("CELLULAR PROCESSES AND SIGNALING",
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
                                                       "Miscellaneous")), y=presencia, x=Genotype)) + 
  geom_bar(stat="identity", position = "fill") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = col,name = "")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Treatment")+ggtitle("Expression in tolerant and sensitive leaves")+
  facet_wrap(~Treatment, scales = "free")+
  scale_y_continuous(labels = function(x) paste0(x*100,""))

ggsave("../figures/Figura_1.png", dpi = 300, height = 6, width = 8)


#### Figure 1a #####
df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

png(filename="../figures/Figura_1a.png", res = 300, height = 1300, width = 1300)
venn(x = list(df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e66101','#d7191c', '#2b83ba','#1a9641'),
     snames = "Sensitive 4h, Sensitive 24h, Tolerant 4h, Tolerant 24h", 
     box = F, ellipse = F,  ilcs = .7, sncs = .7)
dev.off()

##### Figure 2a #####
df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

positions1 <- c(
  "Membrane and transport",
  "Signaling",
  "Protein folding and transport",
  "Protein modification",
  "Proteolysis",
  "Stress and defense",
  "Transcription related",
  "DNA structure and dynamics",
  "Translation and ribosome structure",
  "Photosynthesis",
  "Carbohydrate and energy metabolism",
  "Other metabolism",
  "Unknown")

theme = theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 10, hjust = 0.5))

PN<-ggplot(df %>% filter(Genotype != "NA") %>% 
             filter(Tissue == "leaf")%>% 
             filter(Treatment == "24h")%>% 
             filter(`Up/downb` != "NA")%>% 
             filter(Genotype == "Sensitive") , aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(legend.position = c(0.1, 0.2), text=element_text(size=12,  family="Times New Roman", hjust = .5))+
  guides(fill=guide_legend(title=""))+ xlab("")+
  scale_x_discrete(limits = positions1)+
  scale_fill_manual(values=c("#bababa", "#01665e"))+
  theme_hc(base_family = "Times New Roman", base_size = 12) + 
  theme(plot.title = element_text(hjust=0.5))+ 
  theme(axis.text.y = element_text(angle = 0, hjust = -.04)) + 
  theme(legend.position = c(.8,.5))+ 
  theme(axis.text.y=element_text(size=12, angle=0,hjust=0.55,vjust=0.5))+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+ggtitle("Sensitive 24h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))

PN
Eve<-ggplot(df %>% filter(Genotype != "NA") %>% 
              filter(Tissue == "leaf")%>% 
              filter(Treatment == "4h")%>% 
              filter(`Up/downb` != "NA")%>% 
              filter(Genotype == "Sensitive"), aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+theme+
  theme(text=element_text(size=12,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_hc(base_family = "Times New Roman", base_size = 12) + 
  scale_fill_manual(values = c("#bababa", "#01665e"))+
  scale_x_discrete(limits = positions1)+ xlab("") + 
  theme(legend.position = "none",axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Sensitive 4h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))+coord_flip(ylim = c(30, 0) )+
  scale_y_continuous(trans = "reverse")


Eve

# get ggplot grob
gtM <- ggplotGrob(PN)
# get ggplot grob
gtF <- ggplotGrob(Eve)
plot(gtF)
#### Arrange the components
## First, combine "female" and "male" plots
gt <- cbind(gtF, gtM, size = "first")

png(filename="../figures/Fig_2a.png",width = 2500, height = 1300, res = 300)
plot(gt)
dev.off()

######### Figure 2b ########

positions1 <- c(
  "Wall structure, signaling and interactions",
  "Membrane and transport",
  "Signaling",
  "Protein folding and transport",
  "Protein modification",
  "Proteolysis",
  "Stress and defense",
  "Transcription related",
  "DNA structure and dynamics",
  "Translation and ribosome structure",
  "Photosynthesis",
  "Carbohydrate and energy metabolism",
  "Other metabolism",
  "Unknown")

PN<-ggplot(df %>% filter(Genotype != "NA") %>% 
             filter(Tissue == "leaf")%>% 
             filter(Treatment == "24h")%>% 
             filter(`Up/downb` != "NA")%>% 
             filter(Genotype == "Tolerant") , aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(legend.position = c(0.1, 0.2), text=element_text(size=12,  family="Times New Roman", hjust = .5))+
  guides(fill=guide_legend(title=""))+ xlab("")+
  scale_x_discrete(limits = positions1)+
  scale_fill_manual(values=c("#bababa", "#01665e"))+
  theme_hc(base_family = "Times New Roman", base_size = 12) + 
  theme(plot.title = element_text(hjust=0.5))+ 
  theme(axis.text.y = element_text(angle = 0, hjust = -.04)) + 
  theme(legend.position = c(.8,.5))+ 
  theme(axis.text.y=element_text(size=12, angle=0,hjust=0.55,vjust=0.5))+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+ggtitle("Tolerant 24h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))

Eve<-ggplot(df %>% filter(Genotype != "NA") %>% 
              filter(Tissue == "leaf")%>% 
              filter(Treatment == "4h")%>% 
              filter(`Up/downb` != "NA")%>% 
              filter(Genotype == "Tolerant"), aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+theme+
  theme(text=element_text(size=12,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_hc(base_family = "Times New Roman", base_size = 12) + 
  scale_fill_manual(values = c("#bababa", "#01665e"))+
  scale_x_discrete(limits = positions1)+xlab("") + 
  theme(legend.position = "none",axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Tolerant 4h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))+coord_flip(ylim = c(30, 0) )+
  scale_y_continuous(trans = "reverse")

# get ggplot grob
gtM <- ggplotGrob(PN)
# get ggplot grob
gtF <- ggplotGrob(Eve)
plot(gtF)
#### Arrange the components
## First, combine "female" and "male" plots
gt1<-cbind(gtF, gtM, size = "first")

png(filename="../figures/Fig_2b.png",width = 2500, height = 1300, res = 300)
plot(gt1)
dev.off()


########### ROOTS ########
df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>% filter(Description!= "RECORD REMOVED") %>% 
  filter(Tissue == "root")

###### Figure 3b #####
df$Treatment <- factor(df$Treatment, levels=c('24h','1 week'))


ggplot(df %>% filter(Treatment %in% c("24h", "1 week")), 
       aes(fill=factor(`Functional category`, levels=c("CELLULAR PROCESSES AND SIGNALING",
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
                                                       "Miscellaneous")), y=presencia, x=Genotype)) + 
  geom_bar(stat="identity", position = "fill") +
  xlab("")+theme_classic()+
  scale_fill_manual(values = c(col),name = "")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Treatment")+ggtitle("Expression in tolerant and sensitive roots")+
  facet_wrap(~Treatment, scales = "free")+
  scale_y_continuous(labels = function(x) paste0(x*100,""))

ggsave("../figures/Fig_3b.png", dpi = 300, width = 6, height = 5)

##### Figure 3a #####

png(filename="../figures/Fig_3a.png", res = 300, height = 900, width = 1000)

venn(x = list(
  df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "1 week") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
  df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
  ilabels = F, zcolor =  c('#2b83ba','#1a9641'),
  snames = "Tolerant 1 week, Tolerant 24h", 
  box = F, ellipse = F,  ilcs = .7, sncs = .7)

dev.off()


algo<-VennDiagram::get.venn.partitions(list(Tol_1wk=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "1 week") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            Tol_24= df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")#%>%
compartidos


##### Figure 4a #####
df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "root")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

levels(as.factor(df$`Functional category`))

positions1 <- c(
  "Membrane and transport",
  "Signaling",
  "Protein folding and transport",
  "Protein modification",
  "Proteolysis",
  "Stress and defense",
  "Transcription related",
  "DNA structure and dynamics",
  "Translation and ribosome structure",
  "Photosynthesis",
  "Carbohydrate and energy metabolism",
  "Other metabolism",
  "Unknown",
  "Cell division, differentiation and fate",
  "Wall structure, signaling and interactions")

theme = theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 10, hjust = 0.5))

PN<-ggplot(df %>% filter(Genotype != "NA") %>% 
             filter(Tissue == "root")%>% 
             filter(Treatment == "1 week")%>% 
             filter(`Up/downb` != "NA")%>% 
             filter(Genotype == "Tolerant") , aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(legend.position = c(0.1, 0.2), text=element_text(size=12,  family="Times New Roman", hjust = .5))+
  guides(fill=guide_legend(title=""))+ xlab("")+
  scale_x_discrete(limits = positions1)+
  scale_fill_manual(values=c("#bababa", "#01665e"))+
  theme_hc(base_family = "Times New Roman", base_size = 12) + 
  theme(plot.title = element_text(hjust=0.5))+ 
  theme(axis.text.y = element_text(angle = 0, hjust = -.04)) + 
  theme(legend.position = c(.8,.3))+ 
  theme(axis.text.y=element_text(size=12, angle=0,hjust=0.55,vjust=0.5))+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+ggtitle("Tolerant 1wk")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))+ylim(0,100)

PN

Eve<-ggplot(df %>% filter(Genotype != "NA") %>% 
              filter(Tissue == "root")%>% 
              filter(Treatment == "24h")%>% 
              filter(`Up/downb` != "NA")%>% 
              filter(Genotype == "Tolerant"), aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+theme+
  theme(text=element_text(size=12,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_hc(base_family = "Times New Roman", base_size = 12) + 
  scale_fill_manual(values = c("#bababa", "#01665e"))+
  scale_x_discrete(limits = positions1)+ xlab("") + 
  theme(legend.position = "none",axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Tolerant 24h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))+coord_flip(ylim = c(100, 0) )+
  scale_y_continuous(trans = "reverse")


Eve

# get ggplot grob
gtM <- ggplotGrob(PN)
# get ggplot grob
gtF <- ggplotGrob(Eve)
plot(gtF)
#### Arrange the components
## First, combine "female" and "male" plots
gt <- cbind(gtF, gtM, size = "first")

png(filename="../figures/Fig_4a.png",width = 2500, height = 1300, res = 300)
plot(gt)
dev.off()

