
# script con base corroborada con flor. 
setwd("~/google-drive/history_AW/history_FGC/bin/")

library(tidyverse)
library(readxl)


##### HOJAS  ######## 
df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

##### Fig 1 #####
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
       "#d0746d")

df$`Functional category` = factor(df$`Functional category`, levels=c("CELLULAR PROCESSES AND SIGNALING",
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


df$Treatment <- factor(df$Treatment, levels=c('4h','24h'))

ggplot(df %>% 
         filter(Tissue == "leaf")%>%
         filter(!is.na(`Functional category`)) %>% 
         filter(Treatment %in% c('4h','24h')), 
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
  labs(y= "Percentage", x = "Treatment")+ggtitle("Expression in tolerant and sensitive leaves")+
  facet_wrap(~Treatment, scales = "free")+
  scale_y_continuous(labels = function(x) paste0(x*100,""))

ggsave("../figures/Fig_1.png", dpi = 300, height = 6, width = 7)


### Fig 1 bis (con week) ####

df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

levels(as.factor(df$Treatment))
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
  scale_fill_manual(values = c(col),name = "")+
  theme(strip.text.x = element_text(size=8, angle=0),
        strip.background = element_rect(colour="#bdbdbd", fill="#bdbdbd", linetype="solid"),
        panel.grid.major.y = element_line(colour = "grey70", linetype="dashed",size=0.5))+
  labs(y= "Percentage", x = "Treatment")+ggtitle("Expression in tolerant and sensitive leaves")+
  facet_wrap(~Treatment, scales = "free")+
  scale_y_continuous(labels = function(x) paste0(x*100,""))

ggsave("../figures/Figura_1.png", dpi = 300, height = 6, width = 8)


#### Fig 2 #####
library(venn)

df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))


png(filename="../figures/Fig_2a.png", res = 300, height = 900, width = 1000)

venn(x = list(df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e66101','#d7191c', '#2b83ba','#1a9641'),
     snames = "Sensitive 4h, Sensitive 24h, Tolerant 4h, Tolerant 24h", 
     box = F, ellipse = T,  ilcs = .7, sncs = .7)

dev.off()


### Fig2b #####
algo<-VennDiagram::get.venn.partitions(list(Sensi4h= df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            Sensi24h= df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            Tole4h=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            Tole24h=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()))

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
dfcos


compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")

compartidos  

## compartidos
comp<-df %>% filter(Accession %in% c("147770841", "159895667", "169635157", "225469185",
                                     "255571642", "298205241", "302122828","224058900"))%>%
  group_by(`Functional category`) %>% summarise(Total= sum(presencia)) %>% arrange(desc(Total))

rotate_x <- function(data, column_to_plot, labels_vec, rot_angle) {
  plt <- barplot(data[[column_to_plot]], col='steelblue', xaxt="n")
  text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=.9) 
}

png(filename="../figures/Fig_2b.png", res = 300, height = 1100, width = 1000)
rotate_x(comp, "Total", comp$`Functional category`, 45)
dev.off()

##### plot #####

png(filename="../figures/Fig_2.png", res = 300, height = 1100, width = 1700)
par(mfrow=c(1,2)) 

rotate_x(comp, "Total", comp$`Functional category`, 45)

venn(x = list(df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e66101','#d7191c', '#2b83ba','#1a9641'),
     snames = "Sensitive 4h, Sensitive 24h, Tolerant 4h, Tolerant 24h", 
     box = F, ellipse = T,  ilcs = .7, sncs = .7)

dev.off()


################################# Fig 3a #####
df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
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


names(df)

library(ggthemes)

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
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(text=element_text(size=12,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_hc(base_family = "Times New Roman", base_size = 12) + 
  scale_fill_manual(values = c("#bababa", "#01665e"))+
  scale_x_discrete(limits = positions1) +
  scale_y_continuous(trans = 'reverse')+xlab("") + 
  theme(legend.position = "none",axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Sensitive 4h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))
Eve

# get ggplot grob
gtM <- ggplotGrob(PN)
# get ggplot grob
gtF <- ggplotGrob(Eve)
plot(gtF)
#### Arrange the components
## First, combine "female" and "male" plots
gt <- cbind(gtF, gtM, size = "first")

png(filename="../figures/Fig_3a.png",width = 2500, height = 1300, res = 300)
plot(gt)
dev.off()

######### Fig 3b ########

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
PN


Eve<-ggplot(df %>% filter(Genotype != "NA") %>% 
              filter(Tissue == "leaf")%>% 
              filter(Treatment == "4h")%>% 
              filter(`Up/downb` != "NA")%>% 
              filter(Genotype == "Tolerant"), aes(fill=`Up/downb`, y=presencia, x=`Functional category`)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(text=element_text(size=12,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_hc(base_family = "Times New Roman", base_size = 12) + 
  scale_fill_manual(values = c("#bababa", "#01665e"))+
  scale_x_discrete(limits = positions1) +
  scale_y_continuous(trans = 'reverse')+xlab("") + 
  theme(legend.position = "none",axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(y = "Protein number")+ theme(plot.title = element_text(hjust=0.5))+
  ggtitle("Tolerant 4h")+ 
  theme(plot.title = element_text(size = 14, face = "bold"))
Eve

# get ggplot grob
gtM <- ggplotGrob(PN)
# get ggplot grob
gtF <- ggplotGrob(Eve)
plot(gtF)
#### Arrange the components
## First, combine "female" and "male" plots
gt1<-cbind(gtF, gtM, size = "first")

png(filename="../figures/Fig_3b.png",width = 2500, height = 1300, res = 300)
plot(gt1)
dev.off()

##### Fig 4 #####

df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))%>%
  reshape2::dcast(Reference+Genotype+ `Up/downb` + Treatment~ Accession,
                  value.var = "presencia", fun= sum) %>% 
  unite(Time_Trat, c(Genotype, Treatment), sep = " ", remove = FALSE)%>%
  filter(Treatment != "1 week")

names(df)
dat_sp<-df[,6:length(df)] # spp
dat_env<-df[,1:5]

library(vegan)
library(adegraphics)
library(ade4)

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
    plot.title = element_text(color="black", size=10))+ ggtitle("")+ 
  annotate("text", x = .8, y = .5, label = "p > 0.2")+
  scale_fill_manual(values = c('#d7191c','#e66101','#1a9641','#2b83ba'),name = "")+
  scale_color_manual(values = c('#d7191c','#e66101','#1a9641','#2b83ba'),name = "")+ 
  guides(fill = guide_legend(reverse=TRUE), color =guide_legend(reverse=TRUE) )+
  ggtitle("")

ggsave("../figures/Fig_4.png", dpi = 300)


########### RAICES ########

df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>% filter(Description!= "RECORD REMOVED") %>% 
  filter(Tissue == "root")

###### Fig 5a #####
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

ggsave("../figures/Fig_5_barras_roots.png", dpi = 300, width = 6, height = 5)

##### Fig 5b #####

png(filename="../figures/Fig_5b_roots.png", res = 300, height = 900, width = 1000)

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


##### SPIDER CHART #####
source("../functions/coord_radar.R")

df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>% filter(Description!= "RECORD REMOVED") %>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))%>% 
  filter(`Functional category` != "NA")%>%
  filter(!is.na(`Functional category`))%>% 
  filter(Tissue != "NA")%>%
  filter(!is.na(Tissue))

df


### leaf ####

names(df)

df1<-df%>%
  reshape2::dcast(`Functional category`+Tissue + Genotype+ `Up/downb`~ Treatment, value.var = "presencia", fun= sum)%>%
  gather(Treatment, Total, 5:7) %>% filter(Tissue == "leaf") %>% filter(Treatment != "1 week")

df1

names(df1)

df1<- df1 %>% group_by(`Functional category`)%>%
  mutate(Percentage=Total/sum(Total)*100)

df1

df1$Treatment <- factor(df1$Treatment, levels=c('4h','24h', '1 week'))

df1$Tiempo <- factor(df1$Tiempo, levels=c('4h','24h'))


p1<-ggplot(df1 ,
       aes(x = `Functional category`,
           y = Percentage,
           fill= Treatment,
           group = Treatment)) +
  geom_polygon(alpha = 0.7) + 
  coord_radar()+
  #facet_wrap(Tissue~Tiempo)+
  facet_grid(Tissue~Treatment)+
  theme_bw()+
  labs(x="", y="Parameter percentage")+
  theme(legend.position = "top")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 12))+
  scale_x_discrete(label=abbreviate)+
  scale_color_manual(values = c('#e66101','#1a9641',"red")) +
  scale_fill_manual(values = c('#e66101','#1a9641',"red")) 

p1


### raiz ####

names(df)

df1<-df%>%
  reshape2::dcast(`Functional category`+Tissue + Genotype ~ Treatment, value.var = "presencia", fun= sum)%>%
  gather(Treatment, Total, 4:6) %>% filter(Tissue == "root") %>% filter(Treatment != "4h")

df1
  
names(df1)

df1<- df1 %>% group_by(`Functional category`)%>%
  mutate(Percentage=Total/sum(Total)*100)
df1

levels(as.factor(df1$Tiempo))

df1$Tiempo <- factor(df1$Tiempo, levels=c('24h', '1 week'))

#View(df1)


p2<-ggplot(df1 ,
           aes(x = `Functional category`,
               y = Percentage,
               fill= Genotype,
               group = Genotype)) +
  geom_polygon(alpha = 0.7) + 
  coord_radar()+
  #facet_wrap(Tissue~Tiempo)+
  facet_grid(Tissue~Treatment)+
  theme_bw()+
  labs(x="", y="Parameter percentage")+
  theme(legend.position = "top")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 12))+
  scale_x_discrete(label=abbreviate)+
  scale_color_manual(values = c('#1a9641')) +
  scale_fill_manual(values = c('#1a9641')) 


p2

library(ggpubr)
ggarrange(p1, p2, ncol = 1,common.legend = T,  align = "h", legend = "top", widths = c(1, 0.5))

ggsave("../figures/Fig_6_bis.png",width = 11, height = 8)



###### solo Tissues #####

source("../functions/coord_radar.R")

df <- read_excel("../data/database_proteomic_EDITADA_FGC_bis.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>% 
  mutate(presencia= rep(1, nrow(.)))%>% filter(Descripción!= "RECORD REMOVED")%>%
  separate(Tratamientos, c("Tratamientos","Tiempo"), sep = " ", remove = F) %>% 
  filter(Tiempo != "NA")%>%
  filter(!is.na(Tiempo))%>% 
  filter(`Functional category` != "NA")%>%
  filter(!is.na(`Functional category`))%>% 
  filter(Tissue != "NA")%>%
  filter(!is.na(Tissue))

df$Tiempo[df$Tiempo =="1"] <- "1 week"
df


df1<-df%>%
  reshape2::dcast(Tissue+ `Functional category`~ Tiempo, value.var = "presencia", fun= sum)%>%
  gather(Tiempo, Total, 3:5) 
df1

names(df1)

df1<- df1 %>% group_by(`Functional category`)%>%
  mutate(Percentage=Total/sum(Total)*100)

df1

df1$Tiempo <- factor(df1$Tiempo, levels=c('4h','24h', '1 week'))

df1$Tiempo <- factor(df1$Tiempo, levels=c('4h','24h'))


p1<-ggplot(df1 ,
           aes(x = `Functional category`,
               y = Percentage,
               fill= Tissue,
               group = Tissue)) +
  geom_polygon(alpha = 0.7) + 
  coord_radar()+
  #facet_wrap(Tissue~Tiempo)+
  facet_grid(~Tissue)+
  theme_bw()+
  labs(x="", y="Parameter percentage")+
  theme(legend.position = "top")+ 
  theme(axis.text.x = element_text(face = "bold",
                                   size = 12))+
  scale_x_discrete(label=abbreviate)+
  scale_color_manual(values = c('#e66101','#1a9641',"red")) +
  scale_fill_manual(values = c('#e66101','#1a9641',"red")) 

p1

ggsave("../figures/fig6_tejidos.png")
  









