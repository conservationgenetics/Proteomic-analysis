# Analisis 1 : con numero de Accession #

Cuales son redundantes (compartidad entre trabajos) ?
Cuales son No_redundantes (unicas en los trabajos)


Las redundantes se comportan igual entre trabajos?
    Si suben o bajan
    En hoja y en raiz 
    
# Analsis 2: FUNCION (COG)
    
Exclusivas 
Redundantes 


# analisis 1 # 
library(tidyverse)
library(readxl)
library(VennDiagram)

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

dfa<-df %>% select(Art,Accession)
names(dfa)
dfa<-distinct(dfa[,])
names(dfa)

venn.diagram(
  x = list(
    dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Li et al., 2015")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist()
  ),
  category.names = c("Chen" , "Gong", "Li", "Peng"),
  filename = '../figures/Ven_Art.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff',"blue", "red"),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha("blue",0.3), alpha("red",0.3)))


### extraer las palbras eclusivas y comportatidas por articulo 
algo<-VennDiagram::get.venn.partitions(list(chen= dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist() , 
                                            gong=dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist() , 
                                            li=dfa %>% filter(Art== "Li et al., 2015")%>% select(`Accession`) %>% unlist() , 
                                            peng=dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist()))


#algo$..values..[14]
#algo$..count..[14]

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por articulo 

dfcos

dfcos %>% select(V15)%>%rename(Accesion_chen= V15)%>%
  write.csv("../data/accession_art/exclusivos_chen.csv")

dfcos %>% select(V12)%>%rename(Accesion_li= V12)%>%
  write.csv("../data/accession_art/exclusivos_li.csv")

dfcos %>% select(V8)%>%rename(Accession_peng= V8)%>% filter(Accession_peng != "NA")%>%
  write.csv("../data/accession_art/exclusivos_peng.csv")

dfcos %>% select(V14)%>%rename(Accession_gong= V14)%>% filter(Accession_gong != "NA")%>%
  write.csv("../data/accession_art/exclusivos_gong.csv")

dfcos %>% select(-c(V15, V12, V8, V14))%>%
  gather(columna, Accession, 1:11, na.rm = T)%>% arrange(desc(Accession))%>%
  write.csv("../data/accession_art/compartidos.csv")

#### tabla resumen de proteinas acompartidas y únicas ####
tablares<-dfa %>% group_by(Art) %>% 
  summarise(Num_Proteinas_reportadas= length(unique(Accession)))%>%
  mutate(Unicas= c(956,33, 0, 85), Compartidas= c(124,53,149,78))

tablares

write.csv(tablares, "../data/tablas_resumen/tabla_resumen_art.csv")


############# elementos para el plot # 
library(extrafont)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readxl)
library(tidyverse)

theme <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               axis.text.y = element_blank(),
               axis.title.y = element_blank(),
               plot.title = element_text(size = 10, hjust = 0.5))

# colors # 
mycolors<-c("#32937c",
            "#5362a6",
            "#c45ca2",
            "#cb5a4c")

tablares

positions<-c("Gong et al., 2017",
             "Li et al., 2015",
             "Peng et al., 2018",
             "Chen et al., 2016")

Total<-ggplot(tablares, aes(fill=Art, y=Num_Proteinas_reportadas, x=Art)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(text=element_text(size=10,  family="Times New Roman", hjust = .5))+
  guides(fill=guide_legend(title=""))+ xlab("")+ scale_fill_manual(values=mycolors)+
  theme_pander(base_family = "Times New Roman", base_size = 16)+
  labs(title = "b)", y= "Total") +
  theme(plot.title = element_text(hjust=0.5))+ 
  theme(axis.text.y = element_text(angle = 0, hjust = -.04))+ 
  theme(legend.position = "none")+ scale_x_discrete(limits = positions)

Total

mycolors<-c("#b4943e", "#777acd")

tablares

names(tablares)

tablares1<-tablares %>% gather(categoria, valor, 3:4)

#
tablares1

Compartidos<-ggplot(tablares1,aes(fill=categoria, y=valor, x=Art)) + 
  geom_bar( stat="identity")+coord_flip()+theme+
  theme(text=element_text(size=10,  family="Times New Roman"))+
  guides(fill=guide_legend(title=""))+theme_pander(base_family = "Times New Roman", base_size = 16) + 
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(trans = 'reverse')+xlab("") + 
  theme(legend.position = c(0.3, 0.5),axis.text.y = element_blank(), axis.title.y = element_blank())+ 
  labs(title = "a)", y = "Total")+ 
  theme(plot.title = element_text(hjust=0.5))+ scale_x_discrete(limits = positions)

Compartidos

# get ggplot grob
gtM <- ggplotGrob(Total)
# get ggplot grob
gtF <- ggplotGrob(Compartidos)
#### Arrange the components
## First, combine "female" and "male" plots
gt = cbind(gtF, gtM, size = NULL)

# save figure a-b
png("../figures/Unicas_Compartidas.png",width = 3500, height = 1400, res=300)

# 2. Create the plot
plot(gt)
# 3. Close the file
dev.off()


####################### ahora necesito recuperar la informació guardada en df, con los compartidos 
comp<-read.csv("../data/accession_art/compartidos.csv")

nrow(unique(comp))
dim(comp)

comp$Accession<-as.character(comp$Accession)
df$Accession<-as.character(df$Accession)

str(df)


dim(full_comp)
full_comp<-inner_join(comp, df, by = "Accession")

nrow(unique(full_comp$Accession))

### ya tengo las proteinas compartidas # 
names(full_comp)

full_comp<- full_comp %>% filter(`Up/downb` != "NA")

names(full_comp)

ggplot() + 
  geom_bar(aes(x = Accession, y= Presencia, fill = Tejido), data= full_comp,
           stat="identity", position = "fill")+
  labs(x="num accession", y = "tejidos")+ 
  scale_fill_manual(values = c("orange", "steelblue"))+ 
  theme(axis.text.x = element_text(angle = 0))+coord_flip()+
  facet_grid(~`Up/downb`,space = "free", margins = "free", scales = "free")
