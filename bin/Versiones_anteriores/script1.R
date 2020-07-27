
head(df)

# 1) 
# Composition de la figura 1 
# root = li and chen  a) 
# leaf= todos menos chen b) 
# todas c) 

# 2) rastrear su numero y su funcion
# se comportan igual 


# como se agrupan las compartidas por categoria funcional por tejido # 

df<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")%>%filter(Tejido=="leaf")

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
dfa

venn.diagram(
  x = list(
    dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Li et al., 2015")%>% select(`Accession`) %>% unlist() , 
    dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist()
  ),
  category.names = c("Chen" , "Gong", "Li", "Peng"),
  filename = '../figures/Ven_Art_leaf.png',
  output = TRUE ,
  imagetype="png",
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff',"blue", "red"),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha("blue",0.3), alpha("red",0.3)))


### extraer las palbras eclusivas y comportatidas por articulo 

algo<-VennDiagram::get.venn.partitions(list(chen= dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist(), 
                                            li=dfa %>% filter(Art== "Li et al., 2015")%>% select(`Accession`) %>% unlist()))


algo<-VennDiagram::get.venn.partitions(list(chen= dfa %>% filter(Art== "Chen et al., 2016")%>% select(`Accession`) %>% unlist() , 
                                            gong=dfa %>% filter(Art== "Gong et al., 2017")%>% select(`Accession`) %>% unlist(), 
                                            peng=dfa %>% filter(Art== "Peng et al., 2018")%>% select(`Accession`) %>% unlist()))



algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()
class(dfcos)

# Extraer las palbras exclusivas por articulo 

dfcos

dfcos %>% select(V1)%>%rename(Accession_compartidos_hoja= V1)%>%
  filter(Accession_compartidos_hoja != "NA")%>%
  write.csv("../data/accesiion_ambos/compartidos_hoja.csv")

############## end ####

df_tot<-read_excel("../data/database_proteomic_25022020.xlsx") %>% 
  filter(Species == "Gossypium hirsutum")



comp<-dfcos %>% select(V1)%>%rename(Accession= V1)%>%
  filter(Accession != "NA")

comp
names(df)

summary(as.factor(df_tot$Accession))

head(dfa)

comp<-df %>% filter(Accession %in% c("159895667", "211906474", "224059690", "224128476"))
comp
  
