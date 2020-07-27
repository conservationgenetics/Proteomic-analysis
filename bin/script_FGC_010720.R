
# script con base corroborada con flor. 
setwd("~/google-drive/history_AW/history_FGC/bin/")

###### RESUMEN DE BASE ####

############### cosas compartidad ###########
df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "root")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

#png(filename="../figures/Figura_1a.png", res = 300, height = 1300, width = 1300)

venn(x = list(df %>% filter(Treatment == "24h") %>% filter(Tissue == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              df %>% filter(Treatment == "1 week")%>% filter(Tissue == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e66101','#d7191c'),
     snames = "Raiz 24h, Raiz 1 week", 
     box = F, ellipse = F,  ilcs = .7, sncs = .7)

#dev.off()


levels(as.factor(df$Treatment))

library(venn)

algo<-VennDiagram::get.venn.partitions(list(d1=df %>% filter(Treatment == "24h") %>% filter(Tissue == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            d2=df %>% filter(Treatment == "1 week")%>% filter(Tissue == "root") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

#compartidos<-dfcos %>% dplyr::select(-c(8, 12, 14, 15 ))%>%gather(num, Accession, 1:length(.)) %>% filter(Accession !="NA")
compartidos<-dfcos %>% dplyr::select(c(1))%>%gather(num, Accession, 1:length(.)) %>% filter(Accession !="NA")


View(compartidos)

compartidos<- compartidos$Accession

unico<-df %>% filter(Accession %in% compartidos) %>% arrange(desc(Accession))
View(unico)

unique(unico$Accession)

names(unico)
library(data.table)

alg<-unico %>% unite("gen_traet",c(Genotype, Treatment), remove = FALSE) %>% reshape2::dcast(`Functional category`+ Accession+ Description+ Reference+ gen_traet ~`Up/downb`, value.var = "presencia")
View(alg)

alg1<-alg %>% mutate(presencia=1)%>% dcast(`Functional category`+Accession+ Description+Reference+down+up~gen_traet, value.var = "presencia") 
View(alg1)

#alg2<- alg1 %>% mutate(presencia=1)%>% dcast(`Functional category`+Accession+ Description+down+up+Sensitive_4h+Sensitive_24h+Tolerant_4h+Tolerant_24h+`Tolerant_1 week`~Reference, value.var = "presencia") 
View(alg2)

alg2<- alg1 %>% mutate(presencia=1)%>% dcast(`Functional category`+Accession+ Description+down+up+`Tolerant_1 week`+Tolerant_24h~Reference, value.var = "presencia") 

View(alg2)

library("writexl")

write_xlsx(alg2,"../compartidas_aristas_ven_raiz.xlsx")

















###### compartidos 4h sensible-tolerante #####
algo<-VennDiagram::get.venn.partitions(list(S4=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            #S24=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            T4=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
                                            #T24=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
                                            ))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")#%>%
compartidos<- compartidos$Accession

unico<-df %>% filter(Accession %in% compartidos) %>% arrange(desc(Accession))
unico
unique(unico$Accession)

library(data.table)
alg<-unico %>% reshape2::dcast(Accession+ Description+ Reference+ Genotype ~`Up/downb`, value.var = "presencia")
View(alg)

alg1<-alg %>% mutate(presencia=1)%>% dcast(Accession+ Description+Reference+down+up~Genotype, value.var = "presencia") 

alg2<- alg1 %>% mutate(presencia=1)%>% dcast(Accession+ Description+down+up+Sensitive+Tolerant~Reference, value.var = "presencia") 
View(alg2)
unique(alg2$Accession)

write.csv(alg2, "../compartidos_4h_ST.csv")


###### compartidos 24h sensible-tolerante #####

venn(x = list(S24=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
              T24=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()),
     ilabels = F, zcolor =  c('#e66101','#d7191c', '#2b83ba','#1a9641'),
     snames = "Sensitive 24h, Sensitive 24h", 
     box = F, ellipse = F,  ilcs = .7, sncs = .7)

algo<-VennDiagram::get.venn.partitions(list(S4=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            #S24=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            T4=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
                                            #T24=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")#%>%
compartidos<- compartidos$Accession

unico<-df %>% filter(Accession %in% compartidos) %>% arrange(desc(Accession))
unico
unique(unico$Accession)

library(data.table)
alg<-unico %>% reshape2::dcast(Accession+ Description+ Reference+ Genotype ~`Up/downb`, value.var = "presencia")
View(alg)

alg1<-alg %>% mutate(presencia=1)%>% dcast(Accession+ Description+Reference+down+up~Genotype, value.var = "presencia") 

alg2<- alg1 %>% mutate(presencia=1)%>% dcast(Accession+ Description+down+up+Sensitive+Tolerant~Reference, value.var = "presencia") 
View(alg2)
unique(alg2$Accession)

write.csv(alg2, "../compartidos_24h_ST.csv")


###### compartidos 4 y 24h de sensibles #####
algo<-VennDiagram::get.venn.partitions(list(S4=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            #S24=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            T4=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
                                            #T24=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")#%>%
compartidos<- compartidos$Accession

unico<-df %>% filter(Accession %in% compartidos) %>% arrange(desc(Accession))
unico
unique(unico$Accession)

library(data.table)
alg<-unico %>% reshape2::dcast(Accession+ Description+ Reference+ Genotype ~`Up/downb`, value.var = "presencia")
View(alg)

alg1<-alg %>% mutate(presencia=1)%>% dcast(Accession+ Description+Reference+down+up~Genotype, value.var = "presencia") 

alg2<- alg1 %>% mutate(presencia=1)%>% dcast(Accession+ Description+down+up+Sensitive+Tolerant~Reference, value.var = "presencia") 
View(alg2)
unique(alg2$Accession)

write.csv(alg2, "../compartidos_24h_4h_Sensibles.csv")

###### comp 4 y 24h tolerantes #####
algo<-VennDiagram::get.venn.partitions(list(S4=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            #S24=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            T4=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "4h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
                                            #T24=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")#%>%
compartidos<- compartidos$Accession

unico<-df %>% filter(Accession %in% compartidos) %>% arrange(desc(Accession))
unico
unique(unico$Accession)

library(data.table)
alg<-unico %>% reshape2::dcast(Accession+ Description+ Reference+ Genotype ~`Up/downb`, value.var = "presencia")
View(alg)

alg1<-alg %>% mutate(presencia=1)%>% dcast(Accession+ Description+Reference+down+up~Genotype, value.var = "presencia") 

alg2<- alg1 %>% mutate(presencia=1)%>% dcast(Accession+ Description+down+up+Sensitive+Tolerant~Reference, value.var = "presencia") 
View(alg2)
unique(alg2$Accession)

write.csv(alg2, "../compartidos_24h_4h_Tolerantes.csv")


######### ver todos si suben o bajan #####
df <- read_excel("../data/db_proteomic_EDITADA_FGC.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))


names(df)
View(df)

View(df %>% filter(Accession == "115482792"))

library(data.table)
alg<-df %>% unite("gen_traet",c(Genotype, Treatment), remove = FALSE) %>% reshape2::dcast(`Functional category`+ Accession+ Description+ Reference+ gen_traet ~`Up/downb`, value.var = "presencia")
View(alg)

alg1<-alg %>% mutate(presencia=1)%>% dcast(`Functional category`+Accession+ Description+Reference+down+up~gen_traet, value.var = "presencia") 
View(alg1)

alg2<- alg1 %>% mutate(presencia=1)%>% dcast(`Functional category`+Accession+ Description+down+up+Sensitive_4h+Sensitive_24h+Tolerant_4h+Tolerant_24h+`Tolerant_1 week`~Reference, value.var = "presencia") 
View(alg2)

library("writexl")

write_xlsx(alg2,"../congruencia_1.xlsx")

df <- read_excel("../congruencia_1.xlsx")

comp8<- df %>% filter(Accession %in% c(
  "224058900",
  "147770841",
  "169635157",
  "298205241",
  "302122828",
  "159895667",
  "225469185",
  "255571642")) %>% arrange(`Functional category`) %>% mutate(Shared= "Shared between all")

write_xlsx(comp8,"../compartidos_8.xlsx")

compT_4h_24h<- df %>% filter(Accession %in% c(
  "1345674",
  "195626496",
  "359479172",
  "117988",
  "151368158",
  "220684439",
  "224030379",
  "224058900",
  "253759525",
  "298205241",
  "302782818",
  "169635157",
  "298205241",
  "49333381",
  "302122828",
  "147770841",
  "147819973",
  "289524744",
  "357473095",
  "147770018",
  "255571642",
  "225469185",
  "159895667",
  "224054560")) %>% arrange(`Functional category`) %>% mutate(Shared= "Shared 4h y 24h Tolerant")

write_xlsx(compT_4h_24h,"../compT_4h_24h.xlsx")

unique(compT_4h_24h$Accession)

######### ANALISIS CUADRO #####

df<-read_excel("../cuadro_FGC.xlsx") %>% mutate_(pres=1) %>%
  gather(Tratamiento, valor, 4:10)%>% filter(valor != "NA")

View(df)

df1<-df %>%
  mutate(valor=replace(valor, valor=="↓↑", "both")) %>%
  as.data.frame()%>%
  mutate(valor=replace(valor, valor=="↑", "up")) %>%
  as.data.frame()%>%
  mutate(valor=replace(valor, valor=="↓", "down")) %>%
  as.data.frame()

View(df1 %>% dcast(`Functional category`+Tratamiento~ valor, value.var= "Total"))


names(df1)
library(data.table)

df1<-df1 %>% group_by(`Functional category`, Tratamiento, valor) %>% summarise(Total=sum(pres)) %>% 
  arrange(desc(Tratamiento))

View(df1)

df2<-df1 %>% reshape2::dcast(Tratamiento ~ valor, value.var = "Total") %>% select(-2)

df2

chisq.test(as.matrix(df2))

#### cuadro resumen  #####
library(readxl)

df <- read_excel("../data/db_proteomic_FGC_160720.xlsx")%>%
  separate(Accession, c("A","Accession", "C", "D", "E", "F"), sep = "([\\|\\|\\|])")%>%
  filter(Description!= "RECORD REMOVED")%>% 
  mutate(presencia= rep(1, nrow(.))) %>% 
  #filter(Tissue == "leaf")%>% 
  filter(Treatment != "NA")%>%
  filter(!is.na(Treatment))

View(df)


dim(df)

length(unique(df$Accession))

# 1 # 
df %>% filter(Genotype == "Sensitive") %>% 
  filter(Treatment == "4h")%>%
  filter(Tissue == "leaf") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Sensitive") %>% 
         filter(Treatment == "4h")%>%
         filter(Tissue == "leaf") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))

# 2 # 

df %>% filter(Genotype == "Tolerant") %>% 
  filter(Treatment == "4h")%>%
  filter(Tissue == "leaf") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Tolerant") %>% 
         filter(Treatment == "4h")%>%
         filter(Tissue == "leaf") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))


# 3 # 
df %>% filter(Genotype == "Sensitive") %>% 
  filter(Treatment == "24h")%>%
  filter(Tissue == "leaf") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Sensitive") %>% 
         filter(Treatment == "24h")%>%
         filter(Tissue == "leaf") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))

# 4 # 

df %>% filter(Genotype == "Tolerant") %>% 
  filter(Treatment == "24h")%>%
  filter(Tissue == "leaf") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Tolerant") %>% 
         filter(Treatment == "24h")%>%
         filter(Tissue == "leaf") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))

# 5 # 

df %>% filter(Genotype == "Tolerant") %>% 
  filter(Treatment == "1 week")%>%
  filter(Tissue == "leaf") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Tolerant") %>% 
         filter(Treatment == "1 week")%>%
         filter(Tissue == "leaf") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))


# 6 # 

df %>% filter(Genotype == "Tolerant") %>% 
  filter(Treatment == "24h")%>%
  filter(Tissue == "root") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Tolerant") %>% 
         filter(Treatment == "24h")%>%
         filter(Tissue == "root") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))

# 7 # 

df %>% filter(Genotype == "Tolerant") %>% 
  filter(Treatment == "1 week")%>%
  filter(Tissue == "root") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(Genotype == "Tolerant") %>% 
         filter(Treatment == "1 week")%>%
         filter(Tissue == "root") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))

# 8 # 
names(df)
levels(as.factor(df$`Up/downb`))

df %>% filter(`Up/downb` == "up") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(`Up/downb` == "up") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))

# 9 # 
names(df)
levels(as.factor(df$`Up/downb`))

df %>% filter(`Up/downb` == "down") %>% 
  dplyr::select(Accession) %>% 
  filter(!is.na(Accession))

unique(df %>% filter(`Up/downb` == "down") %>% 
         dplyr::select(Accession) %>% 
         filter(!is.na(Accession)))


# 10 # # resumen: categoría funcional en hojas por patrón de expresión

df %>% filter(`Up/downb` != "NA") %>% filter(Tissue == "leaf") %>% 
  reshape2::dcast(`Functional category`~
                    `Up/downb`, value.var = "presencia", fun= sum)

# 11 # # resumen: categoría funcional en hojas por patrón de expresión

df %>% filter(`Up/downb` != "NA") %>% filter(Tissue == "root") %>% 
  reshape2::dcast(`Functional category`~
                    `Up/downb`, value.var = "presencia", fun= sum)


# 11 # # resumen: categoría funcional en hojas por patrón de expresión

df %>% filter(`Up/downb` != "NA") %>% filter(Tissue == "root") %>% 
  reshape2::dcast(`Functional category`~
                    `Up/downb`, value.var = "presencia", fun= sum)



# 12 # cuadro con pocentajes
df1<-df  %>% filter(Tissue == "leaf") %>% filter(`Functional category` != "NA")%>%
  reshape2::dcast(`Functional category` ~ Treatment, value.var = "presencia", fun= sum)

df1

sum(df1$`4h`)


# 13 # cuadro con pocentajes
df1<-df  %>% filter(Tissue == "root") %>% filter(`Functional category` != "NA")%>%
  reshape2::dcast(`Functional category` ~ Treatment, value.var = "presencia", fun= sum)

df1

sum(df1$`4h`)

###### compartidos  #####

algo<-VennDiagram::get.venn.partitions(list(S4=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            #S24=df %>% filter(Genotype == "Sensitive") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist(),
                                            T4=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h") %>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
                                            #T24=df %>% filter(Genotype == "Tolerant") %>% filter(Treatment == "24h")%>% filter(Tissue == "leaf") %>% dplyr::select(Accession) %>% filter(!is.na(Accession)) %>% unlist()
))
algo$..values..
algo$..count..

library(stringi)
dfcos<-t(stri_list2matrix(algo$..values.., byrow=TRUE)) %>% as.data.frame()

# Extraer las palbras exclusivas por alimentacion 
names(dfcos)

compartidos<-dfcos %>% dplyr::select(V1)%>% dplyr::rename(Accession= V1)%>% #compartidos sentibles y tolerante 
  filter(Accession != "NA")#%>%
compartidos<- compartidos$Accession









