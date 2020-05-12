#Librerías -------------------------------------------------------------------------------------------
library(tidyverse)
library(plyr)
library(igraph)

library(knitr)
library(factoextra)
library(scales)
library(ggplot2)
library(cluster)
library(fpc)

#data -------------------------------------------------------------------------------------------
data <- read.csv('../data/covid10052020.csv', stringsAsFactors = F)
columns <- read.csv('../data/util2.csv', header = F)
data_entidad_um <- read.csv("../data/entidad_um.csv", header = F, stringsAsFactors = F)

cat_entidad <- read.csv("../data/cat_entidad.csv", stringsAsFactors = F)
cat_municipio <- read.csv("../data/cat_municipio.csv", stringsAsFactors = F)
cat_nacion <- read.csv("../data/cat_nacion.csv", stringsAsFactors = F)
cat_origen <- read.csv("../data/cat_origen.csv", stringsAsFactors = F)
cat_result <- read.csv("../data/cat_result.csv", stringsAsFactors = F)
cat_sector <- read.csv("../data/cat_sector.csv", stringsAsFactors = F)
cat_sexo <- read.csv("../data/cat_sexo.csv", stringsAsFactors = F)
cat_sino <- read.csv("../data/cat_sino.csv", stringsAsFactors = F)
cat_tp <- read.csv("../data/cat_tp.csv", stringsAsFactors = F)

data <- data[,c("ORIGEN", "SECTOR", "ENTIDAD_UM", "SEXO", "ENTIDAD_NAC", "ENTIDAD_RES", 
                "MUNICIPIO_RES", "TIPO_PACIENTE", "FECHA_SINTOMAS", "FECHA_DEF", "INTUBADO", "NEUMONIA", "EDAD",
                "NACIONALIDAD", "EMBARAZO", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", 
                "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO",
                "RESULTADO", "MIGRANTE", "PAIS_ORIGEN", "UCI")]

data <- data[data$ENTIDAD_RES==19,]
data <- data[data$RESULTADO==1,]


#Limpieza y tratamiento -------------------------------------------------------------------------------------------
set.seed(1)
matriz_ENFERMEDAD <- data[,c('NEUMONIA','DIABETES', 'EPOC', 'ASMA', 'INMUSUPR', 'HIPERTENSION',
                             'CARDIOVASCULAR', 'OBESIDAD', 'RENAL_CRONICA', 'TABAQUISMO')]
matriz <- as.matrix(matriz_ENFERMEDAD)
adyacency <- t(matriz) %*% matriz

#Factorización de origen
#sort(unique(data$ORIGEN))
#cat_origen$DESCRIPCIÓN
#cat_origen$CLAVE
origen <- as.factor(mapvalues(data$ORIGEN, from=cat_origen$CLAVE, to=cat_origen$DESCRIPCIÓN))

#Factorización de sector
#sort(unique(data$SECTOR))
cat_sector <- cat_sector[1:14,1:2]
#cat_sector$CLAVE
#cat_sector$DESCRIPCIÓN
sector <- as.factor(mapvalues(data$SECTOR, from=cat_sector$CLAVE, 
                              to=cat_sector$DESCRIPCIÓN))

#Factorización de entidad medica
#sort(unique(data$ENTIDAD_UM))
#cat_entidad$CLAVE_ENTIDAD
#cat_entidad$ENTIDAD_FEDERATIVA
#sort(unique(data$ENTIDAD_UM))

entidad_um <- as.factor(mapvalues(data$ENTIDAD_UM, from=cat_entidad$CLAVE_ENTIDAD, 
                                  to=cat_entidad$ENTIDAD_FEDERATIVA))

#Factorización de sexo
#sort(unique(data$SEXO))
#cat_sexo$CLAVE
#cat_sexo$DESCRIPCIÓN
sexo <- as.factor(mapvalues(data$SEXO, from=cat_sexo$CLAVE, 
                            to=cat_sexo$DESCRIPCIÓN))

#Factorización de entidad de residencia
#sort(unique(data$ENTIDAD_RES))
#cat_entidad$CLAVE_ENTIDAD
#cat_entidad$ENTIDAD_FEDERATIVA
entidad_res <- as.factor(mapvalues(data$ENTIDAD_RES, from=cat_entidad$CLAVE_ENTIDAD, 
                                   to=cat_entidad$ENTIDAD_FEDERATIVA))

#Factorización tipo de paciente
#sort(unique(data$TIPO_PACIENTE))
#cat_tp$CLAVE
#cat_tp$DESCRIPCIÓN
tipo_paciente <- as.factor(mapvalues(data$TIPO_PACIENTE, from=cat_tp$CLAVE,
                                     to=cat_tp$DESCRIPCIÓN))

#Factorización intubado
#sort(unique(data$INTUBADO))
#cat_sino$CLAVE
#cat_sino$DESCRIPCIÓN
intubado <- as.factor(mapvalues(data$INTUBADO, from=cat_sino$CLAVE, 
                                to=cat_sino$DESCRIPCIÓN))

#Factorización de neumonía
#sort(unique(data$NEUMONIA))

neumonia <- as.factor(mapvalues(data$NEUMONIA, from=cat_sino$CLAVE,
                                to=cat_sino$DESCRIPCIÓN))

#Categorías de edades
#sort(unique(data$EDAD))

edad <- cut(data$EDAD, breaks=c(-Inf,10,20,30,40,50,60,70,80,Inf),
            labels=c('0 - 10',
                     '11 - 20',
                     '21 - 30',
                     '31 - 40',
                     '41 - 50',
                     '51 - 60',
                     '61 - 70',
                     '71 - 80',
                     '81 en adelante'))

#Factorización de embarazo
#sort(unique(data$EMBARAZO))
embarazo_mujer <- data[data$SEXO==1,]
embarazo <- as.factor(mapvalues(embarazo_mujer$EMBARAZO, from=cat_sino$CLAVE,
                                to=cat_sino$DESCRIPCIÓN))

#Factorización de Diabetes
#sort(unique(data$DIABETES))
diabetes <- as.factor(mapvalues(data$DIABETES, from=cat_sino$CLAVE,
                                to=cat_sino$DESCRIPCIÓN))

#Factorización de EPOC
#sort(unique(data$EPOC))
epoc <- as.factor(mapvalues(data$EPOC, from=cat_sino$CLAVE,
                            to=cat_sino$DESCRIPCIÓN))

#Factorización de ASMA
#sort(unique(data$ASMA))
asma <- as.factor(mapvalues(data$ASMA, from=cat_sino$CLAVE,
                            to=cat_sino$DESCRIPCIÓN))

#Factorización de Inmunosupresión
#sort(unique(data$INMUSUPR))
inmuno <- as.factor(mapvalues(data$INMUSUPR, from=cat_sino$CLAVE,
                              to=cat_sino$DESCRIPCIÓN))

#Factorización de hipertensión
#sort(unique(data$HIPERTENSION))
hiper <- as.factor(mapvalues(data$HIPERTENSION, from=cat_sino$CLAVE,
                             to=cat_sino$DESCRIPCIÓN))

#Factorización de otras complicaciones
#sort(unique(data$OTRA_COM))
otras_comp <- as.factor(mapvalues(data$OTRA_COM, from=cat_sino$CLAVE,
                                  to=cat_sino$DESCRIPCIÓN))

#factorización de cardiovascular
#sort(unique(data$CARDIOVASCULAR))
cardio <- as.factor(mapvalues(data$CARDIOVASCULAR, from=cat_sino$CLAVE,
                              to=cat_sino$DESCRIPCIÓN))

#Factorización de Obesidad
#sort(unique(data$OBESIDAD))
obesidad <- as.factor(mapvalues(data$OBESIDAD, from=cat_sino$CLAVE,
                                to=cat_sino$DESCRIPCIÓN))

#Factorización de insuficiencia renal
#sort(unique(data$RENAL_CRONICA))
renal <- as.factor(mapvalues(data$RENAL_CRONICA, from=cat_sino$CLAVE,
                             to=cat_sino$DESCRIPCIÓN))

#Factorización de tabaquismo
#sort(unique(data$TABAQUISMO))
tabaquismo <- as.factor(mapvalues(data$TABAQUISMO, from=cat_sino$CLAVE,
                                  to=cat_sino$DESCRIPCIÓN))

#Factorización de otro caso
#sort(unique(data$OTRO_CASO))
otro_caso <- as.factor(mapvalues(data$OTRO_CASO, from=cat_sino$CLAVE,
                                 to=cat_sino$DESCRIPCIÓN))

#Factorización resultado
#sort(unique(data$RESULTADO))
cat_result <- cat_result[1:3,]
#cat_result$CLAVE
#cat_result$DESCRIPCIÓN
resultado <- as.factor(mapvalues(data$RESULTADO, from=cat_result$CLAVE,
                                 to=cat_result$DESCRIPCIÓN))

#Factorización de migrantes
#sort(unique(data$MIGRANTE))
migrante <- as.factor(mapvalues(data$MIGRANTE, from=cat_sino$CLAVE,
                                to=cat_sino$DESCRIPCIÓN))


#factorización de Nacionalidad
#sort(unique(data$NACIONALIDAD))
#cat_nacion$CLAVE
#cat_nacion$DESCRIPCIÓN
nacionalidad <- as.factor(mapvalues(data$NACIONALIDAD, from=cat_nacion$CLAVE,
                                    to=cat_nacion$DESCRIPCIÓN))

#Factorización de UCI
#sort(unique(data$UCI))
uci <- as.factor(mapvalues(data$UCI, from=cat_sino$CLAVE,
                           to=cat_sino$DESCRIPCIÓN))

#Factorización de municipio
cat_municipio <- cat_municipio[cat_municipio$CLAVE_ENTIDAD==19,]
municipio <- as.factor(mapvalues(data$MUNICIPIO_RES, from=cat_municipio$CLAVE_MUNICIPIO,
                                 to=cat_municipio$MUNICIPIO))

#Selección de mujeres
mujeres <- as.data.frame(sexo)
mujeres <- mujeres[mujeres$sexo=='FEMENINO',]

#Fecha de inicio de sintomas
sint_date <- as.data.frame(ftable(data$FECHA_SINTOMAS))
sint_date$Var1 <- as.Date(sint_date$Var1)
sint_date <- sint_date[sint_date$Var1 < "2020-05-5",]

#Fechas de defunción
defuncion <- data[data$FECHA_DEF!="9999-99-99",]
sexo_def1 = defuncion
sexo_def2 <- as.factor(mapvalues(sexo_def1$SEXO, from=cat_sexo$CLAVE, 
                                 to=cat_sexo$DESCRIPCIÓN))
edad_def <- cut(defuncion$EDAD, breaks=c(-Inf,10,20,30,40,50,60,70,80,Inf),
                labels=c('0 - 10',
                         '11 - 20',
                         '21 - 30',
                         '31 - 40',
                         '41 - 50',
                         '51 - 60',
                         '61 - 70',
                         '71 - 80',
                         '81 en adelante'))

def_date <- as.data.frame(ftable(defuncion$FECHA_DEF))
def_date$Var1 <- as.Date(def_date$Var1)
def_date <- def_date[def_date$Var1 < "2020-05-5",]

#Generación de gráficos -------------------------------------------------------------------------------

ggplot(as.data.frame(sexo)) +
  theme_bw()+
  geom_bar(aes(x=municipio), fill = "#619DFF",
           stat="count", position="dodge") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Estado",
       y="Casos positivos") +
  ggtitle("Presencia de Covid-19 en los diferentes estados de NL")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=sector),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=sector),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Institución del Sistema Nacional de Salud que brindó la atención")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=origen),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=origen),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo
       Unidad de salud monitora de enfermedades respiratorias (USMER)",
       y="Casos positivos",
       fill="") +
  ggtitle("Vigilancia centinela realizada")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=sexo),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=sexo),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Sexo del paciente")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=edad),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=edad),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Rango de edad del paciente")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=intubado),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=intubado),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que han requerido intubación")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=neumonia),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=neumonia),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron neumonía")

#mujeres <- as.data.frame(sexo)
#mujeres <- mujeres[mujeres$sexo=='FEMENINO',]
ggplot(as.data.frame(mujeres)) +
  theme_bw() +
  geom_bar(aes(x=mujeres, fill=embarazo),
           stat="count", position="dodge") +
  geom_text(aes(x=mujeres, label=..count.., group=embarazo),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron embarazo")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=diabetes),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=diabetes),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron diabetes")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=epoc),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=epoc),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo
       Enfermedad Pulmonar Obstructiva Crónica (EPOC)",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron EPOC")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=asma),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=asma),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron asma")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=inmuno),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=inmuno),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron inmunosupresión")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=hiper),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=hiper),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron hipertensión")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=otras_comp),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=otras_comp),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron otras enfermedades diagnosticadas")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=cardio),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=cardio),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron enfermedades cardiovasculares")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=obesidad),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=obesidad),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron obesidad")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=renal),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=renal),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que presentaron insuficiencia renal crónica")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=tabaquismo),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=tabaquismo),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes que tienen hábito de tabaquísmo")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=otro_caso),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=otro_caso),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Identifica si el paciente tuvo contacto con algún otro caso diagnosticado con SARS CoV-2")

ggplot(as.data.frame(sexo)) +
  theme_bw() +
  geom_bar(aes(x=sexo, fill=uci),
           stat="count", position="dodge") +
  geom_text(aes(x=sexo, label=..count.., group=uci),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="") +
  ggtitle("Identifica si el paciente requirió ingresar a una Unidad de Cuidados Intensivos")

#sint_date <- as.data.frame(ftable(data$FECHA_SINTOMAS))
#sint_date$Var1 <- as.Date(sint_date$Var1)

#sint_date <- sint_date[sint_date$Var1 < "2020-04-30",]

ggplot(sint_date, aes(Var1, Freq)) +
  geom_line(aes(color="Red"), size=1) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x="Fecha",
       y="Casos positivos",
       fill="") +
  ggtitle("Pacientes reportados positivos a COVID-19 por día")


#defuncion <- data[data$FECHA_DEF!="9999-99-99",]
#sexo_def1 = defuncion
#sexo_def2 <- as.factor(mapvalues(sexo_def1$SEXO, from=cat_sexo$CLAVE, 
#                            to=cat_sexo$DESCRIPCIÓN))
#edad_def <- cut(defuncion$EDAD, breaks=c(-Inf,10,20,30,40,50,60,70,80,Inf),
#            labels=c('0 - 10',
#                     '11 - 20',
#                     '21 - 30',
#                     '31 - 40',
#                     '41 - 50',
#                     '51 - 60',
#                     '61 - 70',
#                     '71 - 80',
#                     '81 en adelante'))


#def_date <- as.data.frame(ftable(defuncion$FECHA_DEF))
#def_date$Var1 <- as.Date(def_date$Var1)
#def_date <- def_date[def_date$Var1 < "2020-04-30",]

ggplot(def_date, aes(Var1, Freq)) +
  geom_line(aes(color="Red"), size=1) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x="Fecha",
       y="Muertes",
       fill="") +
  ggtitle("Muertes confirmadas por día")

ggplot(as.data.frame(sexo_def2)) +
  theme_bw() +
  geom_bar(aes(x=sexo_def2, fill=edad_def),
           stat="count") +
  geom_text(aes(x=sexo_def2, label=..count.., group=sexo_def2),
            stat='count',
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Sexo",
       y="Casos positivos",
       fill="Rango de edad") +
  ggtitle("Pacientes que han fallecido por rango de edad")

#generación de medoides
data.resc <- data[,c("ORIGEN", "SECTOR", "SEXO", "TIPO_PACIENTE", "INTUBADO", "NEUMONIA", "EDAD",
                     "EMBARAZO", "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM", 
                     "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA", "TABAQUISMO", "OTRO_CASO", "UCI")]

edad_rango <- cut(data.resc$EDAD, breaks=c(-Inf,10,20,30,40,50,60,70,80,Inf),
                  labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9))

data.resc$EDAD <- edad_rango

apply(data.resc, 2, unique)
data.resc <- as.data.frame(apply(data.resc, 2, as.numeric))
apply(data.resc, 2, unique)

set.seed(1)

submt <- kmeans(data.resc, centers=1)$withinss
for(i in 2:10) submt[i] <- kmeans(data.resc, centers=i)$withinss

plot(1:10, submt, type="b", xlab="Número de clusters", ylab="Diferencias entre elementos de cada grupo",
     main="Identificación de número óptimo de clusters")
abline(v = 5, col="blue", lty=2)

clarafit <- clara(data.resc, 5, samples = 1000)

fviz_cluster(clarafit, data=data, geom = "point",
             pointsize = 2, 
             main="Agrupamiento de pacientes por análisis de componentes principales") + theme_bw()

df <- as.data.frame(clarafit$medoids)

df$ORIGEN <- as.factor(mapvalues(df$ORIGEN, from=cat_origen$CLAVE, to=cat_origen$DESCRIPCIÓN))

df$SECTOR <- as.factor(mapvalues(df$SECTOR, from=cat_sector$CLAVE, 
                                 to=cat_sector$DESCRIPCIÓN))

df$SEXO <- as.factor(mapvalues(df$SEXO, from=cat_sexo$CLAVE, 
                               to=cat_sexo$DESCRIPCIÓN))

df$TIPO_PACIENTE <- as.factor(mapvalues(df$TIPO_PACIENTE, from=cat_tp$CLAVE,
                                        to=cat_tp$DESCRIPCIÓN))

df$INTUBADO <- as.factor(mapvalues(df$INTUBADO, from=cat_sino$CLAVE, 
                                   to=cat_sino$DESCRIPCIÓN))

df$NEUMONIA <- as.factor(mapvalues(df$NEUMONIA, from=cat_sino$CLAVE,
                                   to=cat_sino$DESCRIPCIÓN))

df$EDAD <- as.factor(mapvalues(df$EDAD, from=c(1,2,3,4,5,6,7,8),
                               to=c("0 - 10",
                                    "11 - 20",
                                    "21 - 30",
                                    "31 - 40",
                                    "41 - 50",
                                    "51 - 60",
                                    "71 - 80",
                                    "81 en adelante")))

df$EMBARAZO <- as.factor(mapvalues(df$EMBARAZO, from=cat_sino$CLAVE,
                                   to=cat_sino$DESCRIPCIÓN))

df$DIABETES <- as.factor(mapvalues(df$DIABETES, from=cat_sino$CLAVE,
                                   to=cat_sino$DESCRIPCIÓN))

df$EPOC <- as.factor(mapvalues(df$EPOC, from=cat_sino$CLAVE,
                               to=cat_sino$DESCRIPCIÓN))

df$ASMA <- as.factor(mapvalues(df$ASMA, from=cat_sino$CLAVE,
                               to=cat_sino$DESCRIPCIÓN))

df$INMUSUPR <- as.factor(mapvalues(df$INMUSUPR, from=cat_sino$CLAVE,
                                   to=cat_sino$DESCRIPCIÓN))

df$HIPERTENSION <- as.factor(mapvalues(df$HIPERTENSION, from=cat_sino$CLAVE,
                                       to=cat_sino$DESCRIPCIÓN))

df$OTRA_COM <- as.factor(mapvalues(df$OTRA_COM, from=cat_sino$CLAVE,
                                   to=cat_sino$DESCRIPCIÓN))

df$CARDIOVASCULAR <- as.factor(mapvalues(df$CARDIOVASCULAR, from=cat_sino$CLAVE,
                                         to=cat_sino$DESCRIPCIÓN))

df$OBESIDAD <- as.factor(mapvalues(df$OBESIDAD, from=cat_sino$CLAVE,
                                   to=cat_sino$DESCRIPCIÓN))

df$RENAL_CRONICA <- as.factor(mapvalues(df$RENAL_CRONICA, from=cat_sino$CLAVE,
                                        to=cat_sino$DESCRIPCIÓN))

df$TABAQUISMO <- as.factor(mapvalues(df$TABAQUISMO, from=cat_sino$CLAVE,
                                     to=cat_sino$DESCRIPCIÓN))

df$OTRO_CASO <- as.factor(mapvalues(df$OTRO_CASO, from=cat_sino$CLAVE,
                                    to=cat_sino$DESCRIPCIÓN))

df$UCI <- as.factor(mapvalues(df$UCI, from=cat_sino$CLAVE,
                              to=cat_sino$DESCRIPCIÓN))

names(df) <- c("Origen",
               "Institución que brindó atención",
               "Sexo",
               "Tipo de paciente",
               "Fue intubado?",
               "Presentó neumonía?",
               "Rango de edad",
               "Presentó un embarazo?",
               "Presentó diabetes?",
               "Presentó EPOC",
               "Presentó ASMA",
               "Presentó Inmunosupresión?",
               "Presentó hipertensión",
               "Presentó otra enfermedad?",
               "Presentó problemas cardiovasculares?",
               "Presentó obesidad?",
               "Presentó insuficiencia renal?",
               "Presentó hábito de tabaquismo?",
               "Tuvo contacto con otro positivo?",
               "Requirió cuidados intensivos")


df <- as.data.frame(t(df))

names(df) <- c("Cluster 1", "Cluster 2", "Cluster 3",  "Cluster 4",  "Cluster 5")
