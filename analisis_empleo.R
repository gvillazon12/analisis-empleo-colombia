# Proyecto: Análisis de empleo formal e informal en Colombia
# Autor: Gustavo Villazon
# Descripción: Análisis estadístico con modelos econométricos y visualización de datos

#Librerias 
library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(ggrepel)
library(data.table) 
library(tidyr)
library(knitr)
library(oaxaca)
library(FactoMineR)
library(factoextra)
library(survival)
library(survminer)
install.packages(c("rlang","vctrs","dplyr","ggplot2"), dependencies = TRUE)



# Título: Caracterización de los jefes de hogar que son trabajadores doméstico en Colombia 2023 
Datos <-read_csv("Base_TDG.csv")

### Descritivo a nivel nacional 

# Cantidad de jefes de hogares por departamento a nivel nacional

tabla<-table(Datos$DPTO)

tabla_respo <- Datos %>%
  count(DPTO, name = "Conteo")

kable(tabla_respo, format = "latex", booktabs = TRUE)

# Cantidad de jefes de hogares por tipo de empleo

# Reemplazamos los valores en tipo_empleo por "Formal" o "Informal"
Datos$Tipo_Empl<- ifelse(Datos$P6440 == 1 & Datos$P6450==2,
                      "Formal", "Informal")

Datos %>%
  count(sexo = recode(Tipo_Empl, "Formal" = "Formal", "Informal" = "Informal")) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Formal" = "#65707A", "Informal" = "#C5C7C6")) +
  labs(x = "Tipo de empleo", y = "Porcentaje") +
  ylim(0, 80) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())

### Descriptiva a nivel departamental 

Datos <- Datos[Datos$DPTO == "20", ]

## Estadisticas descriptivas

# Edad de los jefes de hogar

summary(as.numeric(Datos$P6040))

# Ingresos de los jefes de hogar 

summary(as.numeric(Datos$INGLABO))

# Meses que estuvo sin empleo hasta el actual empleo

summary(as.numeric(Datos$P760))

# Meses que duro en su empleo anterior 

summary(as.numeric(Datos$P7026))


# Represnetacion del sexo de los jefes de hogar 

Datos %>%
  count(sexo = recode(P3271, "1" = "Hombres", "2" = "Mujeres")) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Hombres" = "#65707A", "Mujeres" = "#C5C7C6")) +
  labs(x = "Sexo", y = "Porcentaje") +
  ylim(0, 65) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())

# Representacio de la edad de los jefes de hogar
Datos %>%
  mutate(edad_cat = cut(as.numeric(P6040),
                        breaks = c(0, 17, 28, 59, Inf),
                        labels = c("0-17", "18-28", "29-59", "60+")),
         etapa_vida = recode(edad_cat,"0-17" = "Niñez","18-28" = "Juventud","29-59" = "Adultez","60+" = "Vejez")) %>%
  count(etapa_vida) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = etapa_vida, y = pct, fill = etapa_vida)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Niñez" = "#BFC2C1","Juventud" = "#AAAEB0","Adultez" = "#7B868C","Vejez" = "#5D6772")) +
  labs(x = "Rango de edad", y = "Porcentaje") +
  ylim(0, 80) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


# Estudio del estado civil P6070
Datos %>%
  count(estado_civil = case_when(
    P6070 %in% c("1", "2") ~ "Unión libre",
    P6070 == "3" ~ "Casado(a)",
    P6070 == "4" ~ "Divorciado(a)",
    P6070 == "5" ~ "Viudo(a)",
    P6070 == "6" ~ "Soltero(a)"
  )) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = estado_civil, y = pct, fill = estado_civil)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Greys")  +
  labs(x = "Estado civil", y = "Porcentaje") +
  ylim(0, 60) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


# Estudio del mayor grado academico logrado P3042 

tabla_edad_sexo <- Datos %>%
  count(P3042) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

print(tabla_edad_sexo)

# estudio de si tiene contrato o no P6440
Datos %>%
  count(sexo = recode(P6440, "1" = "SI", "2" = "NO")) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("NO" = "#65707A", "SI" = "#C5C7C6"))  +
  labs(x = "Respuestas", y = "Porcentaje") +
  ylim(0, 60) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())

# Ver que clase es el contrato (P6450)

Datos <- Datos[Datos$P6440 == "1", ]

Datos %>%
  count(sexo =case_when(
    P6450 %in% c("1", "9") ~ "Verbal",
    P6450 == "2" ~ "Escrito"))%>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Verbal" = "#65707A", "Escrito" = "#C5C7C6"))  +
  labs(x = "Respuestas", y = "Porcentaje") +
  ylim(0, 70) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


## Tipo de empleo (P6430)
Datos %>%
  count(sexo = recode(Tipo_Empl, "Formal" = "Formal", "Informal" = "Informal")) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Formal" = "#C5C7C6", "Informal" = "#65707A"))  +
  labs(x = "Tipo de empleo", y = "Porcentaje") +
  ylim(0, 60) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())

grafico <- Datos %>%
  count(sexo = recode(Tipo_Empl, "Formal" = "Formal", "Informal" = "Informal")) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Formal" = "#C5C7C6", "Informal" = "#65707A")) +
  labs(x = "Tipo de empleo", y = "Porcentaje") +
  ylim(0, 60) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())

ggsave("tipo_empleo.png",
       plot = grafico,
       width = 10,
       height = 6,
       dpi = 600)

# Estduio si antes del actual empelo tuvo otro (P7020)
Datos %>%
  count(sexo = recode(P7020, "1" = "Si", "2" = "No")) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo, y = pct, fill = sexo)) +
  geom_col(width = 0.5) +  # bordes como en el ejemplo
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("No" = "#65707A", "Si" = "#C5C7C6"))  +
  labs( x = "Respuestas", y = "Porcentaje") +
  ylim(0, 70) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


# Analsis de cunatos meses estuvo sin empleo (P760)

Datos$P760[is.na(Datos$P760)] <- 0

Datos %>%
  mutate(
    grupo = case_when(
      is.na(P760) | P760 == 0 ~ "0",
      TRUE ~ as.character(cut(as.numeric(P760),
                              breaks = c(1, 12, 24, 36, 48, Inf),
                              labels = c("1-12", "13-24", "25-36", "37-48", "49+"),
                              T))
    ),
    etapa_vida = recode(grupo,
                        "0" = "Primer empleo",
                        "1-12" = "1 Año",
                        "13-24" = "2 Años",
                        "25-36" = "3 Años",
                        "37-48" = "4 Años",
                        "49+" = "Mas 5 Años")
  ) %>%
  count(etapa_vida) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = etapa_vida, y = pct, fill = etapa_vida)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primer empleo" = "#E8E8E8",
                               "1 Año" = "#CFCFCF",
                               "2 Años" = "#C5C7C6",
                               "3 Años" = "#9C9C9C",
                               "4 Años" = "#828282",
                               "Mas 5 Años" = "#65707A")) +
  labs(x = "Experiencia laboral (Años)", y = "Porcentaje") +
  ylim(0, 70) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


# Estudio de cuanto tiempo duro en su empleo anterior (P7026)

Datos$P7026[is.na(Datos$P7026)] <- 0

Datos %>%
  mutate(
    grupo = case_when(
      is.na(P7026) | P7026 == 0 ~ "0",
      TRUE ~ as.character(cut(as.numeric(P7026),
                              breaks = c(1, 12, 24, 36, 48, Inf),
                              labels = c("1-12", "13-24", "25-36", "37-48", "49+"),T))
    ),
    etapa_vida = recode(grupo,
                        "0" = "Primer empleo",
                        "1-12" = "1 Año",
                        "13-24" = "2 Años",
                        "25-36" = "3 Años",
                        "37-48" = "4 Años",
                        "49+" = "Mas 5 Años")
  ) %>%
  count(etapa_vida) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = etapa_vida, y = pct, fill = etapa_vida)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Primer empleo" = "#E8E8E8",
                               "1 Año" = "#CFCFCF",
                               "2 Años" = "#C5C7C6",
                               "3 Años" = "#9C9C9C",
                               "4 Años" = "#828282",
                               "Mas 5 Años" = "#65707A")) +
  labs(x = "Experiencia laboral (Años)", y = "Porcentaje") +
  ylim(0, 50) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


# Estduio del ingreso laboral

hist(as.numeric(Datos$INGLABO),breaks=70)

media_variable <- 12000000

# Imputar los valores NA con la media calculada
Datos$INGLABO[is.na(Datos$INGLABO)] <- media_variable

Datos %>%
  mutate(ing_cat = cut(as.numeric(INGLABO),
                       breaks = c(0,1623500, 3247000, 4870500, 6494000, Inf),
                       labels = c("0-1623500", "1623501-3247000", "3247001-4870500", 
                                  "4870501-6494000", "6494000+"),
                       right = FALSE),
    etapa_vida = recode(ing_cat,
                        "0-1623500" = "0-1 SMMLV",
                        "1623501-3247000" = "2 SMMLV",
                        "3247001-4870500" = "3 SMMLV",
                        "4870501-6494000" = "4 SMMLV",
                        "6494000+" = "Mas 5 SMMLV")
  ) %>%
  count(etapa_vida) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = etapa_vida, y = pct, fill = etapa_vida)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ","))),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("1 SMMLV" = "#E8E8E8",
                               "2 SMMLV" = "#CFCFCF",
                               "3 SMMLV" = "#C5C7C6",
                               "4 SMMLV" = "#9C9C9C",
                               "Mas 5 SMMLV" = "#828282")) +
  labs(x = "Experiencia laboral (Años)", y = "Porcentaje") +
  ylim(0, 80) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank())


### Variables cruzadas 

# Edad desagreada por sexo
tabla_edad_sexo <- Datos %>%
  mutate(
    sexo = factor(P3271, levels = c(1, 2), labels = c("Hombres", "Mujeres")),
    edad_cat = cut(as.numeric(P6040),
                   breaks = c(0, 17, 28, 59, Inf),
                   labels = c("0-17", "18-28", "29-59", "60+"))
  ) %>%
  count(sexo, edad_cat) %>%
  group_by(sexo) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()


#tipo de emploe vs sexo
tabla <- table(Datos$P3271,Datos$P6450)
chisq.test(tabla)

#tipo de emploe vs Zona
tabla <- table(Datos$P3271,Datos$CLASE)
tabla
chisq.test(tabla)

#tipo de empleo vs edad
tabla <- table(Datos$P3271,dat$Edad)
chisq.test(tabla)



### Vamos a realizar un Mayor estudio descriptivo como un MCA. 

"1. Tipo de contrato (escrito, verbal, sin contrato)
3. Nivel educativo
4. Sexo del jefe del hogar (hombre, mujer)
5. categorizada (jóvenes, adultos, mayores)"

# Tipo de contrato
dat<-Datos

dat$Zona<-dat$CLASE
dat<- dat %>%
  mutate(Zona = case_when(
    Zona == "1" ~ "Urbano",
    Zona == "2" ~ "Rural"
  ))

dat$Tip_Con<-dat$P6450
table(dat$Tip_Con)

dat<- dat %>%
  mutate(Tip_Con = case_when(
    Tip_Con %in% c("1","9") ~ "Verbal",
    Tip_Con == "2" ~ "Escrito"
  ))

table(dat$Tip_Con)
# Nivel educativo

dat$Niv_Educ<-dat$P3042

dat<- dat %>%
  mutate(Niv_Educ = case_when(
    Niv_Educ == "1" ~ "Ninguno",
    Niv_Educ == "2" ~ "Preescolar",
    Niv_Educ == "3" ~ "primaria",
    Niv_Educ == "4" ~ "secundaria",
    Niv_Educ == "5" ~ "Media académica",
    Niv_Educ == "6" ~ "Media técnica ",
    Niv_Educ == "7" ~ "Normalista",
    Niv_Educ == "8" ~ "Técnica profesional",
    Niv_Educ == "9" ~ "Tecnológica",
    Niv_Educ == "10" ~ "Universitaria",
    Niv_Educ == "11" ~ "Especialización",
    Niv_Educ == "12" ~ "Maestría",
    Niv_Educ == "13" ~ "Doctorado"
  ))


# Sexo al nacer

dat$sexo<-dat$P3271

dat<- dat %>%
  mutate(sexo = case_when(
    sexo == "1" ~ "Masculino",
    sexo == "2" ~ "Femenino"
  ))

table(dat$sexo)

#Edad
dat$Edad<-dat$P6040

dat <- dat %>%
  mutate(Edad = case_when(
    Edad <= 28 ~ "Jovenes",
    Edad <= 59 ~ "Adultos",
    Edad >= 60 ~ "Mayores"
  ))

table(dat$Edad)

datos <- data.frame(Tip_Con=as.factor(dat$Tip_Con),
                    Zona=as.factor(dat$Zona),
                    Edad=as.factor(dat$Edad),
                    sexo=as.factor(dat$sexo),
                    Niv_Educ=as.factor(dat$Niv_Educ))

# Visualización

res.mca <- MCA(datos, graph = T,quali.sup = 1)
fviz_contrib(res.mca,choice="var",axes=2)
fviz_mca_var(res.mca, repel = TRUE)

p <- fviz_mca_var(res.mca, repel = TRUE)
ggsave("mca_variables.png", plot = p, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("mca_variables.png",
       plot = p,
       width = 20,
       height = 12,
       units = "cm",
       dpi = 1200)
# Variables
tipo_empleo <- ifelse(Datos$P6450 == 1, 0,
                      ifelse(Datos$P6450 == 2, 1, Datos$P6450))

edad<-as.numeric(Datos$P6040)
sexo <- ifelse(Datos$P3271 == 2, 0, Datos$P3271)
meses <-as.numeric(Datos$P760)
Zona<-ifelse(Datos$CLASE == 2, 0, Datos$CLASE)
educa<-Datos$P3042

n=5740
evento <- rep(1, n)

# Crear data frame
datos <- data.frame(
  meses_sin_empleo = meses,
  evento = evento,
  tipo_empleo = factor(tipo_empleo, levels = c(0, 1)),
  edad = edad,
  sexo = factor(sexo, levels = c(0, 1)),
  zona=factor(Zona,levels = c(0,1)),
  educa=factor(educa,levels = c(1:13))
  
)
##### KAPLA MEIR#####

# Crear objeto de supervivencia
surv_obj <- Surv(time = datos$meses_sin_empleo, event = datos$evento)

# Modelo Kaplan-Meier por tipo de empleo
km_fit <- survfit(surv_obj ~ tipo_empleo, data = datos)

# Graficar curvas
p<-ggsurvplot(km_fit,
           data = datos,
           conf.int = F,
           pval = F,
           xlab = "Meses",
           ylab = "Probabilidad de seguir desempleado",
           legend.title = "Tipo de empleo",
           legend.labs = c("Informal", "Formal"),
           ggtheme = theme_minimal())
# Prueba de comparación de curvas (Log-rank)
survdiff(surv_obj ~ tipo_empleo, data = datos)

ggsave("kapla.png", plot = p$plot, width = 8, height = 4, dpi = 300, bg = "white")### MODELO COX#############

# hacerlo indidual y despues multiple

# Crear objeto de supervivencia
surv_obj <- Surv(time = datos$meses_sin_empleo, event = datos$evento)

# Modelos univariado
modelo_cox <- coxph(surv_obj ~ tipo_empleo, data = datos)
summary(modelo_cox)

modelo_cox <- coxph(surv_obj ~ edad, data = datos)
summary(modelo_cox)


modelo_cox <- coxph(surv_obj ~ sexo, data = datos)
summary(modelo_cox)


modelo_cox <- coxph(surv_obj ~ zona, data = datos)
summary(modelo_cox)


modelo_cox <- coxph(surv_obj ~ educa, data = datos)
summary(modelo_cox)



# Modelo Cox por tipo de empleo multiple
modelo_cox <- coxph(surv_obj ~ tipo_empleo+edad+sexo+zona+educa, data = datos)
summary(modelo_cox)


# Modelo probit

modelo_probit <- glm(tipo_empleo ~ sexo + edad + zona+ educa,data=datos,
                     family=binomial(link="probit"))

summary(modelo_probit)

##Descomposicion Oaxaca


date1 <- dat %>%
  mutate(
    Tip_Con = factor(as.numeric(P6450 == 1), levels = c(1, 0)),
    edad = as.numeric(P6040),
    sexo = factor(P3271, levels = c(1, 2)),
    ingresos = as.numeric(INGLABO)
  )

date1 <- date1[date1$ingresos != 0, ]

modelo_empleo <- oaxaca(formula = log(ingresos) ~ edad|Tip_Con,data = date1)

summary(modelo_empleo)
