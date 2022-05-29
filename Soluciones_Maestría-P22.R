# Soluciones Tarea 1 y 2 | Primavera 2022
# Creada por Miguel Negrete
# Revisada y editada por Arturo Aguilar

# Para ejecutar este script desde su computadora pueden guardar
# las bases de datos que emplea en un folder en su computadora
# y en dicho folder crear un proyecto referenciado a ese folder
# como vimos en clase

#Cargamos los paquetes que utilizaremos en la tarea

library(tidyverse)
library(stargazer)
library(sandwich)
library(hdm) 
library(magrittr)
library(MatchIt)
library(car)
library(AER)
library(schoolmath)
library(jtools) 
library(dplyr)

#library(optmatch)

#Paquetes para estimar Lee Bounds (Método 2)
#library(devtools)
#install_github("vsemenova/leebounds")
#library(leebounds)

#Cargamos las bases de datos que ocuparemos durante la tarea

#Baseline
baseline<-read.csv("baseline.csv")
baseline <- data.frame(baseline)

#Endline
endline<-read.csv("endline.csv")
endline <- data.frame(endline)



# ====/// PARTE 1 \\\====

# ====// Pregunta 1: BALANCE \\====


# Creamos un dataframe con las variables de interés
vars_1<- baseline%>%select(T_nap, age_,
                     female_, education_, 
                     no_of_children_,
                     unemployed,
                     stress, sleep_eff,
                     health_bsl, out_of_bed, energy)


# Calculamos las medias de cada variable segun T_nap
means_1 <- vars_1%>%group_by(T_nap)%>%summarise(across(age_:energy,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_1 <- c()
tstat_1 <- c()
pval_1 <- c()
for (i in 2:11){
  diff_1 <- c(diff_1,round(summ(lm(vars_1[,i]~vars_1[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_1 <- c(tstat_1,round(summ(lm(vars_1[,i]~vars_1[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_1 <- c(pval_1,round(summ(lm(vars_1[,i]~vars_1[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal <- data.frame(trat = t(means_1[2,2:11]),control = t(means_1[1,2:11]),diff = diff_1,t = tstat_1,pvalue = pval_1)
stargazer(t_bal,summary = F)

# Calculamos el estadistico F y valor-p de un MCO
mco_1 <- lm(T_nap ~ age_ + female_ + education_ + no_of_children_ + unemployed
           + stress + sleep_eff + health_bsl + out_of_bed + energy, vars_1)
lh <-linearHypothesis(mco_1, c("age_=0", "female_=0", "education_=0", "no_of_children_ =0", "unemployed=0", "stress=0", "sleep_eff=0","health_bsl=0", "out_of_bed =0", "energy=0"), white.adjust = "hc1")
(F_1 <- lh$F[2])
(pval_1 <- lh$`Pr(>F)`[2])

# ====// Pregunta 2: EFECTOS DE TRATAMIENTO\\====

# ==== Inciso (a) ====

table_2_a <- endline %>% 
  group_by("Productivity") %>% 
  summarise(Neyman= round(mean(productivity[T_nap==1], na.rm = T)-mean(productivity[T_nap==0], na.rm = T),3),
            SD = round(sqrt(var(productivity[T_nap==1], na.rm = T)/sum(T_nap == 1)+var(productivity[T_nap==0], na.rm = T)/sum(T_nap == 0)),3),
            P_Value = round(2*pnorm(abs(Neyman / SD), 0, 1, lower.tail = F),3))

table_2_a %>% stargazer(summary = F,
                             rownames = F)

# ==== Inciso (b) ====

reg_2_b <- feols(productivity ~ T_nap, se = "hetero", data = endline)
data_2_b<- data.frame(reg_2_b$coeftable)
table_2_b <- endline %>% 
  group_by("Productivity") %>% 
  summarise(Neyman= round(data_2_b[2,1],3),
            SD = round(data_2_b[2,2],3),
            P_Value = round(data_2_b[2,4],3))

table_2_a %>% stargazer(summary = F,
                        rownames = F)
# ==== Inciso (c) ====

reg_2_c <- feols(productivity ~ T_nap + out_of_bed + energy, se = "hetero", data = endline)
data_2_c<- data.frame(reg_2_c$coeftable)
table_2_c <- endline %>% 
  group_by("Productivity") %>% 
  summarise(Neyman= round(data_2_c[2,1],3),
            SD = round(data_2_c[2,2],3),
            P_Value = round(data_2_c[2,4],3))

table_2_c %>% stargazer(summary = F,
                        rownames = F)

# ==== Inciso (e) ====

#Creamos la variable cognitive
endline %<>% mutate(cognitive = (scale(corsi_measure)+scale(hf_measure)+scale(pvt_measure))/3) 

#Realizamos nuestras estimaciones

reg_2_d_i <- feols(nap_time_mins ~ T_nap + out_of_bed + energy, se = "hetero", data = endline)
data_2_d_i <- data.frame(reg_2_d_i$coeftable)

reg_2_d_ii <- feols(sleep_report ~ T_nap + out_of_bed + energy, se = "hetero", data = endline)
data_2_d_ii <- data.frame(reg_2_d_ii$coeftable)

reg_2_d_iii <- feols(happy ~ T_nap + out_of_bed + energy, se = "hetero", data = endline)
data_2_d_iii <- data.frame(reg_2_d_iii$coeftable)

reg_2_d_iv <- feols(cognitive ~ T_nap + out_of_bed + energy, se = "hetero", data = endline)
data_2_d_iv <- data.frame(reg_2_d_iv$coeftable)

reg_2_d_v  <- feols(typing_time_hr ~ T_nap + out_of_bed + energy, se = "hetero", data = endline)
data_2_d_v <- data.frame(reg_2_d_v$coeftable)

#Creamos una tabla para reportar los resultados
Variable <- c("nap_time_mins", "sleep_report", 
              "happy", "cognitive", "typing_time_hr")
ATE <- c(round(data_2_d_i[2,1],3),round(data_2_d_ii[2,1],3),round(data_2_d_iii[2,1],3),round(data_2_d_iv[2,1],3),round(data_2_d_v[2,1],3))
Valor_p <- c(round(data_2_d_i[2,4],3),round(data_2_d_ii[2,4],3),round(data_2_d_iii[2,4],3),round(data_2_d_iv[2,4],3),round(data_2_d_v[2,4],3))
Media_Variable <- c(round(mean(nap_time_mins),3),round(mean(sleep_report,na.rm = T),3),round(mean(happy),3),round(mean(cognitive),3),round(mean(typing_time_hr),3))

#Tabla
df_2_e <- data.frame(Variable, ATE,Valor_p, Media_Variable)
df_2_e %>% stargazer(summary = F,
                        rownames = F)

# ====// Pregunta 3: FISCHER EXACT TEST \\====

# ====Inciso (a) ====

T_nap_1 <- mean(endline[endline$T_nap==1,'productivity'])
T_nap_0 <- mean(endline[endline$T_nap==0,'productivity'])

(neyman <- T_nap_1-T_nap_0) # Estadistico Neyman

cb_varm_T_nap <- var(endline[endline$T_nap==1,'productivity'])/sum(with(endline,T_nap==1)) 
cb_varm_T_nap_0 <- var(endline[endline$T_nap==0,'productivity'])/sum(with(endline,T_nap==0))

(sd_neyman <- sqrt(cb_varm_T_nap+cb_varm_T_nap_0))

PET <- endline%>%select(productivity,T_nap)
PET <- rownames_to_column(PET)

t_stats_4 <- c() 

for (j in 1:999) {
  # Re-aleatorizacion: Tomamos el vector que contiene la dummy de T_nap y le damos un nuevo orden 
  fake <- sample_n(as.data.frame(PET$T_nap),length(PET$T_nap),replace = F)
  # Le asignamos un "nombre" a cada observacion, de esta forma podemos unirla a la endline original
  fake <- rownames_to_column(fake) 
  # Unimos a la base origina el vector de T_nap re-aleatorizado, de esta forma logramos una nueva asignacion de T_nap
  df <- left_join(PET,fake) 
  
  cb_T_nap <- mean(df[df$`PET$T_nap`==1,'productivity']) 
  cb_T_not_nap <- mean(df[df$`PET$T_nap`==0,'productivity'])
  cb_vm_T_nap <- var(df[df$`PET$T_nap`==1,'productivity'])/sum(with(df,`PET$T_nap`==1))
  cb_vm_T_not_nap <- var(df[df$`PET$T_nap`==0,'productivity'])/sum(with(df,`PET$T_nap`==0))
  
  # Neyman
  ney <- cb_T_nap-cb_T_not_nap
  # Error std Neyman
  sd_ney <- sqrt(cb_vm_T_nap+cb_vm_T_not_nap) 
  # Estadistico t
  t_stats_4 <- c(t_stats_4,ney/sd_ney)
}

# Estadisticos t
t_stats_4 <- c(t_stats_4,(neyman/sd_neyman))

# Valor-p
(pval_4 <- sum(abs((t_stats_4))>abs((neyman+0.01)/sd_neyman))/length(t_stats_4)) 


# ====// Pregunta 4: ESTRATIFICACIÓN \\====

# ==== Inciso (a) ====

#Calculamos la mediana de sleep_night y earnings
sleep_median <- median(baseline$sleep_night)
earn_median <- median(baseline$earnings)

#Creamos una variable dummy para identificar a los participantes cuyas observaciones sean mayores o iguales a la mediana
baseline <- baseline %>% mutate(sleep_more_median = if_else(sleep_night >= sleep_median, 1, 0),
                    earnings_more_median = if_else(earnings >= earn_median, 1, 0)) 

#Generamos una tabla
table_4_a <- baseline %>% 
  group_by(sleep_more_median, earnings_more_median, T_nap) %>% 
  summarise(n = n())
table_4_a %>% stargazer(summary = F,
                     rownames = F)

# ==== Inciso (b) ====

#Creamos los grupos definidos en la tarea en endline
endline <- endline %>% 
  inner_join(select(baseline, sleep_more_median, earnings_more_median,pid), by = "pid")%>% 
  mutate(group = if_else(sleep_more_median == 1 & earnings_more_median==1, 1, 
                         if_else(sleep_more_median == 1 & earnings_more_median==0, 2,
                                 if_else(sleep_more_median == 0 & earnings_more_median==1, 3,4))))
#Calculamos Neyman
table_4_b_mean <- endline %>% 
  group_by(group) %>% 
  summarise(nap_time_mins = round(mean(nap_time_mins[T_nap==1], na.rm = T)-mean(nap_time_mins[T_nap==0], na.rm = T),3),
            sleep_report = round(mean(sleep_report[T_nap==1], na.rm = T)-mean(sleep_report[T_nap==0], na.rm = T),3),
            happy = round(mean(happy[T_nap==1], na.rm = T)-mean(happy[T_nap==0], na.rm = T),3),
            cognitive = round(mean(cognitive[T_nap==1], na.rm = T)-mean(cognitive[T_nap==0], na.rm = T),3),
            typing_time_hr = round(mean(typing_time_hr[T_nap==1], na.rm = T)-mean(typing_time_hr[T_nap==0], na.rm = T),3))
#Tabla
table_4_b_mean %>% stargazer(summary = F,
                        rownames = F)

#Calculamos las desviaciones estándar asociadas a Neyman
table_4_b_sd <- endline %>% 
  group_by(group) %>% 
  summarise(nap_time_mins = round(sqrt(var(nap_time_mins[T_nap==1], na.rm = T)/sum(T_nap == 1)+var(nap_time_mins[T_nap==0], na.rm = T)/sum(T_nap == 0)),3),
            sleep_report = round(sqrt(var(sleep_report[T_nap==1], na.rm = T)/sum(T_nap == 1)+var(sleep_report[T_nap==0], na.rm = T)/sum(T_nap == 0)),3),
            happy = round(sqrt(var(happy[T_nap==1], na.rm = T)/sum(T_nap == 1)+var(happy[T_nap==0], na.rm = T)/sum(T_nap == 0)),3),
            cognitive = round(sqrt(var(cognitive[T_nap==1], na.rm = T)/sum(T_nap == 1)+var(cognitive[T_nap==0], na.rm = T)/sum(T_nap == 0)),3),
            typing_time_hr = round(sqrt(var(typing_time_hr[T_nap==1], na.rm = T)/sum(T_nap == 1)+var(typing_time_hr[T_nap==0], na.rm = T)/sum(T_nap == 0)),3))
#Tabla
table_4_b_sd %>% stargazer(summary = F,
                             rownames = F)

#Calculamos los ATE agregados
table_4_b_aggregated <- endline %>% 
  group_by("ATE Agregado") %>% 
  summarise(round(table_4_b_mean[1,2]*sum(group == 1)/length(group) + table_4_b_mean[2,2]*sum(group == 2)/length(group) + table_4_b_mean[3,2]*sum(group == 3)/length(group) + table_4_b_mean[4,2]*sum(group == 4)/length(group),3),
            round(table_4_b_mean[1,3]*sum(group == 1)/length(group) + table_4_b_mean[2,3]*sum(group == 2)/length(group) + table_4_b_mean[3,3]*sum(group == 3)/length(group) + table_4_b_mean[4,3]*sum(group == 4)/length(group),3),
            round(table_4_b_mean[1,4]*sum(group == 1)/length(group) + table_4_b_mean[2,4]*sum(group == 2)/length(group) + table_4_b_mean[3,4]*sum(group == 3)/length(group) + table_4_b_mean[4,4]*sum(group == 4)/length(group),3),
            round(table_4_b_mean[1,5]*sum(group == 1)/length(group) + table_4_b_mean[2,5]*sum(group == 2)/length(group) + table_4_b_mean[3,5]*sum(group == 3)/length(group) + table_4_b_mean[4,5]*sum(group == 4)/length(group),3),
            round(table_4_b_mean[1,6]*sum(group == 1)/length(group) + table_4_b_mean[2,6]*sum(group == 2)/length(group) + table_4_b_mean[3,6]*sum(group == 3)/length(group) + table_4_b_mean[4,6]*sum(group == 4)/length(group),3))

#Tabla
table_4_b_aggregated %>% stargazer(summary = F,
                           rownames = F,
                           colnames= F)

#Calculamos las varianzas para los ATE agregados
table_4_b_aggregated_sd <- endline %>% 
  group_by() %>% 
  summarise(round(sqrt(table_4_b_sd[1,2]^2*(sum(group == 1)/length(group))^2 + table_4_b_sd[2,2]^2*(sum(group == 2)/length(group))^2 + table_4_b_sd[3,2]^2*(sum(group == 3)/length(group))^2 + table_4_b_sd[4,2]^2*(sum(group == 4)/length(group))^2),3),
            round(sqrt(table_4_b_sd[1,3]^2*(sum(group == 1)/length(group))^2 + table_4_b_sd[2,3]^2*(sum(group == 2)/length(group))^2 + table_4_b_sd[3,3]^2*(sum(group == 3)/length(group))^2 + table_4_b_sd[4,3]^2*(sum(group == 4)/length(group))^2),3),
            round(sqrt(table_4_b_sd[1,4]^2*(sum(group == 1)/length(group))^2 + table_4_b_sd[2,4]^2*(sum(group == 2)/length(group))^2 + table_4_b_sd[3,4]^2*(sum(group == 3)/length(group))^2 + table_4_b_sd[4,4]^2*(sum(group == 4)/length(group))^2),3),
            round(sqrt(table_4_b_sd[1,5]^2*(sum(group == 1)/length(group))^2 + table_4_b_sd[2,5]^2*(sum(group == 2)/length(group))^2 + table_4_b_sd[3,5]^2*(sum(group == 3)/length(group))^2 + table_4_b_sd[4,5]^2*(sum(group == 4)/length(group))^2),3),
            round(sqrt(table_4_b_sd[1,6]^2*(sum(group == 1)/length(group))^2 + table_4_b_sd[2,6]^2*(sum(group == 2)/length(group))^2 + table_4_b_sd[3,6]^2*(sum(group == 3)/length(group))^2 + table_4_b_sd[4,6]^2*(sum(group == 4)/length(group))^2),3))

#Tabla
table_4_b_aggregated_sd %>% stargazer(summary = F,
                                   rownames = F)


# ==== Inciso (c) ====

#Cálculo efecto heterogéneo
data_4_c <- endline %>%
  mutate(g_1 = if_else(group == 1,1,0),
         g_2 = if_else(group == 2,1,0),
         g_3 = if_else(group == 3,1,0),
         g_4 = if_else(group == 4,1,0))%>%
  select(pid,T_nap,nap_time_mins,sleep_report,happy,cognitive,typing_time_hr,productivity,g_1,g_2,g_3,g_4)%>%
  group_by(pid)

#Creación de tabla
table_4_c <- data.frame(Grupo = character(0), nap_time_mins = numeric(0), sleep_report = numeric(0), happy = numeric(0), cognitive = numeric(0), typing_time_hr = numeric(0))
table_4_c["Grupo"]=c(1,"",2,"",3,"",4,"")
for (i in Variable){
  reg <- lm(paste(i, "g_1 + g_2 + g_3 + g_4 + T_nap:g_1 + T_nap:g_2 + T_nap:g_3 + T_nap:g_4-1" , sep = "~"),se = "hetero", data = data_4_c)
  reg_cov <-sqrt(diag(vcovHC(reg, type = "HC1")))
  count <- 1
  count_coeff <- 5
  while (count<9) {
    table_4_c[count,match(i,Variable)+1]=reg$coefficients[count_coeff]
    table_4_c[count+1,match(i,Variable)+1]=reg_cov[count_coeff]
    count <- count + 2
    count_coeff <- count_coeff +1
  }
}

#Tabla
table_4_c %>% stargazer(summary = F,
                          rownames = F)

# ====// Pregunta 5: ATRICIÓN \\====

# ==== Inciso (a) ====

#Cálculamos las observaciones perdidas
table_5_a <- endline %>% 
  group_by(T_nap) %>% 
  summarise(Cantidad = sum(drop_indicator==1),
            Porcentaje = round(Cantidad/length(T_nap),3)*100)

#Tabla
table_5_a %>% stargazer(summary = F,
                        rownames = F)

# ==== Inciso (b) ====

#Validez interna

# Filtramos nuestras variables para evaluar solo a los participantes que se quedaron

vars_5_b_VI<- baseline%>%
  filter(drop_indicator==0)%>%
  select(T_nap, age_,
         female_, education_, 
         no_of_children_,
         unemployed,
         stress, sleep_eff,
         health_bsl, out_of_bed, energy)

# Calculamos las medias de cada variable segun T_nap

means_5_b_VI <- vars_5_b_VI%>%group_by(T_nap)%>%summarise(across(age_:energy,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_5_b_VI <- c()
tstat_5_b_VI <- c()
pval_5_b_VI <- c()
for (i in 2:11){
  diff_5_b_VI <- c(diff_5_b_VI,round(summ(lm(vars_5_b_VI[,1]~vars_5_b_VI[,i]),robust = 'HC1')$coeftable[2,1],4))
  tstat_5_b_VI <- c(tstat_5_b_VI,round(summ(lm(vars_5_b_VI[,1]~vars_5_b_VI[,i]),robust = 'HC1')$coeftable[2,3],4))
  pval_5_b_VI <- c(pval_5_b_VI,round(summ(lm(vars_5_b_VI[,1]~vars_5_b_VI[,i]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_5_b_VI <- data.frame(trat = t(means_5_b_VI[2,2:11]),control = t(means_5_b_VI[1,2:11]),diff = diff_5_b_VI,t = tstat_5_b_VI,pvalue = pval_5_b_VI)

# Calculamos el estadistico F y valor-p de un MCO
mco_5_b_VI <- lm(T_nap ~ age_ + female_ + education_ + no_of_children_ + unemployed
                 + stress + sleep_eff + health_bsl + out_of_bed + energy, vars_5_b_VI)
lh <-linearHypothesis(mco_5_b_VI, c("age_=0", "female_=0", "education_=0", "no_of_children_ =0", "unemployed=0", "stress=0", "sleep_eff=0","health_bsl=0", "out_of_bed =0", "energy=0"), white.adjust = "hc1")
(F_5_b_VI <- lh$F[2])
(pval_5_b_VI <- lh$`Pr(>F)`[2])

stargazer(t_bal_5_b_VI,summary = F, notes = paste("Estadístico F = ", round(F_5_b_VI,3),". Valor-p = ",round(pval_5_b_VI,3), sep=""))


#Validez externa

# Filtramos nuestras base para evaluar las diferencias entre los individuos que permanecieron en el estudio contra los que se fueron

vars_5_b_VE<- baseline%>%
  select(drop_indicator, age_,
         female_, education_, 
         no_of_children_,
         unemployed,
         stress, sleep_eff,
         health_bsl, out_of_bed, energy)

# Calculamos las medias de cada variable segun drop_indicator

means_5_b_VE <- vars_5_b_VE%>%group_by(drop_indicator)%>%summarise(across(age_:energy,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_5_b_VE <- c()
tstat_5_b_VE <- c()
pval_5_b_VE <- c()
for (i in 2:11){
  diff_5_b_VE <- c(diff_5_b_VE,round(summ(lm(vars_5_b_VE[,i]~vars_5_b_VE[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_5_b_VE <- c(tstat_5_b_VE,round(summ(lm(vars_5_b_VE[,i]~vars_5_b_VE[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_5_b_VE <- c(pval_5_b_VE,round(summ(lm(vars_5_b_VE[,i]~vars_5_b_VE[,1]),robust = 'HC1')$coeftable[2,4],4))
}

t_bal_5_b_VE <- data.frame(Stayed = t(means_5_b_VE[1,2:11]), Dropped = t(means_5_b_VE[2,2:11]),diff = diff_5_b_VE,t = tstat_5_b_VE,pvalue = pval_5_b_VE)

# Calculamos el estadistico F y valor-p de un MCO
mco_5_b_VE <- lm(drop_indicator ~ age_ + female_ + education_ + no_of_children_ + unemployed
                 + stress + sleep_eff + health_bsl + out_of_bed + energy, vars_5_b_VE)
lh <-linearHypothesis(mco_5_b_VE, c("age_=0", "female_=0", "education_=0", "no_of_children_ =0", "unemployed=0", "stress=0", "sleep_eff=0","health_bsl=0", "out_of_bed =0", "energy=0"), white.adjust = "hc1")
(F_5_b_VE <- lh$F[2])
(pval_5_b_VE <- lh$`Pr(>F)`[2])

stargazer(t_bal_5_b_VE,summary = F, notes = paste("Estadístico F = ", round(F_5_b_VE,3),". Valor-p = ",round(pval_5_b_VE,3), sep=""))


# ====// Pregunta 6: LEE BOUNDS \\====

# ==== Inciso (a) ====

#Always respondents
AR <- sum(endline$drop_indicator==0 & endline$T_nap==0)/sum(endline$T_nap==0)

#Selective Respondents
SR<-sum(endline$drop_indicator==0 & endline$T_nap==1)/sum(endline$T_nap==1) - AR

#Never Respondents
NR <- 1-AR-SR

#Creamos una tabla para reportar los resultados
Tipo<-c("Always Respondents","Selective Respondents","Never Respondents")
Porcentaje <-c(AR,SR,NR)*100
data_6_a <- data.frame(Tipo,Porcentaje)
data_6_a %>% stargazer(summary = F,
                        rownames = F)

# ==== Inciso (b) ====

#Estimamos el número de individuos que pertenecen a Always Respondents

N_AR <-round(sum((endline$drop_indicator==0 & endline$T_nap==1)*(1-((SR)/(SR+AR)))))

#Creamos el intervalo

#Cota inferior y superior
data_lee_bounds <- sort(endline$productivity[endline$drop_indicator==0 & endline$T_nap==1])
CI <- mean(data_lee_bounds[1:N_AR])
CS <-mean(data_lee_bounds[length(data_lee_bounds):(length(data_lee_bounds)-N_AR+1)])

#Media de productividad AR
control_6_b <- mean(endline$productivity[drop_indicator==0 & T_nap==0])

(Lee_Bounds <- c(CI-control_6_b,CS-control_6_b))

#Lee-Bounds Método 2?

#leedata <- data.frame(treat = endline$T_nap, selection = abs(endline$drop_indicator-1), outcome = endline$productivity)
#leedata %>% drop_na()
#(GetBounds(leebounds(leedata)))


# ====/// PARTE 2 \\\====

# ====// Pregunta 1: COARSENED EXACT MATCH \\====

#Realizamos CEM
CEM <- matchit(T_nap ~ female_ + education_, data=endline %>% filter(drop_indicator==0), method = "cem", estimand = "ATE")
cem_d <- match.data(CEM)
CEM_model <- feols(productivity ~ T_nap, data = match.data(CEM), se="hetero", weights=cem_d$weights)
data_CEM_model<-data.frame(CEM_model$coeftable)

#Creamos la tabla de reporte
table_2_1 <- c(ATE=round(data_CEM_model[2,1],3),SD=round(data_CEM_model[2,2],3),Valor_p=round(data_CEM_model[2,4],3),Perdida=nrow(endline %>% filter(drop_indicator==0))-nrow(cem_d))
table_2_1 %>% stargazer(summary = F,
                        rownames = F)

# ====// Pregunta 2: NEAREST NEIGHBOR \\====


base_nn <- endline %>% filter(drop_indicator == 0) %>%
                            left_join(baseline %>% 
                            filter(drop_indicator == 0)%>% 
                            select(pid, sleep_report) %>% 
                            rename(BASE_sleep_report = sleep_report),
                          by = "pid") 

# Estimación un nearest neighbor
nn_1 <-matchit(T_nap ~ female_ + age_ + BASE_sleep_report, data=base_nn, method ="nearest", distance = "mahalanobis", estimand ='ATT',ratio=1, replace = TRUE)
data_nn_1 <- match.data(nn_1)
nn_1_model <- feols(productivity ~ T_nap, data = data_nn_1, se = "hetero", weights = data_nn_1$weights)

# Estimación cinco nearest neighbors
nn_5 <-matchit(T_nap ~ female_ + age_ + BASE_sleep_report, data=base_nn, method ="nearest", distance = "mahalanobis", estimand ='ATT',ratio=5, replace = TRUE)
data_nn_5 <- match.data(nn_5)
nn_5_model <- feols(productivity ~ T_nap, data = data_nn_5, se = "hetero", weights = data_nn_5$weights)


# Estimación diez nearest neighbors
nn_10 <-matchit(T_nap ~ female_ + age_ + BASE_sleep_report, data=base_nn, method ="nearest", distance = "mahalanobis", estimand ='ATT',ratio=10, replace = TRUE)
data_nn_10 <- match.data(nn_10)
nn_10_model <- feols(productivity ~ T_nap, data = data_nn_10, weights = data_nn_10$weights)


#Creamos una tabla para reportar nuestros resultados
table_2_2 <- data.frame(Nearest_Neighbors = c(1,"",5,"",10,""),
               TOT = c(round(nn_1_model$coefficients[2],3),round(nn_1_model$coeftable[2,2],3),round(nn_5_model$coefficients[2],3),
                       round(nn_5_model$coeftable[2,2],3),round(nn_10_model$coefficients[2],3),round(nn_10_model$coeftable[2,2],3)),
               Valor_p = c(round(nn_1_model$coeftable[2,4],4),"",round(nn_5_model$coeftable[2,4],4),"",round(nn_10_model$coeftable[2,4],4),""))
table_2_2 %>% stargazer(summary = F,
                       rownames = F)

# ====// Pregunta 3: PSM \\====

# ====// Inciso (a)

#Estimaciones
PSM_r_1 <- feols(T_nap ~ age_ +female_+ education_+ out_of_bed +energy, data = endline %>% filter(drop_indicator==0), se = "hetero")
PSM_r_2 <- feglm(T_nap ~ age_ +female_+ education_+ out_of_bed +energy, data = endline%>% filter(drop_indicator==0), se = "hetero", family="probit")
PSM_r_3<- feglm(T_nap ~ age_ +female_+ education_+ out_of_bed +energy, data = endline%>% filter(drop_indicator==0), se = "hetero", family="logit")

#Tabla
etable(PSM_r_1,PSM_r_2,PSM_r_3, tex = TRUE)

# ====// Inciso (b)

#Estimaciones
data_PSM_r_1 <- data.frame(pr_score= stats::predict(PSM_r_1, type="response"),
                    T_nap = (endline %>% filter(drop_indicator==0))$T_nap)
data_PSM_r_2 <- data.frame(pr_score= stats::predict(PSM_r_2, type="response"),
                        T_nap = (endline %>% filter(drop_indicator==0))$T_nap)
data_PSM_r_3 <- data.frame(pr_score= stats::predict(PSM_r_3, type="response"),
                       T_nap = (endline %>% filter(drop_indicator==0))$T_nap)
rep_PSM_r_1 <- matchit(T_nap ~ age_ +female_+ education_+ out_of_bed +energy,
                      method = "nearest", data = endline %>% filter(drop_indicator==0), distance = "glm", estimand = "ATT",
                      ratio = 1, replace = TRUE)
rep_PSM_r_2<- matchit(T_nap ~ age_ +female_+ education_+ out_of_bed +energy,
                          method = "nearest", data = endline %>% filter(drop_indicator==0), distance = "glm", link = "probit", estimand = "ATT",
                          ratio = 1, replace = TRUE)
rep_PSM_r_3 <- matchit(T_nap ~ age_ +female_+ education_+ out_of_bed +energy,
                         method = "nearest", data = endline %>% filter(drop_indicator==0), distance = "glm", link = "logit", estimand = "ATT",
                         ratio = 1, replace = TRUE)

#Cáluculo de media y SD
data_PSM_b <- endline %>% filter(drop_indicator==0) %>%
  mutate(OLS = data_PSM_r_1$pr_score, 
         Probit = data_PSM_r_2$pr_score,
         Logit = data_PSM_r_3$pr_score,
         OLS_rep = rep_PSM_r_1$weights,
         Probit_rep = rep_PSM_r_2$weights,
         Logit_rep = rep_PSM_r_3$weights) 

#Creamos las tablas para reportar nuestros resultados

#Tabla 1
table_PSM_b_1 <- data_PSM_b %>%
  select(c("T_nap","OLS","Probit", "Logit")) %>%
  group_by(T_nap) %>%
  summarise_all(list(mean,sd))
table_PSM_b_1<-round(table_PSM_b_1,3)
table_PSM_b_1%>% stargazer(summary = F,
                           rownames = F)
#Tabla 2
table_PSM_b_2 <- data_PSM_b %>%
  select(c("T_nap","OLS_rep", "Probit_rep", "Logit_rep")) %>%
  group_by(T_nap) %>%
  summarise_all(list(mean,sd))
table_PSM_b_2<-round(table_PSM_b_2,3)
table_PSM_b_2%>% stargazer(summary = F,
                           rownames = F)

# ====// Inciso (c)

# Filtramos nuestras variables para evaluar solo a los participantes que se quedaron y utilizamos ponderador
variables_7 <-  vars_1%>%filter(drop_indicator == 0)
variables_7 <- variables_7*data_PSM_b$Probit_rep

# Calculamos las medias de cada variable segun T_nap

means_7_c<- variables_7%>%group_by(T_nap)%>%summarise(across(age_:energy,~ mean(.x, na.rm = T)))

# Calculamos la diferencia de medias
diff_7_c <- c()
tstat_7_c <- c()
pval_7_c <- c()
for (i in 2:11){
  diff_7_c <- c(diff_7_c,round(summ(lm(variables_7[,i]~variables_7[,1]),robust = 'HC1')$coeftable[2,1],4))
  tstat_7_c <- c(tstat_7_c,round(summ(lm(variables_7[,i]~variables_7[,1]),robust = 'HC1')$coeftable[2,3],4))
  pval_7_c <- c(pval_7_c,round(summ(lm(variables_7[,i]~variables_7[,1]),robust = 'HC1')$coeftable[2,4],4))
}
t_bal_7_c <- data.frame(trat = t(means_7_c[2,2:11]),control = t(means_7_c[1,2:11]),diff = diff_7_c,t = tstat_7_c,pvalue = pval_7_c)

# Calculamos el estadistico F y valor-p de un MCO
mco_7_c <- lm(T_nap ~ age_ + female_ + education_ + no_of_children_ + unemployed
                 + stress + sleep_eff + health_bsl + out_of_bed + energy, variables_7)
lh <-linearHypothesis(mco_7_c, c("age_=0", "female_=0", "education_=0", "no_of_children_ =0", "unemployed=0", "stress=0", "sleep_eff=0","health_bsl=0", "out_of_bed =0", "energy=0"), white.adjust = "hc1")
(F_7_c <- lh$F[2])
(pval_7_c <- lh$`Pr(>F)`[2])

#Creación de tabla
stargazer(t_bal_7_c,summary = F, notes = paste("Estadístico F = ", round(F_7_c,3),". Valor-p = ",round(pval_7_c,3), sep=""))

# ====// Inciso (d)

#Estimación TOT
reg_6_d_prod <- feols(productivity ~ T_nap, data = match.data(rep_PSM_r_2), se = "hetero")
reg_6_d_ntm <- feols(nap_time_mins ~ T_nap, data = match.data(rep_PSM_r_2), se = "hetero")
reg_6_d_sr <- feols(sleep_report ~ T_nap, data = match.data(rep_PSM_r_2), se = "hetero")
reg_6_d_happ <- feols(happy ~ T_nap, data = match.data(rep_PSM_r_2), se = "hetero")
reg_6_d_cogn <- feols(cognitive ~ T_nap, data = match.data(rep_PSM_r_2), se = "hetero")
reg_6_d_tth <- feols(typing_time_hr ~ T_nap, data = match.data(rep_PSM_r_2), se = "hetero")

#Creación de tabla
table_2_2 <- data.frame(Variables = c("productivity","","nap_time_mins","","slee_report","","happy","","cognitive","","typing_time_hr",""),
                        TOT = c(round(reg_6_d_prod$coefficients[2],3),round(reg_6_d_prod$coeftable[2,2],3),
                                round(reg_6_d_ntm$coefficients[2],3),round(reg_6_d_ntm$coeftable[2,2],3),
                                round(reg_6_d_sr$coefficients[2],3),round(reg_6_d_sr$coeftable[2,2],3),
                                round(reg_6_d_happ$coefficients[2],3),round(reg_6_d_happ$coeftable[2,2],3),
                                round(reg_6_d_cogn$coefficients[2],3),round(reg_6_d_cogn$coeftable[2,2],3),
                                round(reg_6_d_tth$coefficients[2],3),round(reg_6_d_tth$coeftable[2,2],3)),
                        Valor_p = c(round(reg_6_d_prod$coeftable[2,4],4),"",
                                    round(reg_6_d_ntm$coeftable[2,4],4),"",
                                    round(reg_6_d_sr$coeftable[2,4],4),"",
                                    round(reg_6_d_happ$coeftable[2,4],4),"",
                                    round(reg_6_d_cogn$coeftable[2,4],4),"",
                                    round(reg_6_d_tth$coeftable[2,4],4),""))
table_2_2 %>% stargazer(summary = F,
                        rownames = F,
                        notes = "Errores estándar robustos a heterocedasticidad entre paréntesis")
