library(tidyverse)
library(ggplot2)
library(readxl)
library(xtable)
library(cowplot)
library(broom)
library(dplyr)
# print(xtable(prod_ban_wide2), include.rownames = FALSE)

#data incial
cultivos_global <- read_csv("GitHub/Proyecto_Estadistica/cultivos_global.csv")
data = cultivos_global
data_limpia = cultivos_global %>%
          select(c('Área', 'Elemento', 'Producto', 'Año', 'Unidad', 'Valor'))

#tabla1
prod_ban = data_limpia %>%
            filter(Elemento == 'Producción' & Producto == 'Bananos')

prod_ban_gr = prod_ban %>%
  filter(Área %in% c('Costa Rica', 'Colombia', 'Ecuador')) %>%
  mutate(quinquenio = as.integer(cut(Año, breaks = seq(1960, 2025, by = 5)))) %>%
  group_by(quinquenio, Área)

tabla1 = summarise(prod_ban_gr, minimo=min(Valor, na.rm = TRUE), 
                   promedio=mean(Valor), maximo=max(Valor, na.rm = TRUE), 
                   sd=sd(Valor), median=quantile(Valor, 0.5), 
                   q1=quantile(Valor, 0.25), q3=quantile(Valor, 0.75)
                   )

quinq <- c('1960-1964', '1965-1969', '1970-1974', '1975-1979', '1980-1984', 
           '1985-1989', '1990-1994', '1995-1999', '2000-2004', '2005-2009',
           '2010-2014', '2015-2019', '2020-2024')

tabla1$quinquenio <- quinq[match(tabla1$quinquenio, 1:13)]

tabla1 <- tabla1[with(tabla1, order(tabla1$Área)), c(2, 1, 3, 4, 5, 6, 7, 8, 9)]

tabla1_id_quin <- tabla1 %>% pivot_wider(id_cols = quinquenio, names_from = Área,
                                 values_from = c(sd, promedio, maximo, minimo, q1, median, q3) )

tabla1_id_Área <- tabla1 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadistico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadistico), names_from = quinquenio, values_from = Valor)

t1 <- prod_ban_gr %>%
  summarise(promedio=mean(Valor)) 

t1 <- t1 %>%
  pivot_wider(id_cols = quinquenio, names_from = Área, values_from = promedio )

t1$quinquenio <- quinq[match(t1$quinquenio, 1:13)]

#tabla 2

prod_pin = data_limpia %>%
  filter(Elemento == 'Producción' & Producto == 'Piña tropical')

prod_pin_gr = prod_pin %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Filipinas')) %>%
  mutate(quinquenio = as.integer(cut(Año, breaks = seq(1960, 2025, by = 5)))) %>%
  group_by(quinquenio, Área)

tabla2 = summarise(prod_pin_gr, minimo=min(Valor, na.rm = TRUE), 
                   promedio=mean(Valor), maximo=max(Valor, na.rm = TRUE), 
                   sd=sd(Valor), median=quantile(Valor, 0.5), 
                   q1=quantile(Valor, 0.25), q3=quantile(Valor, 0.75)
)

quinq <- c('1960-1964', '1965-1969', '1970-1974', '1975-1979', '1980-1984', 
           '1985-1989', '1990-1994', '1995-1999', '2000-2004', '2005-2009',
           '2010-2014', '2015-2019', '2020-2024')

tabla2$quinquenio <- quinq[match(tabla2$quinquenio, 1:13)]

tabla2 <- tabla1[with(tabla2, order(tabla2$Área)), c(2, 1, 3, 4, 5, 6, 7, 8, 9)]

tabla2_id_quin <- tabla1 %>% pivot_wider(id_cols = quinquenio, names_from = Área,
                                         values_from = c(sd, promedio, maximo, minimo, q1, median, q3) )

tabla2_id_Área <- tabla2 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadistico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadistico), names_from = quinquenio, values_from = Valor)

t2 <- prod_pin_gr %>%
  summarise(promedio=mean(Valor)) 

t2 <- t2 %>%
  pivot_wider(id_cols = quinquenio, names_from = Área, values_from = promedio )

t2$quinquenio <- quinq[match(t2$quinquenio, 1:13)]


#tabla 3

prod_caf = data_limpia %>%
  filter(Elemento == 'Producción' & Producto == 'Café, verde')

prod_caf_gr = prod_caf %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Viet Nam')) %>%
  mutate(quinquenio = as.integer(cut(Año, breaks = seq(1960, 2025, by = 5)))) %>%
  group_by(quinquenio, Área)

tabla3 = summarise(prod_caf_gr, minimo=min(Valor, na.rm = TRUE), 
                   promedio=mean(Valor), maximo=max(Valor, na.rm = TRUE), 
                   sd=sd(Valor), median=quantile(Valor, 0.5), 
                   q1=quantile(Valor, 0.25), q3=quantile(Valor, 0.75)
)

quinq <- c('1960-1964', '1965-1969', '1970-1974', '1975-1979', '1980-1984', 
           '1985-1989', '1990-1994', '1995-1999', '2000-2004', '2005-2009',
           '2010-2014', '2015-2019', '2020-2024')

tabla3$quinquenio <- quinq[match(tabla3$quinquenio, 1:13)]

tabla3 <- tabla1[with(tabla3, order(tabla3$Área)), c(2, 1, 3, 4, 5, 6, 7, 8, 9)]

tabla3_id_quin <- tabla3 %>% pivot_wider(id_cols = quinquenio, names_from = Área,
                                         values_from = c(sd, promedio, maximo, minimo, q1, median, q3) )

tabla3_id_Área <- tabla3 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadistico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadistico), names_from = quinquenio, values_from = Valor)

t3 <- prod_caf_gr %>%
  summarise(promedio=mean(Valor)) 

t3 <- t3 %>%
  pivot_wider(id_cols = quinquenio, names_from = Área, values_from = promedio )

t3$quinquenio <- quinq[match(t3$quinquenio, 1:13)]

# plot1
options(scipen= 999)
prod_ban_pai <- prod_ban %>%
                  filter(Área %in% c('Costa Rica', 'Colombia', 'Ecuador'))

ggplot(prod_ban_pai, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_boxplot() +
  theme_cowplot(12)

# plot2

prod_Área_caf = data_limpia %>%
  filter(Elemento %in% c('Área cosechada', 'Producción') & Producto == 'Café, verde')

pr_ar_caf_pai <- prod_Área_caf %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Viet Nam')) %>%
  select(Área, Elemento, Valor, Año)

pr_ar_caf_pai <- pr_ar_caf_pai %>% pivot_wider(id_cols = c(Área, Año), names_from = Elemento, values_from = Valor)

ggplot(pr_ar_caf_pai, mapping = aes(x=Año, y=Producción, color=Área)) +
  geom_line() +
  theme_cowplot(12)

# plot3

Área_pin = data_limpia %>%
  filter(Elemento %in% c('Área cosechada') & Producto == 'Piña tropical') %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Filipinas'))

ggplot(Área_pin, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_point() +
  theme_cowplot(12)

############
#Pruebas de Hipotesis 

#Roto las columnas para poder hacer el tes de ANOVA para 
#BANANO

paises_ban<-c("Colombia","Costa Rica","Ecuador")
datosaov_ban<-t1 %>%
 pivot_longer(cols=paises_ban,names_to = "Pais",values_to = "Produccion" )


#Elimino las columnas inecesarias
datosaov_ban<-datosaov_ban%>%
  select(quinquenio,Pais,Produccion)

modelo_anovaBan<-aov(Produccion ~ Pais, data = datosaov_ban)
print(summary(modelo_anovaBan))

###
#Roto las columnas para poder hacer el tes de ANOVA para 
#Cafe
datosaov_caf<-t2 %>%
  pivot_longer(cols=c("Brasil","Costa Rica","Filipinas"),names_to = "Pais",values_to = "Produccion" )

#Elimino las columnas inecesarias
datosaov_caf<-datosaov_caf%>%
  select(quinquenio,Pais,Produccion)

#Modelo ANOVA
modelo_anovaCaf<-aov(Produccion ~ Pais, data = datosaov_caf)
print(summary(modelo_anovaCaf))

###
#Roto las columnas para poder hacer el tes de ANOVA para 
#PiñaTropical
datosaov_pin<-t3 %>%
  pivot_longer(cols=c("Brasil","Costa Rica","Viet Nam"),names_to = "Pais",values_to = "Produccion" )

#Elimino las columnas inecesarias
datosaov_pin<-datosaov_pin%>%
  select(quinquenio,Pais,Produccion)

#Modelo ANOVA
# Realizar la prueba ANOVA
modelo_anovaPin <- aov(Produccion ~ Pais, data = datosaov_pin)
resumen_anovaPin <- summary(modelo_anovaPin)
print(resumen_anovaPin)


#T test
#banano
  
  
  res1_ban<-t.test(t1[["Colombia"]],t1[["Costa Rica"]])
  res2_ban<-t.test(t1[["Colombia"]],t1[["Ecuador"]])  
  res3_ban<-t.test(t1[["Ecuador"]],t1[["Costa Rica"]])  

  print(res1_ban) 
  print(res2_ban) 
  print(res3_ban) 
  
  #Graficos

  
 # resumen_resultado <- tidy(res1_ban)
  # Crear un vector con los nombres de las variables
  #variables <- c("Costa Rica", "Colombia")
  #valores_estimados <- c(resumen_resultado$estimate1, resumen_resultado$estimate2)
  #intervalos_confianza <- c(resumen_resultado$conf.low, resumen_resultado$conf.high)
  # barplot(valores_estimados, 
   #       names.arg = variables, 
    #      ylim = range(intervalos_confianza), 
     #     col = "skyblue", 
      #    main = "Resultados de la prueba t",
       #   xlab = "Variable",
        #  ylab = "Valor estimado")
#  arrows(1:2, intervalos_confianza[1,], 1:2, intervalos_confianza[2,], angle = 90, code = 3, length = 0.1, col = "darkblue")

  
  
  
  
  
  

  #T test
  #cafe
  
  res1_caf<-t.test(t2[["Brasil"]],t2[["Costa Rica"]])
  res2_caf<-t.test(t2[["Filipinas"]],t2[["Brasil"]])  
  res3_caf<-t.test(t2[["Costa Rica"]],t2[["Filipinas"]])  
  
  print(res1_caf) 
  print(res2_caf) 
  print(res3_caf) 

  #ttest
  #Piña
  
  res1_pin<-t.test(t3[["Brasil"]],t3[["Costa Rica"]])
  res2_pin<-t.test(t3[["Brasil"]],t3[["Viet Nam"]])  
  res3_pin<-t.test(t3[["Viet Nam"]],t3[["Costa Rica"]])  
  
  print(res1_pin) 
  print(res2_pin) 
  print(res3_pin)  
  
  