# Bitacora 2, código comentado
# Actualizado por AARC el 10 de oct de 23


# Paquetes para el documento:
library(tidyverse)
library(ggplot2)
library(readxl)
library(xtable)
library(cowplot)

#código para pasar tablas a .tex
# print(xtable(-), include.rownames = FALSE)

#data inicial (se cargan los datos totales)
data = read.csv('cultivos_global.csv')


library(broom)
library(dplyr)

#data incial
cultivos_global <- read_csv("GitHub/Proyecto_Estadistica/cultivos_global.csv")
data = cultivos_global
data_limpia = cultivos_global %>%
          select(c('Área', 'Elemento', 'Producto', 'Año', 'Unidad', 'Valor'))

#seleccionamos las varibles relevantes.
data_limpia = data %>%
          select(c('Área', 'Elemento', 'Producto', 'Año', 'Unidad', 'Valor')) %>%
          mutate(Valor = Valor/10000)


# código para la tabla1
# filtramos los datos para obtener la produccion (prod) de bananos (ban) únicamente
prod_ban = data_limpia %>%
            filter(Elemento == 'Producción' & Producto == 'Bananos')

# filtramos los paises, creamos los quinquenios y agrupamos por quinquenio (en ese orden)
prod_ban_gr = prod_ban %>%
  filter(Área %in% c('Costa Rica', 'Colombia', 'Ecuador')) %>%
  mutate(quinquenio = as.integer(cut(Año, breaks = seq(1960, 2025, by = 5)))) %>%
  group_by(quinquenio, Área)

# creación de la tabla con los datos que queremos
tabla1 = summarise(prod_ban_gr, minimo=min(Valor, na.rm = TRUE), 
                   promedio=mean(Valor), maximo=max(Valor, na.rm = TRUE), 
                   sd=sd(Valor), median=quantile(Valor, 0.5), 
                   q1=quantile(Valor, 0.25), q3=quantile(Valor, 0.75)
                   )

# cambiar el nombre de los quinquenios
quinq <- c('1960-1964', '1965-1969', '1970-1974', '1975-1979', '1980-1984', 
           '1985-1989', '1990-1994', '1995-1999', '2000-2004', '2005-2009',
           '2010-2014', '2015-2019', '2020-2024')
tabla1$quinquenio <- quinq[match(tabla1$quinquenio, 1:13)]

#ordenar las variables de la tabla
tabla1 <- tabla1[with(tabla1, order(tabla1$Área)), c(2, 1, 3, 4, 5, 6, 7, 8, 9)]

#pasar la tabla a formado wide (no se llegó a utilizar pero lo conservo por si lo llegamos a necesitar)
tabla1_id_quin <- tabla1 %>% pivot_wider(id_cols = quinquenio, names_from = Área,
                                 values_from = c(sd, promedio, maximo, minimo, q1, median, q3) )

# pasar la tabla a formado long (la que está en el anexo)
tabla1_id_area <- tabla1 %>% 

tabla1_id_Área <- tabla1 %>%
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadistico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadistico), names_from = quinquenio, values_from = Valor)

# tabla de la bitacora 2 con el promedio
t1 <- prod_ban_gr %>%
  summarise(promedio=mean(Valor)) 

#pasarla a wide
t1 <- t1 %>%
  pivot_wider(id_cols = quinquenio, names_from = Área, values_from = promedio )

#renombra el quinquenio
t1$quinquenio <- quinq[match(t1$quinquenio, 1:13)]

## fin tabla 1 ##

# código para la tabla 2
# filtramos los datos para obtener la produccion (prod) de bananos (ban) únicamente
prod_pin = data_limpia %>%
  filter(Elemento == 'Producción' & Producto == 'Piña tropical')

# filtramos los paises, creamos los quinquenios y agrupamos por quinquenio (en ese orden)
prod_pin_gr = prod_pin %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Filipinas')) %>%
  mutate(quinquenio = as.integer(cut(Año, breaks = seq(1960, 2025, by = 5)))) %>%
  group_by(quinquenio, Área)

# creación de la tabla con los datos que queremos
tabla2 = summarise(prod_pin_gr, minimo=min(Valor, na.rm = TRUE), 
                   promedio=mean(Valor), maximo=max(Valor, na.rm = TRUE), 
                   sd=sd(Valor), median=quantile(Valor, 0.5), 
                   q1=quantile(Valor, 0.25), q3=quantile(Valor, 0.75)
)

# cambiar el nombre de los quinquenios
quinq <- c('1960-1964', '1965-1969', '1970-1974', '1975-1979', '1980-1984', 
           '1985-1989', '1990-1994', '1995-1999', '2000-2004', '2005-2009',
           '2010-2014', '2015-2019', '2020-2024')
tabla2$quinquenio <- quinq[match(tabla2$quinquenio, 1:13)]

#ordenar las variables de la tabla
tabla2 <- tabla2[with(tabla2, order(tabla2$Área)), c(2, 1, 3, 4, 5, 6, 7, 8, 9)]

#pasar la tabla a formado wide (no se llegó a utilizar pero lo conservo por si lo llegamos a necesitar)
tabla2_id_quin <- tabla1 %>% pivot_wider(id_cols = quinquenio, names_from = Área,
                                         values_from = c(sd, promedio, maximo, minimo, q1, median, q3) )

# pasar la tabla a formado long (la que está en el anexo)
tabla2_id_area <- tabla2 %>% 

tabla2_id_Área <- tabla2 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadistico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadistico), names_from = quinquenio, values_from = Valor)

# tabla de la bitacora 2 con el promedio
t2 <- prod_pin_gr %>%
  summarise(promedio=mean(Valor)) 

# pasarla a wide
t2 <- t2 %>%
  pivot_wider(id_cols = quinquenio, names_from = Área, values_from = promedio )

# renombrar el quinquenio
t2$quinquenio <- quinq[match(t2$quinquenio, 1:13)]

## fin tabla 2 ##

# código para la tabla 2
# filtramos los datos para obtener la produccion (prod) de bananos (ban) únicamente
prod_caf = data_limpia %>%
  filter(Elemento == 'Producción' & Producto == 'Café, verde')

# filtramos los paises, creamos los quinquenios y agrupamos por quinquenio (en ese orden)
prod_caf_gr = prod_caf %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Viet Nam')) %>%
  mutate(quinquenio = as.integer(cut(Año, breaks = seq(1960, 2025, by = 5)))) %>%
  group_by(quinquenio, Área)

# creación de la tabla con los datos que queremos
tabla3 = summarise(prod_caf_gr, minimo=min(Valor, na.rm = TRUE), 
                   promedio=mean(Valor), maximo=max(Valor, na.rm = TRUE), 
                   sd=sd(Valor), median=quantile(Valor, 0.5), 
                   q1=quantile(Valor, 0.25), q3=quantile(Valor, 0.75)
)

# cambiar el nombre de los quinquenios
quinq <- c('1960-1964', '1965-1969', '1970-1974', '1975-1979', '1980-1984', 
           '1985-1989', '1990-1994', '1995-1999', '2000-2004', '2005-2009',
           '2010-2014', '2015-2019', '2020-2024')
tabla3$quinquenio <- quinq[match(tabla3$quinquenio, 1:13)]

#ordenar las variables de la tabla
tabla3 <- tabla3[with(tabla3, order(tabla3$Área)), c(2, 1, 3, 4, 5, 6, 7, 8, 9)]

#pasar la tabla a formado wide (no se llegó a utilizar pero lo conservo por si lo llegamos a necesitar)
tabla3_id_quin <- tabla3 %>% pivot_wider(id_cols = quinquenio, names_from = Área,
                                         values_from = c(sd, promedio, maximo, minimo, q1, median, q3) )

# pasar la tabla a formado long (la que está en el anexo)
tabla3_id_Área <- tabla3 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadistico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadistico), names_from = quinquenio, values_from = Valor)

# tabla de la bitacora 2 con el promedio
t3 <- prod_caf_gr %>%
  summarise(promedio=mean(Valor)) 

# pasarla a wide
t3 <- t3 %>%
  pivot_wider(id_cols = quinquenio, names_from = Área, values_from = promedio )

#renombrar el quinquenio
t3$quinquenio <- quinq[match(t3$quinquenio, 1:13)]

## fin tabla 3 ##

### INICIO CON LOS PLOTS ###

# para evitar la notación científica.
options(scipen= 999)
#Dimensiones para todos los gráficos 
h<-5
w<-h*1.6

# plot 1
prod_ban_pai <- prod_ban %>%
                  filter(Área %in% c('Costa Rica', 'Colombia', 'Ecuador'))

ggplot(prod_ban_pai, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_boxplot() +
  labs(x = "País",
       y = "Producción",
       title=" ",
       color = 'País')+ 
  theme_cowplot(12)

ggsave(filename = "BoxPlot.pdf", width = w, height = h, plot = last_plot())

# plot2

prod_Área_caf = data_limpia %>%
  filter(Elemento %in% c('Área cosechada', 'Producción') & Producto == 'Café, verde')

pr_ar_caf_pai <- prod_Área_caf %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Viet Nam')) %>%
  select(Área, Elemento, Valor, Año)

pr_ar_caf_pai <- pr_ar_caf_pai %>% pivot_wider(id_cols = c(Área, Año), names_from = Elemento, values_from = Valor)

ggplot(pr_ar_caf_pai, mapping = aes(x=Año, y=Producción, color=Área)) +
  geom_line(linewidth=1) +
  labs(x = "País",
       y = "Producción",
       title=" ",
       color = 'País')+ 
  theme_cowplot(12)

ggsave(filename = "LinePlot.pdf", width = w, height = h, plot = last_plot())

# plot3

Área_pin = data_limpia %>%
  filter(Elemento %in% c('Área cosechada') & Producto == 'Piña tropical') %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Filipinas'))

ggplot(area_pin, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_point(size=1.5) +
  labs(x = "País",
       y = "Producción",
       title=" ",
       color = 'País')+
  theme_cowplot(12)

ggsave(filename = "PointPlot.pdf", width = w, height = h, plot = last_plot())


## ANTEPROYECTO
# plots
options(scipen= 999)
#Dimensiones para todos los gráficos 
h<-5
w<-h*1.6

# plot 1
prod_ban_pai <- prod_ban %>%
  filter(Área %in% c('Costa Rica', 'Colombia', 'Ecuador'))

ggplot(prod_ban_pai, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_boxplot() +
  labs(x = "País",
       y = "Producción",
       title=" ",
       color = 'País')+ 
  theme_cowplot(12)

ggsave(filename = "BoxPlotBan.pdf", width = w, height = h, plot = last_plot())

# plot 2
prod_pin_pai <- prod_pin %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Filipinas'))

ggplot(prod_pin_pai, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_boxplot() +
  labs(x = "País",
       y = "Producción",
       title=" ",
       color = 'País')+ 
  theme_cowplot(12)

ggsave(filename = "BoxPlotPin.pdf", width = w, height = h, plot = last_plot())

# plot 3
prod_caf_pai <- prod_caf %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Viet Nam'))

ggplot(prod_caf_pai, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_boxplot() +
  labs(x = "País",
       y = "Producción",
       title=" ",
       color = 'País')+ 
  theme_cowplot(12)

ggsave(filename = "BoxPlotCaf.pdf", width = w, height = h, plot = last_plot())

ggplot(Área_pin, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_point() +
  theme_cowplot(12)

############
#Pruebas de Hipotesis 
#ANOVA

#Se rotan las columnas para poder hacer el test del 
#BANANO

paises_ban<-c("Colombia","Costa Rica","Ecuador")
datosaov_ban<-t1 %>%
 pivot_longer(cols=paises_ban,names_to = "Pais",values_to = "Produccion" )


#Se eliminan las columnas inecesarias
datosaov_ban<-datosaov_ban%>%
  select(quinquenio,Pais,Produccion)

#Se aplica la funcion de R aov que aplica el metodo ANOVA
modelo_anovaBan<-aov(Produccion ~ Pais, data = datosaov_ban)

#Imprime un resumen de los resultaos del metodo ANOVA
print(summary(modelo_anovaBan))

###
#Roto las columnas para poder hacer el test de ANOVA para 
#Cafe
datosaov_caf<-t2 %>%
  pivot_longer(cols=c("Brasil","Costa Rica","Filipinas"),names_to = "Pais",values_to = "Produccion" )

#Se elimina las columnas inecesarias
datosaov_caf<-datosaov_caf%>%
  select(quinquenio,Pais,Produccion)

#Se aplica la funcion de R aov que aplica el metodo ANOVA
modelo_anovaCaf<-aov(Produccion ~ Pais, data = datosaov_caf)
#Se imprime un resumen de los resultaos del metodo ANOVA
print(summary(modelo_anovaCaf))

###
#Se rotan las columnas para poder hacer el tes de ANOVA para 
#PiñaTropical
datosaov_pin<-t3 %>%
  pivot_longer(cols=c("Brasil","Costa Rica","Viet Nam"),names_to = "Pais",values_to = "Produccion" )

#Se eliminan las columnas inecesarias
datosaov_pin<-datosaov_pin%>%
  select(quinquenio,Pais,Produccion)

#Modelo ANOVA
# Realizar la prueba ANOVA
modelo_anovaPin <- aov(Produccion ~ Pais, data = datosaov_pin)

#Imprime un resumen de los resultaos del metodo ANOVA
resumen_anovaPin <- summary(modelo_anovaPin)
print(resumen_anovaPin)


#T test
#banano
  
  #Se va a comparar comparar cada uno de los productores de bananno selecionados 
  #Se entiendo que el t-test solo compara dos bases de datos 
  #Se inicia compararando las base de datos de Colombia y Costa Rica
  res1_ban<-t.test(t1[["Colombia"]],t1[["Costa Rica"]])
  #Colombia y Ecuador
  res2_ban<-t.test(t1[["Colombia"]],t1[["Ecuador"]])  
  #Ecuador y Costa Rica
  res3_ban<-t.test(t1[["Ecuador"]],t1[["Costa Rica"]])  

  #Se imprimen los resultados 
  print(res1_ban) 
  print(res2_ban) 
  print(res3_ban) 
  
  #Se comprende que el wilcox Test compara los datos de la misma manera que el Ttest 
  #Se compara dos bases de datos 
  #Colombia y Costa Rica
  wil1_ban<-wilcox.test(t1[["Colombia"]],t1[["Costa Rica"]],alternative="two.sided")
  #Colombia y Ecuador 
  wil2_ban<-wilcox.test(t1[["Colombia"]],t1[["Ecuador"]],alternative="two.sided")
  #Ecuador y Costa Rica 
  wil3_ban<-wilcox.test(t1[["Ecuador"]],t1[["Costa Rica"]],alternative="two.sided")
  
  #se imprime los reultados resumidos en un df
  resultados_ban <- data.frame(
    Prueba = c("Colombia vs. Costa Rica", "Colombia vs. Ecuador", "Ecuador vs. Costa Rica"),
    Estadistica_U = c(wil1_ban$statistic, wil2_ban$statistic, wil3_ban$statistic),
    Valor_p = c(wil1_ban$p.value, wil2_ban$p.value, wil3_ban$p.value)
  )
  
  print(resultados)
  
  
  #Se va a realizar el mismo procedimiento que se realizo con el banano con el producto del cafe 
  #T test
  
  #Brasil vs Costa Rica
  res1_caf<-t.test(t2[["Brasil"]],t2[["Costa Rica"]])
  #filipinas vs Brasil
  res2_caf<-t.test(t2[["Filipinas"]],t2[["Brasil"]])  
  #Costa Rica vs Filipinas
  res3_caf<-t.test(t2[["Costa Rica"]],t2[["Filipinas"]])  
  
  #imprime los resultados 
  print(res1_caf) 
  print(res2_caf) 
  print(res3_caf) 

  
 #Brasil vs Costa Rica
  wil1_caf<-wilcox.test(t2[["Brasil"]],t2[["Costa Rica"]],alternative = "two.sided")
  #Filipinas vs Brasil
  wil2_caf<-wilcox.test(t2[["Filipinas"]],t2[["Brasil"]],alternative = "two.sided")  
  #Costa Rica vs Filipinas
  wil3_caf<-wilcox.test(t2[["Costa Rica"]],t2[["Filipinas"]],alternative = "two.sided")  
  
  #imprime los resultados de manera resumida
  resultados_caf <- data.frame(
    Prueba = c("Brasil vs. Costa Rica", "Filipinas vs. Brasil", "Costa Rica vs. Filipinas"),
    Estadistica_U = c(wil1_caf$statistic, wil2_caf$statistic, wil3_caf$statistic),
    Valor_p = c(wil1_caf$p.value, wil2_caf$p.value, wil3_caf$p.value)
  )
  
  print(resultados_caf)
  
  #Se va a realizar el mismo procedimiento que se realizo con el banano y el cafe  con el producto de la piña 
  #T test
  
  #Brasil vs Costa Rica
  res1_pin<-t.test(t3[["Brasil"]],t3[["Costa Rica"]])
  #Brasil vs Vietnam
  res2_pin<-t.test(t3[["Brasil"]],t3[["Viet Nam"]])  
  #vietnam vs Costa Rica
  res3_pin<-t.test(t3[["Viet Nam"]],t3[["Costa Rica"]])  
  
  #imprime los resultados
  print(res1_pin) 
  print(res2_pin) 
  print(res3_pin) 
  
  #Wilcox test
  #Brasil vs Costa Rica
  wil1_pin<-wilcox.test(t3[["Brasil"]],t3[["Costa Rica"]],alternative = "two.sided")
  #Brasil vs Vietnam
  wil2_pin<-wilcox.test(t3[["Brasil"]],t3[["Viet Nam"]],alternative = "two.sided")  
  #vietnam vs Costa Rica
  wil3_pin<-wilcox.test(t3[["Viet Nam"]],t3[["Costa Rica"]],alternative = "two.sided")  
  #imprime los resultados de manera resumida
  resultados_pin <- data.frame(
    Prueba = c("Brasil vs. Costa Rica", "Brasil vs. Viet Nam", "Viet Nam vs. Costa Rica"),
    Estadistica_U = c(wil1_pin$statistic, wil2_pin$statistic, wil3_pin$statistic),
    Valor_p = c(wil1_pin$p.value, wil2_pin$p.value, wil3_pin$p.value)
  )
  print(resultados_pin)
  