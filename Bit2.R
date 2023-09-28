library(tidyverse)
library(ggplot2)
library(readxl)
library(xtable)
library(cowplot)
# print(xtable(prod_ban_wide2), include.rownames = FALSE)

#data incial
data = read.csv('cultivos_global.csv')
data_limpia = data %>%
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

tabla1_id_area <- tabla1 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadístico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadístico), names_from = quinquenio, values_from = Valor)

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

tabla2_id_area <- tabla2 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadístico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadístico), names_from = quinquenio, values_from = Valor)

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

tabla3_id_area <- tabla3 %>% 
  pivot_longer(cols = c(sd, promedio, maximo, minimo, q1, median, q3),
               names_to = 'Estadístico', values_to = 'Valor') %>%
  pivot_wider(id_cols = c(Área, Estadístico), names_from = quinquenio, values_from = Valor)

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

prod_area_caf = data_limpia %>%
  filter(Elemento %in% c('Área cosechada', 'Producción') & Producto == 'Café, verde')

pr_ar_caf_pai <- prod_area_caf %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Viet Nam')) %>%
  select(Área, Elemento, Valor, Año)

pr_ar_caf_pai <- pr_ar_caf_pai %>% pivot_wider(id_cols = c(Área, Año), names_from = Elemento, values_from = Valor)

ggplot(pr_ar_caf_pai, mapping = aes(x=Año, y=Producción, color=Área)) +
  geom_line() +
  theme_cowplot(12)

# plot3

area_pin = data_limpia %>%
  filter(Elemento %in% c('Área cosechada') & Producto == 'Piña tropical') %>%
  filter(Área %in% c('Costa Rica', 'Brasil', 'Filipinas'))

ggplot(area_pin, mapping = aes(x=Área, y=Valor, color=Área)) +
  geom_point() +
  theme_cowplot(12)
