# Artículo Influencia del pesimismo en la economía y política ecuatoriana
# 2004-2019

# Cargar librerías
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")

# Cargar datos

url <- "https://raw.githubusercontent.com/laboratoriolide/americas-barometer/main/output/csv/ab_04_09.csv"

download.file(url, here("data/ab_04_19.csv"))

df <- read.csv("data/ab_04_19.csv")


# Diseño Muestral 
dm <- svydesign(ids = ~ upm,
                strata = ~ estratopri, 
                weights = ~ weight1500, 
                nest = TRUE,
                na.action = 'na.exclude',
                data = df)

# Tabulación con pesos de muestra ec_eval
ec_eval_tab <- svyby(formula = ~ ec_eval, 
                   by = ~ year, 
                   design = dm,
                   FUN = svymean,
                   na.rm = T,
                   keep.names = F)

# Para graficar ec_eval
      ec_eval_tab_df <- as.data.frame(ec_eval_tab)

      ec_eval_tab_df<-subset(ec_eval_tab_df, ec_eval_tab_df$ec_evalWorse=='Worse') %>% 
          select('ec_evalSame or Better', 'year')

graph_sit_pais<-
  ggplot(ec-eval_tab_df,
         aes(x = year, y = perc, color = legend, group = legend))+
  geom_line(size = 0.8)+
  scale_color_manual(values = c('#61346B','#BF69C2'),
                     breaks = c('Confía en el Presidente','Aprueba el trabajo del Presidente'))+
  geom_point(size = 2.15)+
  geom_line(aes(x = year, 
                y = perc - 1.96*se),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  geom_line(aes(x = year, 
                y = perc + 1.96*se),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')