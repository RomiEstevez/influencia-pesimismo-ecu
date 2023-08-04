# Artículo Influencia del pesimismo en la economía y política ecuatoriana
# 2004-2019

# Cargar librerías
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("here", repos = "http://cran.us.r-project.org")
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

# Tabulación con pesos de muestra 
ec_eval_tab <- svyby(formula = ~ ec_eval, 
                   by = ~ year, 
                   design = dm,
                   FUN = svymean,
                   na.rm = T,
                   keep.names = F)

pres_aprov_dic_tab <- svyby(formula = ~ pres_aprov_dic, 
                            by = ~ year, 
                            design = dm,
                            FUN = svymean,
                            na.rm = T,
                            keep.names = F)

# Tema para gráficos de ggplot2
theme_article <-
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(color = "grey20"),
        plot.subtitle = element_text(color = "grey30"),
        plot.caption = element_text(color = "grey30", hjust = 0, face = 'italic'),
        legend.background = element_blank())

# Para graficar ec_eval
graph_sit_pais<-
  ggplot(ec_eval_tab,
         aes(x = as.character(year), y = ec_evalWorse, group = 1))+
  geom_line(size = 1, 
            color = '#2E5994')+
  geom_point(size = 2.15,
             color = '#2E5994')+
  labs(x = '',
       y = '',
       title = 'Porcentaje que opina que la economía del país es peor\nque hace doce meses') +
  theme_article +
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(limits = c(0.1, 1),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                     labels = c('10', '20', '30', '40', '50', '60', '70', '80', '90', '100'))

  ggsave("figures/grafico_situacion_pais.png",plot = graph_sit_pais, 
         device = "png", 
         width = 10, 
         height = 6, 
         dpi = 1200)

# Para graficar pres_aprov_dic
graph_pres_aprov_dic<-
  ggplot(pres_aprov_dic_tab,
           aes(x = as.character(year), y = pres_aprov_dicNo, group = 1))+
  geom_line(size = 1, 
            color = '#2E5994')+
  geom_point(size = 2.15,
             color = '#2E5994')+
  geom_vline(xintercept = 7.5, color = '#5C7C94', linetype = 'dotted')+
  annotate('label', x = 5, y = 0.4, label = 'Gob. de Correa')+
  geom_vline(xintercept = 2.5, color = '#5C7C94', linetype = 'dotted')+
  annotate('label', x = 1.48, y = 0.5, label = 'Gob. de\nGutiérrez-Palacio')+
  annotate('label', x = 8.05, y = 0.5, label = 'Gob. de\nMoreno')+
  labs(x = '',
       y = '',
       title = 'Pocentaje que desaprueba el trabajo del Presidente de turno') +
  theme_article +
  theme(plot.title = element_text(size = 14)) +
  scale_y_continuous(limits = c(0.1, 1),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                     labels = c('10', '20', '30', '40', '50', '60', '70', '80', '90', '100'))
  
ggsave("figures/grafico_desaprobacion_presidente.png",plot = graph_pres_aprov_dic, 
        device = "png", 
        width = 10, 
        height = 6, 
        dpi = 1200)

# Graph en conjunto
caption_graph_conjunto <-
  'Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org. El % que se ve pesimista ante la situación económica del país se calcula para quienes consideran que el escenario actual es peor que hace 12 meses. El % que se ve pesimista ante el trabajo del Ejecutivo se calcula para quienes consideran que el trabajo del Presidente de turno es regular, malo o pésimo.'

graph_conjunto <-
  graph_sit_pais + graph_pres_aprov_dic + 
  plot_layout(ncol = 2) +
  plot_annotation(title = 'Pesimismo económico y político de los ecuatorianos',
                  caption = str_wrap(caption_graph_conjunto, 210),
                  theme = theme(plot.caption = element_text(color = "grey30", hjust = 0, face = 'italic'),
                                plot.title = element_text(hjust = 0.5, face = 'bold', size = 16)))

ggsave("figures/grafico_conjunto.png",plot = graph_conjunto, 
       device = "png", 
       width = 12.5, 
       height = 7, 
       dpi = 1200)  
  