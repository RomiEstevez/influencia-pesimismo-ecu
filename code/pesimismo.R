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

econ_sit_tab <- svyby(formula = ~ econ_sit, 
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

unem_total_tab <- svyby(formula = ~ unem_total, 
                        by = ~ year, 
                        design = dm,
                        FUN = svymean,
                        na.rm = T,
                        keep.names = F)

pres_conf_dic_tab <- svyby(formula = ~ pres_conf_dic, 
                           by = ~ year, 
                           design = dm,
                           FUN = svymean,
                           na.rm = T,
                           keep.names = F)

# Para jc13, se necesita convertirla en una variable dicotómica.
df$jc13<-ifelse(df$jc13 == 1, 'Yes', 'No') %>% as.factor()

jc13_tab <- svyby(formula = ~ jc13, 
                  by = ~ year, 
                  design = dm,
                  FUN = svymean,
                  na.rm = T,
                  keep.names = F)

# Para b2, se necesita convertirla en una variable dicotómica.
df$b2<-ifelse(df$b2 >= 5, 'Yes', 'No') %>% as.factor()

b2_tab <- svyby(formula = ~ b2, 
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

# Graph Pesimismo económico y político (graph en conjunto)

# Subgráfico 1: ec_eval y econ_sit
# Manejo de datos
sit_econ_pais_df <-
  ec_eval_tab %>%
  select(year, 
         ec_evalWorse, 
         se.ec_evalWorse) %>% 
  rename(perc = ec_evalWorse, 
         se = se.ec_evalWorse) %>% 
  mutate(legend = 'Situación económica del país')

sit_econ_pers_df <-
  econ_sit_tab %>%
  select(year, 
         econ_sitWorse, 
         se.econ_sitWorse) %>% 
  rename(perc = econ_sitWorse, 
         se = se.econ_sitWorse) %>% 
  mutate(legend = 'Situación económica personal')

# Juntar ec_eval y econ_sit
sit_econ_df <-
  bind_rows(sit_econ_pais_df,
            sit_econ_pers_df)
  rownames(sit_econ_df)<-NULL

# Graph pesimismo económico
graph_sit_econ_df <-
  ggplot(sit_econ_df,
         aes(x = as.character(year), y = perc, color = legend, group = legend))+
  geom_line(size = 1)+
  scale_color_manual(values = c('#2E5994','#73A5C6'),
                     breaks = c('Situación económica del país','Situación económica personal'))+
  geom_point(size = 2.15)+
  geom_line(aes(x = as.character(year), 
                y = perc - 1.96*se),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  geom_line(aes(x = as.character(year), 
                y = perc + 1.96*se),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  labs(x = '',
       y = '',
       title = 'Porcentaje que opina que la situación económica del\npaís o su situación personal es peor que hace doce meses') +
  theme_article +
  theme(plot.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.7,0.9)) +
  scale_y_continuous(limits = c(0.1, 1),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                     labels = c('10', '20', '30', '40', '50', '60', '70', '80', '90', '100'))

  ggsave("figures/grafico_pesimismo_econ.png",plot = graph_sit_econ_df, 
         device = "png", 
         width = 10, 
         height = 6, 
         dpi = 1200)

# Subgráfico 2: pres_aprov_dic
graph_pres_aprov_dic<-
  ggplot(pres_aprov_dic_tab,
           aes(x = as.character(year), y = pres_aprov_dicNo, group = 1))+
  geom_line(size = 1, 
            color = '#02826b')+
  geom_point(size = 2.15,
             color = '#02826b')+
  geom_line(aes(x = as.character(year), 
                y = pres_aprov_dicNo - 1.96*se.pres_aprov_dicNo),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  geom_line(aes(x = as.character(year), 
                y = pres_aprov_dicNo + 1.96*se.pres_aprov_dicNo),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  geom_vline(xintercept = 7.5, color = '#5C7C94', linetype = 'dotted')+
  annotate('label', x = 5, y = 0.7, label = 'Gob. de Correa')+
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
  'Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org. El % que se ve pesimista ante la situación económica del país se calcula para quienes consideran que el escenario actual es peor que hace 12 meses. El % que se ve pesimista ante su situación económica se calcula para quienes consideran que su economía actual es peor que hace 12 meses. El % que se ve pesimista ante el trabajo del Ejecutivo se calcula para quienes consideran que el trabajo del Presidente de turno es regular, malo o pésimo. Las líneas punteadas en gris representan los límites inferiores y superiores del intervalo de confianza al 95%.'

graph_conjunto <-
  graph_sit_econ_df + graph_pres_aprov_dic + 
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

# Graph Pesimismo y desempleo
# Manejo de datos
unem_total_tab_df <-
  unem_total_tab %>%
  select(year, 
         unem_total, 
         se) %>% 
  rename(perc = unem_total, 
         se = se) %>% 
  mutate(legend = 'Desempleo') %>% 
  filter(year != 2004, 
         year != 2006)

sit_econ_unem_df <-
  bind_rows(sit_econ_pais_df,
            unem_total_tab_df)
rownames(sit_econ_unem_df)<-NULL

# Graph ec_eval vs unem_total
caption_graph_sit_econ_unem_df <-
  'El % que se ve pesimista ante la situación económica del país se calcula para quienes consideran que el escenario actual es peor que hace 12 meses. El % de desempleo se calcula agrupando a las personas que no tienen trabajo, tanto quienes activamente buscan empleo como quienes no lo hacen. Las líneas punteadas en gris representan los límites inferiores y superiores del intervalo de confianza al 95%. 
  Fuente: El Barómetro de las Américas por el Proyecto de Opinión Pública de América Latina (LAPOP), www.LapopSurveys.org.'

graph_sit_econ_unem_df <-
  ggplot(sit_econ_unem_df,
         aes(x = as.character(year), y = perc, color = legend, group = legend))+
  geom_line(size = 1)+
  scale_color_manual(values = c('#a7ba42','#9DC183'),
                     breaks = c('Situación económica del país','Desempleo'))+
  geom_point(size = 2.15)+
  geom_line(aes(x = as.character(year), 
                y = perc - 1.96*se),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  geom_line(aes(x = as.character(year), 
                y = perc + 1.96*se),
            size = 0.7,
            color = 'grey50', 
            linetype = 'dotted')+
  labs(x = '',
       y = '',
       title = 'Pesimismo Económico vs. Desempleo',
       subtitle = 'Porcentaje de pesimismo ante la economía del país vs. Porcentaje de desempleo',
       caption = str_wrap(caption_graph_sit_econ_unem_df, 175)) +
  theme_article +
  theme(plot.title = element_text(face = 'bold'),
        plot.caption = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = c(0.17,0.3)) +
  scale_y_continuous(limits = c(0, 0.7),
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c(0, '10', '20', '30', '40', '50', '60', '70'))

ggsave("figures/grafico_pesimismo_vs_desempleo.png",plot = graph_sit_econ_unem_df, 
       device = "png", 
       width = 10, 
       height = 6, 
       dpi = 1200)

  