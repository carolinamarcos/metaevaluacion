library(tidyverse)
library(janitor)
library(stringr)
library(ggtext)
library(ggrepel)


# Paleta de colores -------------------------------------------------------

#66c2a5 # verde oscuro
#a6d854 # verde
#fce162 # amarillo
#fc8d62 # naranja
#b3b3b3 # gris


# Importar datos ----------------------------------------------------------

metaevaluacion <- read_csv2("datos/metaevaluacion.csv")
metaevaluacion <- clean_names(metaevaluacion)
  
metaevaluacion <- metaevaluacion %>% 
  rename("cali" = "cali_que_rasgos_generales_presenta_la_evaluacion",
         "x0_ongd_a" = "x0a_ongd_la_ongd_es_sensible_al_genero_cuenta_con_personal_especializado_en_igualdad_de_genero",
         "x0_ongd_b" = "x0a_ongd_la_ongd_es_sensible_al_genero_cuenta_con_personal_especializado_en_igualdad_con_enfoque_interseccional",
         "x0_ongd_c" = "x0a_ongd_la_ongd_es_sensible_al_genero_ha_adoptado_una_politica_interna_de_evaluacion_que_contempla_aspectos_de_igualdad_de_genero",
         "x0_ongd_d" = "x0a_ongd_la_ongd_es_sensible_al_genero_ha_adoptado_una_politica_interna_de_evaluacion_que_contempla_aspectos_con_enfoque_interseccional",
         "x0_ongd_e" = "x0a_ongd_la_ongd_es_sensible_al_genero_a_la_hora_de_calificar_las_propuestas_la_competencia_del_equipo_en_materia_de_igualdad_de_genero_es_valorada_positivamente",
         "x0_ongd_f" = "x0a_ongd_la_ongd_es_sensible_al_genero_a_la_hora_de_calificar_las_propuestas_la_competencia_del_equipo_en_materia_de_igualdad_con_enfoque_interseccional_es_valorada_positivamente",
         "x0_nior" = "x0_nior_los_td_r_incluyen_preguntas_referidas_especificamente_a_cuestiones_de_igualdad_interseccional",
         "x0_tiem_a" = "x0b_tiem_cuanto_tiempo_se_ha_previsto_en_el_cronograma_para_realizar_la_evaluacion_no_de_semanas",
         "x0_tiem_b" = "x0b_tiem_y_especificamente_al_trabajo_de_campo_no_de_semanas", 
         "x0_tiem_c" = "x0b_tiem_se_ha_concecido_alguna_prorroga",
         "x0_tiem_d" = "x0b_tiem_con_cuanta_duracion_no_de_semanas",
         "x0_pres_a" = "x0c_pres_cual_es_el_presupuesto_destinado_a_la_intervencion",
         "x0_pres_b" = "x0c_pres_cual_es_el_presupuesto_destinado_a_la_evaluacion",
         "x0_pres_c" = "x0c_pres_cual_es_el_coste_de_la_hora_de_trabajo_efectivo_de_los_as_evaluadoras",
         "x0_equi_a" = "x0_equi_cuantos_miembros_contiene_el_equipo_evaluador",
         "x0_equi_b" = "x0_equi_cuantos_de_los_miembros_del_equipo_evaluador_cuentan_con_formacion_o_experiencia_en_perspectiva_de_genero_formacion_formal_y_acreditada_en_perspectiva_de_igualdad_de_genero",
         "x0_equi_c" = "x0_equi_cuantos_de_los_miembros_del_equipo_evaluador_cuentan_con_formacion_o_experiencia_en_perspectiva_de_genero_formacion_formal_y_acreditada_en_otros_enfoques_interseccionales",
         "x0_equi_d" = "x0_equi_cuantos_de_los_miembros_del_equipo_evaluador_cuentan_con_formacion_o_experiencia_en_perspectiva_de_genero_experiencia_en_evaluaciones_y_o_proyectos_con_perspectiva_de_igualdad_genero",
         "x0_equi_e" = "x0_equi_cuantos_de_los_miembros_del_equipo_evaluador_cuentan_con_formacion_o_experiencia_en_perspectiva_de_genero_experiencia_en_evaluaciones_y_o_proyectos_con_otros_enfoques_interseccionales",
         "x1_prop_a" = "x1_prop_el_proposito_de_la_evaluacion_alude_a_algun_aspecto_relacionado_con_la_igualdad",
         "x1_prop_b" = "x1_prop_a_cual",
         "x1_cxto_a" = "x1_cxto_el_informe_analiza_el_contexto_de_la_igualdad_marco_normativo_o_politicas_nacionales",
         "x1_cxto_b" = "x1_cxto_el_informe_analiza_el_contexto_de_la_igualdad_posicionamiento_de_socios_locales_y_organizaciones_comunitarias",
         "x1_cxto_c" = "x1_cxto_el_informe_analiza_el_contexto_de_la_igualdad_actitudes_y_comportamientos_individuales",
         "x1_acto" = "x1_acto_el_informa_analiza_los_actores_implicados",
         "x3_nigi_a" = "x3_nigi_si_los_td_r_contemplaban_preguntas_con_enfoque_de_igualdad_de_genero_interseccional_la_propuesta",
         "x3_nigi_b" = "x3_nigi_si_los_td_r_no_contemplaban_preguntas_con_enfoque_de_igualdad_de_genero_interseccional_la_propuesta",
         "x3_nige" = "x3_nige_el_analisis_contempla_a",
         "x3_nicg" = "x3_nicg_en_el_caso_de_que_existan_preguntas_referidas_especificamente_a_cuestiones_de_igualdad_de_genero_en_que_cuestiones_clave_de_genero_indicen",
         "x3_niin" = "x3_niin_el_analisis_incorpora_enfoques_interseccionales",
         "x4_ddrg_a" = "x4_ddrg_los_datos_recogidos_estan_desagregados_por_genero",
         "x4_ddrg_b" = "x4_ddrg_que_categorias_emplean",
         "x4_ddri_a" = "x4_ddri_los_datos_recogidos_estan_desagregados_por_otras_variables",
         "x4_ddri_b" = "x4_ddri_por_cuales",
         "x4_dddg_a" = "x4_dddg_los_datos_disponibles_estan_desagregados_por_genero",
         "x4_dddg_b" = "x4_dddg_en_el_caso_de_que_solo_a_veces_o_nunca_se_ha_incorporado_a_posteriori_en_la_evaluacion_el_enfoque_de_genero",
         "x4_dddi_a" = "x4_dddi_los_datos_disponibles_estan_desagregados_por_otras_variables",
         "x4_dddi_b" = "x4_dddi_por_cuales",
         "x4_dddi_c" = "x4_dddi_en_el_caso_de_que_solo_a_veces_o_nunca_se_ha_incorporado_a_posteriori_en_la_evaluacion_el_enfoque_interseccional",
         "x4_efne" = "x4_efne_se_exploran_efectos_no_esperados",
         "x5_mixt" = "x5_mixt_se_emplean_tecnicas",
         "x6_parn_a" = "x6_parn_que_porcentaje_de_hombres_mujeres_han_participado_en_el_proyecto",
         "x6_parn_b" = "x6_parn_que_porcentaje_de_hombres_mujeres_participan_en_la_evaluacion",
         "x7_efec" = "x7_efec_se_analizan_los_efectos_del_programa_de_forma_desagregada",
         "x7_tria" = "x7_tria_se_triangulan_los_datos",
         "x8_raiz_a" = "x8_raiz_se_explican_la_raiz_de_las_diferencias_detectadas_desde_un_punto_de_vista_de_genero",
         "x8_raiz_b" = "x8_raiz_se_explican_la_raiz_de_las_diferencias_detectadas_desde_un_punto_de_vista_interseccional",
         "x8_mplu" = "x8_mplu_presenta_una_vision_plural_de_las_mujeres",
         "x8_memp" = "x8_memp_presenta_una_vision_empoderada_de_las_mujeres",
         "x8_mnse" = "x8_mnse_presenta_una_vision_no_sexista_de_las_mujeres",
         "x9_judi" = "x9_judi_se_valora_si_el_diseno_del_proyecto_incluye_enfoque_de_igualdad",
         "x9_jupr" = "x9_jupr_se_valora_si_el_proyecto_incorpora_consideraciones_de_igualdad_en_sus_procesos", 
         "x9_jure" = "x9_jure_se_valora_si_los_cambios_producidos_por_el_proyecto_son_positivos_neutrales_o_negativos_desde_un_punto_de_vista_de_la_igualdad",
         "x10_reco" = "x10_reco_se_sugieren_recomendaciones_para_avanzar_en_terminos_de_igualdad",
         "x11_nose_a" = "x11_nose_en_el_informe_y_otros_productos_comunicativos_se_emplea_un_lenguaje_inclusivo_y_no_sexista",
         "x11_nose_b" = "x11_nose_en_el_informe_y_otros_productos_comunicativos_no_se_emplean_fotografias_de_mujeres_exclusivamente_ejerciendo_roles_tradicionales",
         "x11_nose_c" = "x11_nose_en_el_informe_y_otros_productos_comunicativos_el_uso_de_los_colores_e_iconografia_en_los_graficos_no_es_sexista"
)

total_bd <- nrow(metaevaluacion)

# Analizar preguntas de la técnica (vaciado de informacion) ---------------

# ongd
ongd <- metaevaluacion %>%
  select(x0_ongd_a:x0_ongd_f) %>% 
  gather(respuesta, indicador, x0_ongd_a:x0_ongd_f) %>% 
  mutate(respuesta = case_when(respuesta == "x0_ongd_a" ~ "Personal especializado en igualdad de género",
                               respuesta == "x0_ongd_b" ~ "Personal especializado en igualdad interseccional",
                               respuesta == "x0_ongd_c" ~ "Política con enfoque de igualdad de género",
                               respuesta == "x0_ongd_d" ~ "Política con enfoque de igualdad interseccional",
                               respuesta == "x0_ongd_e" ~ "Valoran propuestas con igualdad de género",
                               respuesta == "x0_ongd_f" ~ "Valoran propuestas con igualdad interseccional")) %>% 
  group_by(indicador) %>% 
  mutate(respuesta = fct_relevel(respuesta,
                                 "Valoran propuestas con igualdad interseccional",
                                 "Valoran propuestas con igualdad de género",
                                 "Política con enfoque de igualdad interseccional",
                                 "Política con enfoque de igualdad de género",
                                 "Personal especializado en igualdad interseccional",
                                 "Personal especializado en igualdad de género")) %>% 
  
  count(respuesta)

ongd_g <- ggplot(ongd, aes(respuesta, n, fill = indicador)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) + 
  coord_flip() +
  scale_x_discrete(labels = str_wrap(c("Valoran propuestas con igualdad interseccional",
                                       "Valoran propuestas con igualdad de género",
                                       "Política con enfoque de igualdad interseccional",
                                       "Política con enfoque de igualdad de género",
                                       "Personal especializado en igualdad interseccional",
                                       "Personal especializado en igualdad de género"),
                                     width = 30)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  scale_fill_manual(values=c("Sí" = "#a6d854",
                             "No" = "#fc8d62",
                             "No se sabe" = "#b3b3b3"),
                    name="") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = str_wrap("Posicionamiento de la ONGD respecto a la igualdad", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 1")


# nior

nior <- metaevaluacion %>% 
  select(x0_nior) %>% 
  mutate(x0_nior = fct_relevel(x0_nior,
                               "No se sabe",
                               "No",
                               "Sí, sobre igualdad de género",
                               "Sí, sobre igualdad interseccional")) %>% 
  count(x0_nior) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

nior_g <- ggplot(nior, aes(x0_nior, n)) +
  geom_col(fill = c("#b3b3b3", "#fc8d62", "#a6d854", "#66c2a5")) +
  coord_flip() +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
                          position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x =element_blank()) +
  scale_y_continuous(breaks = c(1:20), minor_breaks = NULL) +
  labs(title = str_wrap("Las preguntas propuestas por las ONGD incorporan la igualdad", 40),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 2")


# tiem

tiem <- metaevaluacion %>%
  select(x0_tiem_a, x0_tiem_b, x0_tiem_d) %>% 
  mutate(x0_tiem_a = case_when(x0_tiem_a != "NS" ~ x0_tiem_a), # los que sí NS > NA
         x0_tiem_b = case_when(x0_tiem_b != "NS" ~ x0_tiem_b),
         x0_tiem_d = case_when(x0_tiem_d != "NS" ~ x0_tiem_d)) %>% 
  mutate_if(is.character, as.double) %>% 
  mutate(linea_b = 1,
         linea_a = 2)
  #rename("Meses previstos en total" = x0_tiem_a,
         #"Meses previstos para el trabajo de campo" = x0_tiem_b)

tiem_sum <- tiem %>%
  summarise("Total. Media" = mean(x0_tiem_a, na.rm = TRUE),
            "Total. Mínimo" = min(x0_tiem_a, na.rm = TRUE),
            "Total. Máximo" = max(x0_tiem_a, na.rm = TRUE),
            "Campo. Media" = mean(x0_tiem_b, na.rm = TRUE),
            "Campo. Mínimo" = min(x0_tiem_b, na.rm = TRUE),
            "Campo. Máximo" = max(x0_tiem_b, na.rm = TRUE),
            "Prorroga. Media" = mean(x0_tiem_d, na.rm = TRUE),
            "Prorroga. Mínimo" = min(x0_tiem_d, na.rm = TRUE),
            "Prorroga. Máximo" = max(x0_tiem_d, na.rm = TRUE)) %>% 
  print()
  
metaevaluacion %>% 
  count(x0_tiem_c) %>% # prorroga si o no
  mutate(porcentaje = n/sum(n))

tiem_g <- ggplot(tiem) +
  geom_boxplot(aes(x0_tiem_a, linea_a)) +
  geom_point(aes(x0_tiem_a, linea_a)) +
  geom_boxplot(aes(x0_tiem_b, linea_b)) +
  geom_point(aes(x0_tiem_b, linea_b)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks = c(1:13),
                     minor_breaks = NULL) +
  annotate("text", x = 2, y = 2, label = "En total") +
  annotate("text", x = 11, y = 1, label = "Trabajo de campo") +
  labs(title = str_wrap("Semanas previstas para la evaluación", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 3")


# pres

pres <- metaevaluacion %>% 
  select(x0_pres_a: x0_pres_b) %>% 
  mutate_if(is.character, as.double) %>% 
  mutate(porcentaje = (x0_pres_b) / x0_pres_a,
         linea = 1)

pres %>%
  summarise(Media = mean(x0_pres_b, na.rm = TRUE),
            Mínimo = min(x0_pres_b, na.rm = TRUE),
            Máximo = max(x0_pres_b, na.rm = TRUE))

metaevaluacion %>% 
  mutate_if(is.character, as.double) %>%
  group_by(x0_equi_a) %>%
  summarise(Media = mean(x0_pres_b, na.rm = TRUE))

pres %>% 
  summarise(mean(porcentaje, na.rm = TRUE))
  
pres_g <- ggplot(pres, aes(porcentaje, linea)) +
  geom_boxplot() +
  geom_point() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(limits = c(0,2)) +
  scale_x_continuous(limits = c(0,0.06),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = str_wrap("Porcentaje del presupuesto de la evaluación respecto al coste total del proyecto", 50),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 4")

metaevaluacion %>% 
  count(x0_pres_c)


# equi

equi <- metaevaluacion %>% 
  select(x0_equi_a:x0_equi_e) %>% 
  mutate(x0_equi_a = case_when(x0_equi_a == "1" ~ 1,
                               x0_equi_a == "2" ~ 2,
                               x0_equi_a == "3" ~ 3,
                               x0_equi_a == "4" ~ 4),
         x0_equi_b = case_when(x0_equi_b == "Ningún miembro" ~ 0,
                               x0_equi_b == "1 miembro" ~ 1,
                               x0_equi_b == "2 miembros" ~ 2,
                               x0_equi_b == "3 miembros" ~ 3,
                               x0_equi_b == "4 miembros" ~ 4),
         x0_equi_c = case_when(x0_equi_c == "Ningún miembro" ~ 0,
                               x0_equi_c == "1 miembro" ~ 1,
                               x0_equi_c == "2 miembros" ~ 2,
                               x0_equi_c == "3 miembros" ~ 3,
                               x0_equi_c == "4 miembros" ~ 4),
         x0_equi_d = case_when(x0_equi_d == "Ningún miembro" ~ 0,
                               x0_equi_d == "1 miembro" ~ 1,
                               x0_equi_d == "2 miembros" ~ 2,
                               x0_equi_d == "3 miembros" ~ 3,
                               x0_equi_d == "4 miembros" ~ 4),
         x0_equi_e = case_when(x0_equi_e == "Ningún miembro" ~ 0,
                               x0_equi_e == "1 miembro" ~ 1,
                               x0_equi_e == "2 miembros" ~ 2,
                               x0_equi_e == "3 miembros" ~ 3,
                               x0_equi_e == "4 miembros" ~ 4)) %>% 
  mutate("Formación en igualdad de género" = x0_equi_b / x0_equi_a,
         "Formación en igualdad interseccional"  = x0_equi_c / x0_equi_a,
         "Experiencia en evaluaciones con enfoque de igualdad de género" = x0_equi_d / x0_equi_a,
         "Experiencia en evaluaciones con enfoque de igualdad interseccional" = x0_equi_e / x0_equi_a)

equi_orde <- equi %>%  
  gather(indicador, respuesta, "Formación en igualdad de género":"Experiencia en evaluaciones con enfoque de igualdad interseccional") %>% 
  mutate(indicador = fct_relevel(indicador,
                                 "Experiencia en evaluaciones con enfoque de igualdad interseccional",
                                 "Experiencia en evaluaciones con enfoque de igualdad de género",
                                 "Formación en igualdad interseccional",
                                 "Formación en igualdad de género")) %>% 
  select(-c(1:5)) %>% 
  group_by(indicador) %>% 
  summarise(media = mean(respuesta, na.rm = TRUE))


equi_g <- ggplot(equi_orde, aes(indicador, media)) +
  geom_col(fill = c("#66c2a5",
                    "#a6d854",
                    "#66c2a5",
                    "#a6d854")) +
  geom_text(aes(label = paste0(trunc(media*100), "%")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_minimal() +
  scale_x_discrete(labels = str_wrap(c("Experiencia en evaluaciones con enfoque de igualdad interseccional",
                                       "Experiencia en evaluaciones con enfoque de igualdad de género",
                                       "Formación en igualdad interseccional",
                                       "Formación en igualdad de género"),
                                     width = 23)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent_format(accuracy = NULL)) +
  labs(title = str_wrap("Capacidades del equipo evaluador", 60),
       subtitle = "% de miembros del equipo con...",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 5")


# prop

prop <- metaevaluacion %>% 
  select(x1_prop_a) %>% 
  count(x1_prop_a) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

prop_g <- ggplot(prop, aes(x1_prop_a, n)) +
  geom_col(fill = c("#fc8d62","#a6d854")) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.y =element_blank()) +
  labs(title = str_wrap("El propósito de la evaluación está formulado en términos de igualdad", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 6")

metaevaluacion %>% 
  count(x1_prop_b)


# cxto

cxto <- metaevaluacion %>% 
  select(x1_cxto_a:x1_cxto_c) %>% 
  gather(indicador, respuesta, x1_cxto_a:x1_cxto_c) %>% 
  mutate(indicador = case_when(indicador == "x1_cxto_a" ~ "Marco normativo o políticas nacionales",
                               indicador == "x1_cxto_b" ~ "Posicionamiento de socios locales y organizaciones comunitarias",
                               indicador == "x1_cxto_c" ~ "Actitudes y comportamientos individuales"),
         fct_relevel(indicador,
                     "Actitudes y comportamientos individuales",
                     "Posicionamiento de socios locales y organizaciones comunitarias",
                     "Marco normativo o políticas nacionales")) %>%
  group_by(indicador) %>% 
  count(respuesta)

cxto_g <- ggplot(cxto, aes(indicador, n, fill = respuesta)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) + 
  coord_flip() +
  scale_fill_manual(values=c("Sí" = "#a6d854",
                             "No" = "#fc8d62",
                             "NA" = "#b3b3b3"),
                    name="") +
  scale_x_discrete(labels = str_wrap(c("Actitudes y comportamientos individuales",
                                       "Marco normativo o políticas nacionales",
                                       "Posicionamiento de socios locales y organizaciones comunitarias"),
                                     width = 30)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = str_wrap("Análisis del contexto de la igualdad", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 7")


# acto

acto <- metaevaluacion %>% 
  select(x1_acto) %>% 
  mutate(x1_acto = fct_relevel(x1_acto,
                               "No",
                               "Sí, se mencionan",
                               "Sí, de forma somera",
                               "Sí, en profundidad")) %>%
  count(x1_acto) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

acto_g <- ggplot(acto, aes(x1_acto, n)) +
  geom_col(fill = c("#fc8d62","#fce162", "#a6d854", "#66c2a5", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x =element_blank()) +
  coord_flip() +
  labs(title = str_wrap("Los actores implicados son analizados en la evaluación", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 8")


# nigi

nigi_si <- metaevaluacion %>% 
  select(x3_nigi_a) %>% 
  mutate(x3_nigi_a = fct_relevel(x3_nigi_a,
                                 "Las incluye y las amplía",
                                 "Las incluye sin ampliarlas")) %>% 
                                 #"No las incluye", 
                                 #"No se sabe"))
  count(x3_nigi_a) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)
  
nigi_si_g <- ggplot(nigi_si, aes(x3_nigi_a, n))+
  geom_col(fill = c("#66c2a5", "#a6d854", "#fc8d62", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, "\n(", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_minimal() +
  labs(title = str_wrap("Propuestas con preguntas con enfoque de igualdad", 60),
       subtitle = "Cuando los TdR sí las proponían",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 9")


nigi_no <- metaevaluacion %>% 
  select(x3_nigi_b) %>% 
  mutate(x3_nigi_b = fct_relevel(x3_nigi_b,
                                 "Las incluye",
                                 "No las incluye")) %>% 
  count(x3_nigi_b) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

nigi_no_g <- ggplot(nigi_no, aes(x3_nigi_b, n))+
  geom_col(fill = c("#a6d854", "#fc8d62", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, "\n (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_minimal() +
  labs(title = str_wrap("Propuestas con preguntas con enfoque de igualdad", 60),
       subtitle = "Cuando los TdR no las proponían",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 10")


# nige

nige <- metaevaluacion %>% 
  select(x3_nige) %>% 
  mutate(x3_nige = case_when(x3_nige == "Las mujeres, Los hombres" ~ "Las mujeres y los hombres",
                             x3_nige == "Las mujeres, Los hombres, Las relaciones entre hombres y mujeres" ~ "Las mujeres, los hombres y las relaciones entre ellos",
                             TRUE ~ x3_nige),
         x3_nige = fct_relevel(x3_nige,
                               "Las mujeres, los hombres y las relaciones entre ellos",
                               "Las mujeres y los hombres",
                               "Las mujeres",)) %>% 
  count(x3_nige) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

nige_g <- ggplot(nige, aes(x3_nige, n))+
  geom_col(fill = c("#fc8d62","#a6d854", "#66c2a5", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_minimal() +
  scale_x_discrete(labels = str_wrap(nige$x3_nige,
                                     width = 30)) +
  labs(title = str_wrap("Categorías de género usadas en la evaluación", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 11")

# nicg

nicg <- metaevaluacion %>% 
  select(x3_nicg) %>% 
  mutate("División sexual del trabajo y roles de género" = case_when(str_detect(x3_nicg,"División sexual del trabajo y roles de género") ~ 1),
         "Participacion de mujeres y hombres en el espacio público y privado" = case_when(str_detect(x3_nicg,"Participacion de mujeres y hombres en el espacio público y privado") ~ 1),
         "Control del uso del cuerpo de las mujeres" = case_when(str_detect(x3_nicg,"Control del uso del cuerpo de las mujeres") ~ 1),
         "Necesidades prácticas y estratégicas de género" = case_when(str_detect(x3_nicg,"Necesidades prácticas y estratégicas de género") ~ 1),
         "Diferentes usos del tiempo por mujeres y hombres" = case_when(str_detect(x3_nicg,"Diferentes usos del tiempo por mujeres y hombres") ~ 1),
         "Acceso y control de recursos y beneficios por mujeres y hombres" = case_when(str_detect(x3_nicg,"Acceso y control de recursos y beneficios por mujeres y hombres") ~ 1)) %>% 
  gather(indicador, respuesta,"División sexual del trabajo y roles de género":"Acceso y control de recursos y beneficios por mujeres y hombres") %>% 
  filter(respuesta == 1) %>% 
  group_by(indicador) %>% 
  count(respuesta) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

nicg_g <- ggplot(nicg, aes(indicador, n)) +
  geom_col(fill = "#a6d854") +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
                                        position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_minimal() +
  scale_x_discrete(labels = str_wrap(nicg$indicador,
                                     width = 35)) +
  ylim(0, total_bd) +
  labs(title = str_wrap("Cuestiones de género analizadas en la evaluación", 40),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 12")


# niin

niin <- metaevaluacion %>% 
  select(x3_niin) %>% 
  mutate("Desigualdad étnica-racial" = case_when(str_detect(x3_niin,"Desigualdad étnica-racial") ~ 1),
         "Desigualdad económica" = case_when(str_detect(x3_niin,"Desigualdad económica") ~ 1),
         "Enfoque de infancia" = case_when(str_detect(x3_niin,"Enfoque de infancia") ~ 1),
         "Enfoque de capacidades" = case_when(str_detect(x3_niin,"Enfoque de capacidades") ~ 1)) %>% 
  gather(indicador, respuesta,"Desigualdad étnica-racial":"Enfoque de capacidades") %>% 
  filter(respuesta == 1) %>% 
  group_by(indicador) %>% 
  count(respuesta) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

niin_g <- ggplot(niin, aes(indicador, n)) +
  geom_col(fill = "#a6d854") +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_minimal() +
  ylim(0, total_bd) +
  labs(title = str_wrap("Enfoques interseccionales aplicados en la evaluación", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 13")


# d

d <- metaevaluacion %>% 
  select(x4_ddrg_a, x4_ddri_a, x4_dddg_a, x4_dddi_a) %>%
  gather(indicador, respuesta, x4_ddrg_a:x4_dddi_a) %>% 
  mutate(indicador = case_when(indicador == "x4_ddrg_a" ~ "Datos recolectados en la evaluación desagregados por género",
                               indicador == "x4_ddri_a" ~ "Datos recolectados en la evaluación desagregados por otras variables interseccionales",
                               indicador == "x4_dddg_a" ~ "Datos disponibles de registro desagregados por género",
                               indicador == "x4_dddi_a" ~ "Datos disponibles de registro desagregados por otras variables interseccionales")) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(indicador = fct_relevel(indicador,
                                 "Datos disponibles de registro desagregados por otras variables interseccionales",
                                 "Datos disponibles de registro desagregados por género",
                                 "Datos recolectados en la evaluación desagregados por otras variables interseccionales",
                                 "Datos recolectados en la evaluación desagregados por género"),
         respuesta = fct_relevel(respuesta,
                                 "No se sabe",
                                 "Nunca",
                                 "A veces",
                                 "Siempre")) %>% 
  group_by(indicador) %>% 
  count(respuesta)

d_g <- ggplot(d, aes(indicador, n, fill = respuesta)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) + 
  coord_flip() +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  scale_fill_manual(values=c("Siempre" = "#a6d854",
                             "A veces" = "#fce162",
                             "Nunca" = "#fc8d62",
                             "No se sabe" = "#b3b3b3",
                             "NA" = "#b3b3b3"),
                    name="") +
  scale_x_discrete(labels = str_wrap(c("Datos disponibles de registro desagregados por otras variables interseccionales",
                                       "Datos disponibles de registro desagregados por género",
                                       "Datos recolectados en la evaluación desagregados por otras variables interseccionales",
                                       "Datos recolectados en la evaluación desagregados por género"),
                                     width = 30)) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = str_wrap("Desagregación de datos", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 14")


# mas sobre datos

metaevaluacion %>%
  count(x4_ddrg_b) %>%
  mutate(porcentaje = n/sum(n))

metaevaluacion %>%
  count(x4_ddri_b) %>%
  mutate(porcentaje = n/sum(n))

metaevaluacion %>%
  count(x4_dddg_b) %>%
  mutate(porcentaje = n/sum(n))

metaevaluacion %>%
  count(x4_dddi_b) %>%
  mutate(porcentaje = n/sum(n))

metaevaluacion %>%
  count(x4_dddi_c) %>%
  mutate(porcentaje = n/sum(n))
  

# efne

efne <- metaevaluacion %>% 
  select(x4_efne) %>% 
  mutate(x4_efne = fct_relevel(x4_efne,
                               "Sí",
                               "No")) %>% 
  count(x4_efne) %>% 
  mutate(porcentaje = n/sum(n))

efne_g <- ggplot(efne, aes(x4_efne, n)) +
  geom_col(fill = c("#a6d854", "#fc8d62", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, " (", round(porcentaje*100,0), "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.y =element_blank()) +
  labs(title = str_wrap("La evaluación analiza efectos no esperados", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 15")


# mixt

mixt <- metaevaluacion %>% 
  select(x5_mixt) %>% 
  group_by(x5_mixt) %>% 
  count(x5_mixt) %>% 
  mutate(porcentaje = round(n/total_bd*100,0))

mixt_g <- ggplot(mixt, aes(x5_mixt, n)) +
  geom_col(fill = c("#a6d854", "#66c2a5", "#b3b3b3")) + 
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.y =element_blank()) +
  labs(title = str_wrap("Metodología empleada en la evaluación", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 16")
  

# parn

parn <- metaevaluacion %>% 
  select(id, x6_parn_a, x6_parn_b) %>%
  rename(x = id) %>% 
  mutate(x6_parn_a = (na_if(x6_parn_a, "NS")),
         x6_parn_b = (na_if(x6_parn_b, "NS"))) %>% 
  separate(col = x6_parn_a,
           into = c("ph", "pm"),
           sep = "/",
           remove	= FALSE,
           convert = TRUE,
           fill = "left") %>% 
  separate(col = x6_parn_b,
           into = c("eh", "em"),
           sep = "/",
           remove	= FALSE,
           convert = TRUE,
           fill = "left") %>% 
  mutate(x = fct_rev(x))

parn_g <- ggplot(parn) +
  geom_point(aes(x = x, y = pm),
             color = "#a6d854",
             size = 8) +
  geom_point(aes(x = x, y = em),
             color="#66c2a5",
             size = 8) +
  geom_segment(aes(x = x, xend = x, y = pm, yend = em),
               color="#b3b3b3") +
  geom_text(label = paste0(round(parn$pm, 0), "%"), y = parn$pm, x = parn$x,
            size = 2.5,
            check_overlap = T) +
  geom_text(label = paste0(round(parn$em, 0), "%"), y = parn$em, x = parn$x,
            size = 2.5,
            check_overlap = T) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_markdown()) + # imprescindible para que funcione el titulo de ggptext
  labs(title = str_wrap("Diferencia en la proporción de mujeres participantes \n en el <span style = 'color: #a6d854; '>proyecto</span> y en la <span style = 'color: #66c2a5; '>evaluación</span>",30),
       y = "",
       x = "",
       caption = "Fuente: Revisión documental")
  

# efec

efec <- metaevaluacion %>% 
  select(x7_efec) %>% 
  mutate(x7_efec = fct_relevel(x7_efec,
                               "Nunca",
                               "A veces",
                               "Casi siempre",
                               "Siempre")) %>% 
  group_by(x7_efec) %>% 
  count(x7_efec) %>% 
  mutate(porcentaje = round(n/total_bd*100))

efec_g <- ggplot(efec, aes(x7_efec, n)) +
  geom_col(fill = c("#66c2a5", "#a6d854", "#fce162", "#fc8d62", "#b3b3b3" )) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x =element_blank()) +
  labs(title = str_wrap("Se analizan los efectos del proyecto sobre la igualdad", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 18")


# tria

tria <- metaevaluacion %>% 
  select(x7_tria) %>% 
  mutate(x7_tria = fct_relevel(x7_tria,
                               "Sí",
                               "No",
                               "No aplica")) %>% 
  group_by(x7_tria) %>% 
  count(x7_tria) %>% 
  mutate(porcentaje = round(n/total_bd,2)*100)

tria_g <- ggplot(tria, aes(x7_tria, n)) +
  geom_col(fill = c("#a6d854","#fc8d62", "#b3b3b3", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, "\n(", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.y =element_blank()) +
  labs(title = str_wrap("Triangulación de la información obtenida", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 19")


# raiz

raiz <- metaevaluacion %>% 
  select(x8_raiz_a:x8_raiz_b) %>% 
  gather(indicador, respuesta, x8_raiz_a:x8_raiz_b) %>%
  mutate(indicador = case_when(indicador == "x8_raiz_a" ~ "Desde un punto de vista de género",
                               indicador == "x8_raiz_b" ~ "Desde un punto de vista interseccional")) %>% 
  mutate(indicador = fct_relevel(indicador,
                                 "Desde un punto de vista interseccional",
                                 "Desde un punto de vista de género"),
         respuesta = fct_relevel(respuesta,
                                 "NA",
                                 "No se sabe",
                                 "Nunca",
                                 "A veces",
                                 "Casi siempre",
                                 "Siempre")) %>% 
  group_by(indicador) %>% 
  count(respuesta)
  
raiz_g <- ggplot(raiz, aes(indicador, n, fill = respuesta)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  scale_fill_manual(values=c("Siempre" = "#66c2a5",
                             "Casi siempre" = "#a6d854",
                             "A veces" = "#fce162",
                             "Nunca" = "#fc8d62",
                             "NA" = "#b3b3b3"),
                      name="") +
  theme_minimal() +
  labs(title = str_wrap("Interpretación de las causas de las diferencias observadas", 30),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 20")

# m

m <- metaevaluacion %>% 
  select(x8_mplu, x8_memp, x8_mnse) %>%
  gather(indicador, respuesta, x8_mplu:x8_mnse) %>% 
  mutate(indicador = case_when(indicador == "x8_mplu" ~ "Visión plural",
                               indicador == "x8_memp" ~ "Visión empoderada",
                               indicador == "x8_mnse" ~ "Visión no sexista"),
         indicador = fct_relevel(indicador,
                                 "Visión no sexista",
                                 "Visión empoderada",
                                 "Visión plural")) %>% 
  group_by(indicador) %>% 
  count(respuesta)

m_g <- ggplot(m, aes(indicador, n, fill = respuesta)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  scale_fill_manual(values=c("Sí" = "#a6d854",
                             "No aplica" = "#b3b3b3",
                             "No"  = "#fc8d62"), 
                    name="") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = str_wrap("La evaluación presenta a las mujeres y niñas desde una...", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 21")


# judi, jupr, jure

ju <- metaevaluacion %>% 
  select(x9_judi, x9_jupr, x9_jure) %>%
  gather(indicador, respuesta, x9_judi:x9_jure) %>% 
  mutate(indicador = case_when(indicador == "x9_judi" ~ "Juicios sobre el diseño",
                               indicador == "x9_jupr" ~ "Juicios sobre los procesos",
                               indicador == "x9_jure" ~ "Juicios sobre los resultados"),
         indicador = fct_relevel(indicador,
                                 "Juicios sobre los resultados",
                                 "Juicios sobre los procesos",
                                 "Juicios sobre el diseño")) %>% 
  group_by(indicador) %>% 
  count(respuesta)

ju_g <- ggplot(ju, aes(indicador, n, fill = respuesta)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  scale_fill_manual(values=c("Sí" = "#a6d854",
                             "No" = "#fc8d62",
                             "NA" = "#b3b3b3"),
                    name="") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = str_wrap("Valoraciones sobre la incorporación de la igualdad", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 22")


# reco

reco <- metaevaluacion %>% 
  select(x10_reco) %>% 
  group_by(x10_reco) %>% 
  count(x10_reco) %>% 
  mutate(porcentaje = round(n/total_bd*100))

reco_g <- ggplot(reco, aes(x10_reco, n)) +
  geom_col(fill = c("#fc8d62", "#a6d854", "#66c2a5", "#b3b3b3")) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  scale_x_discrete(labels = str_wrap(c("No",
                                       "Sí, son específicas sobre igualdad",
                                       "Sí, son específicas sobre igualdad, basadas en el análisis y concretas",
                                       "NA"),
                                     width = 30)) +
  theme_minimal() +
  theme(axis.text.x =element_blank()) +
  labs(title = str_wrap("Recomendaciones específicas sobre igualdad, basadas en el análisis y concretas", 50),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 23")


# nose

nose <- metaevaluacion %>% 
  select(x11_nose_a, x11_nose_b, x11_nose_c) %>%
  gather(indicador, respuesta, x11_nose_a:x11_nose_c) %>% 
  mutate(indicador = case_when(indicador == "x11_nose_a" ~ "Lenguaje inclusivo",
                               indicador == "x11_nose_b" ~ "Fotografías no sexistas",
                               indicador == "x11_nose_c" ~ "Iconografía no sexista")) %>% 
  mutate(respuesta = fct_relevel(respuesta,
                                 "No",
                                 "No aplica",
                                 "Sí")) %>% 
  group_by(indicador) %>% 
  count(respuesta)

nose_g <- ggplot(nose, aes(indicador, n, fill = respuesta)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n, y = n),
            position = position_fill(vjust = 0.5)) + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = NULL)) +
  scale_fill_manual(values=c("Sí" = "#a6d854",
                             "No aplica" = "#b3b3b3",
                             "No" = "#fc8d62"), 
                    name="") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = str_wrap("Comunicación de los resultados de la evaluación", 60),
       subtitle = "",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 24")



  

# Analizar preguntas de metaevaluación (contrastar con estándares) --------


estandares <- 
metaevaluacion %>% 
  mutate(ongd = case_when(x0_ongd_c == "Sí" & x0_ongd_e == "Sí" | x0_ongd_c == "Sí" & x0_ongd_f == "Sí" | x0_ongd_e == "Sí" & x0_ongd_f == "Sí"~ 2,
                          x0_ongd_c == "Sí" | x0_ongd_e == "Sí" | x0_ongd_f == "Sí" ~ 1,
                          x0_ongd_b == "No se sabe" | x0_ongd_c == "No se sabe" | x0_ongd_c == "No se sabe" | x0_ongd_d == "No se sabe" ~ 0,
                          TRUE ~ -1),
         nior = case_when(x0_nior == "Sí, sobre igualdad interseccional" ~ 2,
                          x0_nior == "Sí, sobre igualdad de género" ~ 1,
                          x0_nior == "No se sabe" ~ 0,
                          x0_nior == "No" ~ -1),
         x0_tiem_a = as.numeric(x0_tiem_a), # esto es lo que daba problems
         tiem = case_when(x0_tiem_a >= 12 ~ 1,
                          x0_tiem_a < 12 & x0_tiem_a >= 8 ~ 0,
                          is.na(x0_tiem_a) ~ 0,
                          x0_tiem_a < 8 ~ -1), 
         x0_pres_b = as.numeric(x0_pres_b), # no detectaba la coma los decimales
         pres = case_when(x0_pres_b >= 9000 ~ 2,
                          x0_pres_b >= 6000 ~ 1,
                          x0_pres_b < 6000 & x0_pres_b >=3000 ~ 0,
                          is.na(x0_pres_b) ~0,
                          x0_pres_b < 3000 ~ -1),
         x0_equi_a = case_when(x0_equi_a == "1" ~ 1, # transformacion previa (ver linea 257)
                               x0_equi_a == "2" ~ 2,
                               x0_equi_a == "3" ~ 3,
                               x0_equi_a == "4" ~ 4),
         x0_equi_b = case_when(x0_equi_b == "Ningún miembro" ~ 0,
                               x0_equi_b == "1 miembro" ~ 1,
                               x0_equi_b == "2 miembros" ~ 2,
                               x0_equi_b == "3 miembros" ~ 3,
                               x0_equi_b == "4 miembros" ~ 4),
         x0_equi_c = case_when(x0_equi_c == "Ningún miembro" ~ 0,
                               x0_equi_c == "1 miembro" ~ 1,
                               x0_equi_c == "2 miembros" ~ 2,
                               x0_equi_c == "3 miembros" ~ 3,
                               x0_equi_c == "4 miembros" ~ 4),
         x0_equi_d = case_when(x0_equi_d == "Ningún miembro" ~ 0,
                               x0_equi_d == "1 miembro" ~ 1,
                               x0_equi_d == "2 miembros" ~ 2,
                               x0_equi_d == "3 miembros" ~ 3,
                               x0_equi_d == "4 miembros" ~ 4),
         x0_equi_e = case_when(x0_equi_e == "Ningún miembro" ~ 0,
                               x0_equi_e == "1 miembro" ~ 1,
                               x0_equi_e == "2 miembros" ~ 2,
                               x0_equi_e == "3 miembros" ~ 3,
                               x0_equi_e == "4 miembros" ~ 4),
         x0_equi_bc = case_when(x0_equi_b >= 1 | x0_equi_c >= 1 ~ 1), # combinacion de condiciones
         x0_equi_de = case_when(x0_equi_c >= 1 | x0_equi_d >= 1 ~ 1),
         equi = case_when(x0_equi_bc == 1 & x0_equi_de == 1 ~ 2, # aplico la pre-condicion
                          x0_equi_b >= 1 | x0_equi_c >= 1 ~ 1, 
                          x0_equi_d >= 1 | x0_equi_e >= 1 ~ 0,
                          x0_equi_d == 0 | x0_equi_e == 0 ~ -1), # ninguno tiene formacion sin experiencia
         equi = case_when(is.na(equi) ~ 0, # quitar NAs que no me dejaba arriba en TRUE
                          TRUE ~ equi),
         prop = case_when(x1_prop_a == "Sí" ~ 1,
                          x1_prop_a == "No" ~ -1),
         cxto = case_when(x1_cxto_a == "Sí" & x1_cxto_b == "Sí" | x1_cxto_a == "Sí" & x1_cxto_c == "Sí" | x1_cxto_b == "Sí" & x1_cxto_c == "Sí" ~ 2,
                          x1_cxto_a == "Sí" | x1_cxto_c == "Sí" | x1_cxto_a == "Sí" ~ 1,
                          is.na(x1_cxto_a) ~ 0,
                          TRUE ~ -1),
         acto = case_when(x1_acto == "Sí, en profundidad" ~ 2,
                          x1_acto == "Sí, de forma somera" ~ 1,
                          x1_acto == "Sí, se mencionan" | is.na(x1_acto) ~ 0,
                          x1_acto == "No" ~ -1,
                          TRUE ~ 0),
         nigi = case_when(x3_nigi_a == "Las incluye y las amplía" | x3_nigi_b == "Las incluye" ~ 1,
                          x3_nigi_a == "Las incluye sin ampliarlas" ~ 0,
                          is.na(x3_nigi_a) & is.na(x3_nigi_b) ~ 0,
                          x3_nigi_a == "No las incluye" | x3_nigi_b == "No las incluye" ~ -1),
         nige = case_when(x3_nige == "Las mujeres, Los hombres, Las relaciones entre hombres y mujeres" ~ 1,
                          x3_nige == "Las mujeres, Los hombres" | is.na(x3_nige) ~ 0,
                          x3_nige == "Las mujeres" ~ -1),
         nicg = case_when(str_detect(x3_nicg, "(.*), (.*), (.*), (.*), (.*), (.*)") ~ 2,
                          str_detect(x3_nicg, "(.*), (.*), (.*), (.*), (.*)") ~ 2,
                          str_detect(x3_nicg, "(.*), (.*), (.*), (.*)") ~ 2,
                          str_detect(x3_nicg, "(.*), (.*), (.*)") ~ 1,
                          str_detect(x3_nicg, "(.*), (.*)") ~ 1,
                          str_detect(x3_nicg, "(.*)") ~ 0,
                          is.na(x3_nicg) ~ 0),
         niin = case_when(str_detect(x3_niin, "(.*), (.*)") ~ 2,
                          str_detect(x3_niin, "(.*)") ~ 1,
                          id == "Evaluación 18" | id ==  "Evaluación 21" ~ 0,
                          is.na(x3_niin) ~ -1),
         ddrg = case_when(x4_ddrg_a == "Siempre" ~ 1,
                          x4_ddrg_a == "A veces" | is.na(x4_ddrg_a) ~ 0,
                          x4_ddrg_a == "Nunca" ~ -1),
         ddri = case_when(x4_ddri_a == "Siempre" ~ 1,
                          x4_ddri_a == "A veces" | is.na(x4_ddri_a) ~ 0,
                          x4_ddri_a == "Nunca" ~ -1),
         dddg_b = case_when(str_detect(x4_dddg_b, "Sí.*") ~ "Sí"), # paso intermedio
         dddg = case_when(x4_dddg_a == "Siempre" & dddg_b == "Sí" ~ 2,
                          x4_dddg_a == "Siempre" ~ 1,
                          x4_dddg_a == "A veces" & dddg_b == "Sí" ~ 1,
                          x4_dddg_a == "A veces" | is.na(x4_dddg_a) | x4_dddg_a == "No se sabe" ~ 0,
                          x4_dddg_a == "Nunca" ~ -1),
         dddi = case_when(x4_dddi_a == "Siempre" ~ 1, # no hacemos el paso intermedio porque no se cumple nunca
                          x4_dddi_a == "A veces" | is.na(x4_dddi_a) | x4_dddi_a == "No se sabe" ~ 0,
                          x4_dddi_a == "Nunca" ~ -1),
         efne = case_when(x4_efne == "Sí" ~ 1,
                          is.na(x4_efne) ~ 0,
                          x4_efne == "No" ~ -1),
         mixt = case_when(x5_mixt == "Mixtas" ~ 1,
                          x5_mixt == "Cualitativas" | is.na(x5_mixt) ~ 0)) %>% 
  mutate(x6_parn_a = (na_if(x6_parn_a, "NS")), # transformación previa (ver linea 675)
         x6_parn_b = (na_if(x6_parn_b, "NS"))) %>% 
  separate(col = x6_parn_a,
           into = c("ph", "pm"),
           sep = "/",
           remove	= FALSE,
           convert = TRUE,
           fill = "left") %>% 
  separate(col = x6_parn_b,
           into = c("eh", "em"),
           sep = "/",
           remove	= FALSE, 
           convert = TRUE,
           fill = "left") %>% 
  mutate(p_diferencia = em - pm, # diferencia
         parn = case_when(p_diferencia <= -10 ~ -1,
                          is.na(p_diferencia) ~ 0,
                          TRUE ~ 1),
         efec = case_when(x7_efec == "Siempre" ~ 2,
                          x7_efec == "Casi siempre" ~ 1,
                          x7_efec == "A veces" | is.na(x7_efec) ~ 0,
                          x7_efec == "Nunca" ~ -1),
         tria = case_when(x7_tria == "Sí" ~ 1,
                          x7_tria == "No aplica" | is.na(x7_tria) ~ 0,
                          x7_tria == "No" ~ -1),
         raiz_a = case_when(x8_raiz_a == "Siempre" ~ 2,
                            x8_raiz_a  == "Casi siempre" ~ 1,
                            x8_raiz_a == "A veces" | is.na(x8_raiz_a) ~ 0,
                            x8_raiz_a  == "Nunca" ~ -1),
         raiz_b = case_when(x8_raiz_b == "Siempre" ~ 2,
                            x8_raiz_b  == "Casi siempre" ~ 1,
                            x8_raiz_b == "A veces" | is.na(x8_raiz_b) ~ 0,
                            x8_raiz_b  == "Nunca" ~ -1),
         mplu = case_when(x8_mplu == "Sí" ~ 1,
                          x8_mplu == "No aplica" | is.na(x8_mplu) ~ 0,
                          x8_mplu == "No" ~ -1),
         memp = case_when(x8_memp == "Sí" ~ 1,
                          x8_memp == "No aplica" | is.na(x8_memp) ~ 0,
                          x8_memp == "No" ~ -1),
         mnse = case_when(x8_mnse == "Sí" ~ 1,
                          x8_mnse == "No aplica" | is.na(x8_mnse) ~ 0,
                          x8_mnse == "No" ~ -1),
         judi = case_when(x9_judi == "Sí" ~ 1,
                          is.na(x9_judi) ~ 0,
                          x9_judi == "No" ~ -1),
         jupr = case_when(x9_jupr == "Sí" ~ 1,
                          is.na(x9_jupr) ~ 0,
                          x9_jupr == "No" ~ -1),
         jure = case_when(x9_jure == "Sí" ~ 1,
                          is.na(x9_jure) ~ 0,
                          x9_jure == "No" ~ -1),
         reco = case_when(x10_reco == "Sí, son específicas sobre igualdad, basadas en el análisis y concretas" ~ 2,
                          x10_reco == "Sí, son específicas sobre igualdad" ~ 1,
                          is.na(x10_reco) ~ 0,
                          x10_reco == "No" ~ -1),
         nose = case_when(x11_nose_a == "No" | x11_nose_b == "No" | x11_nose_c == "No" ~ -1,
                          TRUE ~ 1)) %>% 
  select(id,
         ongd,
         nior,
         tiem,
         pres,
         equi,
         prop,
         cxto,
         acto,
         nigi,
         nige,
         nicg,
         niin,
         ddrg,
         ddri,
         dddg,
         dddi,
         efne,
         mixt,
         parn,
         efec,
         tria,
         raiz_a,
         raiz_b,
         mplu,
         memp,
         mnse,
         judi,
         jupr,
         jure,
         reco,
         nose)

# ----

write_csv2(estandares, "datos/estandares.csv")

  
totales <- estandares %>% 
  group_by(id) %>% 
  summarise(total = sum(ongd,
                        nior,
                        tiem,
                        pres,
                        equi,
                        prop,
                        cxto,
                        acto,
                        nigi,
                        nige,
                        nicg,
                        niin,
                        ddrg,
                        ddri,
                        dddg,
                        dddi,
                        efne,
                        mixt,
                        parn,
                        efec,
                        tria,
                        raiz_a,
                        raiz_b,
                        mplu,
                        memp,
                        mnse,
                        judi,
                        jupr,
                        jure,
                        reco,
                        nose)) 

# tabla resumen
# en excel con formato condicional (Gráfico 25)


# ranking por evaluacion

ranking_por_evaluacion <- totales %>% 
  mutate(id = fct_reorder(id, total))

ranking_por_evaluacion_g <- ggplot(ranking_por_evaluacion, aes(id, total)) +
  geom_segment(aes(x = id,
                   xend = id,
                   y = 0,
                   yend = total - 0.6),
               color="#a6d854",
               alpha = 0.6) +
  geom_point(color = "#a6d854",
             size = 8) +
  geom_text(aes(x = id,
                label = total),
            size = 3,
            color = "white",
            fontface = "bold",
            hjust = 0.45) +
  geom_hline(yintercept = -2) +
  geom_hline(yintercept = 8) +
  annotate("text", x = 15, y = 25, label = "Evaluaciones\ntransformadoras") +
  annotate("text", x = 20, y = 3, label = "Evaluaciones\n ciegas a la\nigualdad") +
  annotate("text", x = 8, y = -8, label = "Evaluaciones\n regresivas") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = str_wrap("Cumplimiento de estándares por evaluación", 60),
       subtitle = "Evaluaciones ordenadas de mayor a menor puntuación",
       x = "",
       y = "",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 26")



# ranking por indicador

ranking_por_indicador <- estandares %>%
  gather(indicador, total, ongd:nose) %>% 
  group_by(indicador) %>% 
  summarise(total = sum(total)) %>% 
  mutate(indicador = fct_reorder(indicador, total))



# relacion entre totales y calidad

cali <- metaevaluacion %>% 
  mutate(cali = case_when(str_detect(cali,"(.*), (.*), (.*), (.*), (.*), (.*)")~ "Muy rigurosas",
                          str_detect(cali,"(.*), (.*), (.*), (.*), (.*)")~ "Bastante rigurosas",
                          str_detect(cali,"(.*), (.*), (.*), (.*)")~ "Rigurosas",
                          str_detect(cali,"(.*), (.*), (.*)")~ "Algo rigurosas",
                          str_detect(cali,"(.*), (.*)")~ "Poco rigurosas",
                          str_detect(cali,"(.*)")~ "Nada rigurosas"),
         orden = case_when(cali == "Muy rigurosas" ~ 6,
                           cali == "Bastante rigurosas" ~ 5,
                           cali == "Rigurosas" ~ 4,
                           cali == "Algo rigurosas" ~ 3,
                           cali == "Poco rigurosas" ~ 2,
                           cali == "Nada rigurosas" ~ 1)) %>% 
  select(cali, orden) %>%
  cbind(totales) %>%
  select(id, cali, orden, total) %>% 
  filter(!is.na(orden))

cali_g <- ggplot(cali, aes(total, orden)) +
  geom_point(height = 0.1,
              color = "#a6d854",
              size = 6,
              alpha = 0.5) +
  scale_y_continuous(breaks = c(1:6),
                     minor_breaks = NULL) +
  theme_minimal() +
  
  labs(title = str_wrap("Relación entre calidad metodológica y cumplimiento de estándares de igualdad", 60),
       subtitle = "",
       x = "Puntuación de estándares",
       y = "Rigor metodológico",
       caption = "Fuente: Revisión documental",
       tag = "Gráfico 27")





# Guardar gráficos --------------------------------------------------------

ancho <- 7
alto <- 4

ggsave("graficos/01-ongd.png", plot = ongd_g, width = ancho, height = 5)
ggsave("graficos/02-nior.png", plot = nior_g, width = ancho, height = alto) 
ggsave("graficos/03-tiem.png", plot = tiem_g, width = ancho, height = 3) #especial
ggsave("graficos/04-pres.png", plot = pres_g, width = ancho, height = 3) #especial
ggsave("graficos/05-equi.png", plot = equi_g, width = ancho, height = alto)
ggsave("graficos/06-prop.png", plot = prop_g, width = ancho, height = alto)
ggsave("graficos/07-cxto.png", plot = cxto_g, width = ancho, height = 5) 
ggsave("graficos/08-acto.png", plot = acto_g, width = ancho, height = alto) 
ggsave("graficos/09-nigi_si.png", plot = nigi_si_g, width = ancho, height = alto)
ggsave("graficos/10-nigi_no.png", plot = nigi_no_g, width = ancho, height = alto) 
ggsave("graficos/11-nige.png", plot = nige_g, width = ancho, height = alto) 
ggsave("graficos/12-nicg.png", plot = nicg_g, width = ancho, height = alto)
ggsave("graficos/13-niin.png", plot = niin_g, width = ancho, height = alto)
ggsave("graficos/14-d.png", plot = d_g, width = ancho, height = 5)
ggsave("graficos/15-efne.png", plot = efne_g, width = ancho, height = alto) 
ggsave("graficos/16-mixt.png", plot = mixt_g, width = ancho, height = alto)
ggsave("graficos/17-parn.png", plot = parn_g, width = 8.2, height = 7) #especial
ggsave("graficos/18-efec.png", plot = efec_g, width = ancho, height = alto) 
ggsave("graficos/19-tria.png", plot = tria_g, width = ancho, height = alto)
ggsave("graficos/20-raiz.png", plot = raiz_g, width = ancho, height = 3) 
ggsave("graficos/21-m.png", plot = m_g, width = ancho, height = alto) 
ggsave("graficos/22-ju.png", plot = ju_g, width = ancho, height = alto)
ggsave("graficos/23-reco.png", plot = reco_g, width = ancho, height = alto)
ggsave("graficos/24-nose.png", plot = nose_g, width = ancho, height = alto)

ggsave("graficos/26-ranking-por-evaluacion.png", plot = ranking_por_evaluacion_g, width = ancho, height = 6)
ggsave("graficos/27-cali.png", plot = cali_g, width = ancho, height = 5)

