# Author: FSN
# Maintainers: FSN
# Copyright:   2021, FSN GPL v2 or later
# Data Cleaning
# =============================================

extradiciones_2 = extradiciones %>%
    mutate(delito = case_when(delito == "abuso_sexual_cometido_contra_persona_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_continuo_contra_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_continuo_de_una_persona_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_contra_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_contra_menores_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_contra_persona_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_contra_un_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_contra_unas_personas_menores_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_criminal_calificado_de_persona_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_menores"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_persona_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_un_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_una_menor"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_una_menor"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_una_persona_menor_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_de_unas_personas_menores_de_edad"~ "abuso_sexual_menores",
                                  delito == "abuso_sexual_infantil" ~ "abuso_sexual_menores",
                              delito == "abandonar_el_lugar_donde_ocurrio_un_accidente_que_implico_un_fallecimiento_homicidio" ~ "abandono_lugar_homicidio",
                              delito == "abandonar_el_lugar_donde_ocurrio_un_choque_automovilistico_que_resulto_en_muerte" ~ "abandono_lugar_homicidio",
                              delito == "agesiones_calificadas_con_un_vehiculo_y_abandonar_el_lugar" ~ "abandono_lugar_homicidio",
                              delito == "abandono_de_personas_menor_de_edad" ~ "abandono_personas",
                              delito == "abrir_fuego_en_una_vivienda" ~ "uso_armas_de_fuego",
                              delito == "abuso_a_persona_mayor" ~ "abuso_persona_mayor",
                              delito == "acoso_cibernetico" ~ "acoso",
                              delito == "acoso" ~ "acoso",
                              delito == "acoso_y_violencia_contra_una_pareja_actual_o_anterior" ~ "acoso",
                              delito == "acto_lacsivo_y_lujurioso_con_menor_de_edad" ~ "actos_lascivos_contra_menores",
                              delito == "acto_lascivo_con_una_persona_menor_de_edad" ~"actos_lascivos_contra_menores",
                              delito == "acto_lascivo_y_obsceno_mediante_la_fuerza"~ "actos_lascivos_contra_menores",
                              delito == "actos_lascivos_forzado_con_menor_de_catorce_anos" ~ "actos_lascivos_contra_menores",
                              delito == "actos_lasivos_forzados_con_una_persona_menor_de_14_años_de_edad" ~ "actos_lascivos_contra_menores",
                              delito == "actos_sexuales_ilicitos_contra_menor_de_edad" ~ "actos_lascivos_contra_menores",
                              delito == "agresion_a_mano_armada" ~"agresion_mano_armada",
                              delito == "agresion_sexual_contra_menor_de_edad" ~ "agresion_sexual_contra_menor",
                              delito == "agresion_sexual_contra_menor_de_edad_como_parte_de_un_patron_de_abuso_en_violacion"~"agresion_sexual_contra_menor" ))


                                  ),
           fecha_peticion = dmy(fecha_peticion),
           fecha_resolucion = dmy(fecha_resolucion),
           tiempo_resolucion = as.duration(fecha_resolucion-fecha_peticion))



#
#
#     mutate(delito = as_factor(delito))
#
# tabyl(extradiciones$delito)
