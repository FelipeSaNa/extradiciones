# Author: FSN
# Maintainers: FSN
# Copyright:   2021, FSN GPL v2 or later
# Data Cleaning
# =============================================

extradiciones_2 = extradiciones %>%
    mutate(delito = case_when(delito == "abuso_sexual_cometido_contra_persona_menor_de_edad"|
                                  delito == "abuso_sexual_continuo_contra_menor_de_edad"|
                                  delito == "abuso_sexual_continuo_de_una_persona_menor_de_edad"|
                                  delito == "abuso_sexual_contra_menor_de_edad"|
                                  delito == "abuso_sexual_contra_menores_de_edad"|
                                  delito == "abuso_sexual_contra_persona_menor_de_edad"|
                                  delito == "abuso_sexual_contra_un_menor_de_edad"|
                                  delito == "abuso_sexual_contra_unas_personas_menores_de_edad"|
                                  delito == "abuso_sexual_criminal_calificado_de_persona_menor_de_edad"|
                                  delito == "abuso_sexual_de_menores"|
                                  delito == "abuso_sexual_de_persona_menor_de_edad"|
                                  delito == "abuso_sexual_de_un_menor_de_edad"|
                                  delito == "abuso_sexual_de_una_menor"|
                                  delito == "abuso_sexual_de_una_menor"|
                                  delito == "abuso_sexual_de_una_persona_menor_de_edad"|
                                  delito == "abuso_sexual_de_unas_personas_menores_de_edad"|
                                  delito == "abuso_sexual_infantil" ~ "abuso_sexual_menores"),
           fecha_peticion = dmy(fecha_peticion),
           fecha_resolucion = dmy(fecha_resolucion),
           tiempo_resolucion = as.duration(fecha_resolucion-fecha_peticion))





    mutate(delito = as_factor(delito))

tabyl(extradiciones$delito)
