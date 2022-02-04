# Author: FSN
# Maintainers: FSN
# Copyright:   2021, FSN GPL v2 or later
# Data Analysis
# =============================================

tiempo_promedio_extradicion = extradiciones %>%
    group_by(delito) %>%
    na.exclude() %>%
    summarize(tiempo_promedio_dias = round(mean(tiempo_resolucion/86400)),
              tiempo_promedio_anos = mean(tiempo_resolucion/31557600),
              tiempo_promedio_meses = round(mean(tiempo_resolucion/2629743.8)))


tiempo_promedio_extradicion %>%
    ggplot(aes(x=tiempo_promedio_anos)) +
    geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9, bins = 20) +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Tiempo de resolución de extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por tiempo promedio mensual",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("Meses promedio")+
    ylab("")
ggsave("grafica_tiempo_promedio_anos.png", path = here("plots"))

tiempo_promedio_extradicion %>%
    filter(!is.na(tiempo_promedio_meses),
           delito != "construccion_financiamiento_y_uso_de_un_tunel_que_cruza_una_frontera_internacional",
           delito != "otros_delitos") %>%
    arrange(desc(tiempo_promedio_meses)) %>%
    head(15) %>%
    # mutate(delito=factor(delito, delito)) %>%
    # ggplot(aes(x=delito, y= tiempo_promedio_anos)) +
    ggplot(aes(x=reorder(delito, tiempo_promedio_meses), y= tiempo_promedio_meses)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
    geom_text(aes(label=round(tiempo_promedio_meses)), vjust=0, hjust =0,check_overlap = T, size = 2.5, color= "black")+
    coord_flip() +
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Tiempo de resolución de extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por delito y tiempo promedio mensual",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("") +
    ylab("Meses promedio")
ggsave("grafica_tiempo_resolucion_delito_mayor.png", path = here("plots"))

tiempo_promedio_extradicion %>%
    filter(!is.na(tiempo_promedio_meses),
           delito != "construccion_financiamiento_y_uso_de_un_tunel_que_cruza_una_frontera_internacional",
           delito != "otros_delitos") %>%
    arrange(desc(tiempo_promedio_anos)) %>%
    tail(15) %>%
    # mutate(delito=factor(delito, delito)) %>%
    # ggplot(aes(x=delito, y= tiempo_promedio_anos)) +
    ggplot(aes(x=reorder(delito, tiempo_promedio_meses), y= tiempo_promedio_meses)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
    geom_text(aes(label=round(tiempo_promedio_meses)), vjust=0, hjust =0,check_overlap = T, size = 2.5, color= "black")+
    coord_flip() +
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Tiempo de resolución de extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por delito y tiempo promedio mensual",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("") +
    ylab("Meses promedio")
ggsave("grafica_tiempo_resolucion_delito_menor.png", path = here("plots"))



#graphing extradiciones by year
extradiciones_anual = extradiciones %>%
    group_by(id, ano_peticion) %>%
    distinct(id)
extradiciones_anual = tabyl(extradiciones_anual$ano_peticion)%>%
     rename("ano_peticion" = "extradiciones_anual$ano_peticion",
            "extradiciones" = "n")

extradiciones_anual %>%
    arrange(ano_peticion)%>%
    filter(!is.na(ano_peticion)) %>%
    # head(15) %>%
    ggplot(aes(x=ano_peticion, y= extradiciones)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
    geom_text(aes(label=extradiciones), vjust=0, hjust =0.5,check_overlap = T, size = 2.5, color= "black")+
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por año",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("Año") +
    ylab("Solicitudes de extradición")
ggsave("grafica_solicitudes_anual.png", path = here("plots"))



ggsave("grafica_anual.png", path = here("plots"))

#graph by delito

por_delito =  tabyl(extradiciones$delito) %>%
    rename("delito" = "extradiciones$delito",
           "extradiciones" = "n")
por_delito %>%
    arrange(desc(extradiciones))%>%
    head(15) %>%
    ggplot(aes(x=reorder(delito, extradiciones), y= extradiciones)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
    geom_text(aes(label=extradiciones), vjust=0, hjust =0,check_overlap = T, size = 2.5, color= "black")+
    coord_flip() +
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por delitos más frecuentes",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("") +
    ylab("Solicitudes de extradición")
ggsave("grafica_solicitudes_delito.png", path = here("plots"))

resolucion = extradiciones %>%
    tabyl(resultado) %>%
    mutate(percent = round(percent,2))
