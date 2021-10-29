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
         subtitle = "Desagregación por tiempo promedio anual",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("Años promedio")+
    ylab("")
ggsave("grafica_tiempo_promedio_anos.png", path = here("plots"))

tiempo_promedio_extradicion %>%
    filter(!is.na(tiempo_promedio_anos),
           delito != "construccion_financiamiento_y_uso_de_un_tunel_que_cruza_una_frontera_internacional",
           delito != "otros_delitos") %>%
    arrange(desc(tiempo_promedio_anos)) %>%
    head(15) %>%
    # mutate(delito=factor(delito, delito)) %>%
    # ggplot(aes(x=delito, y= tiempo_promedio_anos)) +
    ggplot(aes(x=reorder(delito, tiempo_promedio_anos), y= tiempo_promedio_anos)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
    coord_flip() +
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Tiempo de resolución de extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por delito y tiempo promedio anual",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("") +
    ylab("Años promedio")
ggsave("grafica_tiempo_resolucion_delito_mayor.png", path = here("plots"))

tiempo_promedio_extradicion %>%
    filter(!is.na(tiempo_promedio_anos),
           delito != "construccion_financiamiento_y_uso_de_un_tunel_que_cruza_una_frontera_internacional",
           delito != "otros_delitos") %>%
    arrange(desc(tiempo_promedio_anos)) %>%
    tail(15) %>%
    # mutate(delito=factor(delito, delito)) %>%
    # ggplot(aes(x=delito, y= tiempo_promedio_anos)) +
    ggplot(aes(x=reorder(delito, -tiempo_promedio_anos), y= tiempo_promedio_anos)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
    coord_flip() +
    theme_ipsum() +
    theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none",
        plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10)) +
    labs(title = "Tiempo de resolución de extradiciones solicitadas por EEUU a México (2006-2021)",
         subtitle = "Desagregación por delito y tiempo promedio anual",
         caption = "Elaboración propia con información obtenida mediante la PNT")+
    xlab("") +
    ylab("Años promedio")
ggsave("grafica_tiempo_resolucion_delito_menor.png", path = here("plots"))



#graphing extradiciones by year
extradiciones %>%
    filter(!is.na(ano_peticion)) %>%
    group_by(ano_peticion) %>%
    ggplot(aes(ano_peticion)) +
    geom_bar( fill="#69b3a2") +
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
    ylab("Total de solicitudes de extradicion")
ggsave("grafica_anual.png", path = here("plots"))

#graph by delito
por_delito %>%
    arrange(desc(extradiciones))%>%
    head(15) %>%
    ggplot(aes(x=reorder(delito, extradiciones), y= extradiciones)) +
    geom_bar(stat="identity", fill="#69b3a2",  color="#e9ecef", alpha=0.9) +
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