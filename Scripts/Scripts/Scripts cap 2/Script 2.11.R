library(ggpubr)

# Cargar datos.
datos <- read.csv2("C:/Inferencia/Mtcars.csv", stringsAsFactors = TRUE,
                   row.names = 1)

# Crear la tabla de frecuencias y convertirla a data frame.
contingencia <- as.data.frame(xtabs(~ Cambios, data = datos))

# Crear gráfico de torta.
g <- ggpie(contingencia,
           x = "Freq",
           label = "Cambios",
           fill = c("red", "yellow", "green"),
           title = "Cantidad de cambios de los automóviles",
           lab.pos = "in")

print(g)