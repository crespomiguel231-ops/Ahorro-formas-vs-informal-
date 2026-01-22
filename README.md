# Ahorro-formas-vs-informal-
# Análisis de Inclusión Financiera y Cultura del Ahorro en RD 🇩🇴

Este repositorio contiene una serie de análisis visuales y un modelo teórico sobre el comportamiento del ahorro y la inclusión financiera en la República Dominicana, basados en datos de la Estrategia Nacional de Inclusión Financiera (ENIF).

##  Visualizaciones Principales

### 1. Barreras para la Apertura de Cuentas
Identificación de los principales obstáculos que impiden a los ciudadanos acceder al sistema bancario formal.
![Barreras para el Ahorro]

### 2. Relación entre Educación y Ahorro
Análisis de cómo la tasa de tenencia de productos financieros aumenta proporcionalmente al nivel de instrucción.
![Nivel Educativo]

### 3. Motivos de Ahorro en los Hogares
Distribución de las prioridades de ahorro según las necesidades declaradas por las familias dominicanas.
![Motivos de Ahorro]

### 4. Brecha de Género
Evolución de la tenencia de productos financieros entre hombres y mujeres (2019-2023).
![Brecha de Género]

---

##  Modelo Teórico de Decisión de Ahorro

Se incluye un modelo de **Utilidad Esperada** que compara el ahorro formal frente al informal, considerando:
* **Costo de acceso (c):** Transporte y comisiones.
* **Riesgo (theta):** Probabilidad de pérdida en el sector informal.
* **Inflación (pi):** Impacto en el efectivo guardado.

![Modelo Teórico](modelo_teorico.png)

*El umbral **Y*** representa el nivel de ingreso mensual a partir del cual el ahorro formal se vuelve óptimo para el usuario.*

---

##  Tecnologías Utilizadas
* **Lenguaje:** R
* **Librerías:** * `ggplot2` (Visualización)
    * `dplyr` (Manipulación de datos)
    * `treemapify` (Mapas de árbol)



```r
library(ggplot2)
library(dplyr)
library(treemapify)

# 1. BARRERAS
barreras <- data.frame(
  Razon = c("Ingresos insuficientes", "Prefiere efectivo", "Falta de documentos", "Costo de mantenimiento", "Otros"),
  Valor = c(45, 20, 15, 12, 8)
) %>%
  arrange(desc(Razon)) %>%
  mutate(prop = Valor / sum(Valor) * 100,
         ypos = cumsum(prop) - 0.5 * prop)

p1 <- ggplot(barreras, aes(x = 2, y = prop, fill = Razon)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = paste0(prop, "%")), color = "black", size = 5) +
  xlim(0.5, 2.5) +
  labs(title = "Principales barreras para el ahorro formal en RD",
       subtitle = "Basado en hallazgos de la Estrategia Nacional de Inclusión") +
  theme_void() +
  scale_fill_brewer(palette = "Pastel1")
ggsave("barreras.png", p1, width = 8, height = 6)

# 2. EDUCACIÓN
df_educa <- data.frame(
  Nivel = c("Primaria", "Secundaria", "Grado Univ.", "Postgrado"),
  Ahorro_Formal = c(22, 45, 78, 92)
)
p2 <- ggplot(df_educa, aes(x = factor(Nivel, levels=Nivel), y = Ahorro_Formal, group = 1)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(size = 3) +
  labs(title = "Relación entre Nivel Educativo y Ahorro Formal",
       x = "Nivel de Instrucción", y = "Tasa de Tenencia (%)") +
  theme_minimal()
ggsave("educacion.png", p2, width = 8, height = 6)

# 3. MOTIVOS (TREEMAP)
df_destino <- data.frame(
  Motivo = c("Emergencias", "Alimentos/Salud", "Educación", "Vivienda", "Negocio Propio"),
  Valor = c(40, 20, 15, 15, 10)
)
p3 <- ggplot(df_destino, aes(area = Valor, fill = Motivo, label = paste(Motivo, "\n", Valor, "%"))) +
  geom_treemap(colour = "white", size = 2) +
  geom_treemap_text(colour = "white", place = "centre", size = 15, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Principales Motivos de Ahorro en RD") +
  theme(legend.position = "none")
ggsave("motivos.png", p3, width = 8, height = 6)

# 4. GÉNERO
df_genero <- data.frame(
  Año = c("2019", "2019", "2023", "2023"),
  Sexo = c("Hombre", "Mujer", "Hombre", "Mujer"),
  Inclusion = c(51.3, 50.0, 58.2, 52.1)
)
p4 <- ggplot(df_genero, aes(x = Año, y = Inclusion, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Brecha de Inclusión Financiera por Género", y = "Tenencia de Productos (%)") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw()
ggsave("genero.png", p4, width = 8, height = 6)

# 5. Region
library(ggplot2)


df_regiones_corregido <- data.frame(
  Region = c("Gran Santo Domingo", "Norte o Cibao", "Sur", "Este"),
  Participacion = c(38.9, 32.9, 16.0, 12.2)
)


ggplot(df_regiones_corregido, aes(x = reorder(Region, -Participacion), y = Participacion, fill = Region)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(Participacion, "%")), vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Gran Santo Domingo" = "#003366", 
                               "Norte o Cibao" = "#1F4E78", 
                               "Sur" = "#D4AC0D", 
                               "Este" = "#2E75B6")) + 
  labs(title = "Distribución Regional de la Población con Productos Financieros",
       subtitle = "Participación de cada macroregión en el total de incluidos (ENIEF 2023)",
       x = "Macroregión", 
       y = "Porcentaje de Participación (%)",
       caption = "Fuente: Informe de Encuesta Nacional de Inclusión y Educación Financiera 2023") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11, face = "bold"))



# 6. MODELO TEÓRICO
W0 <- 10000; rf <- 0.005; c <- 450; theta <- 0.12; pi <- 0.0033; S_rate <- 0.15
u <- function(x) { ifelse(x > 0, log(x), NA) }
utilidad_formal <- function(Y) { S <- Y * S_rate; u(W0 + S * (1 + rf) - c) }
utilidad_informal <- function(Y) { S <- Y * S_rate; (1 - theta) * u(W0 + S * (1 - pi)) + theta * u(W0) }
ingresos <- seq(5000, 40000, by = 100)
df <- data.frame(Y = ingresos)
df$Formal <- sapply(df$Y, utilidad_formal)
df$Informal <- sapply(df$Y, utilidad_informal)
y_star <- df$Y[which.min(abs(df$Formal - df$Informal))]

p5 <- ggplot(df, aes(x = Y)) +
  geom_line(aes(y = Formal, color = "Ahorro Formal (Bancos)"), size = 1.2) +
  geom_line(aes(y = Informal, color = "Ahorro Informal (San/Efectivo)"), size = 1.2) +
  annotate("label", x = y_star, y = max(df$Informal, na.rm=T), 
           label = paste("Umbral Y* ≈ RD$", format(y_star, big.mark=",")), fill = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Modelo de Decisión de Ahorro", x = "Ingreso Mensual (RD$)", y = "Utilidad (A)") +
  scale_color_manual(values = c("Ahorro Formal (Bancos)" = "#003876", "Ahorro Informal (San/Efectivo)" = "#ce1126")) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("modelo_teorico.png", p5, width = 8, height = 6)
  
  