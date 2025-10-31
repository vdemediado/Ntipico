library("ggplot2")
library("latex2exp")
# === Estadísticos ===
x       <- resultados2024.distritos$N
q1      <- as.numeric(quantile(x, 0.25, na.rm = TRUE))
med     <- as.numeric(quantile(x, 0.5, na.rm = TRUE))
mediana <- round(med, digits = 2)
media   <- mean(x, na.rm = TRUE)
media2d <- round(media, digits = 2)
q3      <- as.numeric(quantile(x, 0.75, na.rm = TRUE))
s       <- sd(x, na.rm = TRUE)
iqr     <- IQR(x, na.rm = TRUE)
cvi     <- iqr / med                                   # Coef. de variación basado en IQR
piqr    <- (q3 + q1 - 2 * med) / iqr                   # Asimetría de Bowley (IQR)

# ==== Fuentes: Garamond ====
library(sysfonts)
library(showtext)

# Intentar usar la Garamond del sistema.
# Si no está, usar EB Garamond desde Google como alias "Garamond".
if (!("Garamond" %in% sysfonts::font_families())) {
  try({
    sysfonts::font_add(family = "Garamond",
                       regular = "/Library/Fonts/Garamond.ttf")
  }, silent = TRUE)
}
if (!("Garamond" %in% sysfonts::font_families())) {
  sysfonts::font_add_google(name = "EB Garamond", family = "Garamond")
}
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)  # útil para consistencia en Rmd/Quarto
base_font <- "Garamond"

ggplot(resultados2024.distritos, aes(x = N)) +
  # Histograma:
  geom_histogram(
    binwidth = 0.12, color = GB1[4] , fill = "white",
    boundary = 0
  ) +
  # Banda IQR:
  annotate(
    "rect",
    xmin = q1, xmax = q3, ymin = 0, ymax = Inf,
    fill = GB1[4], alpha = 0.2, color = NA
  ) +
  # (opcional) línea de referencia:
  geom_vline(xintercept = media, linetype = "dashed", linewidth = 1, color = GB1[3]) +
  # Anotaciones con la MISMA fuente del documento:
  annotate(
    "text", x = q1-.4, y = Inf, vjust = 1.3, hjust = 0,
    label = TeX(paste0("Q1 = ", sprintf("%.2f", q1))), family = base_font, size = 3.6
  ) +
  annotate(
    "text", x = media, y = 41, vjust = 1.3, hjust = -.2,
    label = TeX(paste0("$\\bar{N}_v$ = ", sprintf("%.2f", media))), family = base_font, size = 3.6
  ) +
  annotate(
    "text", x = q3+.4, y = Inf, vjust = 1.3, hjust = 1,
    label = TeX(paste0("Q3 = ", sprintf("%.2f", q3))), family = base_font, size = 3.6
  ) +
  labs(
    x = TeX(r"($N_v$)"),
    y = "Frecuencia",
    caption = sprintf(
      "IQR = %.2f",
      iqr
    )
  ) +
  annotate(
    "label", x = 3.5, y = 40,
    label = paste0("Dispersión\n",
                   "IQR = ", sprintf("%.2f", iqr), "\n",
                   "s = ", sprintf("%.2f", s)),
    family = base_font,
    size = 3.8,
    hjust = 0,
    vjust = 1,
    label.size = 0.3,
    label.r = unit(0.15, "lines")
  ) +
  labs(
    x = TeX(r"($N_v$)"),
    y = "Frecuencia",
    caption = "Las coaliciones se toman como un solo partido.\nFuente: Cálculos propios a partir de cómputos distritales del INE (2024)"
  ) +
  theme_minimal(base_family = base_font) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.margin = margin(10, 10, 10, 10)
  )
