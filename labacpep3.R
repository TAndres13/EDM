
setwd("C:/Users/pipet/Downloads")
getwd()

#librerias
library("FactoMineR")
library("dplyr")
library("kableExtra")
library("factoextra")
library("gridExtra")


CIUDADES <- readxl::read_excel("ciudades original-filtrado-con etiquetas (1).xlsx",
                               sheet = 1)
RHINT <- CIUDADES[,c("CIUDADES","RH_1","RH_2","RH_5","RH_6","RH_7","RH_8","RH_9",
                     "RH_10","RH_11","RH_12","RH_13","RH_14","RH_15","RH_16","INT_66",
                     "INT_67",
                     "INT_68","INT_69","INT_70")]
RHINT
colnames(RHINT)[2:20] <- c("PC", "TCP","AA","CBPS","CBES","RAP","CC","CPT","CS","MI","DCFS"
                           ,"VI","H","S","BCPIB","CI","TOE","DP","DM")


#3.Utilizar la funcion PCA del paquete FactoMiner para relalizar un ACP de las variables de recursos humanos
#(RH) utilizadno como ilustrativas las correspondieron al grupo


#CENTRAR Y ESTADARIZAR

RHINTce <- scale(RHINT[,2:20], scale = TRUE, center = TRUE)
colnames(RHINTce) <- colnames(RHINT)[2:20]
RHINTce
rownames(RHINTce) <- RHINT$CIUDADES


#summary(RHINTce)

require(FactoMineR)

#ACP
pcaRHINTce <- PCA(RHINTce, quanti.sup = c(15:19), graph = F)

pcaRHINTce


#VALORES PROPIOS Y VARANZAS
pcaRHINTce$eig

#tablan en latex
require(kableExtra)
vp <- round(pcaRHINTce$eig)
kable(vp, "latex", booktabs = T,
      caption = "Valores Propios", label = "vprankis") %>%
  kable_styling(latex_options = c("striped","scale_down", "hold_position"))


#Se obserbva que las primeras 2 componentes acumulan apenas el 41& de la varinaz y
#5 componentes acumulan el 76% de la varianza 


#tabla latex correlaciones variable factor

kable(round(pcaRHINTce$var$cor,4), "latex", booktabs = T, caption = "Correlaciones variables factor",
      label = "cor_var_fact") %>%
  kable_styling(latex_options = c("striped","scale_down", "hold_position"))


# graficos del acp

require(factoextra)
library(gridExtra)
bipl1 = fviz_pca_var(pcaRHINTce, labelsize = 3) + ggtitle("A) Variables") + xlim (-1.3, 1.3)
bipl2 = fviz_pca_ind(pcaRHINTce, labelsize = 3) + ggtitle("B) Ciudades (objetos)") + xlim(-3,3)
grid.arrange(bipl1, bipl2, nrow=1)


#biplot
#reprenstacion simulatane de universidades y variables

fviz_pca_biplot(pcaRHINTce)


#print(pcaRHINTce)


# Crear un dataframe con todas las métricas
var_coord <- as.data.frame(round(pcaRHINTce$var$coord, 4))
var_contrib <- as.data.frame(round(pcaRHINTce$var$contrib, 2))
var_cos2 <- as.data.frame(round(pcaRHINTce$var$cos2, 4))

# Combinar solo las primeras 2 dimensiones
var_results <- data.frame(
  Variable = rownames(var_coord),
  Coord_Dim1 = var_coord[,1],
  Coord_Dim2 = var_coord[,2],
  Contrib_Dim1 = var_contrib[,1],
  Contrib_Dim2 = var_contrib[,2],
  Cos2_Dim1 = var_cos2[,1],
  Cos2_Dim2 = var_cos2[,2]
)

# Crear la tabla
knitr::kable(var_results, "latex", booktabs = TRUE, 
             caption = "Resultados del PCA: Coordenadas, Contribución y Calidad de Representación",
             label = "pca_results") %>%
  kableExtra::kable_styling(latex_options = c("striped", "scale_down", "hold_position"))


library(knitr)
library(kableExtra)

# Extraer datos para 5 dimensiones
n_dim <- 5  # Número de dimensiones a mostrar

# Crear dataframe con los resultados
var_results <- data.frame(
  Variable = rownames(pcaRHINTce$var$coord),
  
  # Coordenadas (correlaciones)
  round(pcaRHINTce$var$coord[, 1:n_dim], 4),
  
  # Contribuciones (%)
  round(pcaRHINTce$var$contrib[, 1:n_dim], 2),
  
  # Calidad de representación (cos²)
  round(pcaRHINTce$var$cos2[, 1:n_dim], 4)
)

# Renombrar columnas para mejor legibilidad
colnames(var_results) <- c(
  "Variable",
  paste0("Dim", 1:n_dim, " (Coord)"), 
  paste0("Dim", 1:n_dim, " (Contrib)"),
  paste0("Dim", 1:n_dim, " (Cos²)")
)

# Generar tabla en LaTeX con kable
kable(var_results, "latex", booktabs = TRUE, 
      caption = "Resultados del PCA: Coordenadas, Contribución y Cos² para 5 Dimensiones",
      label = "pca_results_5d",
      align = c("l", rep("c", n_dim * 3))) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, 
                     "Coordenadas" = n_dim, 
                     "Contribución (%)" = n_dim, 
                     "Cos²" = n_dim))


#fviz_pca
#select.in selec.bar
  
    fviz_pca_var(pcaRHINTce, axes = c(1, 3),
             geom = c("point", "text"),
             repel = FALSE,
             habillage = "none",
             palette = NULL,
             addEllipses = FALSE,
             col.var = "black",
             fill.var = "white",
             col.var.sup = "blue",
             alpha.var = 1)

    
    
    print(pcaRHINTce$quanti.sup)
    