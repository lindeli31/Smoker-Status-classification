rm(list=ls())

# import libraries --------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#note
# creatinina sierica , è un prodotto di scarto dei muscoli
#filtrato dai reni , valori alti associati a danni renali, valori bassi bassa massa muscolare
# AST (aspartato aminotransferasi) Alto: può indicare danno epatico, infarto, malattie muscolari.
# ALT (alanina aminotransferasi)Valori elevati: sofferenza epatica, epatiti, steatosi, tossicità da farmaci o alcol.
# 
# Gtp (γ-GTP o gamma-GT)
# Abuso di alcol Steatosi epatica (fegato grasso) Ostruzione biliare
# -------------------------------------------------------------------------

data <- read.csv("train_dataset.csv")
target = "smoking"
#factorizing categorial variables 
data <- data %>% mutate(hearing.left.=as.factor(hearing.left.),
                        hearing.right.=as.factor(hearing.right.),
                        dental.caries=as.factor(dental.caries),
                        smoking=as.factor(smoking))

data_no_categorial <- data %>%
  dplyr::select(c(, -dental.caries, -hearing.left., -hearing.right., -smoking))
#--------------------------------------------------
# corrplot
#-------------------------------------------------
library(corrplot)
corrplot(cor(data_no_categorial))
#le correlazioni tra queste variabili non sono tanto utili
#per la classificazione quanto per individuare se ci sono variabili
#le cui informazioni potrebbero essere riassunte insieme 

# HDL Cholesterol -----------
#almost ortogonal 
ggplot(data) +
  geom_point(shape = 20, aes(x = HDL, y = Cholesterol))

# LDL Cholesterol--------------
#high correlation as we have seen on the corrplot
ggplot(data)+geom_point(shape = 20, aes(x = LDL, y = Cholesterol))


#------------------------------------
# Continuous Variables-Smoking
#-----------------------------------

#------------------------------------------
# Overall Boxplots 
#------------------------------------------
#we transform data in long format for faceting
continuous_long<- data %>% 
  dplyr::select(smoking, where(is.numeric)) %>%
  dplyr::select(-c(Urine.protein)) %>% 
  pivot_longer(
    cols = -smoking, 
    names_to = "variable", 
    values_to = "value"
  )

#boxplot with facet, smoking~Urine.protein will be better
#explored in the next section

ggplot(continuous_long)+
  geom_boxplot(aes(x = smoking, y = value, col = smoking))+
  facet_wrap(~variable, scales  = "free_y", ncol = 4)+
  labs( 
       x = NULL, y= NULL)+
  scale_color_brewer(palette = "Set1")+
  theme_gray()+
  theme(
    legend.position = "none", 
    plot.title=element_text(face = "bold"),
    strip.text = element_text(size = 9)
  )

#-----------------------------------------
# Scatterplots
#----------------------------------------

#' Create Scatterplot with Colored Groups
#'
#' Generates a jittered scatterplot of two continuous variables colored by a categorical variable,
#' with automatic factor conversion and error handling.
#'
#' @param x_var Character string for variable on x-axis
#' @param y_var Character string for variable on y-axis
#' @param color_var Character string for grouping variable (default: "smoking")
#' @param jitter Logical indicating whether to add jitter (default: TRUE)
#' 
scatter_plot <- function(x_var, y_var, color_var, jitter) {
  tryCatch({
    # Verifica esistenza colonne
    if (!(x_var %in% names(data))) stop(paste("La variabile", x_var, "non esiste nel dataset"))
    if (!(y_var %in% names(data))) stop(paste("La variabile", y_var, "non esiste nel dataset"))
    if (!(color_var %in% names(data))) stop(paste("La variabile", color_var, "non esiste nel dataset"))
    
    # Conversione variabile colore a fattore
    data[[color_var]] <- as.factor(data[[color_var]])
    
    # Creazione plot
    if(jitter == T){
    p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[color_var]])) +
      geom_jitter(alpha = 0.7) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x = x_var,
        y = y_var,
        color = color_var
      ) +
      theme_minimal()
    
    print(p)
    }
    else{
      p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[color_var]])) +
        geom_point(alpha = 0.7) +
        scale_color_brewer(palette = "Set1") +
        labs(
          x = x_var,
          y = y_var,
          color = color_var
        ) +
        theme_minimal()
      
      print(p)
    }
    
  }, error = function(e) {
    message("ERRORE: ", e$message)
    return(NULL)
  })
}

#heifht hemoglobin----------------------------------

scatter_plot("height.cm.", "hemoglobin", target, jitter = T)

#wiast weight-------------------------------------

scatter_plot("waist.cm.", "weight.kg.", target, jitter= F)


# Cholesterol_LDL_smoking --------------------------


scatter_plot("LDL", "HDL", target, jitter = F)
#the two groups don't seem to have different levels of LDL, HDL

#AST-ALT----------------------------------------

scatter_plot("ALT", "AST", target, jitter = F)


#------------------------------------------------------
# CATEGORICAL variables - smoking 
#------------------------------------------------------

# Bar plot function 
#' Creates a bar plot showing proportions of a categorical variable grouped by another variable,
#' with options for marginalization direction and automatic labeling.
#'
#' @param var Character string specifying the main categorical variable to analyze
#' @param group_var Character string specifying the grouping variable (default: "smoking")
#' @param margin Marginalization direction: 
#'               1 = proportions within each level of `var`,
#'               2 = proportions within each level of `group_var` (default: 1)
#' @param title Optional custom plot title (autogenerated if NULL)
#' @param xlab Optional custom x-axis label (autogenerated if NULL)
#' @param ylab Optional y-axis label (default: "Proportions")
#'
plot_categorical <- function(var, group_var, margin = 1, 
                             title = NULL, xlab = NULL, ylab = "Proportions") {
  tryCatch({
    # Verifica colonne
    if (!(var %in% names(data))) stop(paste("Variabile", var, "non trovata"))
    if (!(group_var %in% names(data))) stop(paste("Variabile di gruppo", group_var, "non trovata"))
    
    # Calcolo proporzioni
    prop_table <- as.data.frame(prop.table(table(data[[var]], data[[group_var]]), margin))
    names(prop_table) <- c("Variable", "Group", "Proportion")
    
    # Titoli automatici se non specificati
    if (is.null(title)) {
      title <- if (margin == 1) {
        paste("Proporzioni di", group_var, "per livello di", var)
      } else {
        paste("Proporzioni di", var, "per livello di", group_var)
      }
    }
    
    if (is.null(xlab)) xlab <- if (margin == 1) var else group_var
    
    # Plot
    p <- ggplot(prop_table, aes(
      x = if (margin == 1) Variable else Group,
      y = Proportion,
      fill = if (margin == 1) Group else Variable
    )) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(
        aes(label = round(Proportion, 3)),
        position = position_dodge(width = 0.7),
        vjust = -0.5,
        size = 3
      ) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = title,
        x = xlab,
        y = ylab,
        fill = if (margin == 1) group_var else var
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = if (length(unique(data[[var]])) > 5) 45 else 0)
      )
    
    print(p)
    return(invisible(prop_table))
    
  }, error = function(e) {
    message("Errore: ", e$message)
    return(NULL)
  })
}
#we have to remind the overal size of two groups
#may bias the estimation of the relative frequencies
table(data$smoking)

#Urine.protein

plot_categorical("Urine.protein", target, margin = 1, title = "Proporzioni di fumatori
                 per ogni livello di proteina nelle urine", xlab = "Protien Levels")
plot_categorical("Urine.protein", target, margin = 2, title = "Proporzioni di fumatori
                 per ogni livello di proteina nelle urine", xlab = "Protien Levels")

#Hearing capability

plot_categorical("hearing.left.", target, margin = 2, title = "") 

plot_categorical("hearing.right.", target, margin = 2, title = "") 

  
#probabilmente da inglobare queste due in un'unica variabile
#dicotomica che indichi difetti all'udito


# dental caries -----------------------------------------------------------

plot_categorical("dental.caries", target, margin = 2, title ="")
