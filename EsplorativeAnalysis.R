rm(list=ls())

# import libraries --------------------------------------------------------

library(dplyr)
library(ggplot2)


# -------------------------------------------------------------------------


data <- read.csv("train_dataset.csv")
#factorizing categorial variables 
data$hearing.left.<- as.factor(data$hearing.left.)
levels(data$hearing.left.)
data$hearing.right.<- as.factor(data$hearing.right.)
levels(data$hearing.right.)
data$dental.caries<- as.factor(data$dental.caries)
data$smoking<- as.factor(data$smoking)
glimpse(data)


# pca ---------------------------------------------------------------------


data_no_categorial <- data %>% 
  select(c(, -dental.caries, -hearing.left., -hearing.right.))



#boxplot  smoke, age --------------------------------------------------------------


ggplot(data, aes(x = factor(smoking), y = age, fill = factor(smoking)))+
  geom_boxplot()+
  theme_minimal()


# smoke UrineProtein ------------------------------------------------------
table(data$Urine.protein, data$smoking)
counts<- data %>% 
  count(Urine.protein, smoking, name = "n")
counts

ggplot(counts, aes(x=Urine.protein, y = n, fill = smoking))+
  geom_col(position ="dodge", width = .5)+
  scale_fill_manual(values =c("#4E79A7", "#F28E2B"))+
  labs(
    title    = "Proteinuria by Smoking Status",
    subtitle = "Absolute counts for each urine-protein level (1 = lowest, 6 = highest)",
    x        = "Urine-protein level",
    y        = "Number of patients",
    fill     = "Smoking status"
  ) +
  theme_minimal()+
  theme(
     legend.position = "top",
     plot.title    = element_text(face = "bold", size = 16),
     plot.subtitle = element_text(size = 12),
     
   )

ggplot(counts, aes(x = Urine.potrein, y = n))
