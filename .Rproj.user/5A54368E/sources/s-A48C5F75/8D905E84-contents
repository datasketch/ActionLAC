library(tidyverse)
library(openxlsx)


data <- read_csv('data/ActionLAC.csv', col_types = cols(.default = "c"))
# info <- data
# info$País <- iconv(data$País,to="ASCII//TRANSLIT")
# 
# info <- info %>% filter(País %in% c("Argentina", "Bolivia", "Paraguay", "Chile", "Brasil", "Ecuador", "Costa Rica", "Guatemala", "Mexico",
#                                     "Venezuela", "El Salvador", "Peru"))
# 
# write_csv(info, 'data/ActionLAC_filter.csv')

arge <- data %>% filter(País == 'Argentina') 
# HERRAMIENTO

unique(arge$Herramienta)


# ÁREA DE ACCIÓN

unique(arge$`Área de Acción`)

#TIPO DE ACCIÓN

unique(arge$`Tipo de Acción`)


# TIPO DE ACTOR

unique(arge$`Tipo de Actor`)


#INSTITUCIÓN

unique(arge$`Instituición o red`)
