####################################################
#           IMPORTANDO BIBLIOTECAS                 #
####################################################
library(dplyr)

####################################################
#               IMPORTANDO DADOS                   #
####################################################
setwd("D:/Projetos/ciencia_de_dados_pos/fundamentos de estatistica/CO2EmissionByVehicle")
dados <- read.csv('dataset/CO2 Emissions_Canada.csv')
View(dados)
glimpse(dados)

####################################################
#             VARIAVEIS DO DATASET                 #
####################################################

#Make                             MARCA
#Model                            MODELO
#Vehicle.Class                    CLASS DO VEICULO DEPENDENDO DA SUA UTILIDADE, CAPACIDADE E PESO
#Engine.Size.L.                   TAMANHO DO MOTOR EM LITROS
#Cylinders                        NÚMERO DE CILINDROS
#Transmission                     TIPO DA TRANSMISSÃO COM O NÚMERO DE MARCHAS
#Fuel.Type                        TIPO DO COMBUSTÍVEL
#Fuel.Consumption.City..L.100.km. CONSUMO NA CIDADE EM LITROS A CADA 100KM
#Fuel.Consumption.Hwy..L.100.km.  CONSUMO NA ESTRADA EM LITROS A CADA 100KM
#Fuel.Consumption.Comb..L.100.km. CONSUMO COMBINADO EM LITROS A CADA 100KM (55% CIDADE, 45% ESTRADA)
#Fuel.Consumption.Comb..mpg.      CONSUMO COMBINADO EM MILHAS POR GALÃO
#CO2.Emissions.g.km.              EMISSÃO DO CO2 EM GRAMAS POR KILOMETRO NO CONSUMO COMBINADO


####################################################
#               AJUSTE DO DATASET                  #
####################################################

dados$Fuel.Type[dados$Fuel.Type == "D"] <- "Diesel"
dados$Fuel.Type[dados$Fuel.Type == "Z"] <- "G. Premium"
dados$Fuel.Type[dados$Fuel.Type == "E"] <- "Ethanol"
dados$Fuel.Type[dados$Fuel.Type == "X"] <- "G. Normal"
dados$Fuel.Type[dados$Fuel.Type == "N"] <- "Gas Natural"

####################################################
#                    FUNÇÕES                       #
####################################################
median_plot <- function(data, 
                        median_by, 
                        height = 10, 
                        title, 
                        x_lab="", 
                        y_lab="", 
                        las = 1, 
                        cex_n_size = 0.7, 
                        bar_color = "blue",
                        horiz = FALSE){
  
  m_data   <- tapply(data, median_by, median)
  m_data_s <- sort(m_data, decreasing = FALSE)
  par(mar=c(height,4,4,4))
  
  barplot(m_data_s,
          main = title,
          ylab = y_lab,
          xlab = x_lab,
          names.arg = as.factor(names(m_data_s)),
          col = bar_color,
          las = las,
          cex.names = cex_n_size,
          horiz = horiz)
}

median_plot_beside <- function( data_a, 
                                data_b, 
                                median_by, 
                                height = 10, 
                                title, 
                                x_lab="", 
                                y_lab="", 
                                las = 1, 
                                cex_n_size = 0.7, 
                                bar_color = "blue",
                                legend = NULL,
                                horiz = FALSE,
                                col = NULL){
  
  m_data_a <- tapply(data_a, median_by , median)
  m_data_a_s <- sort(m_data_a, decreasing = FALSE)
  
  m_data_b <- tapply(data_b, median_by, median)
  m_data_b_s <- sort(m_data_b, decreasing = FALSE)
  
  df <- data.frame(t(m_data_a_s))
  df[nrow(df) + 1,] = as.numeric(m_data_b_s)
  
  par(mar=c(height,4,4,4))
  
  barplot(as.matrix(df),
          main = title,
          ylab = y_lab,
          xlab = x_lab,
          names.arg = as.factor(names(m_data_a_s)),
          las = las,
          cex.names = cex_n_size,
          horiz = horiz,
          legend = legend,
          col = col,
          beside = TRUE )
  
}

####################################################
#               ANALISE DESCRITIVA                 #
####################################################

############################################################
# O que tem menor média de consumo combinado na questão de:#
############################################################
## Marca
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Make, 
             height = 8, 
             title = "Média de consumo por marca do veículo",
             y_lab = "Consumo combinado (L/100km)",
             las = 2)

## Classe do veiculo
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Vehicle.Class, 
             height = 10, 
             title = "Média de consumo por classe de veículo",
             y_lab = "Consumo combinado (L/100km)",
             las = 2)

## Combustível
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Fuel.Type, 
             height = 10, 
             title = "Média de consumo por tipo de combustível",
             y_lab = "Consumo combinado (L/100km)",
             cex_n_size = 0.8)

## Cilindros
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by =dados$Cylinders, 
             height = 9, 
             title = "Média de consumo por quantidade de cilindros",
             x_lab = "Quantidade de cilindros",
             y_lab = "Consumo combinado (L/100km)",
             cex_n_size = 1)

## Tipo da transmissão
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Transmission, 
             height = 9, 
             title = "Média de consumo por tipo de transmissão",
             x_lab = "Tipo da transmissão",
             y_lab = "Consumo combinado (L/100km)",
             las = 2,
             cex_n_size = 0.8)

##########################################################
# O que tem menor media de emissão de CO2 na questão de: #
##########################################################
## Marca
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Make, 
             height = 8, 
             title = "Média de emissão de CO2 por marca do veículo",
             y_lab = "Emissão de CO2 (g/km)",
             las = 2,
             cex_n_size = 0.7)

## Classe do veiculo
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Vehicle.Class, 
             height = 10, 
             title = "Média de emissão de CO2 por classe de veículo",
             y_lab = "Emissão de CO2 (g/km)",
             las = 2,
             cex_n_size = 0.7)


## Combustível
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Fuel.Type, 
             height = 10, 
             title = "Média de emissão de CO2 por tipo de combustível",
             y_lab = "Emissão de CO2 (g/km)",
             las = 1,
             cex_n_size = 1)

## Cilindros
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Cylinders, 
             height = 10, 
             title = "Média de emissão de CO2 por quantidade de cilindros",
             y_lab = "Emissão de CO2 (g/km)",
             x_lab = "Quantidade de cilindros",
             las = 1,
             cex_n_size = 1)

## Tipo da transmissão
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Transmission, 
             height = 10, 
             title = "Média de emissão de CO2 por tipo de transmissão",
             y_lab = "Emissão de CO2 (g/km)",
             x_lab = "Tipo da transmissão",
             las = 2,
             cex_n_size = 1)

###########################################################
#         Comparativo de média de consumo entre           #
#            cidade/rodovia na questão de:                #  
###########################################################

## Marca
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Make,
                    title = "Media de consumo por marca",
                    height = 10,
                    y_lab = "Consumo (L/100km)",
                    col = c("red", "blue"),
                    legend = c("cidade", "estrada"),
                    cex_n_size = 0.9)

## Classe do veiculo
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Vehicle.Class,
                    title = "Media de consumo por classe",
                    height = 9,
                    y_lab = "Consumo (L/100km)",
                    col = c("red", "blue"),
                    legend = c("cidade", "estrada"),
                    cex_n_size = 0.6)

## Combustível
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Fuel.Type,
                    title = "Media de consumo por tipo de combustível",
                    height = 6,
                    y_lab = "Consumo (L/100km)",
                    col = c("red", "blue"),
                    legend = c("cidade", "estrada"),
                    cex_n_size = 0.9,
                    las = 1 )

## Cilindros
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Cylinders,
                    title = "Media de consumo por cilindro",
                    height = 3,
                    y_lab = "Consumo (L/100km)",
                    col = c("red", "blue"),
                    legend = c("cidade", "estrada"),
                    cex_n_size = 1)

## Tipo da transmissão
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Transmission,
                    title = "Media de consumo por tipo de transmissão",
                    height = 5,
                    y_lab = "Consumo (L/100km)",
                    col = c("red", "blue"),
                    legend = c("cidade", "estrada"),
                    cex_n_size = 1,
                    las = 2)

