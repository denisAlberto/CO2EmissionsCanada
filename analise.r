####################################################
#           IMPORTANDO BIBLIOTECAS                 #
####################################################
library(dplyr)

####################################################
#               IMPORTANDO DADOS                   #
####################################################
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
#Cylinders                        N�MERO DE CILINDROS
#Transmission                     TIPO DA TRANSMISS�O COM O N�MERO DE MARCHAS
#Fuel.Type                        TIPO DO COMBUST�VEL
#Fuel.Consumption.City..L.100.km. CONSUMO NA CIDADE EM LITROS A CADA 100KM
#Fuel.Consumption.Hwy..L.100.km.  CONSUMO NA ESTRADA EM LITROS A CADA 100KM
#Fuel.Consumption.Comb..L.100.km. CONSUMO COMBINADO EM LITROS A CADA 100KM (55% CIDADE, 45% ESTRADA)
#Fuel.Consumption.Comb..mpg.      CONSUMO COMBINADO EM MILHAS POR GAL�O
#CO2.Emissions.g.km.              EMISS�O DO CO2 EM GRAMAS POR KILOMETRO NO CONSUMO COMBINADO


####################################################
#               AJUSTE DO DATASET                  #
####################################################

dados$Fuel.Type[dados$Fuel.Type == "Z"] <- "G. Premium"
dados$Fuel.Type[dados$Fuel.Type == "X"] <- "G. Comum"
dados$Fuel.Type[dados$Fuel.Type == "D"] <- "Diesel"
dados$Fuel.Type[dados$Fuel.Type == "E"] <- "Ethanol"
dados$Fuel.Type[dados$Fuel.Type == "N"] <- "Gas Natural"

####################################################
#                    FUN��ES                       #
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

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

####################################################
#               ANALISE DESCRITIVA                 #
####################################################

#######################################################
#       Tabela de analise entre combustiveis          #
#######################################################
t <-table(dados$Fuel.Type)

##Gasolina Comum
grep <- dados[grep("G. Comum", dados$Fuel.Type, ignore.case=T),]
s<-shapiro.test(grep$CO2.Emissions.g.km.[0:5000])
analitic <- data.frame(comb = "G. Comum", 
                       count=t['G. Comum'],
                      sd = sd(grep$CO2.Emissions.g.km.), 
                      var=var(grep$CO2.Emissions.g.km.),
                      mean=mean(grep$CO2.Emissions.g.km.),
                      median=mean(grep$CO2.Emissions.g.km.),
                      moda=getmode(grep$CO2.Emissions.g.km.),
                      min = quantile(grep$CO2.Emissions.g.km.)[1],
                      quartil.1=quantile(grep$CO2.Emissions.g.km.)[2],
                      quartil.2=quantile(grep$CO2.Emissions.g.km.)[3],
                      quartil.3=quantile(grep$CO2.Emissions.g.km.)[4],
                      max = quantile(grep$CO2.Emissions.g.km.)[5],
                      amp = range(grep$CO2.Emissions.g.km.)[2]-range(grep$CO2.Emissions.g.km.)[1],
                      coef.var = sd(grep$CO2.Emissions.g.km./mean(grep$CO2.Emissions.g.km.)),
                      normal=s$p.value > 0.05)

##Gasolina Premium
grep <- dados[grep("G. Premium", dados$Fuel.Type, ignore.case=T),]
s<-shapiro.test(grep$CO2.Emissions.g.km.[0:5000])
analitic[nrow(analitic) + 1,] <- data.frame(comb = "G. Premium", 
                                            count=t['G. Premium'],
                                            sd = sd(grep$CO2.Emissions.g.km.), 
                                            var=var(grep$CO2.Emissions.g.km.),
                                            mean=mean(grep$CO2.Emissions.g.km.),
                                            median=mean(grep$CO2.Emissions.g.km.),
                                            moda=getmode(grep$CO2.Emissions.g.km.),
                                            min = quantile(grep$CO2.Emissions.g.km.)[1],
                                            quartil.1=quantile(grep$CO2.Emissions.g.km.)[2],
                                            quartil.2=quantile(grep$CO2.Emissions.g.km.)[3],
                                            quartil.3=quantile(grep$CO2.Emissions.g.km.)[4],
                                            max=quantile(grep$CO2.Emissions.g.km.)[5],
                                            amp=range(grep$CO2.Emissions.g.km.)[2]-range(grep$CO2.Emissions.g.km.)[1],
                                            coef.var = sd(grep$CO2.Emissions.g.km./mean(grep$CO2.Emissions.g.km.)),
                                            normal=s$p.value > 0.05)

##Diesel
grep <- dados[grep("Diesel", dados$Fuel.Type, ignore.case=T),]
s<-shapiro.test(grep$CO2.Emissions.g.km.[0:5000])
analitic[nrow(analitic) + 1,] <- data.frame(comb = "Diesel", 
                                            count=t['Diesel'],
                                            sd = sd(grep$CO2.Emissions.g.km.), 
                                            var=var(grep$CO2.Emissions.g.km.),
                                            mean=mean(grep$CO2.Emissions.g.km.),
                                            median=mean(grep$CO2.Emissions.g.km.),
                                            moda=getmode(grep$CO2.Emissions.g.km.),
                                            min = quantile(grep$CO2.Emissions.g.km.)[1],
                                            quartil.1=quantile(grep$CO2.Emissions.g.km.)[2],
                                            quartil.2=quantile(grep$CO2.Emissions.g.km.)[3],
                                            quartil.3=quantile(grep$CO2.Emissions.g.km.)[4],
                                            max=quantile(grep$CO2.Emissions.g.km.)[5],
                                            amp=range(grep$CO2.Emissions.g.km.)[2]-range(grep$CO2.Emissions.g.km.)[1],
                                            coef.var = sd(grep$CO2.Emissions.g.km./mean(grep$CO2.Emissions.g.km.)),
                                            normal=s$p.value > 0.05)

##Ethanol
grep <- dados[grep("Ethanol", dados$Fuel.Type, ignore.case=T),]
s<-shapiro.test(grep$CO2.Emissions.g.km.[0:5000])
analitic[nrow(analitic) + 1,] <- data.frame(comb = "Ethanol", 
                                            count=t['Ethanol'],
                                            sd = sd(grep$CO2.Emissions.g.km.), 
                                            var=var(grep$CO2.Emissions.g.km.),
                                            mean=mean(grep$CO2.Emissions.g.km.),
                                            median=mean(grep$CO2.Emissions.g.km.),
                                            moda=getmode(grep$CO2.Emissions.g.km.),
                                            min = quantile(grep$CO2.Emissions.g.km.)[1],
                                            quartil.1=quantile(grep$CO2.Emissions.g.km.)[2],
                                            quartil.2=quantile(grep$CO2.Emissions.g.km.)[3],
                                            quartil.3=quantile(grep$CO2.Emissions.g.km.)[4],
                                            max=quantile(grep$CO2.Emissions.g.km.)[5],
                                            amp=range(grep$CO2.Emissions.g.km.)[2]-range(grep$CO2.Emissions.g.km.)[1],
                                            coef.var = sd(grep$CO2.Emissions.g.km./mean(grep$CO2.Emissions.g.km.)),
                                            normal=s$p.value > 0.05)

##Gas Natural
grep <- dados[grep("Gas Natural", dados$Fuel.Type, ignore.case=T),]
s<-shapiro.test(grep$CO2.Emissions.g.km.[0:5000])
analitic[nrow(analitic) + 1,] <- data.frame(comb = "Gas Natural", 
                                            count=t['Gas Natural'],
                                            sd = sd(grep$CO2.Emissions.g.km.), 
                                            var=var(grep$CO2.Emissions.g.km.),
                                            mean=mean(grep$CO2.Emissions.g.km.),
                                            median=mean(grep$CO2.Emissions.g.km.),
                                            moda=getmode(grep$CO2.Emissions.g.km.),
                                            min = quantile(grep$CO2.Emissions.g.km.)[1],
                                            quartil.1=quantile(grep$CO2.Emissions.g.km.)[2],
                                            quartil.2=quantile(grep$CO2.Emissions.g.km.)[3],
                                            quartil.3=quantile(grep$CO2.Emissions.g.km.)[4],
                                            max=quantile(grep$CO2.Emissions.g.km.)[5],
                                            amp=range(grep$CO2.Emissions.g.km.)[2]-range(grep$CO2.Emissions.g.km.)[1],
                                            coef.var = sd(grep$CO2.Emissions.g.km./mean(grep$CO2.Emissions.g.km.)),
                                            normal=s$p.value > 0.05)
View(analitic)

############################################################
#      m�dia de consumo combinado na quest�o de:           #
############################################################
## Marca
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Make, 
             height = 8, 
             title = "M�dia de consumo por marca do ve�culo",
             y_lab = "Consumo combinado (L/100km)",
             las = 2)

## Classe do veiculo
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Vehicle.Class, 
             height = 10, 
             title = "M�dia de consumo por classe de ve�culo",
             y_lab = "Consumo combinado (L/100km)",
             las = 2)

## Combust�vel
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Fuel.Type, 
             height = 10, 
             title = "M�dia de consumo por tipo de combust�vel",
             y_lab = "Consumo combinado (L/100km)",
             cex_n_size = 0.8)

## Cilindros
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by =dados$Cylinders, 
             height = 9, 
             title = "M�dia de consumo por quantidade de cilindros",
             x_lab = "Quantidade de cilindros",
             y_lab = "Consumo combinado (L/100km)",
             cex_n_size = 1)

## Tipo da transmiss�o
median_plot( data = dados$Fuel.Consumption.Comb..L.100.km., 
             median_by = dados$Transmission, 
             height = 9, 
             title = "M�dia de consumo por tipo de transmiss�o",
             x_lab = "Tipo da transmiss�o",
             y_lab = "Consumo combinado (L/100km)",
             las = 2,
             cex_n_size = 0.8)

##########################################################
# O que tem menor media de emiss�o de CO2 na quest�o de: #
##########################################################
## Marca
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Make, 
             height = 8, 
             title = "M�dia de emiss�o de CO2 por marca do ve�culo",
             y_lab = "Emiss�o de CO2 (g/km)",
             las = 2,
             cex_n_size = 0.7)

## Classe do veiculo
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Vehicle.Class, 
             height = 10, 
             title = "M�dia de emiss�o de CO2 por classe de ve�culo",
             y_lab = "Emiss�o de CO2 (g/km)",
             las = 2,
             cex_n_size = 0.7)


## Combust�vel
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Fuel.Type, 
             height = 10, 
             title = "M�dia de emiss�o de CO2 por tipo de combust�vel",
             y_lab = "Emiss�o de CO2 (g/km)",
             las = 1,
             cex_n_size = 1)

## Cilindros
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Cylinders, 
             height = 10, 
             title = "M�dia de emiss�o de CO2 por quantidade de cilindros",
             y_lab = "Emiss�o de CO2 (g/km)",
             x_lab = "Quantidade de cilindros",
             las = 1,
             cex_n_size = 1)

## Tipo da transmiss�o
median_plot( data = dados$CO2.Emissions.g.km., 
             median_by = dados$Transmission, 
             height = 10, 
             title = "M�dia de emiss�o de CO2 por tipo de transmiss�o",
             y_lab = "Emiss�o de CO2 (g/km)",
             x_lab = "Tipo da transmiss�o",
             las = 2,
             cex_n_size = 1)

###########################################################
#         Comparativo de m�dia de consumo entre           #
#            cidade/rodovia na quest�o de:                #  
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

## Combust�vel
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Fuel.Type,
                    title = "Media de consumo por tipo de combust�vel",
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

## Tipo da transmiss�o
median_plot_beside( data_a = dados$Fuel.Consumption.City..L.100.km., 
                    data_b = dados$Fuel.Consumption.Hwy..L.100.km.,
                    median_by = dados$Transmission,
                    title = "Media de consumo por tipo de transmiss�o",
                    height = 5,
                    y_lab = "Consumo (L/100km)",
                    col = c("red", "blue"),
                    legend = c("cidade", "estrada"),
                    cex_n_size = 1,
                    las = 2)

###########################################################
#                  Regress�o Linear                       #  
###########################################################
##Gasolina Comum Vs Emiss�o de CO2
grep <- dados[grep("G. Normal", dados$Fuel.Type, ignore.case=T),]
a <-lm(CO2.Emissions.g.km. ~ Fuel.Consumption.City..L.100.km., data = grep)
plot( main = "Gasolina comum vs Emiss�o de CO2",
      xlab = "Emiss�o de CO2 (g/km)",
      ylab = "Gasolina comum (L/100km)",
      grep$CO2.Emissions.g.km.,
      grep$Fuel.Consumption.City..L.100.km.,
      xlim = c(100, 600),
      ylim = c(5,30),
      col = "blue", cex = 0.6, lwd = 2 )

##Gasolina Premium Vs Emiss�o de CO2
grep <- dados[grep("G. Premium", dados$Fuel.Type, ignore.case=T),]
plot( main = "Gasolina premium vs Emiss�o de CO2",
      xlab = "Emiss�o de CO2 (g/km)",
      ylab = "Gasolina premium (L/100km)",
      grep$CO2.Emissions.g.km.,
      grep$Fuel.Consumption.City..L.100.km., 
      xlim = c(100, 600),
      ylim = c(5,30),
      col = "blue", cex = 0.6, lwd = 2 )

## Diesel Vs Emiss�o de CO2
grep <- dados[grep("diesel", dados$Fuel.Type, ignore.case=T),]
plot( main = "Diesel vs Emiss�o de CO2",
      xlab = "Emiss�o de CO2 (g/km)",
      ylab = "Diesel (L/100km)",
      grep$CO2.Emissions.g.km.,
      grep$Fuel.Consumption.City..L.100.km., 
      xlim = c(100, 600),
      ylim = c(5,30),
      col = "blue", cex = 0.6, lwd = 2 )

## Ethanol Vs Emiss�o de CO2
grep <- dados[grep("Ethanol", dados$Fuel.Type, ignore.case=T),]
plot( main = "Ethanol vs Emiss�o de CO2",
      xlab = "Emiss�o de CO2 (g/km)",
      ylab = "Ethanol (L/100km)",
      grep$CO2.Emissions.g.km.,
      grep$Fuel.Consumption.City..L.100.km., 
      xlim = c(100, 600),
      ylim = c(5,30),
      col = "blue", cex = 0.6, lwd = 2 )



ethanol <- dados[grep("Ethanol", dados$Fuel.Type, ignore.case=T),]
g_normal <- dados[grep("G. Normal", dados$Fuel.Type, ignore.case=T),]
gas_natural <- dados[grep("Gas Natural", dados$Fuel.Type, ignore.case=T),]

plot( g_premium$Fuel.Consumption.City..L.100.km., g_premium$CO2.Emissions.g.km.,  col="red" )
par(new=TRUE)
plot( diesel$Fuel.Consumption.City..L.100.km., 
      diesel$CO2.Emissions.g.km., 
      pch = 21,
      bg = "red",   
      col = "blue", 
      cex = 0.6,    
      lwd = 2 )

plot(g_premium$Fuel.Consumption.City..L.100.km. ~ g_premium$CO2.Emissions.g.km.)
plot(diesel$Fuel.Consumption.City..L.100.km. ~ diesel$CO2.Emissions.g.km.)
plot(ethanol$Fuel.Consumption.City..L.100.km. ~ ethanol$CO2.Emissions.g.km.)
plot(g_normal$Fuel.Consumption.City..L.100.km. ~ g_normal$CO2.Emissions.g.km.)
plot(gas_natural$Fuel.Consumption.City..L.100.km. ~ gas_natural$CO2.Emissions.g.km.)






m_data_a <- tapply(dados$Fuel.Consumption.City..L.100.km., dados$Make , median)
m_data_a_s <- sort(m_data_a, decreasing = FALSE)

m_data_b <- tapply(dados$Fuel.Consumption.Hwy..L.100.km., dados$Make, median)
m_data_b_s <- sort(m_data_b, decreasing = FALSE)

df <- data.frame(cidade=m_data_a_s, rodovia=m_data_b_s)
boxplot(df)

par(mar=c(height,4,4,4))


boxplot(dados$Fuel.Consumption.City..L.100.km.)


