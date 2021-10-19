# -------------TRABAJO FINAL: RED NEURONAL -------------------------------------
#              Botello Estrada Emma Yolotzin

# Se escogen las librerias a usar --------------------------------------------
library(tidyverse)
library(devtools)
library (usethis)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(neuralnet)
# Descripción de la base a usar ------------------------------------------
# Variables contenidas
names(infert)
# Número de participantes
nrow(infert)
# Edad de las participantes
summary(infert$age)
# Número de casos 
n_case<-infert %>% 
  select(case) %>% 
  filter(case=="1") %>% 
  summarise(
    muestra= n(),
  )
n_case

# Número de controles
n_ctrl<-infert %>% 
  select(case) %>% 
  filter(case=="0") %>% 
  summarise(
    muestra= n(),
  )
n_ctrl

# Se elige una parte de datos para entrenar y para probar el modelo -------
set.seed(222)
casos_train<-sample(1:nrow(infert), round(0.70*nrow(infert)))
train_data<- as.data.frame(infert[casos_train,])
test_data<-as.data.frame(infert[-casos_train,])

# Se entrena al modelo ----------------------------------------------------
set.seed(222)
fertilidad.bp <- neuralnet(case ~ parity + induced + spontaneous, 
                           data = train_data, hidden = 2, err.fct = "ce", 
                           linear.output = FALSE, 
                           algorithm = "backprop", learningrate = 0.01, rep= 2)
# fertilidad.bp

# Se muestran los resultados numéricos del modelo -------------------------
fertilidad.bp$result.matrix

# Se grafica el modelo ----------------------------------------------------

plot(fertilidad.bp)
plot.nnet(fertilidad.bp)

# Se prueba el modelo -----------------------------------------------------
prediccion<- neuralnet::compute(fertilidad.bp, test_data)
prediccion$net.result

# Se redondean los outpouts del modelo -------------------------------------
# Para comparar los datos predichos con los esperados se redondean al entero más cercano
prediccion_r<-round(prediccion$net.result, digits = 0)

# Se genera un data frame con outpouts esperados y predichos --------------
outpout_final<- cbind( test_data$case, as.data.frame(prediccion_r))
colnames(outpout_final)<- c("Outpout_esperado", "Outpout_predicho")

# Se construye la matriz de confusión -------------------------------------
predict.table <-table(outpout_final$Outpout_esperado
                      , outpout_final$Outpout_predicho)
predict.table

# Se evalua el desempeño del modelo  --------------------------------------
# Se calcula el misclassification error 
miss_error<- 1-sum(diag(predict.table))/sum(predict.table)
miss_error


    # El modelo tiene un error de clasificación de 25.7 %


# Efecto de cada variable input en los outpouts ---------------------------
par(mfrow=c(2,2))
gwplot(fertilidad.bp,selected.covariate="parity", min=-2.5, max=5)
gwplot(fertilidad.bp,selected.covariate="induced", min=-2.5, max=5)
gwplot(fertilidad.bp,selected.covariate="spontaneous", min=-2.5, max=5)

