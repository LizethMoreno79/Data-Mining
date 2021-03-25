summary(cars)
str(cars)
attach(cars)
res <- boxplot(dist, ylab = "speed")
str(res)
ind <- which(dist<res$stats[1] | dist>res$stats[5])

#install.packages("outliers",dependencies = TRUE)
library(outliers)
hist(dist)
hist(dist,breaks = 9)
shapiro.test(dist)
grubbs.test(dist)
ind <- which.max(dist)
out <- dist[ind];out


# Apply Grubbs' test to the nitrate data
# Referencia:
# Grubbs, Frank (February 1969), Procedures for Detecting Outlying 
# Observations in Samples, Technometrics, 11(1), pp. 1-21.
# Stefansky, W. (1972), Rejecting Outliers in Factorial Designs, 
# Technometrics, 14, pp. 469-479.

#La prueba de Grubbs también se conoce como 
#la prueba residual normalizada máxima.

#Definición	La prueba de Grubbs se define para la hipótesis:
#  H_0 :	No hay valores atípicos en el conjunto de datos.
#  H_a :	Hay exactamente un valor atípico en el conjunto de datos
rivers <- read.csv("river.csv",head=T,sep = ";")
str(rivers)
grubbs.test(rivers$nitrate,two.sided = TRUE)

# Find row index of the max of the nitrate data
which.max(river$nitrate)

# Runs Grubbs' test excluding row 156
grubbs.test(river$nitrate[-156])
#############################################################
#Para detectar outliers en series de tiempo se necesita 
#############################################################
#https://roth.rbind.io/post/anomaly-detection/anomaly-detection/
#https://github.com/twitter/AnomalyDetection
#Referencia:
# 
# View contents of dataset
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# Show the time series of nitrate concentrations with time
plot(nitrate ~ index, data = river, type = "o")
table(rivers$months)
# Calculate the mean nitrate by month
monthly_mean <- tapply(river$nitrate, river$months, FUN = mean)
monthly_mean

# Plot the monthly means 
plot(monthly_mean, type = "o", xlab = "Month", ylab = "Monthly mean")

# Create a boxplot of nitrate against months
boxplot(nitrate~months,data=river)

# Run Seasonal-Hybrid ESD for nitrate concentrations
############################################
###############################################
# Use Seasonal-Hybrid ESD for nitrate concentrations
river_anomalies <- AnomalyDetectionVec(x = river$nitrate, period = 12, direction = 'both', plot = T)

# Print the anomalies
river_anomalies$anoms

# Print the plot
river_anomalies$plot

# Prueba de ESD híbrida estacional versus prueba de Grubbs
# Recuerde que al usar la prueba de Grubbs en los datos de nitrato de río, solo se encontró que la fila 156 era anómala,
#mientras que la ESD híbrida estacional identificó otras 2 anomalías de alto valor. ¿Cuál de los siguientes 
#proporciona la mejor explicación para la diferencia entre los dos enfoques?
#   Las anomalías adicionales son inusuales con respecto a la contribución estacional en su momento de ocurrencia.