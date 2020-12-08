# Covid Data Portugal Dec 2020

# Libraries
library(zoo)
library(ggplot2)

# Define relevant variables
no_anos=10  # cannot be less than one
ano_mais_antigo = 2019-no_anos
print(paste("Ano mais antigo considerado é:", ano_mais_antigo))



# import file: COVID_19
CovidData_20201201 <- read.csv("C:\\Users\\Nuno\\Documents\\COVID\\covid19pt-data-master\\data.csv")
str(CovidData_20201201)
head(CovidData_20201201)

#convert date
CovidData_20201201$datas_covid <- as.Date(CovidData_20201201[,"data"], format="%d-%m-%Y")

# Daily Deaths, append a 0 at the beginning
CovidData_20201201$delta_obitos_covid_7day <- rollmean(append(diff(CovidData_20201201[,"obitos"], lag=1), 0, after = 0), 7, align="center", fill="extend")

# Plot daily deaths
plot(CovidData_20201201[,"datas_covid"], CovidData_20201201[,"delta_obitos_covid_7day"], type="l")




# import file: Deaths in Portugal last 10 Years
Obitos_2019 <- read.csv("C:\\Users\\Nuno\\Documents\\COVID\\Dados_SICO_20201201.csv")
str(Obitos_2019)
head(Obitos_2019)
Obitos_2019$datas_hist <- as.Date(Obitos_2019[,1], format="%d-%m-%y")
class(Obitos_2019[,"datas_hist"])

# 10 year average: the first 3 values are not very important so they were filled with "extend"
Obitos_2019$obitos_7_day_avg <- rollmean(rowMeans(Obitos_2019[, (12-no_anos):12], na.rm=TRUE), 7, align="center", fill="extend")
Obitos_2019$obitos_stdDev <- rollmean(apply(Obitos_2019[, (12-no_anos):12], 1, sd, na.rm=TRUE), 7, align="center", fill="extend")

# Deaths Y2020
Obitos_2019$obitos_2020_7day <- rollmean(Obitos_2019[, 13], 7, align="center", fill="extend")



# COVID + AVG
start_idx <- as.numeric(difftime(CovidData_20201201[1, "datas_covid"], Obitos_2019[1,14])) + 1
end_idx <- start_idx + length(CovidData_20201201[,"delta_obitos_covid_7day"]) - 1
CovidData_20201201$avg_plus_covid <- Obitos_2019[start_idx:end_idx, "obitos_7_day_avg"] + CovidData_20201201[,"delta_obitos_covid_7day"]




### Plot Yearly Deaths vs Covid
# Define colors
color_pallete <- c("2009-2019"="black", "Avg+StdDev"="gray", "Avg-StdDev"="gray", "2020"="coral3",
                   "Avg+COVID"="darkslateblue")

# Draw plot
p1 <- ggplot(Obitos_2019, aes(x=datas_hist)) + 
    scale_x_date(date_labels="%m/%d", date_breaks="1 month") +
    geom_line(aes(y=obitos_7_day_avg, color="2009-2019"), size=.5, linetype=2) + # AVG baseline
    geom_line(aes(y=obitos_7_day_avg+obitos_stdDev, color="Avg+StdDev")) + # StdDev top
    geom_line(aes(y=obitos_7_day_avg-obitos_stdDev, color="Avg-StdDev")) +  # StdDev bot
    geom_ribbon(aes(ymin=obitos_7_day_avg-obitos_stdDev, ymax=obitos_7_day_avg+obitos_stdDev), # Std Dev Fill
                fill="cornflowerblue", alpha=0.2) +
    geom_line(aes(y=obitos_2020_7day, color="2020"), size=.5) + # Total deaths in 2020
    geom_line(data=CovidData_20201201, aes(x=datas_covid, y=avg_plus_covid, color="Avg+COVID"), size=.5) +
    theme(legend.position = "right") +
    labs(x="Date", y="Deaths", color="Legend", title="Covid deaths in Portugal, 2020") +
    scale_color_manual(values=color_pallete)

p1


### Plot 2020 Deaths vs Covid ("normalized by average")
# Append covid deaths to total deaths frame
Obitos_2019[start_idx:end_idx,"covid_deaths"] <- CovidData_20201201[,"delta_obitos_covid_7day"]

# Define colors
color_2 <- c("Total"="coral3","StdDev"="gray", "Covid" = "darkslateblue", "Average" = "black")


# Draw plot
p2 <- ggplot(Obitos_2019, aes(x=datas_hist)) + 
    labs(x="Date", y="Deaths", title="Covid deaths in Portugal normalized by average deaths in 2009-2019.",
         color="Legenda") +
    geom_line(aes(y=obitos_2020_7day-obitos_7_day_avg, color="Total"), size=.5) +
    geom_line(aes(y=obitos_stdDev, color="StdDev"), size=0.5) +
    geom_line(data = CovidData_20201201, aes(x=datas_covid, y=delta_obitos_covid_7day, color="Covid"), size=.5) +
    geom_hline(aes(yintercept = 0, color = "Average"), linetype="dashed") +
    geom_ribbon(data=Obitos_2019, aes(ymin=covid_deaths, ymax=obitos_2020_7day-obitos_7_day_avg), fill="cornflowerblue", alpha=0.2) +
    scale_color_manual(values=color_2) +
    theme(legend.position = "right") +
    scale_x_date(date_labels="%m/%d", date_breaks="1 month", limits=as.Date(c("2020-03-9","2020-11-30")))
p2



# Final Results

Death_Difference = round(sum(Obitos_2019[start_idx:end_idx, "Y2020"])
                         - sum(Obitos_2019[start_idx:end_idx, "obitos_7_day_avg"])
                         - sum(Obitos_2019[start_idx:end_idx,"covid_deaths"])
                         , 0)

















