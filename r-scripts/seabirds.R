# Combine cockle data with elevation data using a relational database approach 
# Schiermonnikoog transect

# clear everything in memory
rm(list=ls())

# load libraries
renv::restore()
library(tidyverse) 
# load the elevation data and show the first 10 records of the dataset
elevdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::filter(year %in% c(2019,2021,2022,2023,2024) & !is.na(TransectPoint_ID))|>
  dplyr::select(year,TransectPoint_ID,elevation_m)   # select  only distance_m and elevation 
elevdat
# plot the change in transect  elevation along the transect, using a separate graph for each for each year 
elevdat |>
  ggplot(aes(x=TransectPoint_ID, y=elevation_m, col=year)) +geom_line()

# plot the change in transect  elevation along the transect, using a separate line color for each year 

# read the birds abundance data, call the dataset birds
# filter to use only years 2019,2021,2022,2023,2024 and only 3 or less replicates and species Orchestia_gammarellus
# group by year and TransectPoint_ID
# calculate the sum of the number of birds found per year and TransectPoint
# do all the above in one pipeline
# calculate the mean number of birds and mean size for each distance
birdsdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRRwj4K6riKOkORXZ7NcrjocSmHtpY2ecxpBSRXasjfqy3ySJpiaps5evSa90Tk_m5IEmDMk8n1qSlg/pub?gid=822377591&single=true&output=csv") |>
  dplyr::filter(year %in% c(2019,2021,2022,2023,2024))|>
  dplyr::group_by(Year,Species) |>
  dplyr::summarise(CountSum=sum(Count, na.rm=TRUE))
print(birdsdat)

dplyr::filter(year==2017,
                CockleObs_ID!=468,
                CockleObs_ID!=1531) |>
  dplyr::group_by(TransectPoint_ID) |> #group by distance
  dplyr::summarize(n_obs=n(),
                   avg_l=mean(length_mm, na.rm=T),
                   sd_l=sd(length_mm,na.rm=T),
                   se_l=sd_l/sqrt(n_obs))
print(cdat2017)



# plot (with a line and points)  how the number of cockles changes with distance along the transect


##### merge the cockle and elevation data into a single table you call "combidat"
# using Distance_ID as the common variable between the two tables

combidat<-dplyr::left_join(elevdat2017,cdat2017,by="TransectPoint_ID") |>
  dplyr::mutate(n_obs=tidyr::replace_na(n_obs,0))
combidat


# show in a plot how cockle density changes with elevation
combidat |>
  ggplot(aes(x=elevation_m, y=n_obs)) +
  geom_point() + 
  # fit a linear regression
  geom_smooth(method="loess")


# predicted at 0.5 m (x)
# y = b0 + b1x   (b0 is intercept and b1 is the slope, x is elevation, y is no cockles




# show this model as a line in ggplot, with the confidence interval

# fit a better model, using a loess smoother
# show this model in ggplot

##### plot  how the size (as mean length) of cockles changes with  elevation along the transect
# omit the observations where length is NA (because no cockles were measures)
# fit a quadratic model (second-order polynomial)
# show for each point also the standard errors
# add appropriate labels for the x and y axis 

