# Data

# Final 1

---
title: "NYPD Incident Report (HTML)"
author: "JJuran"
date: '2022-10-02'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**NYPD Incident Report**

For this project, we were tasked with analyzing historical NYPD Incident Data, collected from 2016 through 2021. The raw data, found on the Data.gov website (<https://catalog.data.gov/dataset/strategic-subject-list-historical>), contains 21 fields with 844 observations. The data contained a wide variety of information, from victim information to location data to offender information.

Through this analysis, I began by tackling two questions. First, I explored what the murder breakdown by time and date looked like. I analyzed the distribution of time of day and by month for the murders. Second, I explored how the distribution of crime scattered across New York. I wanted to see if there was a certain location that was more likely to contain a murder.

**Data Exploration and Cleaning:**

To begin, I began by importing the data into R Studio and started to explore the information. I wanted to gain an understanding of what fields contained missing information, what types of data types were in the file, and begin to form a plan of how to transform the raw data into something usable.

```{r cars}
library(readr)
NYPD <- read_csv('NYPD_Incident.csv') 
NYPD2 <- read_csv('NYPD_Incident.csv')
summary(NYPD)
```

**Question 1 Analysis: Are there any patterns or insights from looking at the breakdown of murders by time and date?**

From looking at the breakdown of murders by hour of the day, there is a clear pattern that the number of murders slows down around 5am and stays low until about noon. From 1pm to midnight, the overall number of murders gradually increases. From midnight to 5am, the number of murders slowly dwindles, however, still remains higher that the numbers during the mid-morning. This analysis, if anything, confirms a well-known phrase: nothing good happens after midnight!

```{r pressure, echo=FALSE}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

which(is.na(NYPD$INCIDENT_KEY))
which(is.na(NYPD$OCCUR_DATE))
which(is.na(NYPD$OCCUR_TIME))
which(is.na(NYPD$BORO))
which(is.na(NYPD$LOC_OF_OCCUR_DESC))
which(is.na(NYPD$PRECINCT))
which(is.na(NYPD$JURISDICTION_CODE))
which(is.na(NYPD$LOC_CLASSFCTN_DESC))
which(is.na(NYPD$LOCATION_DESC)) #yes NA
which(is.na(NYPD$STATISTICAL_MURDER_FLAG)) #yes NA
which(is.na(NYPD$PERP_AGE_GROUP)) #yes NA
which(is.na(NYPD$PERP_SEX)) #yes NA
which(is.na(NYPD$PERP_RACE)) #yes na
which(is.na(NYPD$VIC_AGE_GROUP)) #switched out unknown value for overall average
which(is.na(NYPD$VIC_SEX))
which(is.na(NYPD$VIC_RACE))
which(is.na(NYPD$X_COORD_CD))
which(is.na(NYPD$Y_COORD_CD))
which(is.na(NYPD$Latitude))
which(is.na(NYPD$Longitude))
which(is.na(NYPD$`New Georeferenced Column`))

#Transform Raw Data into workable data set
NYPD <- NYPD %>%
  select(-c(BORO, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD, Latitude,
            Longitude, `New Georeferenced Column`)) %>%
  unite(OCCUR_DATE_TIME, OCCUR_DATE:OCCUR_TIME, sep = ' ', remove = TRUE) %>%
  mutate(OCCUR_DATE_TIME = mdy_hms(OCCUR_DATE_TIME)) %>%
  rename(LOCATION_TYPE = LOCATION_DESC,
         DATE_TIME = OCCUR_DATE_TIME,
         MURDER = STATISTICAL_MURDER_FLAG)
summary(NYPD)
```

Analysis follows:

```{r}
#Analysis - Question 1 - Compare murders by Hour, Month, and Year
  #Will Convert Fields to different data types as go through analysis
NYPD$MURDER <- as.numeric(NYPD$MURDER)

murdersByHour <- NYPD %>%
  group_by(hour = format(lubridate::floor_date(DATE_TIME, 'hour'), "%H")) %>%
  summarize(murders = sum(MURDER))
ggplot(murdersByHour, aes(x = hour, y = murders)) + geom_bar(stat = "identity") +
  labs(title = "New York 2016 - 2021: Murders by Hour")

murdersByMonth <- NYPD %>%
  group_by(month = format(lubridate::floor_date(DATE_TIME, 'month'), '%m')) %>%
  summarize(murders = sum(MURDER))
ggplot(murdersByMonth, aes(x = month, y = murders), fill = vic_sex) + geom_bar(stat = "identity") +
  labs(title = "New York 2016 - 2021: Murders by Month")

```

**Question 2: What does the distribution of crime look like?**

By plotting the number of murders against the precinct, we can gain a better understanding of which areas saw the most murders. I overlapped the Boro field over the plot, with color, to better see the breakdown. From this graph, we can see that Brooklyn and the Bronx overwhelmingly had the highest murder counts. Manhattan had two precincts that had higher counts, but the rest were relatively low. Queens and Staten Island had comparably low murder counts.

```{r}
Incident_data <- NYPD2
Incident_data <- NYPD2 %>%
  select(-c("INCIDENT_KEY", "JURISDICTION_CODE",
            "X_COORD_CD","Y_COORD_CD", 
            `New Georeferenced Column`))

Incident_data %>% mutate(OCCUR_DATE = mdy(OCCUR_DATE))
Incident_data$STATISTICAL_MURDER_FLAG <- as.numeric(Incident_data$STATISTICAL_MURDER_FLAG)

Incident_data
Incident_data_ommitted_na <- na.omit(Incident_data)

graph_incidents <-Incident_data %>% ggplot(aes(PRECINCT, fill = BORO)) + geom_bar() + 
  theme(legend.position = "bottom", legend.key.size = unit(.20,"cm")) 
graph_incidents

graph_incidents2 <- Incident_data %>% ggplot(aes(LOC_OF_OCCUR_DESC, fill = BORO)) + geom_bar() +
  theme(legend.position = "top", legend.key.size = unit(0.30, "cm"))
graph_incidents2

```

**Model:**

For the model part of the analysis, I decided to run two different ones to enhance the analysis I ran above. First, I created a model that looked at murder and the date/time. I wanted to expand off of the first question I explored. Through running this model, I found that murders could not be predicted off of the date or time independently. The R-Squared value for this model was extremely low, indicating a very poor score.

For the second model, I looked at how well the murders could be predicted by the incident key - basically looking for a pattern in the reports. Again, this model did not perform very well. I believe that if I included more fields, such as Boro, Precinct, and maybe location points, the model could predict the murders better.

```{r}
#model <- lm(NYPD$MURDER ~ NYPD$DATE_TIME, Data = NYPD)
#model
#summary(model)

  #no linear relationship between murder and date/time
#model2 <- lm(NYPD$MURDER ~ NYPD$INCIDENT_KEY, data = NYPD)
#model2
#summary(model2)

```

![](images/Screen%20Shot%202022-10-01%20at%2019.04.08.png)

**Conclusion:**

Through this project I learned more about how to work with data in R Studio and how to use the present information to guide analysis. With all of the work above done, I would like to note a potential source of bias within my work. First off, I focused in on the fields that were completely filled out - with not nulls/missing information. This could limit my view, and thus my analysis, on the murders that occurred. Second, I did not pull in any additional data that could have supplemented my analysis. I was working with a limited perspective on the crimes - there could have been other factors that impacted these patterns, such as weather, gangs, police force distribution, and more.

Thank you for reading through this report. I hope you found it interesting and informational!


#Final 2
---
title: "Data_Science_Final"
author: "JJuran"
date: '2022-10-02'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## COVID-19 Data Analysis

Over the past few years, COVID-19 has greatly impacted our lives. Through these tough times, the World Health Organization and other large organizations have collected tons of data surrounding the pandemic. The data set used through this analysis was provided by Johns Hopkins University. It contains information surrounding cases, deaths, dates, states, provinces, and more. Through this analysis, I combined four unique data sets to aid in my analysis of the pandemic.

```{r cars}
library(readr)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)

us_confirmed <- read_csv("time_series1.csv")
global_confirmed <- read_csv("time_series_2.csv")
us_deaths <- read_csv("time_series_3.csv")
global_deaths <- read_csv("time_series_4.csv")
```

##Global Data

After reading in the four different data sets, I cleaned and combined two of them relating to the global COVID-19 data. One of the data sets contained information about all the COVID-19 cases that have been reported since early 2020. The other data set contained infromation about all of the COVID-19 deaths that have been reported.

```{r}
global_confirmed <- global_confirmed %>%
  pivot_longer(cols = -c(`Province/State`,
                         `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "cases") %>% select(-c(Lat,Long))
view(global_confirmed)

#global deaths
global_deaths <- global_deaths %>%
  pivot_longer(cols = -c(`Province/State`,
                         `Country/Region`,
                         Lat, Long),
               names_to = "date",
               values_to = "deaths") %>%
  select(-c(Lat, Long))

#connect all global information
global <- global_confirmed %>%
  full_join(global_deaths) %>%
  rename(Country_Region = `Country/Region`,
         Province_State = `Province/State`) %>% mutate(date = mdy(date))
summary(global)
```

##Global Data Visualizations

The views below show an outline of the COVID-19 data relating to all of the global information.

```{r}
#looking at only when cases are positive
global <- global %>% filter(cases > 0)
summary(global)
global %>% filter(cases > 28000000) #check for validity


global_vis <- global %>%
  group_by(Province_State, Country_Region, date) %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>%
  select(Province_State, Country_Region, date, cases, deaths) %>%
  ungroup()

ggplot(aes(x = date, y = cases), data = global) + geom_point(aes(y = cases, color = "Province_State"))

ggplot(aes(x = date, y = deaths), data = global) + geom_point(aes(y = deaths, color = "Combined_Key"))

```

##US Data

After gaining a little bit of a better understanding around the global data, I wanted to focus some of my attention on cleaning, combining, visualizing and analyzing the COVID-19 Data within the United States. Through the cleaning process, I pivoted the tables so that all of the dates were one attribute. Also, I combined the tables so that each record was one date, in one location, with cases and deaths reported.

```{r}
#US Cases
us_confirmed

us_confirmed %>% pivot_longer(cols = -c(UID:Combined_Key),
                              names_to = "date",
                              values_to = "cases")

us_confirmed <- us_confirmed %>%
  pivot_longer(cols = -c(UID:Combined_Key),
               names_to = "date",
               values_to = "cases") %>%
  select(Admin2: cases) %>%
  mutate(date = mdy(date)) %>%
  select(-c(Lat, Long_))
us_confirmed

#US Deaths
us_deaths <- us_deaths %>%
  pivot_longer(cols = -c(UID:Population),
               names_to = "date",
               values_to = "deaths") %>%
  select(Admin2:deaths) %>%
  mutate(date = mdy(date)) %>%
  select(-c(Lat,Long_))
us_deaths

#join us data together
us <- us_confirmed %>%
  full_join(us_deaths)
us
summary(us)

#Combine
global <- global %>%
  unite("Combined_Key",
        c(Province_State, Country_Region),
        sep = ", ",
        na.rm = TRUE,
        remove = FALSE)
```

##US Visualizations

Below, I've included multiple visualizations representing COVID-19 data - both cases and deaths - over the entire United States and then in specific states (Colorado, Texas, Idaho, and more). I wanted to see data relating to enviornments I was personally connected to during the early stages of the pandemic.

```{r}

#Visualization
us_by_state <- us %>%
  group_by(Province_State, Country_Region, date) %>%
  summarize(cases = sum(cases), deaths = sum(deaths),
            Population = sum(Population)) %>%
  mutate(deaths_per_mill = deaths *1000000/Population) %>%
  select(Province_State, Country_Region, date, cases, deaths, deaths_per_mill, Population) %>%
  ungroup()
us_by_state

#US totals
us_totals <- us_by_state %>%
  group_by(Country_Region, date) %>%
  summarize(cases = sum(cases), deaths = sum(deaths),
            Population = sum(Population)) %>%
  mutate(deaths_per_mill = deaths *1000000/Population) %>%
  select(Country_Region, date,
         cases, deaths, deaths_per_mill, Population) %>%
  ungroup()
tail(us_totals)

#Visualization
us_totals %>%
  filter(cases > 0) %>%
  ggplot(aes(x= date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90))+
  labs(title = "COVID19 in US", y = NULL)
  
#look at one specific state
state <- "Colorado"
us_by_state %>%
  filter(Province_State == state) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID-19 in ", state), y = NULL)

state2 <- "Texas"
us_by_state %>%
  filter(Province_State == state2) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID-19 in ", state2), y = NULL)

state3 <- "Idaho"
us_by_state %>%
  filter(Province_State == state3) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID-19 in ", state3), y = NULL)

state4 <- "New York"
us_by_state %>%
  filter(Province_State == state4) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID-19 in ", state4), y = NULL)

state5 <- "Florida"
us_by_state %>%
  filter(Province_State == state4) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID-19 in ", state5), y = NULL)

state6 <- "Alabama"
us_by_state %>%
  filter(Province_State == state6) %>%
  filter(cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) +
  geom_point(aes(color = "cases")) +
  geom_line(aes(y = deaths, color = "deaths")) +
  geom_point(aes(y = deaths, color = "deaths")) +
  scale_y_log10() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  labs(title = str_c("COVID-19 in ", state6), y = NULL)

max(us_totals$date)  
max(us_totals$deaths)

ggplot(aes(x = date, y = cases), data = us_by_state) + geom_point()

#Analyzing Data
us_by_state <- us_by_state %>%
  mutate(new_cases = cases - lag(cases),
         new_deaths = deaths - lag(deaths))
us_totals <- us_totals %>%
  mutate(new_cases = cases - lag(cases),
         new_deaths = deaths - lag(deaths))

tail(us_totals %>% select(new_cases, new_deaths, everything()))

us_state_totals <- us_by_state %>%
  group_by(Province_State) %>%
  summarize(deaths = max(deaths), cases = max(cases),
            population = max(Population),
            cases_per_thou = 1000*cases/population,
            deaths_per_thou = 1000*deaths/population) %>%
  filter(cases > 0, population > 0)

summary(us_state_totals)
```
The graph above explores how cases varied by state across the United States. 

##US Data Models

I built three models to look at different relationships within the data above. The first one explores the relationship between cases and deaths. Specifically, explores how cases can predict deaths. The R-Squared value was very high on this model, indicating a pretty good-fitting model.

The second model explored how well the attribute 'new deaths' could predict 'new cases'. The R-Squared value on this model was marginally lower (0.15). I believe if I would have included other predictor variables, such as population and location, the output may have been much better.

The third model I analyzed explored if Population could predict the number of deaths. The R-Squared value from this model was better than the second model, however, not as great as the first. The value only came out to be 0.604. I believe I could have improved this model if I would have also included cases as an additional predictor.

```{r}
#Modeling Data

mod <- lm(deaths ~ cases, data = us_by_state)
summary(mod)

mod2 <- lm(new_cases ~ new_deaths, data = us_by_state)
summary(mod2)

mod3 <- lm(deaths ~ Population, data = us_deaths)
summary(mod3)
```

##Conclusion

Working through the COVID-19 data felt almost surreal. It's interesting to work through data that holds such a tangible connection to us. Experiencing COVID-19 and the pandemic in person, and then looking at the data points, was interesting. I felt very removed from the data, but I think that is because of the physical toll we felt while living through the pandemic.

Despite working through many new emotions through this project, I also learned a ton about how to wrangle, clean and visualize information. Working with the time series data sets were a great reminder on how to join, pivot and load information into new data frames. I also loved learning new ways to build graphs through 'ggplot2'.

Thank you so much for reading through this report and I hope it provided a new perspective on the COVID-19 data!
