library(tidyverse)
library(broom)

#I found this huge csv from the IOM's Global Migration Data and Analysis Centre detailing numbers and locations of dead and/or missing migrants from 2014 to today. It's updated often and has a lot of extremely interesting information on missing migrants across the world
MisMig <- read_csv("Missing_Migrants_Global_Figures_allData_0.csv")
MisMig

#The first thing I'd like to do is compare the Number of Dead of all the different Migration Routes to see which are or have been the deadliest, and possibly see some trends on how they've changed over time
MisMig %>% select("Website Date","Number of Dead", "Migration route")
RouteDeaths <- MisMig %>% select("Website Date","Number of Dead", "Migration route")
RouteDeaths

#Now, I'll remove the NA values in the Migration route column
RouteDeaths <- RouteDeaths %>%
  drop_na()
RouteDeaths

#Here, after some trial and error, I needed to change the column names in order to avoid using quotation marks in the aes() field when plotting. It also simplifies further coding with these variables
colnames(RouteDeaths) <- c('date', 'dead', 'route')
RouteDeaths

#This is my first attempt at a descriptive graphic. I eliminated a couple migrant routes by filtering for the specific routes I wanted. 
RouteDeaths %>% 
  filter(route %in% c("Afghanistan to Iran", "Belarus-EU border", "Caribbean to US", "Central Mediterranean", "Comoros to Mayotte", "Darien Gap", "Dominican Republic to Puerto Rico", "DRC to Uganda", "Eastern Mediterranean", "English Channel to the UK", "Haiti to Dominican Republic", "Horn of Africa to Yemen crossing", "Iran to Türkiye", "Italy to France", "Sahara Desert crossing", "Syria to Türkiye", "Türkiye-Europe land route", "Ukraine to Europe", "US-Mexico border crossing", "Venezuela to Caribbean", "Western Africa / Atlantic route to the Canary Islands", "Western Balkans", "Western Mediterranean" )) %>%
  ggplot(aes(date, dead, color = route)) +
  geom_col() +
  facet_wrap(~ route) +
  guides(color = FALSE)

#In the above visualization, I noticed that there was on extremely high value in the Central Mediterranean route, so I decided to eliminate the outlier in order to scale the y axis accordingly
RouteDeaths %>% 
  filter(route %in% c("Afghanistan to Iran", "Belarus-EU border", "Caribbean to US", "Central Mediterranean", "Comoros to Mayotte", "Darien Gap", "Dominican Republic to Puerto Rico", "DRC to Uganda", "Eastern Mediterranean", "English Channel to the UK", "Haiti to Dominican Republic", "Horn of Africa to Yemen crossing", "Iran to Türkiye", "Italy to France", "Sahara Desert crossing", "Syria to Türkiye", "Türkiye-Europe land route", "Ukraine to Europe", "US-Mexico border crossing", "Venezuela to Caribbean", "Western Africa / Atlantic route to the Canary Islands", "Western Balkans", "Western Mediterranean" )) %>%
  filter(dead <= 300) %>%
  ggplot(aes(date, dead, color = route)) +
  geom_col() +
  facet_wrap(~ route) +
  guides(color = FALSE)

#Now, trends in migrant deaths over time are much clearer. Finally, I now need to format the legends
RouteDeaths %>% 
  filter(route %in% c("Afghanistan to Iran", "Belarus-EU border", "Caribbean to US", "Central Mediterranean", "Comoros to Mayotte", "Darien Gap", "Dominican Republic to Puerto Rico", "DRC to Uganda", "Eastern Mediterranean", "English Channel to the UK", "Haiti to Dominican Republic", "Horn of Africa to Yemen crossing", "Iran to Türkiye", "Italy to France", "Sahara Desert crossing", "Syria to Türkiye", "Türkiye-Europe land route", "Ukraine to Europe", "US-Mexico border crossing", "Venezuela to Caribbean", "Western Africa / Atlantic route to the Canary Islands", "Western Balkans", "Western Mediterranean" )) %>%
  filter(dead <= 300) %>%
  ggplot(aes(date, dead, color = route)) +
  geom_col() +
  facet_wrap(~ route) +
  guides(color = FALSE) +
  labs(x="Date", y="Number of Migrant Deaths", 
       title="Migrant Deaths along Major Global Migration Routes",
       subtitle="2014-2022")


 