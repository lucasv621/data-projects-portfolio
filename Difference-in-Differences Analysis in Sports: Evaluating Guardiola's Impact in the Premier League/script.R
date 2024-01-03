### Importing libraries ###
library(dplyr)
library(readxl)
library(ggplot2)

### Setting WD ###
setwd('C:/Users/lucas/OneDrive/Escritorio/MiM+A/Business Applications of Data Analytics/Entrega tp final')

### Reading dfs ###
df_city <- read_excel("city_stats.xlsx",sheet="Hoja1")
df_utd <- read_excel("utd_stats.xlsx",sheet="Hoja1")

### Processing ###
df_city$Team <- "Manchester City"
df_city$Date <- as.Date(df_city$Date)
guardiola_joins <- as.Date("2016-07-01") ## Date when the Man. City started being "treated"
## Identifying city as the treatment group
df_city$treatment <- 1

df_utd$Team <- "Manchester United"
df_utd$Date <- as.Date(df_utd$Date)
df_utd$treatment <- 0

### Bind of dfs ###
df <- rbind(df_city,df_utd)

## Transformations over some pre-existing columns
df <- df %>% 
  rename(Amount_spent = `Amount spent on transfers (MM euros)`) %>% 
  rename(Amount_sold = "Amount sold on transfers (MM euros)")
df$Cumsum_spent <- as.numeric(df$Cumsum_spent)
df$Cumsum_sold <- as.numeric(df$Cumsum_sold)
df$Amount_spent <- as.numeric(df$Amount_spent)

### Calculating the points of each match
df$Points <- ifelse(df$Result == 'W', 3, ifelse(df$Result == "D",1,0))

## Sorting the dataframe and calculating a cumsum of the points of each team
df <- df %>% 
  arrange(Team,Date) %>% 
  group_by(Team) %>% 
  mutate(cumsum_pts = cumsum(Points))

## Creating the "after" variable to distinguish pre and after treatment dates
df <- df %>% 
  mutate(after = as.numeric(Date >= guardiola_joins))


## Creating the transfer_mkt_balance variable
# Difference btw Amount sold and spend in the transfer market, it will be used to represent how much a team
# spends in each season in contrast with the money earned selling players
df$transfer_mkt_balance = df$Amount_sold - df$Amount_spent


## Pre trends plot
pre <- df[df$Date <= guardiola_joins,]
ggplot(pre, aes(x= Date, y=cumsum_pts, color=Team,group=Team)) +
  geom_line(data = subset(pre,Team=="Manchester United")) +
  geom_line(data = subset(pre,Team=="Manchester City")) +
  geom_vline(xintercept = guardiola_joins, linetype="dotted") +
  scale_color_manual(values = c("Manchester United" = "red", "Manchester City" = "blue")) +
  labs(title="Pre-Guardiola cummulative sum of points trend", x="Date",y="Cummulative sum of points") +
  theme_minimal()

## Pre vs After treatment trends plot
ggplot(df, aes(x= Date, y=cumsum_pts, color=Team,group=Team)) +
  geom_line(data = subset(df,Team=="Manchester United")) +
  geom_line(data = subset(df,Team=="Manchester City")) +
  geom_vline(xintercept = guardiola_joins, linetype="dotted") +
  scale_color_manual(values = c("Manchester United" = "red", "Manchester City" = "blue")) +
  labs(title="Guardiola's effect on the cummulative sum of points trend", x="Date",y="Cummulative sum of points") +
  theme_minimal()


## Subsetting for a t test
mu_before <- subset(df, after == 0 & treatment == 0, select=Points)
mc_before <- subset(df, after == 0 & treatment == 1, select=Points)
mu_after <- subset(df, after == 1 & treatment == 0, select=Points)
mc_after <- subset(df, after == 1 & treatment == 1, select=Points)

## T-tests 
t.test(mu_before,mc_before)
t.test(mu_after,mc_after)



## Regression for cumsum points 
## controlling for the transfer mkt financial balance and international matches
model <- lm(cumsum_pts ~ treatment + after + treatment*after + transfer_mkt_balance  + Group_stage + Knock_out_stage, data=df)
summary(model)
