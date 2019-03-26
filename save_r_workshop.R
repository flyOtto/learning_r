2+2
seq(1:5)
my_vector <- c(1,3,4,5)
mean(my_vector)
print('Look whats happening')
library(tidyverse)

df <- read_csv('worked_data.csv')
head(df)
summary(df)
str(df)
glimpse(df)

#Arrange

#total sales starting from highest

df_example_1 <- df %>%
  group_by(Game)%>%
  summarise(total_sales_ever = sum(Total.Sales, na.rm = TRUE))%>%
  arrange(desc(total_sales_ever))%>%
  head(10)
df_example_1  

#Total games released by console starting top

df_example_2 <- df %>%
  group_by(Console) %>%
  summarise(total_games_released = n_distinct(Game)) %>%
  arrange(desc(total_games_released))
df_example_2

#What are the most shipped games? Not including NAs

df_example_3 <- df %>%
  group_by(Game) %>%
  summarise(total_shipped_ever = sum(Total.Shipped, na.rm = TRUE)) %>%
  arrange(desc(total_shipped_ever)) %>%
  head(10)
df_example_3

#Mutate

#How old are the top sold games?

df_example_4 <- df %>%
  filter(Release.Date.Pretty >= as.Date('1990-01-01')) %>% #filteröi lähtien tuo pvm
  arrange(desc(Total.Sales)) %>%
  mutate(age_in_days = Sys.Date() - Release.Date.Pretty) %>%
  select(Game, Total.Sales, age_in_days)
df_example_4

#How much are North America sales from total?

df_example_5 <- df %>%
  mutate(percentage_North_America_from_total = NA.Sales/Total.Sales) %>%
  arrange(desc(percentage_North_America_from_total)) %>%
  filter(percentage_North_America_from_total < 1) %>%
  select(Game, Console, percentage_North_America_from_total, NA.Sales, Total.Sales)
df_example_5

#Only for PC

df_example_5 %>%
  filter(Console == "PC") %>%
  select(Game, Console, percentage_North_America_from_total, Total.Sales)

#How long ago was the data last updated?

df_example_6 <- df %>%
  mutate(days_since_last_update_of_data = Sys.Date() - Last.Update.Pretty) %>%
  arrange(days_since_last_update_of_data) %>%
  select(Game, days_since_last_update_of_data, Last.Update.Pretty)
df_example_6

#How does the critic score compare to user score for top sold games?

df_example_7 <- df %>%
  filter(is.na(Critic.Score) == FALSE, is.na(User.Score) == FALSE) %>%
  mutate(difference_in_score = User.Score - Critic.Score) %>%
  arrange(difference_in_score) %>%
  select(Game, difference_in_score, Total.Sales, User.Score, Critic.Score)
df_example_7

# p <- ggplot(data= , mapping = aes(x = , y = )) + (<--- %>% sijasta)
# p <- p + geom_point() 
# How does the critic score compare to user score for top sold games?

p <- ggplot(data = df_example_7, mapping = aes(x = Critic.Score, y = User.Score, size = Total.Sales))
p <- p + geom_point()
p <- p + theme_bw()
p <- p + labs(x = 'Critic Score', y = 'User Score', title = 'How does the critic score compare to user score for top sold games?', size = 'Total Sales')
p

# How many PS games are sold over years?

df_example_9 <- df %>%
  filter(Console %in% c('PS', 'PS2', 'PS3', 'PS4'), Release.Year >= 1994) %>%
  group_by(Release.Year, Console) %>%
  summarise(Total.Sales = sum(Total.Sales, na.rm = T)) #T=TRUE
df_example_9

p1 <- ggplot(data = df_example_9, mapping = aes(x = Release.Year, y = Total.Sales, colour = Console))
p1 <- p1 + geom_line()
p1 <- p1 + geom_point()
p1
  