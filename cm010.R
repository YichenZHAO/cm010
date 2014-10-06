install.packages("plyr", dependencies=TRUE)
library(plyr)
library(dplyr)
library(ggplot2)

gd_url <- "http://tiny.cc/gapminder"
gDat <- read.delim(file = gd_url)
str(gDat)

max_le_by_cont <- ddply (gDat, ~continent, summarize, max_le = max(lifeExp))


######

j_coefs <- ddply(gDat, ~ country + continent,
                 function(x, offset = 1952){
                   the_fit <- lm(lifeExp ~ I(year - offset), x)
                   setNames(coef(the_fit), c("intercept", "slope"))
                 })
str(j_coefs)

class(j_coefs$country)
mode(j_coefs$country)
typeof(j_coefs$country)
levels(j_coefs$country)
nlevels(j_coefs$country)
nlevels

ggplot(j_coefs, aes(x=slope, y=country)) + geom_point(size=3)
ggplot(j_coefs, aes(x=slope, y=reorder(country, slope))) + geom_point(size=3)

##
## Egypt, Haiti, Romaniam Thailand, Venezuela
h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
hDat <- gDat %>%
  filter(country %in% h_countries)
hDat %>% str
table(hDat$country)
levels(hDat$country)
nlevels(hDat$country)

iDat <- droplevels(hDat)
levels(iDat$country)
levels(iDat$continent)
iDat <- hDat %>% droplevels

## reorder()

## get a new data.frame with the max life expectancy for each country in iDat
i_le_max <- iDat %>%
  group_by(country) %>%
  summarize(max_le = max(lifeExp))
i_le_max

ggplot(i_le_max, aes(x=country, y=max_le, group=1)) +
  geom_path() + geom_point(size=3)
ggplot(iDat, aes(x=year, y=lifeExp, group=country)) +
  geom_line(aes(color=country))


## reorder(your_factor, your_quantative_variable, your_sumarization_function)
jDat <- iDat %>%
  mutate(country = reorder(country, lifeExp, max))
data.frame(before = levels(iDat$country),
           after = levels(jDat$country))
j_le_max <- jDat %>%
  group_by(country) %>%
  summarize(max_le = max(lifeExp))
j_le_max

ggplot(j_le_max, aes(x = country, y = max_le, group =1)) +
  geom_path() + geom_point(size=3)
ggplot(jDat, aes(year, lifeExp, group = country)) +
  geom_line(aes(color = country))+
  guides(color = guide_legend(reverse = TRUE))
