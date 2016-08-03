library(XLConnect)
library(dplyr)
library(tidyr)
giniData <- readWorksheet(loadWorkbook("indicator SI_POV_GINI.xls.xlsx"), sheet = 1)
gdpPCData <- readWorksheet(loadWorkbook("indicator pwt.xlsx"), sheet = 1)
murderData <- readWorksheet(loadWorkbook("Homicide age adjusted indicator LIVE -05 20100919.xlsx"), sheet = 1)
lifeExpData <- readWorksheet(loadWorkbook("indicator life_expectancy_at_birth.xlsx"), sheet = 1)


giniAggr <- tidyr::gather(giniData, 'year','rate',2:34)
giniAggr <- na.omit(giniAggr)
giniAggr <- setNames(aggregate(giniAggr$rate, list(giniAggr$GINI.index), mean), c('country','giniIndex'))

gdpAggr <- tidyr::gather(gdpPCData, 'year','gdpPerCapita',2:56)
gdpAggr  <- na.omit(gdpAggr)
gdpAggr <- setNames(aggregate(gdpAggr$gdpPerCapita, list(gdpAggr$Col1), mean), c('country', 'gdp'))

murderAggr <- tidyr::gather(murderData, 'year','murderRate',2:57)
murderAggr <- na.omit(murderAggr)
murderAggr <-  setNames(aggregate(murderAggr$murderRate, list(murderAggr$Murder.per.100.000..age.adjusted), mean), c('country','murders'))

lifeAggr <- tidyr::gather(lifeExpData, 'year','lifeExp',2:217)
lifeAggr  <- na.omit(lifeAggr)
lifeAggr <- setNames(aggregate(lifeAggr$lifeExp, list(lifeAggr$Life.expectancy.with.projections..Yellow.is.IHME), mean), c('country','lifeexp'))

consolidated  <- merge(x =giniAggr, y = gdpAggr, by='country')
consolidated  <- merge(x =consolidated, y = murderAggr, by='country')
consolidated  <- merge(x =consolidated, y = lifeAggr, by='country')

library(ggplot2)
#ggplot(aes(x = giniIndex, y = gdp), data = consolidated) + geom_point() + geom_smooth() + geom_point(data = subset(consolidated, country == 'Russia'), aes(x = giniIndex, y = gdp), color ='red', size=5)
giniRussia <- tidyr::gather(subset(giniData, GINI.index == 'Russia' ), 'year','rate',2:34)
giniRussia <- na.omit(giniRussia)

ggplot(aes(x = year, y = rate, group = 1, label=rate), data = giniRussia) + geom_line() + geom_text()

