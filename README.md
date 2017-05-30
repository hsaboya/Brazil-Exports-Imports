# Brazil-Exports-Imports
Geographical analysis of Brazilian Imports and Exports

I'm trying to create an R code for taking Brazil's Exports and Imports databases as input, to generate any kind of interesting analysis.

```{r}
library(plyr)
library(ggplot2)
library(ggmap)
library(maps)
```

Read files, and database set-up:

```{r}
#database

base <- "~/EXP_MPP_VIA_2016.csv"
exp <- read.csv(base, sep = ";")

#product codes
product.table <- read.csv("~/PRODUTOS.csv", sep = ",")
product.table <- product.table[,c(1,3)]
names(product.table) <- c("CO_NCM","NO_NCM_ING")

#country codes
country.table <- read.csv("~/PAISES.csv", sep = ",")
country.table <- country.table[,c(1,3)]
names(country.table) <- c("CO_PAIS","NO_PAIS_ING")

#group codes
group.table <- read.csv("~/GRUPOS.csv", sep = ",")
group.table <- group.table[,c(2,3,4,5)]

#Merged tables
exp.up <- merge(x = exp, y = product.table, by.x = "CO_NCM", by.y = "CO_NCM", all.x = T, all.y = F)
exp.up <- merge(x = exp.up, y = country.table, by.x = "CO_PAIS", by.y = "CO_PAIS", all.x = T, all.y = F)
exp.up <- merge(x = exp.up, y = group.table, by.x = "CO_NCM", by.y = "CO_NCM", all.x = T, all.y = F)
```

Check:

```{r}
all.equal(colSums(exp[,6:9]),colSums(exp.up[,6:9]))
all.equal(nrow(exp),nrow(exp.up))

exp <- exp.up; rm(exp.up, product.table, country.table, group.table)
```

Filter:

```{r}
exp.filter <- exp[exp$NO_EXP_SET %in% c("FRUTAS","CAFE","SOJA","SUCO DE LARANJA"),]
```

Aggregate data by product:

```{r}
kg.sum <- function(my.data) {sum(my.data$KG.LIQUIDO)}

exp.filter.2 <- ddply(exp.filter, .(NO_NCM_ING), kg.sum)
exp.filter.2 <- exp.filter.2[order(exp.filter.2$V1, decreasing = T),]

ggplot(data = exp.filter.2[1:5,]) +
  geom_col(mapping = aes(x = NO_NCM_ING, y = V1)) +
  theme(axis.text.x=element_text(angle=60,hjust=1))

rm(exp.filter.2)
```

Plots (by product):

```{r}
#Produto a ser analizado
product <- "Fresh apples"

exp.filter.3 <- exp.filter[exp.filter$NO_NCM_ING %in% product,]
exp.filter.3 <- exp.filter.3[order(exp.filter.3$CO_ANO, exp.filter.3$CO_MES),]

summary <- tapply(exp.filter$KG.LIQUIDO, exp.filter$CO_MES, sum)

rm(exp.filter.3)
```

Aggregate data by country:

```{r}
exp.filter.4 <- ddply(exp.filter, .(NO_PAIS_ING), kg.sum)
exp.filter.4 <- exp.filter.4[order(exp.filter.4$V1, decreasing = T),]
exp.filter.4$NO_PAIS_ING <- as.character(exp.filter.4$NO_PAIS_ING)
exp.filter.4$V1 <- exp.filter.4$V1/1000000

#Download world map------------------------------------------------------------------------------
world <- map_data("world")


#Fix different names from databases--------------------------------------------------------------
unique(subset(exp.filter.4$NO_PAIS_ING, !(exp.filter.4$NO_PAIS_ING %in% world$region)))

exp.filter.4[match("United States", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "USA"
exp.filter.4[match("United Kingdom", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "UK"
exp.filter.4[match("Congo", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "Republic of Congo"
exp.filter.4[match("Canary, Islands", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "Cayman Islands"
exp.filter.4[match("French Guyana", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "French Guiana"
exp.filter.4[match("Dominica Island", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "Dominica"
exp.filter.4[match("Cote D'Ivore", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "Ivory Coast"
exp.filter.4[match("Bahrein", exp.filter.4$NO_PAIS_ING),]$NO_PAIS_ING <- "Bahrain"

world[which(world$subregion == "Hong Kong"),]$region <- "Hong Kong"
world[which(world$region == "Trinidad"),]$region <- "Trinidad and Tobago"
world[which(world$region == "Tobago"),]$region <- "Trinidad and Tobago"
world[which(world$region == "Grenadines"),]$region <- "Saint Vicent and the Granadines"
world[which(world$region == "Saint Vincent"),]$region <- "Saint Vicent and the Granadines"
world[which(world$region == "Antigua"),]$region <- "Antigua and Barbuda"
world[which(world$region == "Barbuda"),]$region <- "Antigua and Barbuda"
world[which(world$region == "Saint Kitts"),]$region <- "Saint Christopher and Nevis"
world[which(world$region == "Nevis"),]$region <- "Saint Christopher and Nevis"


#Merging data.frames-----------------------------------------------------------------------------
world <- merge(x = world, y = exp.filter.4, by.x = "region", by.y = "NO_PAIS_ING", all.x = T)
world <- arrange(world, order)

final.map <- ggplot(data = world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(V1, 7))) +
  scale_fill_brewer("Brazil Exports (Thousand Tons)", palette = "Blues") +
  labs(titel = base) +
  geom_path(colour = 'gray', linemitre = 2) +
  coord_equal(ratio = 1.25) +
  theme_bw()

final.map
```



