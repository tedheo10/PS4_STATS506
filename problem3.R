# Problem 3 - Visualization
# a. a change in the sales price in USD over time
# The average price of artworks has not changed significantly over time, but the number of high-priced artworks being sold is increasing, and their price range is also rising. 

artsales <- read.csv("df_for_ml_improved_new_market.csv")

colnames(artsales)
view(artsales)

artsales_year <-
  aggregate(artsales, by = list(artsales$year), FUN = mean, na.rm = TRUE)
artsales_year <- artsales_year[order(artsales_year$year), ]
artsales_year

with(artsales_year, plot(price_usd ~ year, type = "l"))

with(artsales, plot(price_usd ~ year, main = "The Sales Price over time", col = "lightgrey"))
with(artsales_year, lines(price_usd ~ year))
with(artsales_year, lines(price_usd ~ year, lwd = 2, col = "red"))

dev.off()
oldpar <- par(no.readonly = TRUE) # code from the class note of STATS 506
par(bg = "darkgrey",
    fg = "lightblue",
    col.axis = "lightgreen",
    col.lab = "purple",
    lwd = 4)
par(oldpar)

# b. the distribution of genre of sales across years

library(ggplot2)
genre <- artsales$

# Genre vector
genre_art <- rep(NA, length(artsales$Genre___Photography))

genre_art[artsales$Genre___Photography == 1] <- "Photography"
genre_art[artsales$Genre___Print == 1] <- "Print"
genre_art[artsales$Genre___Sculpture == 1] <- "Sculpture"
genre_art[artsales$Genre___Painting == 1] <- "Painting"
genre_art[artsales$Genre___Others == 1 & artsales$Genre___Painting == 0] <- "Others"
table(genre_art)

ggplot(artsales, aes(x = year, fill = genre_art)) +
  geom_histogram() 

# c. the genre affect the change in sales price over time