# Problem 3 - Visualization
# a. a change in the sales price in USD over time
# The average price of artworks has increased over time, but the number of high-priced artworks being sold is increasing, and their price range is also rising. 

artsales <- read.csv("df_for_ml_improved_new_market.csv")

# the average price of the art sales
artsales_year <-
  aggregate(price_usd ~ year, data = artsales, FUN = mean, na.rm = TRUE)

with(artsales, plot(price_usd ~ year, 
                    main = "The Sales Price", 
                    xlab = "Year",
                    ylab = "Price",
                    col = "darkgrey",
                    ylim = c(0, 1000000)))
par(new = TRUE)
with(artsales_year, plot(price_usd ~ year, 
                         lty = 2, 
                         lwd = 2, 
                         col = rgb(0, 0, 0, alpha = 0), 
                         axes = FALSE, 
                         xlab = "", 
                         ylab = "", 
                         ylim = c(0, 100000)))
with(artsales_year, lines(price_usd ~ year,
                          lty = 2, 
                          lwd = 2, 
                          col = "red",
                          ylim = c(0, 100000)))

axis(side = 4, at = pretty(range(0, 100000)), col = "red")

legend("topright", 
       legend = c("Observed", "Average"),
       lty = c(NA, 2), lwd = c(NA, 2),
       pch = c(1, NA),
       col = c("darkgrey", "red"))

dev.off()
oldpar <- par(no.readonly = TRUE) # code from the class note of STATS 506
par(oldpar)


# b. the distribution of genre of sales across years
# The sale of artworks has been increasing across all genres. However, the proportion of paintings has been decreasing. Photography and sculpture have remained the main genres of art. Interestingly, print sales began in 2000, and since 2005, their proportion nearly matched that of paintings. 

library(ggplot2)
genre <- artsales$

# create a vector with the values of each genre 
genre_art <- rep(NA, length(artsales$Genre___Photography))

genre_art[artsales$Genre___Photography == 1] <- "Photography"
genre_art[artsales$Genre___Print == 1] <- "Print"
genre_art[artsales$Genre___Sculpture == 1] <- "Sculpture"
genre_art[artsales$Genre___Painting == 1] <- "Painting"
genre_art[artsales$Genre___Others == 1 & artsales$Genre___Painting == 0] <- "Others"

artsales$genre_art <- as.factor(genre_art)
artsales$genre_art <- factor(artsales$genre_art, levels = c("Photography", "Sculpture", "Painting", "Print", "Others")) # sort the order of the genres 

ggplot(artsales, aes(x = year, fill = genre_art, levels = genre_art)) +
  geom_histogram(position = "stack", binwidth = .5) +
  ggtitle("The Art Sales Distribution") + 
  labs(fill = "Art Genre") +
  scale_y_continuous(name = "The Number of Sales") +
  scale_x_continuous(name = "Year") +
  scale_color_discrete(name = "Genre") +
  theme(plot.title = element_text(color = "black", hjust = .5),
        legend.title = element_text(color = "black", hjust = .5),
        legend.position = "right",
        legend.background = element_rect(linewidth = 1,
                                         color = "lightgrey",
                                         fill = "lightgrey"),
        plot.background = element_rect(fill = "white"),
        text = element_text(size = 12))

# c. the change of genre in sales price over time
# The average prices of photography and sculpture have been rising over time. In contrast, the average price of paintings has been decreasing since 2008, after having increased until that year. Although the average price of prints shows an overall upward trend, it experiences significant variations. 

# the average price of each genre
artsales_genre <-
  aggregate(price_usd ~ year + genre_art, artsales, FUN = mean, na.rm = TRUE)

ggplot(artsales, 
       aes(x = year, y = price_usd, color = genre_art, shape = genre_art)) +
  geom_count() +
  geom_line(data = artsales_genre, aes(x = year, y = price_usd*15, group = 1), 
            color = "red3", lwd = 1, lty = 2) +
  facet_wrap(vars(genre_art)) +
  ggtitle("The Art Sales Price") + 
  labs(color = "Art Genre",
       shape = "Art Genre") +
  scale_y_continuous(name = "Price", 
                     limits = c(0, 1500000),
                     sec.axis = sec_axis(transform = ~ . / 15,
                                         name = "Average Price (scaled)")) +
  scale_x_continuous(name = "Year") +
  annotate("text", x = Inf, y = 0, label = "Average", fontface = "bold",
           hjust = 1.1, vjust = -8, color = "red3", size = 3) +
  theme(plot.title = element_text(color = "black", hjust = .5),
        legend.title = element_text(color = "black", hjust = .5),
        legend.position = "right",
        legend.background = element_rect(linewidth = 1,
                                         color = "lightgrey",
                                         fill = "lightgrey"),
        plot.background = element_rect(fill = "white"),
        text = element_text(size = 12),
        axis.text.y.right = element_text(color = "red3"),
        axis.title.y.right = element_text(color = "red3"),
        axis.ticks.y.right = element_line(color = "red3"))
