library(ggplot2)
theme_white <- function() {
  theme_update(panel.background = theme_blank(),
               panel.grid.major = theme_blank())
}
# theme_set(theme_bw())
# theme_white()

file <- c("http://data.giss.nasa.gov/gistemp/graphs/Fig.A2.txt")
rows <- length(readLines(file)) - 5
df <- read.table(file, skip = 4, nrows = rows,
                   header = FALSE, na.strings = "*")
df <- df[, 1:2]
names(df) <- c("year", "mean")
p <- ggplot(df, aes(year, mean)) + labs(x = NULL,
                                          y = ("GISS Temperature Anomaly C")) + ylim(-0.65,
                                                                                     0.65)
mainplot <- p %+% subset(df, year > 1975) +
  geom_line() + geom_smooth(se = FALSE) +
  opts(title = "Global Land-Ocean Temperature Anomaly C")
p1 <- p + geom_rect(aes(xmin = 1975, xmax = max(year),
                          ymin = min(mean, na.rm = TRUE), ymax = 0.65),
                      fill = alpha("lightblue", 0.2)) + scale_x_continuous(breaks = NA) +
  scale_y_continuous(breaks = NA) + labs(y = NULL) +
  opts(title = "Full data: 1880-2008") +
  opts(plot.title = theme_text(face = "bold")) +
  opts(panel.border = theme_blank())
subplot <- p1 + geom_line(colour = I("grey"),
                            size = 0.8) + geom_smooth(se = FALSE, subset = .(year >
                                                                               1975))
vp <- viewport(width = 0.4, height = 0.4, x = 1,
                 y = unit(0.7, "lines"), just = c("right",
                                                  "bottom"))

full <- function() {
  print(mainplot)
  theme_set(theme_bw(base_size = 8))
  theme_white()
  print(subplot, vp = vp)
  theme_set(theme_bw())
}
full()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(aes(colour = "loess"), method = "loess", se = FALSE) + 
  geom_smooth(aes(colour = "lm"), method = "lm", se = FALSE) +
  labs(colour = "Method") +
  annotate("text", x = 3.5, y = 35, label = sprintf('\u2191'))

plot(x=0, type="n")
text(1, .5, paste0("0.5", intToUtf8(9660)))
text(1, -.5, paste0("-0.5", intToUtf8(9650)))

tridf = data.frame(a=1:10,b = c(rep(intToUtf8(9660),5),rep(intToUtf8(9650),5)))
View(tridf)
