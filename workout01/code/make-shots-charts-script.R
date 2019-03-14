#Title:
#Desciption:
#Inputs:
#Outputs:

library(jpeg)
library(grid)
klay_scatterplot <- ggplot(data = klay) + geom_point(aes(x = x, y= y, color = shot_made_flag))

court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(readJPEG(court_file), width = unit(1, "npc"), height = unit(1,"npc"))
klay_shot_chart <- ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle("Shot Chat: Klay Thompson (2016 Season)") + theme_minimal()

pdf("../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle("Shot Chat: Andre Iguodala (2016 Season)") + theme_minimal()
dev.off()
pdf("../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
ggplot(data = green) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle("Shot Chat: Draymond Green (2016 Season)") + theme_minimal()
dev.off()
pdf("../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle("Shot Chat: Kevin Durant (2016 Season)") + theme_minimal()
dev.off()
pdf("../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle("Shot Chat: Klay Thompson (2016 Season)") + theme_minimal()
dev.off()
pdf("../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle("Shot Chat: Stephen Curry (2016 Season)") + theme_minimal()
dev.off()
pdf("../images/gsw-shot-charts.pdf", width = 8, height = 7)
ggplot(gsw) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + facet_wrap(. ~ name, ncol = 3) #facet_grid(. ~ name) +
dev.off()
# png("../images/gsw-shot-charts.png", width = 8, height = 7, units = "in", res = 100)
# ggplot(gsw) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + facet_grid(. ~ name) + facet_wrap(. ~ name, ncol = 3) 
# dev.off()
png("../images/gsw-shot-charts.png", width = 8, height = 7, units = "in", res = 100)
ggplot(gsw,aes(x = x, y = y, color = shot_made_flag)) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(size = .2) + facet_wrap(~ name, ncol = 3) 
dev.off()