
library(lme4)
library(ggplot2)  # load the package


## The Script where the strokes gained model is giong to be built
data <- read.csv("strokesGainedInterpolated.csv")
golf_data <-  read.csv("GolfData.csv")


#add a total strokes gained column
golf_data
by_cyl <- golf_data %>% group_by(Name) %>% group_by(Course)  %>% group_by(Course)

#building the model on the interpolasted data

#actual strokes: y
#yards: x

m0 <- lm(happiness ~ 1, data=data)  # to obtain Total SS
model.fairway <- lm(fairway ~ yards + fairway, data=data)  # Model 1

model.rough <- lm(rough ~ yards + rough, data=data)  # Model 1
plot(model.rough)

plot(x = yards, y = fairway, data = data)



model.midpoint.rough <- lm(midpoint ~ yards +  rough, data=data)  # Model 2
plot(model.midpoint.rough)

(prelim_plot.rough <- ggplot(data, aes(x = yards, y = rough)) +
    geom_jitter() +
    geom_smooth(method = "loess"))


(prelim_plot.fairway <- ggplot(data, aes(x = yards, y = fairway)) +
    geom_point() +
    geom_smooth(method = "loess"))


model.midpoint.fairway.add <- lm(midpoint ~ yards + fairway, data=data)  # Model 2


model.midpoint.fairway.mult <- lm(midpoint ~ yards * fairway, data=data)  # Model 2

model.midpoint.fairway.mult <- lm(midpoint ~ yards * fairway, data=data)  # Model 2



model2.1<-lm(expected_strokes_gained ~ yards + fairway, data = data)


qqnorm(resid(model.midpoint.fairway.add))
qqline(resid(model.midpoint.fairway.add)) 
summary.lm(model.midpoint.fairway)
plot(model.midpoint.fairway.add)


#making a new midpoint column based on approach game


##replacing the values in the strokes to add grouping based on shot length in midpoint
#data <-replace(data$midpoint, (df$midpoint > 38 & df$midpoint < 63),"Short Approach")


qqnorm(resid(model.midpoint.fairway.mult))
qqline(resid(model.midpoint.fairway.mult)) 
summary.lm(model.midpoint.fairway.mult)


yardage.dataframe <-data.frame(yards=c(100,200,150,180), fairway = c(1.9, 2.1, 2.5, 2.8))
predict(model.fairway, newdata = yardage.dataframe)



 

