
library(lme4)
library(GGally) 
library(tidyverse)
library(ISLR)
library(mgcv)
library(here)

## The Script where the strokes gained model is going to be built
data <- read.csv(here("Strokes Gained Model/strokesGainedInterpolated.csv"))
golf_data <-  read.csv(here("Strokes Gained Model/GolfData.csv"))
total_new.data <- read.csv(here("Strokes Gained Model/total_new.csv"))

#add a total strokes gained column
golf_data
data.player <- golf_data %>% group_by(Name) %>% group_by(Course)  %>% group_by(Course)

player.1.data <- data.player %>% filter(Name == "Player 1")

(prelim.plot.strokes.gained <- ggplot(player.1.data, aes(x = Score, y = SG..Approach)) +
        geom_jitter() +
        geom_smooth(method = "loess"))


#Total New Data
#create a new response for putts + 1 in the dataframe 
#strokes remanining until the approach list 
new.model <- smooth.spline((putts + 1) ~  Approach.Dist, data = total_new.data)
plot(new.model)

(prelim.plot.strokes.gained <- ggplot(total_new.data, aes(x = Approach.Dist, y = strokesgained)) +
        geom_jitter() +
        geom_smooth(method = "lm"))


scratch.model <- lm(SG..Approach ~ Score + X.50.Total + X51.75 +  X76.100 + X101.125 +  X126.150 + X151.175 +  X176.200 + X201.225 +  X226., data=data.player)  # Model 1
score_model <- lm(Score ~   X.50.Total + X51.75 +  X76.100 + X101.125 +  X126.150 + X151.175 +  X176.200 + X201.225 +  X226., data=data.player)  # Model 1

plot(scratch.model)
model.gaussian <- gam(SG..Approach ~ Score + s(X.50.Total) + X51.75 , data= player.1.data, family=poisson)



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

qqnorm(resid(model.midpoint.fairway.mult))
qqline(resid(model.midpoint.fairway.mult)) 
summary.lm(model.midpoint.fairway.mult)


yardage.dataframe <-data.frame(yards=c(100,200,150,180), fairway = c(1.9, 2.1, 2.5, 2.8))
predict(model.fairway, newdata = yardage.dataframe)



 

