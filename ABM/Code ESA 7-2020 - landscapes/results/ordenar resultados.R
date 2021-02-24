# Empty workspace
rm(list=ls())
library(here)
setwd("D:/ordenar sept 2020/modelo nuevo ABM/results")

##move columns (function)
moveMe <- function(data, tomove, where = "last", ba = NULL) {
    temp <- setdiff(names(data), tomove)
    x <- switch(
        where,
        first = data[c(tomove, temp)],
        last = data[c(temp, tomove)],
        before = {
            if (is.null(ba)) stop("must specify ba column")
            if (length(ba) > 1) stop("ba must be a single character string")
            data[append(temp, values = tomove, after = (match(ba, temp)-1))]
        },
        after = {
            if (is.null(ba)) stop("must specify ba column")
            if (length(ba) > 1) stop("ba must be a single character string")
            data[append(temp, values = tomove, after = (match(ba, temp)))]
        })
    x
}

sim1.50 <- read.csv("human_tick_1-50.csv",header = FALSE)
sim2.100 <- read.csv("human_tick_51-100.csv",header = FALSE)
sim3.150 <- read.csv("human_tick_101-150.csv",header = FALSE)
sim4.200 <- read.csv("human_tick_151-200.csv",header = FALSE)
sim5.220 <- read.csv("human_tick_201-222.csv",header = FALSE)

sim2.100 <- as.data.frame(sim2.100[,-c(1)] )
sim3.150 <- as.data.frame(sim3.150[,-c(1)] )
sim4.200 <- as.data.frame(sim4.200[,-c(1)] )
sim5.220 <- as.data.frame(sim5.220[,-c(1)] )

sim1.50 <- cbind(sim1.50, sim2.100, sim3.150, sim4.200, sim5.220)

summary.sim1.50 <- as.data.frame(sim1.50[c(1:8), ] )
summary.sim1.50 <- as.data.frame(t(summary.sim1.50))

Newnames <- as.data.frame(summary.sim1.50[c(1), ])
Newnames <- as.data.frame(t(Newnames))
class(Newnames$V1)
names(summary.sim1.50) <- Newnames$V1
summary.sim1.50 <- summary.sim1.50[-c(1),]

library(tidyr)
summary.sim1.50$`landscape-scenario`[summary.sim1.50$`landscape-scenario` == ""] <- NA
summary.sim1.50 <- summary.sim1.50 %>% fill(`landscape-scenario`)

library(dplyr)
summary.sim1.50$`[reporter]` <- recode_factor(summary.sim1.50$`[reporter]`,
                                `count turtles` = "tot people",
                                 `count turtles with [pcolor = 63]` = "people parks",
                                `count turtles with [pcolor = red]` = "people yard with ticks",
                                `count turtles with [pcolor = grey]` = "people in yard no ticks",
                                `count turtles with [color = black]` = "non-exposed people",
                                `count turtles with [color = blue]` = "exposed people",
                                `count turtles with [tick_status = 1]` = "human-tick contact",
                                `count turtles with [ticks-yard = 1]` = "human-tick in yard")
summary.sim1.50$`landscape-scenario` <- as.numeric(as.character(summary.sim1.50$`landscape-scenario`))
summary.sim1.50$`[run number]` <- as.numeric(as.character(summary.sim1.50$`[run number]`))
summary.sim1.50$runID <- paste(summary.sim1.50$`[run number]`,".",summary.sim1.50$`landscape-scenario`)
summary.sim1.50 <- moveMe(summary.sim1.50, c("runID"), where = "after", ba="[run number]")
summary.sim1.50 <- select(summary.sim1.50, -`[run number]`)

##all values
all.sim1.50 <- sim1.50[-c(4:10),]
time_v <- as.data.frame(seq(1:540))
time_v <- time_v %>% add_row(`seq(1:540)` = 0)
time_v <- time_v %>% add_row(`seq(1:540)` = 0)
time_v <- time_v %>% add_row(`seq(1:540)` = 0)
time_v <- time_v[order(time_v$`seq(1:540)`),]
time_v <- as.data.frame(time_v)
time_v$time_v[time_v$time_v == 0]<-""
time_v$time_v <- paste0("day.", time_v$time_v)
time_v$time_v[time_v$time_v == "day."]<-""

all.sim1.50.comb <- cbind(all.sim1.50,time_v)

all.sim1.50.comb$V1 <- paste0(all.sim1.50.comb$V1, all.sim1.50.comb$time_v)
all.sim1.50.comb <- select(all.sim1.50.comb, -time_v)
check <- subset(all.sim1.50.comb, all.sim1.50.comb$V1.V1 == "")

#rename and traspose
all.sim1.50.comb <- t(all.sim1.50.comb)
all.sim1.50.comb <- as.data.frame(all.sim1.50.comb)
all.sim1.50.comb <- subset(all.sim1.50.comb, all.sim1.50.comb$`1` != "")

Newnames.1 <- as.data.frame(all.sim1.50.comb[c(1), ])
Newnames.1 <- as.data.frame(t(Newnames.1))
names(all.sim1.50.comb) <- Newnames.1$V1
all.sim1.50.comb <- all.sim1.50.comb[-c(1),]

all.sim1.50.comb$`landscape-scenario`[all.sim1.50.comb$`landscape-scenario` == ""] <- NA

all.sim1.50.comb <- all.sim1.50.comb %>% tidyr::fill(`landscape-scenario`)

all.sim1.50.comb$`landscape-scenario` <- as.numeric(as.character(all.sim1.50.comb$`landscape-scenario`))
all.sim1.50.comb$`[run number]` <- as.numeric(as.character(all.sim1.50.comb$`[run number]`))

all.sim1.50.comb$`[reporter]` <- recode_factor(all.sim1.50.comb$`[reporter]`,
                                              `count turtles` = "tot people",
                                              `count turtles with [pcolor = 63]` = "people parks",
                                              `count turtles with [pcolor = red]` = "people yard with ticks",
                                              `count turtles with [pcolor = grey]` = "people in yard no ticks",
                                              `count turtles with [color = black]` = "non-exposed people",
                                              `count turtles with [color = blue]` = "exposed people",
                                              `count turtles with [tick_status = 1]` = "human-tick contact",
                                              `count turtles with [ticks-yard = 1]` = "human-tick in yard")



all.sim1.50.comb$runID <- paste0(all.sim1.50.comb$`[run number]`, ".",all.sim1.50.comb$`landscape-scenario`)
all.sim1.50.comb$dup <- paste0(all.sim1.50.comb$runID, all.sim1.50.comb$`[reporter]`)
all.sim1.50.comb <- subset(all.sim1.50.comb[!duplicated(all.sim1.50.comb$dup),])

#wide to long
library(splitstackshape)
long.sim1.50 <- merged.stack(all.sim1.50.comb, id.vars= c("[run number]","landscape-scenario","[reporter]"), var.stubs=c("day"), sep = "day.")
long.sim1.50$.time_1 <- as.numeric(long.sim1.50$.time_1)
long.sim1.50 <- long.sim1.50[order(long.sim1.50$`landscape-scenario`,long.sim1.50$`[run number]`, long.sim1.50$`.time_1`, long.sim1.50$`[reporter]`), ]

long.sim1.50 <- select(long.sim1.50, -dup)
colnames(long.sim1.50)[which(names(long.sim1.50) == ".time_1")] <- "time"
colnames(long.sim1.50)[which(names(long.sim1.50) == "day")] <- "value"

#long to wide
final_DB <- spread(long.sim1.50, `[reporter]`, value)
final_DB <- subset(final_DB, final_DB$`tot people`!=0)

#load landscapes
landscapes <- read.csv("landscape_ref.csv",header = TRUE)
final_DB<- merge(final_DB, landscapes, by.x = "landscape-scenario", by.y = "count")
write.csv(final_DB, "final_DB.csv", row.names = F)

final_DB <- read.csv("final_DB.csv")
#modelo lineal
library(MASS)

modelo <- glm.nb(`human-tick contact` ~ p + A + time + offset(log(`tot people`)), data= final_DB,maxit=1000)

summary(modelo)
#confidence intervals and relative abundance in the exp scale
confint(modelo)
ci<-confint.default(modelo)
exp(cbind(RA=coef(modelo),ci))

modelo.int <- glm.nb(`human-tick contact` ~ p*A + time + offset(log(`tot people`)), data= final_DB,maxit=1000)

summary(modelo.int)
#confidence intervals and relative abundance in the exp scale
confint(modelo.int)
ci<-confint.default(modelo.int)
exp(cbind(RA=coef(modelo.int),ci))

table(final_DB$p)
table(final_DB$A)
#GAM
final_DB$y<- final_DB$human.tick.contact
final_DB$offset <- log(final_DB$tot.people)
final_DB$yb <- cbind(final_DB$`human-tick contact`,final_DB$`tot people`-final_DB$`human-tick contact`)
    

table(final_DB$A)
library(mgcv)
modelo.GAM <- gam(y~ A + s(p,  bs = 'cc', k = 6) + offset(offset),family=nb(),data=final_DB, method = "RELM")
plot(modelo.GAM)
print(modelo.GAM)
summary(modelo.GAM)
modelo.GAM$family$getTheta(TRUE) ## extract final theta estimate

library(mgcViz)
vis.gam(x = modelo.GAM,
        view = c("p","A"),
        plot.type = "persp",
        theta = -30,
        r= 2)

vis.gam(x = modelo.GAM,
        view = c("p","A"),
        plot.type = "contour")

modelo.GAM.int <- gam(y~ A + s(p,  bs = 'cc', k = 6) + s(A,  by = p, bs = 'cc')+ offset(offset),family=nb(),data=final_DB, method = "RELM")
plot(modelo.GAM.int)
print(modelo.GAM.int)
summary(modelo.GAM.int)
modelo.GAM.int$family$getTheta(TRUE) ## extract final theta estimate

library(mgcViz)
vis.gam(x = modelo.GAM.int,
        view = c("p","A"),
        plot.type = "persp",
        theta = 40)

vis.gam(x = modelo.GAM.int,
        view = c("p","A"),
        plot.type = "contour")

library(lme4)
modelo.2<-glmer.nb(`human-tick contact` ~ p*A + offset(log(`tot people`)) + (1|runID),data=final_DB,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(modelo.2)
#model failed

#graficos

final_DB$tick.encounter <- (final_DB$`human-tick contact`/final_DB$`tot people`)*1000


Sum.res <- dplyr::summarize(group_by(final_DB,runID,A, p),
                          tick = median(tick.encounter),
                          tick.95 = quantile(tick.encounter, probs = 0.95, na.rm = TRUE),
                          tick.05 = quantile(tick.encounter, probs = 0.05, na.rm = TRUE))

Sum.res.b <- dplyr::summarize(group_by(Sum.res,A, p),
                            tick.a = median(tick),
                            tick.95.a = quantile(tick, probs = 0.95, na.rm = TRUE),
                            tick.05.a = quantile(tick, probs = 0.05, na.rm = TRUE))

library(viridis)

plot.A <- ggplot(Sum.res.b, aes(x=A, y=tick.a, group=p, color = p)) +
    geom_ribbon(aes(ymin=tick.05.a, ymax=tick.95.a),alpha=0.2) +
    geom_line()+
    scale_color_viridis(discrete = FALSE)+
    labs(x = "Habitat occupancy (%)", y = "daily human-tick encounters per 1000 people")+
    theme_minimal()

plot.A


Sum.res.c <- dplyr::summarize(group_by(final_DB,runID,p, A),
                            tick = median(tick.encounter),
                            tick.95 = quantile(tick.encounter, probs = 0.95, na.rm = TRUE),
                            tick.05 = quantile(tick.encounter, probs = 0.05, na.rm = TRUE))

Sum.res.d <- dplyr::summarize(group_by(Sum.res,p, A),
                              tick.a = median(tick),
                              tick.95.a = quantile(tick, probs = 0.95, na.rm = TRUE),
                              tick.05.a = quantile(tick, probs = 0.05, na.rm = TRUE))

Sum.res.d.05 <- subset(Sum.res.d, Sum.res.d$A<0.4)
Sum.res.d.1 <- subset(Sum.res.d, Sum.res.d$A>0.4)

plot.A <- ggplot(Sum.res.d.05, aes(x=p, y=tick.a, group=A, color = A)) +
    geom_ribbon(aes(ymin=tick.05.a, ymax=tick.95.a),
                alpha=0.2) +
    scale_color_viridis(discrete = FALSE)+
    geom_line()+
    labs(x = "Percolation probability", y = "daily human-tick encounters per 1000 people")+
    theme_minimal()
plot.A

plot.A <- ggplot(Sum.res.d, aes(x=p, y=tick.a, group=A, color = A)) +
    geom_ribbon(aes(ymin=tick.05.a, ymax=tick.95.a),
                alpha=0.2) +
    scale_color_viridis(discrete = FALSE)+
    geom_line()+
    labs(x = "Percolation probability", y = "daily human-tick encounters per 1000 people")+
    theme_minimal()
plot.A