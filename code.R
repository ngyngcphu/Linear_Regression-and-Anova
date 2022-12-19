#----------------------------------------------------------------------#
#--------------------Bai_1---------------------------------------------#
data <- read.csv("C:\\Users\\dell\\Desktop\\XSTK\\gia_nha.csv")
new_DF <- data[, c("price", "sqft_living15", "floors", "condition", "sqft_above", "sqft_living")]

apply(is.na(new_DF), 2, sum)
new_DF <- na.omit(new_DF)

new_DF$price <- log(new_DF$price)
new_DF$sqft_living15 <- log(new_DF$sqft_living15)
new_DF$sqft_above <- log(new_DF$sqft_above)
new_DF$sqft_living <- log(new_DF$sqft_living)

mean <- apply(new_DF[, c(1, 2, 5, 6)], 2, mean)
median <- apply(new_DF[, c(1, 2, 5, 6)], 2, median)
sd <- apply(new_DF[, c(1, 2, 5, 6)], 2, sd)
min <- apply(new_DF[, c(1, 2, 5, 6)], 2, min)
max <- apply(new_DF[, c(1, 2, 5, 6)], 2, max)
x <- data.frame(mean = c(mean), median = c(median), sd = c(sd), min = c(min), max = c(max))
x <- as.data.frame(x, row.names = c("price", "sqft_living15", "sqft_above", "sqft_living"))

data2 <- apply(new_DF[, c("floors", "condition")], 2, table)

hist(new_DF$price,
    xlab = "Price", ylab = "Number of Houses",
    main = "DISTRIBUTION OF PRICE",
    labels = TRUE,
    ylim = c(0, 8000),
    col = "aquamarine1", border = "black"
)

price_by_floors <- boxplot(new_DF$price ~ new_DF$floors,
    ylab = "PRICE",
    xlab = "FLOORS",
    main = "DISTRIBUTION OF PRICE BY FLOORS",
    col = "pink"
)
price_by_condition <- boxplot(new_DF$price ~ new_DF$condition,
    ylab = "PRICE",
    xlab = "CONDITION",
    main = "DISTRIBUTION OF PRICE BY CONDITION",
    col = "red"
)

pairs(new_DF$price ~ new_DF$sqft_living15,
    labels = c("PRICE", "SQFT_LIVING15"),
    col = "red2", pch = 16,
    main = "DISTRIBUTION OF PRICE BY SQFT_LIVING15"
)
pairs(new_DF$price ~ new_DF$sqft_above,
    labels = c("PRICE", "SQFT_ABOVE"), pch = 16,
    col = "dodgerblue1",
    main = "DISTRIBUTION OF PRICE BY ABOVE"
)
pairs(new_DF$price ~ new_DF$sqft_living,
    labels = c("PRICE", "SQFT_LIVING"),
    col = "green3", pch = 16,
    main = "DISTRIBUTION OF PRICE BY SQFT_LIVING"
)

model1 <- lm(
    formula = price ~ sqft_living + sqft_above + sqft_living15 + as.factor(condition) + as.factor(floors),
    data = new_DF
)
summary(model1)

model2 <- lm(
    formula = price ~ sqft_living + sqft_above + sqft_living15 + as.factor(floors),
    data = new_DF
)
summary((model2))

anova(model1, model2)

plot(model1, which = 1, col = "blue3")

prediction <- data.frame(
    sqft_above = c(mean(new_DF$sqft_above), max(new_DF$sqft_above)),
    sqft_living = c(mean(new_DF$sqft_living), max(new_DF$sqft_living)),
    sqft_living15 = c(mean(new_DF$sqft_living15), max(new_DF$sqft_living15)),
    condition = c(3, 3),
    floors = c(2, 2)
)
rownames(prediction) <- c("mean", "max")
pred <- data.frame(predict(model1, prediction, interval = "confidence"))
pred <- as.data.frame(pred)
pred$range <- pred$upr - pred$lwr
pred

pred$range[2] / pred$range[1]
#-------------------------------------------------------------------------------------#
#---------------------------------------Bai_4------------------------------------------#
load("C:\\Users\\dell\\Desktop\\XSTK\\flights.rda")
newFlights <- subset(flights, select = c(
    "carrier", "origin", "dep_time", "arr_time", "dep_delay", "air_time"
))
apply(is.na(newFlights), 2, sum)
sapply(newFlights, function(col) {
    round((sum(length(which(is.na(col)))) /
        nrow(newFlights)) * 100.00, 8)
})
library("magrittr")
library("tidyverse")
newFlights <- newFlights %>%
    mutate(dep_time = case_when(
        is.na(dep_time) ~ as.integer(median(dep_time, na.rm = TRUE)),
        TRUE ~ dep_time
    ))
newFlights <- newFlights %>%
    mutate(arr_time = case_when(
        is.na(arr_time) ~ median(arr_time, na.rm = TRUE),
        TRUE ~ arr_time
    ))
newFlights <- newFlights %>%
    mutate(dep_delay = case_when(
        is.na(dep_delay) ~ median(dep_delay, na.rm = TRUE),
        TRUE ~ dep_delay
    ))
newFlights <- newFlights %>%
    mutate(air_time = case_when(
        is.na(air_time) ~ median(air_time, na.rm = TRUE),
        TRUE ~ air_time
    ))
apply(is.na(newFlights), 2, sum)

columns <- c("mean", "sd", "min", "max", "q25", "q50", "q75")
temp_1d <- cbind(
    tapply(newFlights$dep_delay, newFlights$carrier, mean),
    tapply(newFlights$dep_delay, newFlights$carrier, sd),
    tapply(newFlights$dep_delay, newFlights$carrier, min),
    tapply(newFlights$dep_delay, newFlights$carrier, max),
    tapply(newFlights$dep_delay, newFlights$carrier, quantile, probs = 0.25),
    tapply(newFlights$dep_delay, newFlights$carrier, quantile, probs = 0.5),
    tapply(newFlights$dep_delay, newFlights$carrier, quantile, probs = 0.75)
)
colnames(temp_1d) <- columns
boxplot(newFlights$dep_delay ~ newFlights$carrier,
    data = newFlights,
    main = "dep_delay", xlab = "carrier", ylab = ""
)
library("sqldf")
d1 <- sqldf("select * from newFlights where carrier = 'AA'")
d2 <- sqldf("select * from newFlights where carrier = 'AS'")
d3 <- sqldf("select * from newFlights where carrier = 'B6'")
d4 <- sqldf("select * from newFlights where carrier = 'DL'")
d5 <- sqldf("select * from newFlights where carrier = 'F9'")
d6 <- sqldf("select * from newFlights where carrier = 'HA'")
d7 <- sqldf("select * from newFlights where carrier = 'OO'")
d8 <- sqldf("select * from newFlights where carrier = 'UA'")
d9 <- sqldf("select * from newFlights where carrier = 'US'")
d10 <- sqldf("select * from newFlights where carrier = 'VX'")
d11 <- sqldf("select * from newFlights where carrier = 'WN'")

new_Data <- as.data.frame(rbind(
    d1[-which(d1$dep_delay %in% boxplot(d1$dep_delay, plot = FALSE)$out), ],
    d2[-which(d2$dep_delay %in% boxplot(d2$dep_delay, plot = FALSE)$out), ],
    d3[-which(d3$dep_delay %in% boxplot(d3$dep_delay, plot = FALSE)$out), ],
    d4[-which(d4$dep_delay %in% boxplot(d4$dep_delay, plot = FALSE)$out), ],
    d5[-which(d5$dep_delay %in% boxplot(d5$dep_delay, plot = FALSE)$out), ],
    d6[-which(d6$dep_delay %in% boxplot(d6$dep_delay, plot = FALSE)$out), ],
    d7[-which(d7$dep_delay %in% boxplot(d7$dep_delay, plot = FALSE)$out), ],
    d8[-which(d8$dep_delay %in% boxplot(d8$dep_delay, plot = FALSE)$out), ],
    d9[-which(d9$dep_delay %in% boxplot(d9$dep_delay, plot = FALSE)$out), ],
    d10[-which(d10$dep_delay %in% boxplot(d10$dep_delay, plot = FALSE)$out), ],
    d11[-which(d11$dep_delay %in% boxplot(d11$dep_delay, plot = FALSE)$out), ]
))
boxplot(new_Data$dep_delay ~ new_Data$carrier,
    xlab = "carrier",
    ylab = "dep_delay", main = "dep_delay", outline = FALSE,
    col = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

delay <- subset(newFlights[c(1, 5)], newFlights$origin == "PDX")
AA <- subset(delay, delay$carrier == "AA")
AS <- subset(delay, delay$carrier == "AS")
B6 <- subset(delay, delay$carrier == "B6")
DL <- subset(delay, delay$carrier == "DL")
F9 <- subset(delay, delay$carrier == "F9")
HA <- subset(delay, delay$carrier == "HA")
OO <- subset(delay, delay$carrier == "OO")
UA <- subset(delay, delay$carrier == "UA")
US <- subset(delay, delay$carrier == "US")
VX <- subset(delay, delay$carrier == "VX")
WN <- subset(delay, delay$carrier == "WN")

library("nortest")
ad.test(AS$dep_delay)
ad.test(B6$dep_delay)
ad.test(DL$dep_delay)
ad.test(F9$dep_delay)
ad.test(HA$dep_delay)
ad.test(OO$dep_delay)
ad.test(UA$dep_delay)
ad.test(US$dep_delay)
ad.test(VX$dep_delay)
ad.test(WN$dep_delay)

qqnorm(AA$dep_delay, col = "blue", main = "Q-Q plot of AA")
qqline(AA$dep_delay, col = "red")

qqnorm(AS$dep_delay, col = "blue", main = "Q-Q plot of AS")
qqline(AS$dep_delay, col = "red")

qqnorm(B6$dep_delay, col = "blue", main = "Q-Q plot of B6")
qqline(B6$dep_delay, col = "red")

qqnorm(DL$dep_delay, col = "blue", main = "Q-Q plot of DL")
qqline(DL$dep_delay, col = "red")

qqnorm(F9$dep_delay, col = "blue", main = "Q-Q plot of F9")
qqline(F9$dep_delay, col = "red")

qqnorm(HA$dep_delay, col = "blue", main = "Q-Q plot of HA")
qqline(HA$dep_delay, col = "red")

qqnorm(OO$dep_delay, col = "blue", main = "Q-Q plot of OO")
qqline(OO$dep_delay, col = "red")

qqnorm(UA$dep_delay, col = "blue", main = "Q-Q plot of UA")
qqline(UA$dep_delay, col = "red")

qqnorm(US$dep_delay, col = "blue", main = "Q-Q plot of US")
qqline(US$dep_delay, col = "red")

qqnorm(VX$dep_delay, col = "blue", main = "Q-Q plot of VX")
qqline(VX$dep_delay, col = "red")

qqnorm(WN$dep_delay, col = "blue", main = "Q-Q plot of WN")
qqline(WN$dep_delay, col = "red")

library("car")
leveneTest(dep_delay ~ as.factor(carrier), data = delay)

ano_test <- aov(dep_delay ~ carrier, data = delay)
ano_test

summary(ano_test)
TukeyHSD(ano_test)
#----------------------------------------------------------------------------------------#
#-----------------------------------Phan_rieng------------------------------------------#
# import data
data <- read.csv("C:\\Users\\dell\\Desktop\\XSTK\\machine.data", header = FALSE)
# rename title column
colnames(data) <- c(
    "vendor_name", "model_name", "myct", "mmin",
    "mmax", "cach", "chmin", "chmax", "prp", "erp"
)
# data cleaning
new_data <- subset(data, select = c(
    "myct", "mmin",
    "mmax", "cach", "chmin", "chmax", "prp"
))

apply(is.na(new_data), 2, sum)

# data visualization
mean <- apply(new_data, 2, mean)
medium <- apply(new_data, 2, median)
sd <- apply(new_data, 2, sd)
min <- apply(new_data, 2, min)
max <- apply(new_data, 2, max)
data_table <- t(data.frame(mean, medium, sd, min, max))
# histogram
hist(new_data$prp,
    main = "Distribution of PRP", xlab = "PRP", labels = TRUE,
    ylim = range(0, 200)
)
# pair
pairs(prp ~ myct, new_data)
pairs(prp ~ mmin, new_data)
pairs(prp ~ mmax, new_data)

pairs(prp ~ cach, new_data)
pairs(prp ~ chmin, new_data)
pairs(prp ~ chmax, new_data)
# linear regression
model1 <- lm(prp ~ myct + mmin + mmax + cach
    + chmin + chmax, data = new_data)
summary(model1)
model2 <- lm(prp ~ myct + mmin + mmax + cach + chmax, data = new_data)
summary(model2)
anova(model1, model2)
plot(model2, which = 1)
data_test <- data.frame(
    myct <- c(mean(new_data$myct), max(new_data$myct)),
    mmin <- c(mean(new_data$mmin), max(new_data$mmin)),
    mmax <- c(mean(new_data$mmax), max(new_data$mmax)),
    cach <- c(mean(new_data$cach), max(new_data$cach)),
    chmax <- c(mean(new_data$chmax), max(new_data$chmax))
)
rownames(data_test) <- c("test1", "test2")
colnames(data_test) <- c("myct", "mmin", "mmax", "cach", "chmax")
pred <- data.frame(predict(model2, data_test, interval = "confidence"))
pred$range <- pred$upr - pred$lwr

pred
pred[2, 1] - max(new_data[, 7])
pred$range[2] / pred$range[1]