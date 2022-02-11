# 20C14001 - Le Duong Tuan Anh
install.packages("car")
library(car)
data(Salaries)

####### Additional Question #1
# 1. Count rank/sex
counts <- table(Salaries$rank, Salaries$sex)
counts
# Proportions
ptab <- prop.table(counts, margin=1)
ptab
# 2. Histogram with proportions
hist(Salaries$yrs.service, prob = TRUE, xlab="Year of Services")
lines(density(Salaries$yrs.service), lwd = 2, col = "red")
# 3. boxplot salary
boxplot(Salaries$salary, ylab="Salary")
# 4. boxplot salary by rank
boxplot(Salaries$salary ~ Salaries$rank, xlab="Rank", ylab="salary", horizontal = TRUE)


####### Additional Question #2
# 1. plot salary vs years since phd
plot(Salaries$salary ~ Salaries$yrs.since.phd, xlab="Years since PhD", ylab = "Salary")

# 2. correlation 
# print correlation between all numeric cols
cor(Salaries[, c(3,4,6)])
# correlation between years of services vs year since phd is high (>0.9)
plot(Salaries$yrs.service ~ Salaries$salary)
plot(Salaries$yrs.since.phd ~ Salaries$salary)
plot(Salaries$yrs.service ~ Salaries$yrs.since.phd)
