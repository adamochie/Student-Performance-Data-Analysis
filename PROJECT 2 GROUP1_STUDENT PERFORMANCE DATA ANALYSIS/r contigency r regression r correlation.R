#1#Hypothesis testing on 2 mean sample
#declaration
G1 <- c(student_data$G1)
G2 <- c(student_data$G2)
alpha <- c(0.05)
n1<-c(363)
n2<-c(363)
#standard deviation
s1<-c(sd(G1))
s2<-c(sd(G2))
#critical value two tail, cv = 1.966539
cv.G1G2<- c(qt(p=.025,df=362,lower.tail = FALSE))
#t test dependant
t.test(G1,G2,paired = TRUE)
tstats <- c(1.3826)
#not equal 10.93939 and 10.79614
mean(G1)
mean(G2)


#2#data contingency HEALTH VS GUARDIAN (only mother/father)
table(student_data$guardian)
table(student_data$health)
contigency = data.frame(
            "HEALTH" = c(student_data$health),
            "GUARDIAN" = c(student_data$guardian)
            ) 
#create contigency 2 way  table
contable = table(contigency)
print(contable)

install.packages("plotrix")
library(plotrix)

#3#regression ABSENCE vs GO OUT
x <- c(student_data$absences)
y <- c(student_data$goout)
#create linear regression model
model <- lm(y~x)
print(model) #from below  [y = 3.073679 + 0.006163x]  slide 20
#Data Frame
df <- as.data.frame(cbind(x, y))
print(df)
# Summary of the regression model
summary(model)
#scatterplot regression
plot(x,y, main = "Absence vs The frequency of student Going Out Regression model",
          xlab = "Number of School Absences",
          ylab = "Going out with friends",pch = 16)
abline(lm(y~x))


install.packages("plotrix")
library(plotrix)

#4#scatterplot correlation GRADE1 vs GRADE2
table(student_data$G1)
plot(x=student_data$G1,y=student_data$G2, xlab = "First Period Grade",
                                          ylab = "Second Period Grade", 
                                            main = "Correlation Period Grade",pch = 16)
abline(lm(student_data$G1~student_data$G2))
#confidence level 95%
cor.test(student_data$G1,student_data$G2)
#confidence level 90%
cor.test(student_data$G1,student_data$G2,conf.level = 0.90)
#greater than 0
cor.test(student_data$G1,student_data$G2,alternative = "greater")

#1#Hypothesis testing on 2 mean sample
#declaration
G1 <- c(student_data$G1)
G2 <- c(student_data$G2)
alpha <- c(0.05)
n1<-c(363)
n2<-c(363)
#standard deviation
s1<-c(sd(G1))
s2<-c(sd(G2))
#critical value two tail, cv = 1.966539
cv.G1G2<- c(qt(p=.025,df=362,lower.tail = FALSE))
#t test dependant
t.test(G1,G2,paired = TRUE)
tstats <- c(1.3826)
#not equal 10.93939 and 10.79614
mean(G1)
mean(G2)
