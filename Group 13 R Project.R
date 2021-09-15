> Numeric <- read_excel("RNumericData.xlsx")
> View(Numeric)
# Load the package
library(ggstatsplot)
# Load the dataset 
data("RNumericData")
# Create a boxplot of the dataset, outliers are shown as two distinct points
boxplot(Total_Trans_Amt ~ Gender, RNumericData)
#Create a boxplot that labels the outliers  
ggbetweenstats(RNumericData,
               Gender, Total_Trans_Amt, outlier.tagging = TRUE, color="#00ab9e")
Q <- quantile(RNumericData$Total_Trans_Amt, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(RNumericData$Total_Trans_Amt)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range?
eliminated<- subset(RNumericData, RNumericData$Total_Trans_Amt > (Q[1] - 1.5*iqr) &
                      RNumericData$Total_Trans_Amt < (Q[2]+1.5*iqr))
ggbetweenstats(eliminated, Gender, Total_Trans_Amt, outlier.tagging = TRUE) 

library(tidyr)
library(purrr)
RProjectData <- RProjectData %>% mutate(Attrition_Flag = recode(Attrition_Flag, 
                                                                "Attrited Customer" = 1, 
                                                                "Existing Customer" = 0))
table(RProjectData$Attrition_Flag)
RProjectData %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

RProjectData -> cc

library(ggplot2)
c <- ggplot(cc,aes(Customer_Age))
c + geom_bar(fill='#69a297', alpha=0.9) 



c <- ggplot(cc,aes(Gender))
c + geom_bar(fill=c("#00ab9e","#ccff33"), width=0.7, alpha=0.7)


cc$Income_Category <- factor(cc$Income_Category, 
                             levels=c("Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K",
                                      "$80K - $120K", "$120K +"))
c <- ggplot(cc, aes(Income_Category))
c + geom_bar(fill=c("#00ab9e", "#53bb84", "#ccff33",
                    "#69a297", "#50808e", "#012a4c"), width=0.7, alpha=0.8)

cc$Education_Level <- factor(cc$Education_Level, 
                             levels=c( "Unknown", "Uneducated", "High School",
                                       "College", "Graduate", "Post-Graduate", "Doctorate"))
c <- ggplot(cc, aes(Education_Level))
d <- c + geom_bar(fill=c("#00ab9e", "#53bb84", "#ccff33", 
                         "#a3c9a8", "#69a297", "#50808e", "#012a4c"), width=0.7, alpha=0.8)
d + coord_flip()

mydata <- read_excel("RRegressionData.xlsx")


# Multiple Linear Regression 
fit <- lm(Total_Trans_Amt ~ Avg_Utilization_Ratio + Total_Trans_Ct + 
            Total_Relationship_Count + Months_on_book + Income_Category + 
            Dependent_count , data=mydata)
summary(fit) # show results
coefficients(fit) # model coefficients
fitted(fit) # predicted values
residuals(fit) # residuals
vcov(fit) # covariance matrix for model parameters


chisq.test(Chi_Sq1)

#palette: "#00ab9e", "#53bb84", "#ccff33", "#a3c9a8", "#84b598", "#69a297", "#50808e", "#012a4c"

#removing unrequired columns
library(dplyr)
cc<-BankChurners %>% select(-CLIENTNUM, -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2)
View(cc)
summary(cc)

#df containing numerical variables
cc1<-cc %>% select(-Attrition_Flag,-Gender,-Education_Level,-Marital_Status,-Income_Category,-Card_Category)
View(cc1)
summary(cc1)

#skewness and kurtosis
library(moments)
skewness(cc1$Avg_Utilization_Ratio)
kurtosis(cc1$Avg_Utilization_Ratio)

skewness(cc1$Credit_Limit)
kurtosis(cc1$Credit_Limit)

#correlation b/w numerical variables
cc1.cor=cor(cc1)
cc1.cor

#visualising correlation b/w numerical variables
library(corrplot)
corrplot(cc1.cor, method="color", col=colorRampPalette(c("#012a4c","#50808e","white","#ccff33","#00ab9e"))(200))

#visualisation 
library(tidyr)
library(purrr)
#continuous variable distributions
cc <- cc %>% mutate(Attrition_Flag = recode(Attrition_Flag, "Attrited Customer" = 1, "Existing Customer" = 0))
table(cc$Attrition_Flag)
cc1 %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

library(ggplot2)
#age distribution
c <- ggplot(cc1,aes(Customer_Age))
c + geom_bar(fill='#69a297', alpha=0.9) 

#income barchart
cc$Income_Category <- factor(cc$Income_Category, levels=c("Unknown", "Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +"))
c <- ggplot(cc, aes(Income_Category))
c + geom_bar(fill=c("#00ab9e", "#53bb84", "#ccff33", "#69a297", "#50808e", "#012a4c"), width=0.7, alpha=0.8)

#education level bar chart
cc$Education_Level <- factor(cc$Education_Level, levels=c( "Unknown", "Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate"))
c <- ggplot(cc, aes(Education_Level))
d <- c + geom_bar(fill=c("#00ab9e", "#53bb84", "#ccff33", "#a3c9a8", "#69a297", "#50808e", "#012a4c"), width=0.7, alpha=0.8)
d + coord_flip()

#attrition flag pie chart
ggplot(BankChurners, aes(x = "", y = Attrition_Flag, fill=Attrition_Flag)) +
  geom_bar(stat = "identity", alpha=0.8) + scale_fill_manual(values=c("#ccff33","#69a297")) +
  coord_polar("y",start=0) + theme_void()

#one sample t-test
#H0: mu = 0.5, H1: mu <0.5
t.test(cc1$Avg_Utilization_Ratio, mu=0.5, alt="less", conf.level=0.95)
