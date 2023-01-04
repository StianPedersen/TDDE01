##install.packages("dplyr")
##install.packages("tidyr")
##install.packages("readxl")

df_birth <- read.csv("birthstatistics.csv", header = TRUE)
df_blog <- read.csv("blogData_test.csv", header = FALSE)
library("readxl")
df_tecator <- read_excel("tecator.xls")
write.csv(df_tecator, file = "tecator.csv", row.names = FALSE) 
tecator1 <- as.data.frame(df_tecator) 
library("dplyr")
colnames(tecator1) <- c("ID", colnames(select(tecator1, -Sample))) #Changes tecator1 colnames
colnames(tecator1)[which(names(tecator1) == "Protein")] <- "BicepsJuice" #Change by name

modified_tecator <- tecator1[tecator1$Channel1 > 3 & tecator1$Channel2 > 3, 5:8]

no_ID_tecator <- select(tecator1, -ID)
#tecator1$ID=c()  Alternativ lösning


library(stringr) #Load stringr lib
index = str_which(colnames(tecator1), "Channel") #Creates index for all channel cols
tecatorChannel = tecator1[,index] #Makes a df with only channels
means = colMeans(tecatorChannel) #Takes the means of the Col.
tecator1[,index] = tecator1[,index]/matrix(means, 
                                           nrow=nrow(tecatorChannel),
                                           ncol=ncol(tecatorChannel),
                                           byrow=TRUE) #Dela tecator channels med mean.


sumsq <- apply(X = as.matrix(tecator1[1:5,]), 
               MARGIN = 1,
               FUN = function(x) return(sum(x^2))) #Apply FUN to each row in X 
tecator2 <- matrix(sumsq, ncol = 1) #Create a matrix with sumsq and only one col.

X <- as.matrix(tecator1[,-(101:103)])
Y <- as.matrix(select(tecator1, Fat))
result <- solve(t(X)%*%X, t(X)%*%Y) #
det(t(X)%*%X)#Det = zero so there is no inverse ...

tecator1$ChannelX <- as.factor(ifelse(tecator1$Channel1 > 1,
                                      "high", #ifelse is smart
                                      "low")) #factor finds Levels
#Make linear regression with Fat as dependent and each Channel as dependent
#Then show intercept for each lm.
intercept = numeric(100)
for (i in 1:100)
{
  linear_model <- lm(paste("Fat~Channel", i, sep = ""), data = tecator1)#lm(Fat~tecator[i+1])
  intercept[i] = coef(linear_model)[1] 
}
intercept

x=c(1,3)
y=5*x+1
plot(x,y, type="l", col="blue") #Type L is line.

#dplyr och tidyr
library("dplyr")
library("tidyr")
birth1 <- tibble(df_birth)
birth2 <- select(birth1, X2002:X2020)
#alt
birth2 <- birth1 %>%
  select(X2002:X2020)
birth1$Status <- ifelse(birth1$foreign.Swedish.background == "born in Sweden with two parents born in Sweden",
                        "Yes",
                        "No")
#alt
birth1 <- birth1 %>% 
  mutate(Status = ifelse(birth1$foreign.Swedish.background == "born in Sweden with two parents born in Sweden",
                "Yes",
                "No"))
birth1 <- subset(birth1,select = -24) #Drop column 24

birth1 %>% count(sex, region) #Counts the intersect.

#Summarize all the ppl born x20** in each region with the same status. 
birth3 <- birth1 %>% 
  select(X2002:X2020, Status, region) %>%
  group_by(Status, region) %>%
  summarize_all(sum) %>%
  ungroup()

#compute percentage of people in 2002 having Status=Yes in different counties. 
#Report a table with column region and Percentage sorted by Percentage.
birth4 <- birth3 %>% 
  group_by(region) %>%
  mutate(Percentage = X2002/sum(X2002)*100) %>%
  filter(Status == "Yes") %>%
  select(region, Percentage) %>%
  ungroup() %>%
  arrange(Percentage)
birth4  

#Long
birth5 = birth1%>%
  group_by(region, sex, foreign.Swedish.background, Status)%>%
  pivot_longer(X2002:X2020, names_to="Year", values_to = "Born")%>%
  mutate(Year=as.numeric(stringr::str_remove(Year, "X"))) 
#Wide
birth6 = birth5%>%
  group_by(region, sex, foreign.Swedish.background, Status)%>%
  pivot_wider(names_from = Year, values_from = Born, names_prefix = "Y_")

#If all is zero
blogS <- tibble(df_blog) %>% select_if(function(x) !all(x==0))
blogS
