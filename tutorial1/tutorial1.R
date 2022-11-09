# PART 1
# Exercise 1
birth <- read.csv('birthstatistics.csv')

# Excercise 2
blog <- read.csv('blogData_test.csv', header = FALSE)     

# Excercise 3
tecator <- readxl::read_excel('tecator.xls')

# Excercise 4
write.csv(tecator, "tecator.csv", row.names = FALSE)

# Part 2
# Excercise 1
tecator1 = as.data.frame(tecator)

# Excercise 2
rownames(tecator1) =tecator1$Sample+10

# Excercise 3
colnames(tecator1)[1] = "ID"

# Excercise 4
print(tecator1[tecator1$Channel1 >3 & tecator1$Channel2>3, 5:8])

# Excercise 5
tecator1 = subset(tecator1, select = -c(ID))

# Exercise 6
library(stringr)
index = str_which (colnames(tecator1), "Channel")
tecatorChannel=tecator1[,index]
means = colMeans(tecatorChannel)
tecator1[,index] = tecator1[,index]/matrix(means,nrow=nrow(tecatorChannel),ncol = ncol(tecatorChannel),byrow = TRUE)

# Exercise 7
sumsq = apply(tecator1[1:5], MARGIN = 1, FUN = function(x) return(sum(x^2)))
tecator2 = matrix(sumsq, ncol=1)

# Exercise 8
X = as.matrix(tecator1[,-c(101,102,103)])
y = as.matrix(tecator1[,"Fat", drop = F])
result = solve(((t(X)%*%X)^-1),(t(X)%*%y))

# Exercise 9
tecator1$ChannelX = as.factor(ifelse(tecator1$Channel1 > 1, "high", "low"))

# Exercise 10
Intercepts = numeric(100)
for (i in 1:length(Intercepts)){
  regr= lm(formula = paste("Fat~Channel",i,sep = ""), data = tecator1)
  Intercepts[i] = coef(regr)[1]  
}
print(Intercepts)

# Exercise 11
x = c(1,3)
y = 5*x + 1
plot(x,y,type="l",col="blue")

# Data manipulation
# 1
birth1 = dplyr::tibble(birth)

#2
birth2 = birth1 %>% dplyr::select(X2002:X2020)

#3
birth1 = birth1 %>% dplyr::mutate(Status = ifelse(foreign.Swedish.background == "born in Sweden with two parents born in Sweden", "yes","no"))

#4
birth1%>%dplyr::count(sex,region)

# 5
birth3 = birth1%>% dplyr::select(-sex, -foreign.Swedish.background)%>%dplyr::group_by(Status, region)%>%dplyr::summarise_all(sum)%>%dplyr::ungroup()

# 6
birth4 = birth3%>% dplyr::group_by(region)%>%
        dplyr::mutate(Percentage = X2002/sum(X2002)*100)%>%
        dplyr::filter(Status == "yes")%>%
        dplyr::ungroup()%>%
        dplyr::arrange(Percentage)

# 7
birth5 = birth1%>%dplyr::group_by(region,sex,foreign.Swedish.background, Status)%>%
        tidyr::pivot_longer(X2002:X2020, names_to="Year", values_to = "Born")%>%
        dplyr::mutate(Year=as.numeric(stringr::str_remove(Year,"X")))

# 8
birth6 = birth5%>%dplyr::group_by(sex,region,foreign.Swedish.background, Status)%>%
        tidyr::pivot_wider(names_from = Year, values_from = Born, names_prefix = "Y_")


# 9
blog_filtered = blog%>%dplyr::select_if( function(x) !all(x==0))
blog_filtered   




