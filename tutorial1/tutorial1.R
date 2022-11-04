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

# Excercise 6








