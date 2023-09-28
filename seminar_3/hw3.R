typeof(mtcars) # mtcars относится к типу list

typeof(mtcars[2]) # второй столбец тоже относится к типу list

names(mtcars)
mtcars["Fiat 128","cyl"] # У fiat128 4 цилиндра
row.names(mtcars[mtcars$cyl == 4, ]) # все машины с 4 цилиндрами

?mtcars
min(c(1, 2, 3))
min(mtcars$cyl) # минимальное число цилиндров
row.names(mtcars[mtcars$cyl == 4, ]) # все машины с 4 цилиндрами

corr = cor(mtcars)
typeof(corr) # относится к типу double

t = corr["mpg", ] < -0.7
names(t[t == TRUE]) 
# корреляция < -0.7 наблюдается с:
# 1. Number of cylinders 
# 2. Displacement
# 3. Gross horsepower
# 4. Weight

#------------------------------------------------------------------------------#

vec = rnorm(100, mean = 40, sd = 10)
typeof(vec)
vec[seq(1, length(vec), by = 3)] # подвектор с каждым 3им значением
vec[-seq(1, length(vec), by = 5)] # подвектор без каждого 5-го элемента
vec[as.integer(vec) %% 2 == 0] # подвектор с четной целой частью

#------------------------------------------------------------------------------#

tree = list(list("a", list("b", "c")), list("d", "e"))

names(tree) = c("left", "right")
names(tree$left) = c("left", "right")
names(tree$left$right) = c("left", "right")
names(tree$right) = c("left", "right")

unlist(tree) # вектор со всеми значениями
unlist(tree$left) # левое поддерево
tree$left$right # узел с листьями b и c

#------------------------------------------------------------------------------#