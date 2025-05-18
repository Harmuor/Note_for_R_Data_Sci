# 第13章 使用magrittr进行管道操作
# 陌生的老熟人“ %>% ”，其实是magrittr包的功能
library(magrittr)
library(dplyr)
# 管道的替代方式和不适用情景
# 这个看书吧还是，话说都是我自己曾经踩过的坑

#  %T% 三通管道，允许我们生成主线工作流中的一些副产物
# 其实使用{function(x); x}我觉得更方便，意思是执行函数并接着干嘛干嘛。。。
x <- c(1, 2, 3, 4) 
x %>% 
  set_names(c('a', 'b', 'c', 'd')) %>%  
  {plot(x); x} %>% 
  {print(x); x} %>% 
  as.data.frame()



# 第14章 函数
# 当一段代码需要复制粘贴>2次的时候，或最好使用函数
to_be_1 <- function(x) {
  rng <- range(x, na.rm = T, finite = T)
  (x - rng[1])/(rng[2] - rng[1])
}

to_be_1(c(-Inf, 12, 32, 44, 65, 103, Inf))

rep_as_same <- function(x, y) { 
  rep(y, length.out = length(x)) 
}




# ctrl + shift + R：分节标记插入

# This is a insert label --------------------------------------------------


# if 条件执行
if (condition1) {
  # 执行操作1
} else {
  # 执行操作2
}


# 在if中，推荐使用“||”和“&&”，与普通的逻辑符号不同
# 这俩只会对头一个元素进行判断并只返回一个布尔值，而if只想要一个布尔值
# 或者能用any()函数和all()函数来进行转化
x <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
y <- c(FALSE, TRUE, FALSE, FALSE, TRUE)

if (any(x|y)) {
  print('x和y的“或者”逻辑运算的结果至少一个True')
} else {
  print('x和y的“或者”逻辑运算的结果没有一个True')
}

if (all(x & y)) {
  print('x[1]和y[1]的“并且”逻辑运算的结果全票通过')
} else {
  print('x[1]和y[1]的“并且”逻辑运算的结果没全票通过')
}




# 多重条件
# 假如我们的条件判断并非二元对立的
# 这里顺便说一下，“==”可以用identical()来代替
x = 3

if (identical(x, 1)) {
  print('x is 1')
} else if (identical(x, 2)) {
  print('x is 2')
} else {
  print('x is neither 1 or 2')
}


# 不过书上说了，if的数量不要太多，可以考虑使用switch()函数来平替
# switch()的功能直接看效果吧
swi <- function(x, y, s) {
  # 根据s输入的东西来预设不同操作
  switch(s, 
         plus = x + y, 
         minus = x - y, 
         times = x * y, 
         divide = x / y)
}

swi(4, 2, 'plus')
swi(4, 2, 'minus')
swi(4, 2, 'times')
swi(4, 2, 'divide')


# 当代码比较短，花括号{}可以省略
x <- (4 + 4) * 34 %/% 12
if (x %% 3 > 2) '我算对了' else '其实是蒙的'




# 根据时间欢迎的代码
# 使用cut将时间这个连续体转换为离散变量就比较方便
library(lubridate)

greet <- function() {
  # 获取当前时间然后分时间段
  tm <- cut(hour(now()), 
            breaks = c(0, 6, 11, 12, 18), 
            labels = c('Wee Hour', 'Morning', 'Noon', 'Afternoon'),
            right = F)
  # 根据时间段判断该说啥
  if (tm == 'Wee Hour') {
    '凌晨了快去睡觉'
  } else if (tm == 'Morning') {
    '早上好'
  } else if (tm == 'Noon') {
    '中午好'
  } else if (tm == 'Afternoon') {
    '下午好'
  } else {
    '晚上好'
  }
}

greet()


# 实现fizzbuzz 函数，接受一个数值作为输入。如果这个数值能被3整除，那么就返回“fizz”；
# 如果能被5整除，就返回“buzz”；如果能同时被3和5整除，则返回“fizzbuzz”；否则，就返回这个数值。

fizzbuzz <- function(x) {
  # 其实用all()在本题中就比较方便
  if (all(x %% 3 == 0 & x %% 5 == 0)) {
    'fizzbuzz'
  } else if (x %% 5 == 0) {
    'fizz'
  } else if (x %% 3 == 0) {
    'buzz'
  } else {
    'You got nothing'
  }
}

fizzbuzz(55)




# 了解参数 stopifont()检查每个参数是否为真，如错误返回有关信息
# 假如我们在使用自定义函数时，一个参数没写对，需要一个指路牌告诉我们哪里没对
# 假如我们要写个计算加权均分的函数

wt_average <- function(x, wt, na.rm = FALSE) {
  
  # 一个参数在一个stopifnot可以声明很多
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(wt))
  
  # 然后是当na.rm参数为TRUE的时候剔除NA，默认的F情景可以先不管
  if (na.rm == T) {
    miss <- is.na(x) | is.na(wt)
    x <- x[!miss]
    wt <- wt[!miss]
  }
  
  # 这里才是进行加权平均
  sum(x * wt) / sum(wt)
}

x <- c(1:15, NA)
wt <- c(20:6, NA)

wt_average(x, wt, na.rm = T)




# 点点点(...)参数，这意味着可以接受任意数量的输入
library(stringr)
dot <- function(...) {
  str_c(...)
}

dot('x', 'y', 'z')


# 函数支持管道
# 转换函数和副作用函数
# 有时为了让控制台不那么杂乱，我们可以使用invisible()来隐去显示一些输入
library(dplyr)
df <- tribble(
  ~`x`, ~y, ~z, 
  1, 2, 3, 
  6, 5, 4, 
  7, 8, 9
)

show_missings <- function(df) { 
  n <- sum(is.na(df)) 
  cat("Missing values: ", n, "\n", sep = "") 
  invisible(df)
}

show_missings(df)


# 函数的环境
# 看书吧这一块




# 第15章 向量
# 确认向量类型
typeof(4.5)
typeof('jdssaj')
typeof(NULL)


# 获取向量长度
length('sdfsdf')
length(c('sdfljs', 'sdflsd', 'sdflj', 'lhjiuh'))


# 一般r生成默认double，如果需要生成的是integer，加L后缀
typeof(1)
typeof(1L)


# 不过需要注意的是，在比较浮点数向量的时候，==操作容易出错，用dplyr::near()比较好
library(dplyr)
x <- sqrt(2)^2

x - 2 == 0 #这里就出错了
near(0, x - 2) #这样就好得多，能忽略极小误差


# 对于数值，有四种缺失值NA、NaN、-Inf和Inf，对于这些值的逻辑操作，分别对应着：
is.na(NA)
is.na(NaN)

is.nan(NaN)
is.finite(Inf)
is.infinite(-Inf)


# 对原子向量的操作
# 强制转换那个分隐性和显性

# 检验向量，除了typeof()
is.character('sdfds')
is.numeric(233)
is.integer(333)
is.double(2.333)
is.factor('dsfsd')
is.vector(c(1, 2, 3, 4))


# 向量循环，就是俩一长一短向量对齐，短的重复几次变得和长的一样长，这个叫向量循环
# r理论上“最小颗粒”是向量，而不是标量




# 向量命名，两种方式。这对向量取子集很重要
# 方式1 创建的时候命名好
x <- c('ele1' = 1, 'ele2' = 2, 'ele3' = 3, 'ele4' = 4, 'ele5' = 5, 'ele6' = 6)

# 方式2 使用set_names()命名
x <- c(1, 2, 3, 4)
set_names(x, c('第一列', '第二列', '第三列', '第四列'))




# 向量取子集。向量命名很重要
# 这个类似于tibble中的filter()，其实也算老熟人原来也用过。即“[]”中括号

# 用正整数取就很直观
x <- c(NA, 'ele1' = 1, 'ele2' = 2, 'ele3' = 3, 'ele4' = 4, NA)
x[c(1, 2, 3)]
x[2]

# 用负数取就是剔除特定项
x[c(-2, -3)]
x[-2]

# 用元素名来呢
x['ele1']


# 在更为广泛的使用中，一般按照逻辑操作来取子集
x[!is.na(x)]
x[x %% 2 == 0]
x[x >= 3]


# 还有个变体[[]]，专门用在list和tibble里
# []会单独提取数据框的特定列，而[[]]则是还原向量，降低一个层级返回结果
df <- data.frame(a = 1:3, b = 4:6)
a1 <- df[1]
a2<- df[[1]]

is.atomic(a1)
is.atomic(a2)




# 递归向量（列表）
x <- list(1, 2, 3)
y <- list(c(1, 2, 3), 'Harmuor', 'FALSE', '3.22')
(x)
(y)

z <- list('A' = x, 'B' = y, 'C' = c('sdfsd', 'ouo'), 'D' = c(1, 2, 3)) #列表还能包含其他列表


# 如果想要更清楚知道列表结构
str(x)
str(y)
str(z)
set_names(x, c('a', 'b', 'c'))




# 提取列表子集 一般有仨方式
lista <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

lista[1]
lista[1:2]

lista[4]
lista[[4]]
lista[[4]][1]
lista[[4]][[1]]

lista$a
lista$d




# 特性 这一节看不懂思密达
# 使用attr()为读取或设置某向量的特性
x <- 1:10
attr(x, 'hello')

attr(x, 'hello') <- 'hi'
attr(x, 'bye') <- 'see you'

# 使用attributes()读取某向量所有的特性
attributes(x)




# 拓展向量
# 因子向量
fct <- c('male', 'female', 'female', 'male', 'male')
fct <- as.factor(fct) %>% 
  factor(levels = c('male', 'female'), 
         labels = c('男', '女'))

attributes(fct) #多了俩特性


# 日期向量
dt <- as.Date('2025-03-27')
attributes(dt)

#可以这家增加时区特性
attr(dt, 'tzone') <- 'Asia/Shanghai'


# tibble
# 是对r基础数据框的发展，我一猜就是：
# 相比于list，这俩货绝对要求元素长度相等，不然数据分析个屁
library(dplyr)
tble <- tibble(a1 = c(1, 2, 3), 
               a2 = c('a', 'b', 'c'), 
               a3 = c(1.2, 2.3, 3.4))
attributes(tble)


# 其实基于tibble的定义，列表也可以作为数据框的一列，但是然并卵
lista <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
eee <- tibble(a2 = lista)
attributes(eee)
str(eee)
View(eee)

eee[[1]][[4]][[2]] #双重嵌套的列表




# 第16章 使用purrr实现迭代
library(tidyverse)

# 这里书上给了个批量处理各列均值的例子
df <- tibble( 
  a = rnorm(10), 
  b = rnorm(10), 
  c = rnorm(10), 
  d = rnorm(10))

output <- vector(mode = 'double', length = ncol(df))
for (i in seq_along(df)) {
  # seq_along(df) 用于生成索引序列（向量），ncol(df)是直接返回数据框的列数（单个数字）
  
  output[i] <- mean(df[[i]])
  # [[]]提取的是元素本身，[]会返回一个子列表（仍然是列表类型）
}

output



# for循环基础 通过练习来学习
# 练习1 计算出mtcars 数据集中每列的均值
df1 <- mtcars
(mtcars)
attributes(df1)

answer1 <- vector(mode = 'double', length = ncol(df1))

for (i in seq_along(df1)) {
  answer1[[i]] <- mean(df1[[i]])
}

answer1



# 练习2 确定 nycflights13::flights 数据集中每列的类型
library(nycflights13)
df2 <- nycflights13::flights

answer2 <- vector(mode = 'character', length = ncol(df2))

for (i in seq_along(df2)) {
  answer2[[i]] <- typeof(df2[[i]])
}

answer2



# 练习3 计算出iris 数据集中每列唯一值的数量
df3 <- iris

answer3 <- vector('integer', ncol(df3))

for (i in seq_along(df3)) {
  answer3[[i]] <- n_distinct(df3[[i]])
}

answer3



# 练习4 分别使用μ= -10、0、10和100的正态分布生成10个随机数

answer4 <- list('double', 4)

for (i in seq_along(c(-10, 0, 10, 100))) {
  j <- c(-10, 0, 10, 100)[i]
  answer4[[i]] <- rnorm(10, j, 1)
}

answer4

# 试着能不能作用数据框
library(tidyverse)

answer4.5 <- tibble(row_id = seq_len(10)) #需要提前留出足够长度的tibble
answer4.5 <- tibble(row_id = seq(from = 1, to = 10)) #其实这样也一样

for (i in seq_along(c(-10, 0, 10, 100))) {
  j <- c(-10, 0, 10, 100)[[i]]
  col_name <- paste0('V', i) #给列动态命名，主要还是paste0()函数
  answer4.5[[col_name]] <- rnorm(10, j, 1)
}

answer4.5



# 练习5 使用for循环和prints()打印出儿歌“Alice the Camel”的歌词

lyrics <- c('Alice the Camel has one hump. Alice the Camel has one hump. Alice the Camel has one hump. Go Alice go!', 
            'Ruby the Rabbit has two ears. Ruby the Rabbit has two ears. Ruby the Rabbit has two ears. Go Ruby go!', 
            'Sally the Sloth has three toes. Sally the Sloth has three toes. Sally the Sloth has three toes. Go Sally go!', 
            'Felix the Fox has four legs. Felix the Fox has four legs. Felix the Fox has four legs. Go Felix go!', 
            'Lilly the Ladybug has five spots. Lilly the Ladybug has five spots. Lilly the Ladybug has five spots. Go Lilly go!', 
            'Andy the Ant has six legs. Andy the Ant has six legs. Andy the Ant has six legs. Go Andy go!', 
            'Larry the Lizard has seven stripes. Larry the Lizard has seven stripes. Larry the Lizard has seven stripes. Go Larry go!', 
            'Sammy the Spider has eight legs. Sammy the Spider has eight legs. Sammy the Spider has eight legs. Go Sammy go!')

names <- c('Alice', 'Ruby', 'Sally', 'Felix', 'Lilly', 'Andy', 'Larry', 'Sammy')

for (i in seq_along(names)) {
  print(paste(names[[i]], ': ', lyrics[[i]]))
}



# 练习6 将儿歌“Ten in the Bed”转换成一个函数，将其扩展为任意数量的小朋友和任意种类的寝具

ten_in_the_bed <- function(n, furniture = 'bed') {
  for (i in n:1) {
    if (n > 1) {
      print(paste('There are ', i, ' kid(s) on the ', furniture, ','))
      print(paste('and ', n - i, ' kid(s) on the floor'))
    } else {
      print(paste('There are ', i, ' kid(s) on the ', furniture, '.'))
      print(paste('I get all of the', furniture, ' now! He say'))
    }
  }
}

ten_in_the_bed(10, furniture = 'sofa')



# 练习7 将歌曲“99 Bottles of Beer on the Wall”转换成一个函数，将其扩展为任意数量、任意容器、任意液体和任意表面
beer_99 <- function(n, liquid = 'beer', surface = 'wall') {
  for (i in n:1) {
    if (n > 1) {
      print(paste(i, ' bottles of beer on the wall, ', i, ' bottles of beer.'))
      print(paste('Take one down and pass it around, ', i - 1, ' bottles of beer on the wall.'))
    } else {
      print(paste(i, ' bottle of beer on the wall, ', i, ' bottle of beer.'))
      print(paste('Take one down and pass it around, no more bottles of beer on the wall.'))
    }
  }
}

beer_99(n = 99, liquid = 'beer', surface = 'wall')




# for循环变体
# 修改现有对象，拿之前函数归一化的例子
df <- tibble( 
  a = rnorm(10), 
  b = rnorm(10), 
  c = rnorm(10), 
  d = rnorm(10))

to_be_1 <- function(x) {
  rng <- range(x, na.rm = T, finite = T)
  (x - rng[1])/(rng[2] - rng[1])
}

# 然后就这样在循环中直接修改就行
for (i in seq_along(df)) {
  df[[i]] <- to_be_1(df[[i]])
}




# 循环模型 按名称索引而非我们单独创造，感觉还不如单独创造一个呢
df <- tibble( 
  a = rnorm(10), 
  b = rnorm(10), 
  c = rnorm(10), 
  d = rnorm(10))

ans <- vector('numeric', ncol(df))

for (i in names(df)) {
  ans <- mean(df[[i]], na.rm = T)
}




# 未知输出长度，这个看不懂思密达，好像也没那么常用

# 未知迭代次数，使用while循环
# 比如我们需要知道连续三次硬币正面所需的迭代次数
i = 1
while (i <= 10) {
  i = i + 1
  print(i)
}


# 比如我们需要模拟抛硬币

tribble <- 0
i <- 0

while (tribble < 3) {
  coins <- sample(c('T', 'F'), 1, 0.5)
  if  (coins == 'T') {
    tribble = tribble + 1
  } else {
    tribble = 0
  }
  i = i + 1
  print(paste('运行了', i, '次得到了', tribble, '连'))
}




# 练习  编写一个函数，使其输出一个数据框中所有数值列的均值及名称。
library(stringr)
df <- iris

show_means <- function(x, na.rm = F) {
  for (i in names(x)) {
    if (is.numeric(x[[i]])) {
      
      # 格式化列名并使用 str_pad() 左对齐到 15 个字符
      col_names <- str_pad(i, width = 15, side = "right")
      
      print(paste(col_names, ': ', mean(x[[i]], na.rm = na.rm)))
    } else {
      next
    }
  }
}

show_means(df, na.rm = T)




# for循环与函数式编程
# 将for嵌套于函数中；将一函数作为参数传输另一函数中
# 求一求我们这位老熟人各列的均值中位标准差
library(tidyverse)
df <- tibble( 
  a = rnorm(10), 
  b = rnorm(10), 
  c = rnorm(10), 
  d = rnorm(10))

col_summarise <- function(x, fun, na.rm = F) { #将另一函数作为fun参数塞进去
  
  output <- vector('numeric', ncol(x))
  
  for (i in seq_along(x))
    if (is.numeric(x[[i]])) {
      output[[i]] <- fun(x[[i]], na.rm = na.rm) #刚刚塞进去的fun这里发光发热
    } else {
      print(paste('第', i, '列不是数字哈'))
      next
    }
    output
}

# 完美
col_summarise(df, mean)
col_summarise(df, median)
col_summarise(df, sd)




# purrr的函数簇 本章剩下的主要内容
# map_x()函数簇，使用一个向量作为输入，并对向量的每个元素应用一个函数，
# 然后返回和输入向量同样长度（同样名称）的一个新向量。向量的类型由映射函数的后缀决定。
map_dbl(df, mean, na.rm = T, trim = 0.5)
map_lgl(df, is.numeric)


# 比如我们想要针对不用子表fit一个模型
# 之所以需要匿名函数function(x)是因为map的#2参数只接受1个输入，
# 同时map只会将#1参数的输入机械放进.F的第一个参数，不加匿名函数引导一下，有时它不知道如何去历遍
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df)) %>% 
  map(summary) %>% 
  map_dbl(function(x) x$r.squared) #这个其实也就是map_dbl(~.$r.squared)
  
  
# 等价于，~是对匿名函数的简写语法糖
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .)) %>% 
  map(summary) %>% 
  map_dbl('r.squared') #map函数会自动解读这个字符串为一个属性名称，并尝试从列表的每个元素中提取该属性值

# split() == group_by() %>% nest()
# 其实这个书上说就是应用函数族apply()、lapply()和tapply()




# 使用safely()对操作失败的处理
# 能保证一列错误不耽误我们获取其他非错误操作的结果，这让我感觉不爽，我全想要

safely(log(10)) #这是错误的，safely接受整个函数，这样实际是让它只接受log10这个数

safe_log <- safely(log) 
str(safe_log(10)) 


# safely()和map共同使用
ddd <- map(c(10, 20, 30, 40, 50), safely(log), base = 10)
str(ddd)

# 然后这个结果可以转换成俩单独列表，各自分别呈现成功或失败的结果
ddd <- transpose(ddd)
ddd


# 除了safely，还可以使用possibly和quietly这俩修饰函数，总是返回成功结果
# possibly()可以通过NA_x_参数，规定如果出现错误按缺失值
ddd <- map_dbl(list(1, 2, 3, 4, 'a'), possibly(log, NA_real_), base = 10)
str(ddd)

# quietly不会用思密达
ddd <- map(list('a-b', 'c-d', 'e-f', 'g-h', 3.1415926), quietly(log), base = 10)




# 多参数映射
# 之前我们尝试过给不同均值生成随机分布的函数映射，
# 假如这时候我们还想加入不同标准差的时候，这就是多参数映射了
mean <- c(-10, 0, 10, 100)
sd <- c(1, 3, 5, 7)
df <- list() #一定需要初始化的空白list

for (i in seq_along(mean)) {
  for (j in seq_along(sd)) {
    df[[paste0('mean-', mean[[i]], ' sd-', sd[[j]])]] <- rnorm(n = 10, mean = mean[[i]], sd = sd[[j]])
  }
}

str(df)

# 这时候如果仅仅还是map就有点麻烦，所以就需要map2或者pmap
# 不过结果我觉得不如嵌套循环全面，这个还是比较机械的，俩变化变量长度得一样
map2(mean, sd, rnorm, n = 10)


# 还有一个pmap，将列表作为参数，一次性可以弄几个动态变量，仍然要求各个长度相等。不过这个允许使用tibble
N = c(10, 20, 30)
mean = c(-10, 0, 10)
sd = c(1, 3, 5)
arg1 <- list(N, mean, sd)

pmap(arg1, rnorm)




# 调用不同函数，有时即使是需调用的函数也是动态变量，感觉意义有限，不，后来看了19章，还是有用的
func <- list(rnorm, sample) #这里得是函数列表，下面是参数列表

arg2 <- list(parameters_rnorm = list(n = 10, mean = 100, sd = 15), #参数列表需要汇总一下
             parameters_sample = list(x = c(0, 1, 2, 3, 4), size = 3, prob = rep(0.2, 5))) 

invoke_map(func, arg2)




# 游走函数 使得调用函数更丝滑搭配管道符
# 其实我不懂思密达，为什么不直接切面生成好图像后ggsave呢
library(ggplot2) 

plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point()) 

paths <- stringr::str_c(names(plots), ".pdf") 

pwalk(list(paths, plots), ggsave, path = getwd())




# for的其他形式
# 预测函数discard()、keep()，可以用来搭配返回逻辑值的函数快速筛选
keep(iris, is.factor)
discard(iris, is.factor)

# some()、every()分别用来确定预测值是否对某个元素为真以及是否对所有元素为真
x <- list(1:5, letters, list(10))

some(x, is.character)
every(x, is.vector)

# 突然想起来了这俩也算差不多功能
any(is.na(x))
all(is.vector(x))

# detect()可以找出预测值为真的第一个元素，detect_index()则可以返回该元素的位置
detect(c(1, 2, 3, NA, 5), is.na)
detect_index(c(1, 2, 3, NA, 5), is.na)




# 归约与累计reduce()和accumulate()。看不懂感觉也用不上思密达











