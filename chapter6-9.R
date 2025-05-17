# 第6章略

# 第7章 tibble
# 这一章需要聚焦tidyverse包的核心包--tibble包
library(tidyverse)

# 可以使用函数将r数据框转换为tibble数据框
(iris) #假如要转化这个数据框
as_tibble(iris)
as.tibble(iris)


# 在之前的脚本里，已经用tibble()函数构建tibble框这里再规范一下
# 最好养成习惯，列名用``反引号括起来
data1 <- tibble(`column_1` = seq(from = 1, 
                                 to = 10, 
                                 by = 1), 
                `column_2` = rnorm(n= 10, 
                                   mean = 100, 
                                   sd = 10))


# 除此之外，tibble是按列构建表格，如果我们有按行构建表格的需求时
# 需要使用tribble()函数，这个需求其实蛮常见的
data4 <- tribble(
  ~col_1, ~col_2, ~col_3, 
  1, 2, 3, 
  4, 5, 6, 
  7, 8, 9
)

# 同时，tibble的打印可以更为灵活地控制来着？
print(data1, 
      n = 2, #这个参数控制打印几行 
      width = Inf) #这个参数如果=Inf，就打印所有的列




# 第8章 读取数据
# 这一章需要聚焦tidyverse包的核心包--readr包
library(readr) #其实这里直接导入tidyverse也行

# 对于csv文件，分隔符号有三种，第一种用英文逗号；第二种用英文分号；第三中用Tab键
# 每种分隔都有对应的函数来导入数据:

readr::read_csv('./saved_data/diamonds.csv') #读取逗号分隔文件
readr::read_csv2('./saved_data/diamonds1.csv') #读取分号分隔文件
readr::read_tsv('./saved_data/diamonds2.csv') #读取制表符分隔文件
readr::read_delim('./saved_data/diamonds.csv') #读取使用任意分隔符的文件


# 然而，有些情况下，为了优化储存结构，一些数据文件是没有分隔符的。
# 这样的文件往往是“固定宽度文件”，每个列占用一定的字符。
# 下面示范如何处理该类型文件，假如有这样的一个文件：

# 员工ID字段占据前5个字符。
# 姓名字段占据接下来的10个字符。
# 年龄字段占据接下来的3个字符。

widths <- c(5, 10, 3) #定义每个列都多宽：第一列5；第二列10；第三列3

readr::read_fwf('./saved_data/fwfdata.csv', 
                fwf_widths(widths)) # 这里就告诉该函数刚刚的宽度定义

dat <- readr::read_fwf('./saved_data/fwfdata.csv', fwf_widths(widths)) %>% 
  transmute(id = X1, names = X2, ages = X3)

# 还有的read_log()感觉用得少，先不看了就


# 在以上情况中，默认将第一行作为列名。可以用skip参数跳过n行；或者comment参数跳过以某字符为开头的行
readr::read_csv('first line of metadata
                #the second line also is metadata
                3, 2, 1
                3, 4, 4
                3, 3, 2
                7, ., .', 
                skip = 1, #跳过第一行
                comment = '#', # 忽略以“#”为开头的行
                col_names = c('col_1', 'col_2', 'col_3'), #不然就按录入的第一行作为列名了或者这个参数直接=False也行
                na = '.' #这个参数可以定义在原文件中的哪个字符录入为缺失值NA
                )

# 解析向量和解析文件那一节看不懂思密达

# 写入文件，这里用diamonds做演示
# na参数定义缺失值如何写入；append参数可以把数据添加到已有文件
# 可以先创造和路径
dir.create('./test')
write_csv(diamonds, './test/SaveDiamonds.csv')


# haven包管SPSS、SAS、Stata的数据
# readxl包管Excel文件




# 第9章 使用dplyr处理关系数据
# 这一章需要再回去高清nycflights13包里的那几个数据集了md
library(dplyr)
library(ggplot2)
library(nycflights13)

# 找到每年乘坐飞机少的日子
library(lubridate)

ggplot(flights) +
  geom_histogram(aes(x = flight), binwidth = 50) +
  scale_x_continuous(breaks = seq(0, 6400, by = 200)) +
  scale_y_continuous(breaks = seq(0, 10000, by = 500)) +
  coord_cartesian(xlim = c(0, 6400))

df <- flights %>% 
  select(month, day, flight) %>% 
  mutate(date = paste(month, day, sep = '-')) %>% 
  filter(flight < 2)

ggplot(df) +
  geom_histogram(aes(date), stat = 'count') +
  coord_flip()

# 验证主键的方法，找唯一，结果为T的就是了
is_key <- n_distinct(weather$year) == nrow(weather)

# 或者n要是不大于一就是，要是真打印出个表反而就不是
weather %>% 
  count(year, month, day) %>% 
  filter(n > 1)


# 其实这一节我觉得对我之后的需求而言，就是学会了数据框的链接
# 这种链接方式某种意义上比bind_cols好用，可以让程序按照我要求匹配每一行
# 这里编出俩数据

data1 <- dplyr::tibble(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12), #注意id差异，这里没有11
                       class = c(2, 1, 1, 1, 2, 1, 2, 2, 1, 2, 2),
                       name = c('Cheng', 'Bob', 'Mary', 'Mike', 'Chen', 'Himihola', 'Sara', 'Dive', 'Gray', 'Jim', 'Eric'), 
                       gender = c('male', 'female', 'male', 'female', 'female', 'female', 'male', 'male', 'male', 'male', 'female'))

data2 <- dplyr::tibble(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), #注意id差异，这里没有12
                       student_number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                       class = c(2, 1, 1, 1, 2, 1, 2, 2, 1, 2, 2), 
                       region = c('east', 'west', 'west', 'west', 'east', 'east', 'west', 'west', 'east', 'east', 'east'), 
                       Chinese = c(89, 78, 76, 64, 94, 95, 79, 82, 90, 89, 93), 
                       Math = c(98, 45, 52, 60, 88, 86, 81, 76, 76, 69, 80))

# 然后利用今天学的，链接俩表格
# 这里其实可以看出，连接键key就是id，但是俩表的id其实对不上，这里不同的链接函数的效果会有差异

data_total <- left_join(data1, data2, by = 'id') #这个保留了左边的编号12，但是右边要链接的内容为空
data_total <- right_join(data1, data2, by = 'id') #这个保留了右边的编号11，但是左边要链接的内容为空
data_total <- inner_join(data1, data2, by = 'id') #这个是直接取交集
data_total <- full_join(data1, data2, by = 'id') #这个直接全部保留


# 再刚刚的练习中，我们使用by参数来控制指定用哪个键来链接俩表，其实这个参数可调
# by参数可以直接打，声明可以省略
data_total <- left_join(data1, data2) #这个叫“自然链接”，程序自己找键，但是有点不放心
data_total <- left_join(data1, data2, by = c('id', 'class')) #这样可以指定多个键
data_total <- left_join(data1, data2, c('id' = 'student_number')) #假如同一个变量在俩表里是不同名字可以这样来指定


# 筛选链接
# 在刚刚的函数，它们会尽力将俩表的信息都合并到一起
# 但是现在要介绍的函数重点将不再是合并，而是筛选

data_filtered <- semi_join(data1, data2, by = 'id') #不做合并，找出左表中的交集内容并保存之
data_filtered <- anti_join(data1, data2, by = 'id') #找出左表中的例外，余下的交集丢弃之


# 集合操作
# 书上述这个用得少，但不代表没用
# 集合操作需要x和y具有相同的变量，针对观测值操作
# 先编俩数据
df1 <- tribble(
  ~x, ~y, 
  1, 1, 
  2, 2)

df2 <- tribble(
  ~x, ~y, 
  1, 1, 
  3, 3)


intersect(df1, df2) # 返回俩表中的共有观测个案

union(df1, df2) #返回俩表中所有种类的观测个案，可以用来合并+筛重复值

setdiff(df1, df2) #返回在df1中，但不在df2中的观测个案

setdiff(df2, df1) #返回在df2中，但不在df1中的观测个案


