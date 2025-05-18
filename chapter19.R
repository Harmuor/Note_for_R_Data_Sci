# 第19章 使用purrr和broom处理多个模型
# 这一章就是使用列表列来进行多个模型的处理和对比，结合broom包
library(modelr)
library(tidyverse)
library(broom)




# 列表列
# 列表列有点像数据框领域的压缩包，封装数据信息的
# 之前我们了解过list可以嵌套list，这个其实相当单个列表列。列表列是指在tibble里的某一列，该列的单元格内容是list（元素）
a <- c('jisdf', 'sdf', 'dd')
b <- c(1, 2, 3, 4, 3)
c <- c(2.2, 3.2, 44.3, 32.2, 66.3)

list1 <- list(a, b, c)
list2 <- list(b, a)
list3 <- list(list1, list2)

# tibble和tribble都可以创建列表列
tibble(n = seq_along(list3), 
       `这是列表列` = list3)

tribble(
  ~n, ~这是列表列, 
  1, list1, 
  2, list2, 
  3, list3)


# 模型不就是以列表的形式存在么，如果是列表列，每个单元格就是一个模型，更方便达到对比不同模型的目的（model列）
# 创建列表列（含模型的） —> 按需转换列表列 -> 将列表列还原为数据框或原子向量（提取数据结果）




# 创建列表列：（1）tidyr::nest将分组数据框转换为嵌套数据框；（2）mutate；（2）summarise
# tidyr::nest()，看书没动，不如实际操作，先准备个数据顺便复习以下合并
data1 <- dplyr::tibble(name = c('Bob', 'Mary', 'Mike', 'Cheng', 'Himihola', 'Sara', 'Dive', 'Gray', 'Jim', 'Eric'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('male', 'female', 'male', 'female', 'female', 'female', 'male', 'male', 'male', 'male'), 
                       class = c(1, 1, 1, 2, 1, 2, 2, 1, 2, 2), 
                       region = c('west', 'west', 'west', 'east', 'east', 'west', 'west', 'east', 'east', 'east'), 
                       Chinese = c(78, 76, 64, 94, 95, 79, 82, 90, 89, 93), 
                       Math = c(45, 52, 60, 88, 86, 81, 76, 76, 69, 80), 
                       English = c(77, 80, 90, 91, 95, 90, 85, 86, 82, 81))

data2 <- dplyr::tibble(name = c('Mus', 'Iury', 'Eds', 'Chen', 'Yuli', 'Eason', 'Jenney', 'KK', 'Author', 'Wily'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('female', 'female', 'female', 'male', 'female', 'male', 'female', 'male', 'female', 'male'), 
                       class = c(1, 1, 2, 2, 2, 2, 1, 2, 1, 2), 
                       region = c('west', 'east', 'west', 'east', 'west', 'west', 'east', 'east', 'west', 'east'), 
                       Chinese = c(95, 79, 82, 90, 89, 93, 78, 76, 64, 94), 
                       Math = c(45, 81, 52, 60, 88, 86, 76, 76, 69, 80), 
                       English = c(90, 91, 86, 82, 81, 95, 77, 80, 90, 85))

df <- bind_rows(data1, data2)

df_nested <- df %>% 
  group_by(class) %>% 
  nest()

# split == group_by %>% nest
df_split <- df %>% 
  split(.$class)


# mutate()，书上管这叫使用向量化函数
df <- tibble(x1 = c('a, b, c', 'x, y, z'))
df_mu <- df %>% 
  mutate(x2 = str_split(x1, pattern = ','))

# 用unnest可以还原列表列，话说有啥用
unnest(df_mu, cols = x2)


# 当然之前整的map、lapply其实都可以算是创建列表列的玩意，可以见chapter13-16的第681-697行
func <- c('rnorm', 'sample') #这里是函数，下面是得是参数列表

arg2 <- list(parameters_rnorm = list(n = 10, mean = 100, sd = 15), #参数列表需要汇总一下
             parameters_sample = list(x = c(0, 1, 2, 3, 4), size = 3, prob = rep(0.2, 5)))

dd <- tibble(func, arg2)

dd <- dd %>% 
  mutate(result = invoke_map(func, arg2))


# 使用多值摘要
# summarise()只能用于返回单个数值的函数，但如果返回多个数值的函数比如quantile、runif，就得将结果包装成list再之后摘要
four <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(`1/4` = list(quantile(mpg))) #求四分位数

# 直接使用unnest会发现结果有点乱
unnest(four)

# 所以得提前准备好一个向量来标识出每个数值的所在四分位点
prob <- c(0.01, 0.25, 0.50, 0.75, 0.99)
new_four <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(prob), q = list(quantile(mpg, prob)))

unnest(new_four, cols = c(q, p))


# 使用命名列表
# 命名列表好像就是新增一个对列表列的命名列，这样方便对列表列进行迭代操作什么的
# 我们用那个班级成绩单来演示
df <- bind_rows(data1, data2) %>% 
  split(.$class) %>% 
  enframe(name = 'class', value = 'report') #使用tibble::enframe来进行列表列命名

# 俩列表同时迭代得用map2，不是么
df <- df %>% 
  mutate(sry = map2(class, report, summary))

(df$sry)




# 简化列表列
# 在实际的数据分析和可视化工作中，列表列的数据得做一个降级简化才能直接使用（让程序access）
# 1 列表转换为向量
df <- tribble(
  ~x, 
  letters[1:5], 
  1:3, 
  runif(5)
)

# 可以使用mutate，可是浅薄的我还不知道具体有啥用
df <- df %>% 
  mutate(type = map(x, typeof), 
         length = map(x, length))


# 嵌套还原
# 之前的unnest好像就是，不能同时还原俩每行包含元素数量不一样的列表列哈
df <- tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(cols = y)

df <- tribble( 
  ~x, ~y, ~z, 
  1, "a", 1:2, 
  2, c("b", "c"), 3) %>% unnest(y, z) #像这个yz每行元素数量不一样，不能很完美还原




# 使用broom生成整洁数据
# 这一节挺少的篇幅（山东の倒装），假设我们有几个模型
df <- sim4
df_grid <- data_grid(df, 
                     x1 = seq_range(x = x1, n = 5), 
                     x2 = seq_range(x = x2, n = 5))

mod1 <- lm(y ~ x1 + x2, data = df)
mod2 <- lm(y ~ x1 * x2, data = df)


# 而根据书上，broom包里有仨函数会比较中用
# 1 broom::glance(model)获取模型的重要摘要

glance(mod1)

# 那么可以具体这样应用，感觉会工整一些这数据
models <- list(mod1, mod2)
mod_info <- tibble(mod = c('mod1', 'mod2'), 
                   info = map(models, glance))


# 2 broom::tidy(model)返回模型系数摘要
tidy(mod1)

models <- list(mod1, mod2)
coef <- tibble(mod = c('mod1', 'mod2'), 
               coeffect = map(models, tidy))


# 3 broom::augment(model, data)能够返回数据中每个观测值对模型的贡献什么的
augment(mod1, data = df)

models <- list(mod1, mod2)
coef <- tibble(mod = c('mod1', 'mod2'), 
               database = map(models, augment, data = df))
