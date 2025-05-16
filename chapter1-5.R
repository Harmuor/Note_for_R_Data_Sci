library(nycflights13)
library(dplyr)
library(ggplot2)
library(lvplot)
library(hexbin)
library(modelr)

data1 <- nycflights13::flights %>%
  dplyr::select(month, year, day, dep_delay, arr_delay, dep_time, arr_time) %>%
  dplyr::arrange(year, month, day, dep_delay, dep_time, arr_delay, arr_time) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(count = n(), 
                   delay = mean(dep_delay, na.rm = TRUE), 
                   time = mean(dep_time, na.rm = TRUE))

(data1)


# count()函数用于只计数求和的情况，不加权重wt参数就是算频率，加上对应wt求和
count(data1, month)
count(data1, month, wt = arr_delay)


# sum()和mean()配合逻辑语句可作为快捷条件计数
a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
sum(a >= 5) #条件计数
mean(a < 7) #条件算比率


sum(is.na(data1$dep_time)) #检查缺失值
n_distinct(data1$month) #检测唯一值，就是变量水平的种类数


a <- rnorm(20, 4, 2)
b <- rnorm(20, 3, 1)
data2 <- tibble(a, b) %>% 
  rename(, ggg = a, hhh = b)

ggplot(data2)+
  geom_histogram(mapping = aes(x = ggg), binwidth = 0.5, color = "black", fill = "#007FFF")

ggplot(data1)+
  geom_histogram(mapping = aes(x = delay), binwidth = 0.5, color = "black", fill = "#007FFF")


#多重分组，说是可以循序渐进，但是为什么不用后面我写的那一段
data2 <- nycflights13::flights
daily <- group_by(data2, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day,  flights  = sum(flights)))
(per_year <- summarise(per_month, flights  = sum(flights)))

(monthly <- group_by(data2, year, month) %>% 
   summarise(flights = n()))

# 顺带一提，取消分组的函数
ungroup(daily)




# 分组新变量和筛选
# 原来那个航班的数据集不太符合我的认知习惯
# 这里我选择自己去创造一个数据集
data1 <- dplyr::tibble(name = c('Bob', 'Mary', 'Mike', 'Cheng', 'Himihola', 'Sara', 'Dive', 'Gray', 'Jim', 'Eric'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('male', 'female', 'male', 'female', 'female', 'female', 'male', 'male', 'male', 'male'), 
                       class = c(1, 1, 1, 2, 1, 2, 2, 1, 2, 2), 
                       region = c('west', 'west', 'west', 'east', 'east', 'west', 'west', 'east', 'east', 'east'), 
                       Chinese = c(78, 76, 64, 94, 95, 79, 82, 90, 89, 93), 
                       Math = c(45, 52, 60, 88, 86, 81, 76, 76, 69, 80), 
                       English = c(77, 80, 90, 91, 95, 90, 85, 86, 82, 81)
                       )

data2 <- dplyr::tibble(name = c('Mus', 'Iury', 'Eds', 'Chen', 'Yuli', 'Eason', 'Jenney', 'KK', 'Author', 'Wily'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('female', 'female', 'female', 'male', 'female', 'male', 'female', 'male', 'female', 'male'), 
                       class = c(1, 1, 2, 2, 2, 2, 1, 2, 1, 2), 
                       region = c('west', 'east', 'west', 'east', 'west', 'west', 'east', 'east', 'west', 'east'), 
                       Chinese = c(95, 79, 82, 90, 89, 93, 78, 76, 64, 94), 
                       Math = c(45, 81, 52, 60, 88, 86, 76, 76, 69, 80), 
                       English = c(90, 91, 86, 82, 81, 95, 77, 80, 90, 85)
                       )

#练习一下merge数据
data3 <- dplyr::bind_rows(data1, data2) %>% 
  mutate(gender = factor(gender, # 这个函数可调整分类变量的排序
                         levels = c('male', 'female'))) %>% 
  mutate(class = factor(class, levels = c(1, 2)))


data_su <- data3 %>%  
  group_by(class, gender) %>% 
  summarise(count = n(), 
            Chinese = mean(Chinese), 
            Math = mean(Math), 
            English = mean(English))
(data_su)

# group_by结合filter查出每个分组排名最差的成员
# note：rank()函数用来获取排名数字
group_by(data1, class) %>% 
  filter(rank(desc(Math)) > 3)

# group_by结合filter查出达到某频数的所有组
group_by(data1, age) %>% 
  filter(n() > 2)


# 对新造的数据进行个可视化（分类型）
ggplot(data = data_su) + 
  geom_bar(mapping = aes(x = class, y = Chinese, fill = gender), 
           stat = 'identity', 
           position = 'dodge') + #这个参数让男女柱子位置调整
  scale_fill_manual(values = c('male' = '#007FFF', 'female' = 'pink')) # 这个给分类的变量自定义颜色


# 做个可视化
ggplot(data = data3) + 
  geom_jitter(mapping = aes(x = Math, y = Chinese, size = class, color = gender)) + 
  scale_size_manual(values = c('1' = 3, '2' = 5)) + # 这个给分类的散点定义大小
  scale_color_manual(values = c('male' = 'blue', 'female' = 'pink'))




# 看了一下书后面的内容，看来确实需要去熟悉一下数据集了md
# 一个离散变量和一个连续变量的可视化
ggplot(data = mpg)+
  geom_boxplot(mapping = aes(x = class, y = hwy))

# 那么，可以用reorder函数对x轴排序，看起来整齐一些
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = mean), #按hwy的mean对class排序
                             y = hwy)) +
  coord_flip() #把轴旋转90°

# 据书上说，lvplot包较于基础ggplot2会更适合对大数据进行箱图绘制现在试试
library(lvplot)
ggplot(data = diamonds) +
  geom_lv(mapping = aes(x = reorder(cut, price, FUN = mean),
                        y = price))


# 对两个离散变量做可视化
# 首先可以试试geom_count
ggplot(data = diamonds, mapping = aes(x = cut, y = color)) +
  geom_count()

# 然后还有类似热图的方式，不过需要先结合count()函数
data1 <- count(mpg, class, drv)
ggplot(data = data1, mapping = aes(x = class, y = drv)) +
  geom_tile(mapping = aes(fill = n))


# 接着是对两个连续变量进行可视化
ggplot(data = diamonds) +
  geom_jitter(mapping = aes(x = carat, y = price), alpha = 1/100)

# 当数据量较大时，书上说可以使用geom_bin2d和geom_hex对数据进行分箱
# 其中，geom_hex需要安装hexbin包
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bin2d()

# 还有就是直接用多个箱型图，注意是俩连续变量哈
# 这个就是依赖geom_plot的映射美学中假如group = cut_width()参数来实现
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = carat, 
                        y = price, 
                        group = cut_width(carat, 1, varwidth = F))) #使用varwidth参数调整箱型宽度是否和包含的n数量成正比

# 还有个方法是group参数使用cut_number()函数
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = carat, 
                             y = price, 
                             group = cut_number(carat, 5, varwidth = T)))

# 练习：对cut、carat还有price仨变量进行可视化
ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, 
                         y = price)) +
  facet_wrap(~ cut)

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = x, y = y)) + 
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11)) #Cartesian：笛卡尔




# 模式和模型
# 模式是：多个变量之间系统性的关系
# 模型是：从数据中抽取模式的认知方式or工具
# 这里仿着书上用老忠诚温泉的数据做几个图
ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, 
                           y = waiting))

ggplot(data = faithful) +
  geom_bin2d(mapping = aes(x = eruptions, 
                           y = waiting))

ggplot(data = faithful) +
  geom_hex(mapping = aes(x = eruptions, 
                         y = waiting))


# lm函数能够实现建模，不过是后面第四章内容
mod <- lm(price ~ carat, data = diamonds) # 建立模型，克拉预测价格
summary(mod) # 查看报告

diamonds_with_residuals <- diamonds %>% 
  add_residuals(model = mod) #把每个点的残差加进来，等于下面这行
diamonds_with_residuals <- add_residuals(data = diamonds, model = mod)

ggplot(diamonds_with_residuals) +
  geom_boxplot(aes(cut, resid)) #这里用克拉~价格的残差和cut做分析就是相当去去除克拉的强相关后单看cut和price的关系

