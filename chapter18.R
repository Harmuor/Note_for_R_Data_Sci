# 第18章 模型建构
library(ggplot2)
library(tidyverse)
library(nycflights13)
library(lubridate)
library(modelr)

options(na.action = na.warn)


# 一些必要的数据集不如先了解一下
?diamonds
?flights




# 为什么质量差的钻石更贵
# 在之前的章节中，已经通过可视化发现质量越差（cut、color、clarity）的钻石反而价格高（price）
# 这一节就通过该问题学习建模


# 价格与重量
df <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lcarat = log10(carat), 
         lprice = log10(price)) #取对数可以进行线性转换

# 画个图肉眼发现一定的粗糙模式
ggplot(df) + 
  geom_hex(aes(lcarat, lprice), bins = 50)

# 针对这一粗糙模型，可以使用正式建模精细之
mod_pred <- lm(lprice ~ lcarat, data = df)

# 同样的，来给个预测素材
pred <- df %>% 
  add_predictions(mod_pred, 'pre_lprice') %>% 
  add_residuals(mod_pred, 'pre_lresiduals') %>% 
  mutate(pre_price = 10^pre_lprice, 
         pre_residuals = 10^pre_lresiduals)

# 然后画个图
ggplot() + 
  geom_hex(data = pred, mapping = aes(carat, price), bins = 50) + 
  geom_line(data = pred, mapping = aes(carat, pre_price), linewidth = 1.5, color = 'red')

# 同样可以检查残差以检查我们是否使用模型将数据中的模式给移除了（好像还残留了一点）
# 而这个残留的，在残差里的模式，就是我们移除carat因素后，其他因素（如品质）对价格的影响了
ggplot() + 
  geom_hex(data = pred, aes(carat, pre_residuals), bins = 50)

# 这时候我们再使用残差数据看品质对价格的影响
# 这时候品质越好价格越贵的模式开始显现出来了
ggplot(data = pred) + geom_boxplot(aes(cut, pre_residuals))
ggplot(data = pred) + geom_boxplot(aes(clarity, pre_residuals))
ggplot(data = pred) + geom_boxplot(aes(color, pre_residuals))


# 更复杂的模型
mod_complicated <- lm(lprice ~ lcarat + cut + clarity + color, data = df)

summary(mod_complicated)

# 仍然需要生成数据网格以获取预测值，.model参数可以保证生成的网格适配对应的模型
grid <- df %>% 
  data_grid(cut, .model = mod_complicated) %>% 
  add_predictions(model = mod_complicated)

# 然后就可以做绘图了
ggplot(grid) + 
  geom_point(aes(cut, pred))

# 对残差的解读同样是非常重要的
df <- df %>% 
  add_residuals(model = mod_complicated)

ggplot(df) + 
  geom_hex(aes(lcarat, resid), bins = 50)
# 在残差中，过大或过小的数值有可能是因为模型局限或者数据本身有错误造成。如果是后者，那么我们或可以捡漏




# 哪些因素影响每日航班数量
# 首先准备以下需要的数据，把flights数据集给简化以下，然后也可以可视化
df <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

ggplot(df) + 
  geom_freqpoly(aes(date, n), stat = 'identity')


# 一周中的每一天
# 刚刚的可视化中，因为周内效应，很难从总体的图中把握模式，这里进行一定处理
df <- df %>% 
  mutate(weekday = wday(date, label = T))

ggplot(df) + 
  geom_boxplot(aes(weekday, n)) #这里能看出一些明显模式了


# 为了进一步提取这个模式，建模
# 这里顺道说一嘴，分类变量适合哑变量建模；有序变量适合多项式编码建模
# 这里演示以下哑变量建模的过程，多项式编码是lm()的默认

df$weekday <- df$weekday %>% 
  factor(ordered = FALSE) %>%  #首先将数据转为无序
  relevel(ref = "周一") #设置周一为基准组，这样得出的就是相比于周一其他水平对应的y值如何如何

mod <- lm(n ~ weekday, data = df) #然后就是正常的建模了

grid <- df %>% 
  data_grid(weekday, .model = mod) %>% 
  add_predictions(mod)


for_plot <- df %>% 
  group_by(weekday) %>% 
  summarise(n = mean(n))

ggplot() + 
  geom_bar(data = for_plot, aes(weekday, n), stat = 'identity') + 
  geom_point(data = grid, aes(weekday, pred))


# 然后再对比残差，也就是去掉星期因素的数据模样
# 虽然画出来的图我啥也没看出来，但是书上说能看出模式（我：？？？）
# 可能是因为这个分布不够正态且随机吧
df <- df %>% 
  add_predictions(mod) %>% 
  add_residuals(mod)

ggplot(df) + 
  geom_point(aes(date, resid))

# 细细究来
# 发现。。。好勾八乱，然后书上说周六的有猫腻（季节性）
# 果然实验室数据实在太细糠了
ggplot(df) + 
  geom_line(aes(date, resid, color = weekday))


# 星期六的季节性变化
# 其实这并不是我所感兴趣的，但是书中代码实现方式仍是非常值得学习的：一年内每个周六航班数量的折线图
df <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  mutate(weekday = wday(date, label = T)) %>% 
  filter(weekday == '周六') %>% 
  group_by(date) %>% 
  summarise(n = n())
  
# 发现秋天周六的航班少、夏天周六的航班多
ggplot(df) + 
  geom_point(aes(date, n)) + 
  geom_line(aes(date, n)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b')


# 然后书上根据上面那个图开始做探究为什么周六航班数夏天多秋天少
# 然后说什么可能因为春夏假期少、秋天假期多，但后开始验证这个假设了
# 首先模拟出一年中的仨学期
df$term <- cut(df$date, 
               breaks = ymd(20130101, 20130605, 20130825, 20140101), 
               labels = c('spring', 'summer', 'autmn'))

ggplot(df) + 
  geom_point(aes(date, n, color = term)) + 
  geom_line(aes(date, n, color = term)) + 
  scale_x_date(date_breaks = '1 month', date_labels = '%b')

# 然后还看了这个季节变量对一周中其他天的影响
df <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  mutate(weekday = wday(date, label = T), 
         term = cut(date, 
                    breaks = ymd(20130101, 20130605, 20130825, 20140101), 
                    labels = c('spring', 'summer', 'autmn')))

ggplot(df) + 
  geom_boxplot(aes(y = n, color = term)) + 
  facet_wrap(~ weekday)
  
# 就这才刚刚完成初步的可视化，还tm没建模呢
# 那么通过刚刚，差不多能明确俩因素：季节、星期几
# 考虑这俩都是无序分类变量，我程柏然要用哑变量做
dummy <- function(x) {
  x <- x %>% 
    factor(ordered = F) %>% 
    relevel(ref = levels(x)[1])
  return(is.ordered(x))
}

dummy(df$weekday)
dummy(df$term)

# 后面就正常建模呗
mod <- lm(n ~ weekday * term, data = df)
summary(mod)

# 显示模型的可视化
grid <- df %>% 
  data_grid(weekday, term, .model = mod) %>% 
  add_predictions(mod)

ggplot() + 
  geom_boxplot(data = df, aes(weekday, n)) + 
  geom_point(data = grid, aes(weekday, pred), color = 'blue', size = 3, alpha = 0.25) + 
  facet_wrap(~ term)

# 再然后是残差的可视化
df <- df %>% 
  add_residuals(mod)

# 看来效果还是一般般？
ggplot(df) + 
  geom_point(aes(date, resid)) + 
  geom_line(aes(date, resid))


# 计算出的变量
# 这一节就是说用函数来提高效率和工作的准确率


# 年度时间：另一种方法
# 这一节就是把自然条样模型ns()那个应用到刚刚n ~ weekday * term里
library(splines)

df <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  mutate(weekday = wday(date, label = T), 
         term = cut(date, 
                    breaks = ymd(20130101, 20130605, 20130825, 20140101), 
                    labels = c('spring', 'summer', 'autmn')))

dummy(df$weekday)

mod1 <- lm(n ~ weekday * ns(date, 5), data = df)

# 别忘了连续变量的网格生成用seq_range()
grid <- df %>% 
  data_grid(weekday, date = seq_range(date, 13), .model = mod1) %>% 
  add_predictions(mod1)

ggplot() + 
  geom_point(data = grid, aes(date, pred, color = weekday)) + 
  geom_line(data = grid, aes(date, pred, color = weekday)) + 
  scale_x_date(date_breaks = '1 month', date_labels = '%b') + 
  scale_color_manual(values = c('周一' = 'red', 
                                '周二' = 'green', 
                                '周三' = 'black', 
                                '周四' = 'brown', 
                                '周五' = 'blue', 
                                '周六' = 'purple', 
                                '周日' = 'orange'))

# 还是残差，感觉还是差点意思
df <- df %>% 
  add_residuals(mod1)

ggplot(df) + 
  geom_point(aes(date, resid)) + 
  scale_x_date(date_breaks = '1 month', date_labels = '%b') + 
  facet_wrap(~ weekday)
