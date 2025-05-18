# 第17章 使用modelr包处理模型
library(tidyverse)
library(ggplot2)
library(modelr)
options(na.action = na.warn) #更改全局设置：对na的处理为执行但警告



# 练习1 简单对数据集做个模型

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

mod1 <- lm(y ~ x, data = sim1a)
coef1 <- coef(mod1)

ggplot(sim1a, aes(x, y)) + 
  geom_point() + 
  geom_abline(slope = coef1[2], intercept = coef1[1])




# 模型可视化
# 模型预测
# 使用data_grid创建“不存在”的预测项目（毕竟仅仅针对观测项目的预测只能叫做“训练”）

# 假如我们有个数据集是关于房价的
df <- tribble(
  ~region, ~size, ~price, 
  'urban', 50, 100, 
  'suburban', 70, 80, 
  'rural', 120, 70)

# 可是当我想要进行预测操作的时候，肯定是想预测现有组合之外的房子
# data_grid就可以将指定变量进行排列组合，形成新的“观测”
df_grid <- data_grid(df, price, size)

# 使用add_predict()将预测数值作为新列添加到数据框里
df_model <- lm(price ~ size, data = df)
df_pre <- add_predictions(data = df, model = df_model)


# 最后来进行可视化，这里的训练>测试>可视化的流程根据书说适用于所有的模型（相比于ggplot_abline
ggplot(data = df_pre, mapping = aes(x = size, y = price)) + 
  geom_point() + 
  geom_line(mapping = aes(y = pred), data = df_pre, color = 'red')


# 残差
# 可以使用add_residuals()给加一列残差，不过需要注意，填入的数据需要y值才能计算残差
# 当然也可以给残差绘图

mod <- function(data){
  lm(price ~ size, data = data)
}

df_pre_resid <- df %>% 
  mod() %>% 
  add_predictions(data = df, model = .) %>% 
  add_residuals(data = ., model = mod(.))

# 绘制频率曲线，残差和总为0
ggplot(data = df_pre_resid, mapping = aes(x = resid)) + 
  geom_freqpoly(stat = 'count')

# 配和pred_x绘制散点图
ggplot(df_pre_resid, aes(size, resid)) + 
  geom_ref_line(h = 0) +  #绘制y = 0的参考线
  geom_point()


# 练习1 使用loess()试试拟合，跟geom_smooth比比
df <- mtcars

df_pre_resid <- df %>% 
  loess(disp ~ mpg, data = .) %>% 
  add_predictions(data = df, model = .) %>% 
  add_residuals(data = ., model = loess(disp ~ mpg, data = .))

ggplot(data = df_pre_resid, aes(x = mpg, y = disp)) + 
  geom_point() + 
  geom_line(mapping = aes(y = pred), color = 'red', linewidth = 3) + 
  geom_smooth()


# 练习2 比较add_predictions、gather_predictions和spread_predictions的不同
df <- mtcars

mod1 <- lm(disp ~ mpg, data = df)
mod2 <- loess(disp ~ mpg, data = df)

df_1 <- add_predictions(df, mod)  #这个单独加一列预测值
df_2 <- gather_predictions(df, mod1, mod2)  #可以比较俩模型的预测值，最终输出长格式数据
df_3 <- spread_predictions(df, mod1, mod2)  #可以比较俩模型，最终输出段格式数据




# 公式与模型族
# 首先通过model_matrix讲了讲公式

# 这里的所谓公式感觉是指变量关系在r中的一种呈现形式，如y ~ x
# 而model_matrix则是将公式转换为r函数能够理解的[运算形式]

df <- tribble(
  ~y, ~x1, ~x2, 
  3, 4, 15, 
  5, 7, 11, 
  7, 13, 9, 
  8, 14, 8, 
  10, 17, 5)


# 那么假如我们想要构建公式 y ~ x1 + x2，这个公式在模型中首先被转换为下面的矩阵
model_matrix(df, y ~ x1 + x2)


# 分类变量
# 如果上一个例子感觉那个矩阵除了加一行截距没卵用，这里如果有分类变量可能会看出来点作用
df <- tribble(
  ~y, ~x1, ~x2, ~gender, ~class, 
  3, 4, 15, 'male', '1', 
  5, 7, 11, 'male', '2', 
  7, 13, 9, 'female', '3', 
  8, 14, 8, 'female', '3',
  10, 17, 5, 'male', '1')

# 可以看出它为分类变量性别班级创建了多个哑变量（dummy variable）做0-1编码
model_matrix(df, y ~ x1 + gender + class)

# 不过看起来分类变量似乎并不适合线性模型，因为为了损失最小，它倾向预测每个分类的均值
# 感觉单纯这样预测意义不大
mod <- lm(y ~ class, data = df)

# 所以从图里看就很明显
add_predictions(data = df, mod) %>% 
  ggplot(data = ., aes(x = class, y = y))+
  geom_point(size = 3)+
  geom_point(aes(y = pred), color = 'red')


# 交互项 连续+分类
# 这个通过 y ~ x1 * x2来实现，只要使用了*，交互项及其各个组成部分都会包含在模型中，挺好
# 书上演示了可加模型和交互模型的可视化区别，这里复现以作为练习吧
df <- modelr::sim3

mod1 <- lm(y ~ x1 + x2, df)
mod2 <- lm(y ~ x1 * x2, df)

grid <- df %>% 
  data_grid(x1, x2) %>%  #这一步是为了创造所有可能的预测项目
  gather_predictions(mod1, mod2)

# 绘制模型可视化，叠加模型各直线没有斜率差异
ggplot(df, aes(x1, y, color = x2)) + #散点还是需要原始数据
  geom_point() + 
  geom_line(data = grid, aes(x = x1, y = pred)) + #data_grid只是用于预测提供足够的预测值以便可视化
  scale_color_manual(values = c('a' = "red", 'b' = "blue", 'c' = "green", 'd' = "black")) + 
  facet_wrap(~ model)

# 利用残差一窥
df_res <- df %>% 
  gather_residuals(mod1, mod2)

# 然后对残差绘图，这里有个肉眼检查窍门，残差没有规律可循就是好，有规律就是不好
ggplot(df_res, aes(x1, resid, color = x2)) + 
  geom_point() + 
  scale_color_manual(values = c('a' = 'red', 'b' = 'green', 'c' = 'blue', 'd' = 'black')) +
  facet_grid(model ~ x2)


# 交互项 连续+连续
# 这里其实和连续+分类差不多，但是在使用data_grid生成唯一值的时候，连续变量明显水平忒多了，
# 所以可以使用seq_range()函数使用x变量最小值和最大值之间间隔相等的5个值来生成网格，
# 这个函数的pretty、trim、expand参数还是怪有点用的
df <- sim4
df_grid <- data_grid(df, 
                     x1 = seq_range(x = x1, n = 5), 
                     x2 = seq_range(x = x2, n = 5))

mod1 <- lm(y ~ x1 + x2, data = df)
mod2 <- lm(y ~ x1 * x2, data = df)

df_grid <- df_grid %>% 
  gather_predictions(mod1, mod2)

df_res <- df %>% 
  gather_residuals(mod1, mod2)

# 这里使用散点图
# 不过划线时由于x2不再离散，aes需要多声明一个group参数
ggplot() +
  geom_line(data = df_grid, aes(x = x1, y = pred, color = x2, group = x2)) + 
  facet_wrap(~ model)

# 当然从热力图看不明显差别
ggplot() + 
  geom_tile(df_grid, mapping = aes(x1, x2, fill = pred)) + 
  facet_wrap(~ model)


# 变量转换
# 这一节说白了就是通过指数对数平方开方，再结合泰勒定理，让原本的线性模型能拟合一些非线性的数据
df <- sim3

# 比如我们想弄个f(x) = x1^2 + x1的函数，公式不能写y ~ x^2，而是：
model_matrix(df, y ~ I(x1^2) + x1)

# 反例
model_matrix(df, y ~ x1^2 + x1)


# 然后就根据泰勒公式，任何平滑曲线都可以通过多项式来求和逼近
# 那么这个变量转化的应用则可：y ≈ a1 * x1 + a2 * x^2 + a3 * x^3 ...
# 但是在输入公式时候一个个输入I(x^2)、I(x^3)什么的太繁琐，这里就有一个辅助函数
model_matrix(df, y ~ poly(x1, 3))


# 但是书里貌似更推荐使用spline::ns()来替代poly()，因为说ploy的多项式可能会超出数据范围，ns更安全
library(splines)

model_matrix(df, y ~ ns(x1, 3))


# 然后就编了个数据集练习非线性拟合，这里就做复现练习吧
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50), 
  y = 4 * sin(x), + rnorm(length(x))
)

# 之后使用了五个基函数数量不同的模型
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)
mod6 <- lm(y ~ ns(x, 6), data = sim5)

# 然后拟合，可视化比较
df <- sim5 %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6)

ggplot(data = df) + 
  geom_point(aes(x, y)) + 
  geom_line(aes(x = x, y = pred), 
            color = 'red', linewidth = 3, alpha = 0.5) + 
  facet_wrap(~ model)


# 但是经过反思，这样的模型好像nb，但问题是我当前水平很难获取每个基函数的具体表达式。




# 缺失值
# 一般而言，r对缺失值要么报错要么闷声丢，所以在本章开始的option(na.action = na.warn)就是为了至少能给提醒一声
df <- data.frame(x = c(1, 2, NA, 4, 5), 
                 y = c(2, NA, 3, 4, 5))

# 一般这样的df会因为有na而报错，这时候可以声明na.action参数来保留但在计算时忽略na
mod <- lm(y ~ x, data = df, na.action = na.exclude)

# 使用nobs()可以知道建模实际上使用了多少样本
nobs(mod)




# 更多模型族这节建议直接看书P267