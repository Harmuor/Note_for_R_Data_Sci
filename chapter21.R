# 第21章 使用ggplot2进行图形化沟通
# 这一找那个就是通过ggplot2尽可能让图形通俗易懂，方便给不同领域的人交流沟通

# 准备工作
library(tidyverse)




# 标签
# 具体来说这个就是ggplot2中的labs函数，能够给图形、xy轴、图例加题目（就是“标签”）
ggplot(mpg) + 
  geom_point(aes(displ, hwy, color = class)) + 
  geom_smooth(aes(displ, hwy), se = F) + 
  labs(
    title = '这里是标题',  #这里给整个图片加标题
    subtitle = '这里是副标题', 
    caption = '这里是右下角标题', 
    x = '这里是x轴名字', 
    y = '这里是y轴的名字', 
    color = '这里是图例的标题'
  )

# 然后这个功能也能直接打出公式，不过得把引号替换成quote()函数
df <- tibble(x = runif(3), 
             y = runif(3))

ggplot(df, aes(x, y)) + 
  geom_point() + 
  labs(x = quote(sum(x[i]^2, i == 1, n)), 
       y = quote(alpha + beta + frac(delta, theta)))




# 注释
# 除了标题外，很多时候我们需要根据不同类别给图片中的一些元素贴上标签
# 这里先介绍了geom_text函数，使用起来和geom_point类似

# 比如这里我们想将每个类别中能耗效率最高的车车给标出来
the_best <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy)) == 1)
  
ggplot(data = mpg, aes(displ, hwy, color = class)) + 
  geom_point() + 
  geom_text(data = the_best, 
            aes(x = displ, 
                y = hwy, 
                label = model), #这里设定标签内容是型号（model）
            nudge_y = 2, #这里让标签在数据点上方 
            nudge_x = 0.5  #这里让标签在数据点右方
            ) 

# 但是上面那个出图还是有点乱，这里还可以使用geom_label()函数，它能给标签加文本框
ggplot(data = mpg, aes(displ, hwy, color = class)) + 
  geom_point() + 
  geom_label(data = the_best, 
            aes(x = displ, 
                y = hwy, 
                label = model), #这里设定标签内容是型号（model）
            nudge_y = 2, #这里让标签在数据点上方 
            nudge_x = 0.5, #这里让标签在数据点右方
            alpha = 0.5
            ) 

# 然而，标签还是存在重叠的问题，这时候需要ggrepel包来解决问题了
library(ggrepel)

ggplot(mpg, aes(displ, hwy, color = class)) + 
  geom_point() + 
  ggrepel::geom_label_repel(data = the_best, 
                            aes(x = displ, 
                                y = hwy, 
                                label = model))

# 不过说实话，虽然这样加标签有时候有用，为什么我不直接在ppt给图形加上呢。这样整就感觉有点麻烦还乱了




# 标度
# 这里我感受最直观的就是可以控制坐标轴刻度、图例位置什么的


# 坐标轴刻度
# 控制刻度最主要使用breaks和labels这俩参数。前者控制刻度的位置、后者控制每个刻度或者图例显示的内容
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  scale_y_continuous(breaks = seq(10, 50, by = 10), 
                     labels = c('a', 'b', 'c', 'd', 'e'))

# 当然，也可以labels参数为NULL，这样就像何振宏教授那样，刻度后期用ppt加上就也可
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  scale_x_continuous(labels = NULL) + 
  scale_y_continuous(breaks = seq(10, 50, by = 10), 
                     labels = NULL)


# 在日期数据类型上也有一套方法
# 这是美国总统任期的数据
presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id)) + 
  geom_point() +  #用来设定线段起点
  geom_segment(aes(xend = end, yend = id)) +  #用来画线段，这里设定终点位置
  scale_x_date(name = '开始 - 结束年份',  #看，x轴标题也可以在这里设置，也可以为NULL
               breaks = presidential$start, 
               date_labels = '19%y')


# 图例布局
# 如果要更改整体图例的位置，可以使用theme()函数，该函数控制图形中与数据无关的部分。

im <- ggplot(mpg, aes(displ, hwy, color = class)) + 
  geom_point()

im + theme(legend.position = 'left')
im + theme(legend.position = 'right')
im + theme(legend.position = 'top')
im + theme(legend.position = 'bottom')
im + theme(legend.position = NULL) #当然也可以选择不显示图例


# 除了theme整体调整，还可以使用guides()搭配guide_legend（离散）或者guide_colorbar（连续）来做更细节的调整
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth(se = F) + 
  theme(legend.position = 'bottom') + 
  guides(color = guide_legend(title = '图例', 
                             nrow = 1,  #控制图例的行数
                             # override.aes控制图例的美学，接受list输入，具体如下
                             override.aes = list(size = 4, 
                                                 shape = 15, 
                                                 alpha = 0.75)))


# 标度替换 这个经常进行转换的是数值和颜色

# 先说数值的标度转换
# 还记得之前为了让diamonds的cut和价格关系更明显，我们取对数的操作吗？
# 其实这个操作可以直接在ggplot中进行的，同时刻度值还是原值（不进行转换）

ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50) +
  scale_x_log10() + 
  scale_y_log10()


# 再说颜色的标度转换
# 一般来说图片颜色都是自动分配的，但是有时候咱就是需要对颜色调整
# 就之前也搞过，分类变量用sclae_color_manual

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(
    values = c(Republican = "red", Democratic = "blue")
  )

# 下面是连续变量的处理，用scale_color_gradient或者scale_fill_gradient
# 不过scale_color_gradient2可能更好看一点
# 书中示范则使用了viridis包

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) + 
  geom_hex(bins = 50) + 
  # 下面示范scale_fill_gradient2的调试
  scale_fill_gradient2(low = 'blue', 
                       mid = 'black', 
                       high = 'red', 
                       midpoint = 25) + 
  coord_fixed() #这个保证xy轴比例一致，单位长度一致

# 这个是用viridis
ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()




# 缩放
# 这一节的重点是通过coord_cartesian之类的函数设置xy轴的取值范围来达到缩放图形的效果
# 简单应用的话就是这样的

fig <- ggplot(data = mpg, aes(displ, hwy, color = class)) + 
  geom_point()

fig + coord_cartesian(xlim = c(3, 6),  #设置x轴取值
                      ylim = c(15, 45)) # 设置y轴取值


# 复杂应用则是这样的情景：当我们想对比两张图的时候，如果xy轴刻度和范围不一样就很麻烦
# 这个时候就需要在两张图使用同样的刻度，那么可以通过变量来辅助实现。
# 以下方法比facet更为灵活、适用面更广

#假如要比较这俩组的数据
suv <- mpg %>% filter(class == 'suv')
compact <- mpg %>% filter(class == 'compact')

# 那么就提前备好刻度设置（用变量的方式
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
color_scacle <- scale_color_discrete(limits = unique(mpg$drv))

# 然后绘制这俩图就行了
ggplot(suv, aes(displ, hwy)) + 
  geom_point(aes(color = drv)) + 
  x_scale + 
  y_scale + 
  color_scacle

ggplot(compact, aes(displ, hwy)) + 
  geom_point(aes(color = drv)) + 
  x_scale + 
  y_scale + 
  color_scacle




# 主题 -- 用来定义非数据元素
# 这一节书上就说了可以用8种主题，加上之前我们使用theme(legend.position = '')来定义图例位置，就这些
# 不过还提了一嘴图片中的字体是在这一块定义的，将来这个可能有点用




# 保存图形
# （1）通过ggsave
# 把文件名保存路径尺寸和单位设定好就行了

ggplot(mpg, aes(displ, hwy)) + 
  geom_point()
ggsave(filename = 'test.pdf', 
         path = './ggsave', 
         width = 1080, 
         height = 1080, 
         units = 'px')

  
# （2）使用Rmarkdown
# 在Rmd中，让图形大小一致对于视觉和排版是至关重要的。
# 关于参数的设置最好还是回到书上p329。这里就当作笔记本记下来几个可能的常用设施
# 顺便一提这5个参数都是knitr::opts_chunk$set()函数里的



```{r 宽度一致}
fig.width = 6  # 6英寸，这个不绝对，可以改，但要全局一遍所有图形宽度一致
fig.asp = 0.618 # 黄金比例
# 以上为全局设置
# 在单个代码段中只调正fig.asp
```

```{r 输出图形大小}
out.width = '70%'
fig.align = 'center'

# 如果想在一行放n个图形
out.width = '50%' #这样能放俩
out.width = '33%' #这样是仨
out.width = '25%' #这样能放四个
```
