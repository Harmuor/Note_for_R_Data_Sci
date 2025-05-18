Sys.getlocale("LC_TIME") #检查语言和编码格式
Sys.setlocale("LC_TIME", "en_US.UTF-8") #临时设置一下语言为英语，重启后还会恢复




# 终于把第10章给煎熬完了
# 第11章 使用forcats处理因子

# 出了tidyverse意外，需要额外加载forcats包
library(tidyverse)
library(forcats)
library(ggplot2)


# 创建因子
# 以月份为例。假设这是我们的数据
month <- c('September', 'November', 'February', 'April', 'October', 'me')

# 这时候程序只是将之识别为字符串，没啥意义，比如：
rank(month)
sort(month)


# 先创建个排序变量
month_level <- c('January', 'February', 'March', 'April', 'May', 'June', 
                 'July', 'August', 'September', 'October', 'November', 'December')

# 然后可以使用，这时候你会发现排序是被你规定过了
# 同时需要注意，level没有规定的me不会在sort()中显示
months <- factor(month, levels = month_level)
rank(months)
sort(months)


# 想要知道某变量的排序规则，使用levels()
levels(months)


# 有时我们想要因子变量安排原排序来定序，下面展示两种方法
# 方法1 unique()
remain <- factor(month, levels = unique(month))

levels(remain)

# 方法2 fct_inorder()
# 两种实现方式
remain <- factor(fct_inorder(month))

remain <- month %>% 
  factor() %>% 
  fct_inorder()

levels(remain)




# 综合社会调查
# 后面我们需要依托数据集forcats::gss_cat
data1 <- forcats::gss_cat


# 当因子保存在tibble中，除了levels()，还可以使用count()或者直接可视化
# 不过有个问题，一些因子水平可能没人填，就可能被忽略
levels(data1$race)

count(data1, race)

# 忽略因子水平的问题可以通过scale_x_discrete缓解
ggplot(data1) +
  geom_bar(aes(x = race), stat = 'count') +
  scale_x_discrete(drop = F)




# 修改因子水平的值，比修改水平顺序更为强大
count(data1, partyid) #出来的结果表述太繁杂

# 使用fct_recode()对因子水平重新编码
data1 <- forcats::gss_cat
data1 <- data1 %>% 
  mutate(partyid_1 = fct_recode(partyid, 
                               'repulican, strong' = 'Strong republican', 
                               'repulican, weak' = 'Not str republican', 
                               'independent, near rep' = 'Ind,near rep', 
                               'independent' = 'Independent', 
                               'independent, near demo' = 'Ind,near dem', 
                               'democrat, weak' = 'Not str democrat', 
                               'democrat, strong' = 'Strong democrat', 
                               'others' = 'Other party', 
                               'others' = 'Don\'t know', 
                               'others' = 'No answer')) %>% 
  group_by(partyid_1) %>% 
  mutate(count_p = n()) %>% 
  ungroup()


# 然而，使用fct_recode()时一定要小心：如果合并了原本不同的分类，那么就会产生误导性的结果
# 术业有专攻，fct_collapse在合并多个水平的时候会更丝滑

data1 <- data1 %>% 
  mutate(partyid_2 = fct_collapse(partyid_1, 
                                   'Repulican' = c('repulican, strong', 'repulican, weak'), 
                                   'Independent' = c('independent, near rep', 'independent', 'independent, near demo'), 
                                   'Democrat' = c('democrat, weak', 'democrat, strong'), 
                                   'Others' = c('others')))



# 练习：美国民主党、共和党和中间派的人数比例是如何随时间而变化？
data1 <- gss_cat %>% 
  mutate(partyid_1 = fct_recode(partyid, 
                                'repulican, strong' = 'Strong republican', 
                                'repulican, weak' = 'Not str republican', 
                                'independent, near rep' = 'Ind,near rep', 
                                'independent' = 'Independent', 
                                'independent, near demo' = 'Ind,near dem', 
                                'democrat, weak' = 'Not str democrat', 
                                'democrat, strong' = 'Strong democrat', 
                                'others' = 'Other party', 
                                'others' = 'Don\'t know', 
                                'others' = 'No answer')) %>% 
  mutate(partyid_2 = fct_collapse(partyid_1, 
                                  'Repulican' = c('repulican, strong', 'repulican, weak'), 
                                  'Independent' = c('independent, near rep', 'independent', 'independent, near demo'), 
                                  'Democrat' = c('democrat, weak', 'democrat, strong'), 
                                  'Others' = c('others'))) %>% 
  group_by(year) %>% 
  mutate(count_year = n()) %>% 
  group_by(year, partyid_2) %>% 
  mutate(count_partyid = n(), 
         `prop(%)` = count_partyid/count_year * 100)



ggplot(data1) +
  geom_bar(aes(x = year, y = `prop(%)`, fill = partyid_2), 
           stat = 'identity', 
           position = 'dodge') +
  scale_x_continuous(breaks = seq(2000, 2015, 1)) +
  scale_fill_manual(values = c('Repulican' = '#3D9A8B', 
                              'Independent' = '#2EB19F', 
                              'Democrat' = '#63C2D2', 
                              'Others' = '#B7DEFF'))


# 这个变量重新编码有点好用，当我们需要根据得分分组的时候也许有用
# 拿之前编的数据练一练
data2 <- dplyr::tibble(name = c('Bob', 'Mary', 'Mike', 'Cheng', 'Himihola', 'Sara', 'Dive', 'Gray', 'Jim', 'Eric'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('male', 'female', 'male', 'female', 'female', 'female', 'male', 'male', 'male', 'male'), 
                       class = c(1, 1, 1, 2, 1, 2, 2, 1, 2, 2), 
                       region = c('west', 'west', 'west', 'east', 'east', 'west', 'west', 'east', 'east', 'east'), 
                       Chinese = c(78, 76, 64, 94, 95, 79, 82, 90, 89, 93), 
                       Math = c(45, 52, 60, 88, 86, 81, 76, 76, 69, 80), 
                       English = c(77, 80, 90, 91, 95, 90, 85, 86, 82, 81))

data3 <- dplyr::tibble(name = c('Mus', 'Iury', 'Eds', 'Chen', 'Yuli', 'Eason', 'Jenney', 'KK', 'Author', 'Wily'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('female', 'female', 'female', 'male', 'female', 'male', 'female', 'male', 'female', 'male'), 
                       class = c(1, 1, 2, 2, 2, 2, 1, 2, 1, 2), 
                       region = c('west', 'east', 'west', 'east', 'west', 'west', 'east', 'east', 'west', 'east'), 
                       Chinese = c(95, 79, 82, 90, 89, 93, 78, 76, 64, 94), 
                       Math = c(45, 81, 52, 60, 88, 86, 76, 76, 69, 80), 
                       English = c(90, 91, 86, 82, 81, 95, 77, 80, 90, 85))

# 假如我们想把数学成绩从原始分转为abcd
# 这里就不能用合并分子水平的fct_collapse()了，而需要使用cut()
data4 <- bind_rows(data2, data3) %>% 
  mutate(group_m = cut(Math, 
                       breaks = c(-Inf, 60, 70, 80, 90, 100), 
                       labels = c('F', 'D', 'C', 'B', 'A'), 
                       right = F #该参数False为左闭右开
                       )) %>% 
  mutate(group_m = factor(group_m, levels = c('A', 'B', 'C', 'D', 'F'))) #这样其实降序排序了

levels(data4$group_m)




# 第12章 使用lubridate处理日期数据
library(tidyverse)
library(ggplot2)
library(lubridate)
library(nycflights13)


# 可用函数返回当前时间
now()
today()


# 也可以用字符串创建特定时间，中间的空格逗号横杠都可以
# 年月日的顺序也可以随意
ymd('2025 3 27')
mdy('3, 27, 2025')
dmy('27-3-2025')
ymd("2015-Mar-07")

ymd(20250327) #甚至这也行

# 还可以进一步写时间，不过这样没刚刚那么自由了，具体时间和年月日之间必须是空格
ymd_hm('20250327 12:30')
ymd_hms('20250327 12:30:00')
ymd_hms('2025 03 27 12:30:00', tz = 'Asia/Shanghai') #可以按使用 IANA 时区数据库中的名称调整时区

ymd(20200630, tz = 'Asia/Shanghai')


# 除了使用字符串创建时间，在数据框中我们也可以使用列来创建
# 比如我们来看flight的那个数据框哈
data_f <- flights %>% 
  select(year, month, day, hour, minute)

(data_f)


# 这时候我们就可以使用make_datetime()来创建一个看起来更顺眼的日期列
data_f <- mutate(data_f, dt =make_datetime(year, month, day, hour, minute))




# 练一下给flights的dep_time和arr_time、sched_dep_time和sched_arr_time也规范一下时间呈现
# 首先别忘了这四列数据的呈现非常脑瘫，百位千位是小时、十位个位是分钟，我不明白它中间加个冒号会死吗
trans_datetime <- function(year, month, day, time) {
  make_datetime(year, month, day, time%/%100, time%%100)
} #我tm竟然能够看懂函数定义了


data_f <- flights %>% 
  select(year, month, day, hour, minute, origin, ends_with('delay'), ends_with('time')) %>% 
  filter(!is.na(dep_time) & !is.na(arr_time)) %>% 
  mutate(origin_dt = make_date(year, month, day), 
         dep_time = trans_datetime(year, month, day, dep_time), 
         arr_time = trans_datetime(year, month, day, arr_time), 
         sched_dep_time = trans_datetime(year, month, day, sched_dep_time), 
         sched_arr_time = trans_datetime(year, month, day, sched_arr_time))


# 然后可以试着做一下可视化
# 注意，当将日期时间型数据作为数值使用时（比如在直方图中），1表示1秒
ggplot(data_f) +
  geom_freqpoly(aes(dep_time), binwidth = 86400) #一天 = 86400秒

# 还可以按某一天来呈现图像
data_f %>% 
  filter(origin_dt == ymd(20131002)) %>% 
  ggplot() +
  geom_freqpoly(aes(dep_time), binwidth = 600) #每十分钟取一个点




# 获取和设置日期时间成分，这玩意真的妙
# 访问器函数：year()、month()、mday()（一个月中的第几天）、yday()（一年中的第几天）、# wday()（一周中的第几天）、hour()、minute() 和 second()
date_random <- ymd_hms('20250327 12:30:45', tz = 'Asia/Shanghai')

year(date_random)
mday(date_random) #所在月的第27天
yday(date_random) #运行之能看出是所在年的第86天
wday(date_random) #运行之能看出是所在周的周五Friday
minute(date_random)

# 同时label参数能管是否返回月份星期英文缩写，abbr能管是否返回其全称
wday(date_random, label = T)
month(date_random,label = T, abbr = F)




# 这玩意能有妙用啊
# 比如可以帮助我们发现，周末的航班比weekdays少一些
data_f %>% 
  mutate(weekdays = wday(origin_dt, label = T, abbr = F)) %>% 
ggplot() +
  geom_bar(aes(weekdays), fill = '#63C2D2')


# 或者一小时内哪几分钟延迟多
data_f %>% 
  mutate(departure = minute(dep_time)) %>% 
  group_by(departure) %>% 
  summarise(delay = mean(dep_delay, na.rm = T), 
            count_d = n()) %>% 
ggplot() +
  geom_bar(aes(departure, delay), 
                 stat = 'identity', 
                 color = 'black', fill = 'white') + 
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60))




# 对时间的舍入floor_date()、round_date()和ceiling_date()
# 很多时候时间数据会比较杂多，比如我们按天分析的时候不需要多余的时分信息
# 或者我们按小时分析的时候不需要分钟秒钟信息

count(data_f, week = floor_date(dep_time, 'week')) #每一周的航班数量统计

floor_date(data_f$origin_dt, 'week')

da <- ymd_hms('20200713, 14:25:45')

ceiling_date(da, 'year')
round_date(da, 'hour')
floor_date(da, 'week')
# 实际上是对时间变脸进行取舍的标准化，比如分析教学周为x轴的活动数量的时候会有用




# 修改时间
# 通过访问器函数修改
da <- ymd_hms('20250827, 14:25:45')
year(da) <- 1999
(da)

month(da) <- 2
(da)

# 或者可以通过update()函数来进行多个成分的同时修改
# 话说我没事改时间干啥
# 如果某一成分的时间加过量了，它会自动给往后顺延
update(da, year = 2025, month = 3, day = 8, hour = 33, minute = 70, second = 120)
update(da, day = -2000) #还可以这样调整查询或修改时间，缺点：实测容易不准



# 每周哪一天延误时间比较短呢
data_f %>% 
  mutate(weekday = wday(origin_dt, label = T)) %>% 
  group_by(weekday) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = T)) %>% 
  ggplot() +
  geom_bar(aes(weekday, dep_delay), stat = 'identity', color = 'black', fill = 'white')




# 时间间隔，对时间的变量进行运算的处理
# 这种能够运算的时间曰：时期变量 Duration
my_age <- (today() - ymd(20011002))
as.duration(my_age)

# 也可以通过字符串创建
dseconds(1333)
dminutes(137)
dhours(78)
ddays(31)
dweeks(17)
dmonths(53)
dyears(34)


# 这种变量使用秒为单位，这会在夏时制的情况下出一些问题，莫名其妙多了一小时
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York") 

one_pm 
one_pm + ddays(1) 


# 为了解决那个尴尬，所以我们需要“阶段”这一变量
# 阶段不以秒为单位，而是“人工”时间，创建也直观
one_pm + days(1)

seconds(4444)
minutes(137:34) #这里的冒号是从几到几的意思
hours(13)
weeks(c(12, 33))
months(12)
years(23)


# 区间%--%，表示时间范围
my_age <- ymd(20011002) %--% today()

# 这样能更具体计算，假如我们计算的时候需要考虑闰年、2月
time <- ymd_hms('20200127 12:30:30') %--% ymd_hms('20250127 12:30:30')
time/days(1)
time/months(1)
as.duration(time)



# 输入生日返回年龄的函数。这里我用了两种方式
age1 <- function(your_birthday) {
  (ymd(your_birthday) %--% today())/years(1)
}

age2 <- function(your_birthday) {
  as.duration(ymd(your_birthday) %--% today())
}




# 时区
# 可以使用Sys.timezone()函数找出你的当前时区
Sys.timezone()

# 可以使用OlsonNames()函数来查看完整的时区名称列表
OlsonNames()

# 时区多数时候就是tz参数或者tzone参数
ymd(20011002, tz = 'Asia/Hong_Kong')
ymd(20011002, tz = 'Asia/Shanghai')


today(tzone = 'Asia/Hong_Kong')
now(tzone = 'Europe/Stockholm')
now(tzone = 'US/Mountain')


# 有些时候数据如果标注错误的时区，可使用force_tz()函数进行修改
da <- c(ymd_hms('20250327 12:45:45', tz = 'Europe/Stockholm'), 
        ymd_hms('20250528 12:45:45', tz = 'Europe/Stockholm'), 
        ymd_hms('20250612 12:45:45', tz = 'Europe/Stockholm'), 
        ymd_hms('20250807 12:45:45', tz = 'Europe/Stockholm'))

force_tz(da, tz = 'Asia/Shanghai')






























