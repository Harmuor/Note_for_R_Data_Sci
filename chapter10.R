# 第10章 使用stringr处理字符串
# 本章重点为“正则表达式”
library(stringr)
library(dplyr)

# 加引号就是字符
string1 <- 'This is a string'
string2 <- '破釜沉舟'

# 但是有些字符因为比较特殊，所以需要在前面加上转义符--反斜杠“\”
string3 <- "\\"

# 然而，print函数在查看字符的时候会把转义符也加进来，这在一定程度干扰我们
# 所以可以使用writeLines()函数
print(string3)
writeLines(string3)

# 使用stringr::str_length()函数可以查看字符长度
str_length(c('This is a sentence', 'to check out how long a string is', NA))

# 使用stringr::str_c()函数可以对字符串进行组合，也可以通过这个对字符串加前后缀
str_c(string1, string2)
str_c(string1, string2, sep = ' - ') # sep参数可以给合体的字符加分隔符号


str_c('前缀- ', string1, string2, ' -后缀') #但似乎无法给俩独立string分别加前后缀
str_c('prefix- ', c(string1, string2), ' -suffix') #其实是需要将分别加工的string向量化


# 为了之后方便这里造个数据
string4 <- tibble(`x1` = c(string1, string2, string3), 
                  `x2` = c(1, 2, 3), 
                  `x3` = c('我', '程', '柏'), 
                  `x4` = c('是', '诚', '然'), 
                  `y2` = c('10_02', '20_01', '08_16'))

str_c('/- ', string4$x1) # 不过可以给数据框的指定列加工，还不错


# 然而，如果我们将字符向量填进去，会发现合并后向量元素是不会少的，这意味着没用合并
# 这里来个对比
str_c(string1, string2)
str_c(c(string1, string2)) #字符向量没用


# 当我们有将含多个元素的字符向量合并的需求的时候，直接加一个参数就好
# 这里collapse参数功能类似sep，这使得我们可以操作数据框的字符列，因为那个是向量
str_c(c(string1, string2), collapse = '_')


str_c('prefix- ', c(string1, string2), ' -suffix', collapse = '_') #看来会先分别加前后缀，在合并字符向量
str_c(string4$x1, collapse = '___')


# 实验一下这个函数能不能让俩列合二为一新列
string4 <- string4 %>% 
  mutate(`y1` = str_c(x3, x4, collapse = '_')) #不太好用

string4 <- string4 %>% 
  mutate(`y1` = paste(x3, x4, sep = '_')) #这个好使


# 字符串取子集 str_sub()，感觉有点鸡肋
str_sub(string4$y2, start = 1, end = 3) #可以向量化操作
str_sub(string4$y2, start = -4, end = -1) #也可以倒着数


# 不过取子集有个妙用，比如我们需要将每一列人名首字母转换大小写的时候
# 这里召唤之前编的数据
data1 <- dplyr::tibble(name = c('Bob', 'Mary', 'Mike', 'Cheng', 'Himihola', 'Sara', 'Dive', 'Gray', 'Jim', 'Eric'), 
                       age = c(12, 12, 11, 13, 12, 14, 10, 11, 13, 10), 
                       gender = c('male', 'female', 'male', 'female', 'female', 'female', 'male', 'male', 'male', 'male'), 
                       class = c(1, 1, 1, 2, 1, 2, 2, 1, 2, 2), 
                       region = c('west', 'west', 'west', 'east', 'east', 'west', 'west', 'east', 'east', 'east'), 
                       Chinese = c(78, 76, 64, 94, 95, 79, 82, 90, 89, 93), 
                       Math = c(45, 52, 60, 88, 86, 81, 76, 76, 69, 80), 
                       English = c(77, 80, 90, 91, 95, 90, 85, 86, 82, 81))

str_sub(data1$name, 1, 1) <- str_to_lower(str_sub(data1$name, 1, 1)) #转小写
str_sub(data1$gender, 1, 1) <- str_to_upper(str_sub(data1$gender, 1, 1)) #转大写
# 这里其实就得先选中需要选的某几个字符，然后在转的时候也选中对应的字符，有点映射的意思。否则会出错

# 当然也可以全部转
data1$name <- str_to_upper(data1$name)


# 还有就是大小写转换的地区差异，用locale参数解决
# locale的声明需要对照ISO 639语言编码标准
# 土耳其语中有带点和不带点的两个i，它们在转换为大写时是不同的： 
str_to_upper(c("i", "ı")) 
 
str_to_upper(c("i", "ı"), locale = "tr") 


# 在一个就是按字母排序的问题，不同的地区也会有差异
# 排序本身用str_sort或者str_order来解决
str1 <- c('apple', 'banana', 'elephant')

# 如果按英语排序，就很简单，abc嘛
str_order(str1)
str_sort(str1)

# 可有些语言不是abc，比如夏威夷语, 所以还是locale参数
str_order(str1, locale = 'haw')
str_sort(str1, locale = 'haw')


# 补充一个在数据清理时会比较好用的函数str_trim()
# 能够清洗字符两端多余的空格制表符什么的
str2 <- '    many blank spaces here      '
str2_trimed <- str_trim(str2)


# 之前讲了字符串组合，这里讲拆分，str_split()函数
# 还记得string4里面带-数字那一列吗？
str_split(string4$y2, pattern = '_')

# 不过遗憾，str_split不太方便直接操作数据框
# 所以可以使用tidyr::separate()
library(tidyr)
string4 <- string4 %>% 
    separate(y2, #要拆分的列，目标
           into = c('y_21', 'y_22'),  #拆分后的去向，新列名
           sep = '_',  #拆分依据
           remove = FALSE) #拆分后是否还保留原列




# 使用正则表达式进行模式匹配
# 先从简单的开始，这个工作可以由srt_view()或者str_view_all()完成
# 先准备一下这一节的几个学习材料
str1 <- c('apa', 'apple', 'banana', 'elephant')
str2 <- c('a.b', 'cac', 'abc')
str3 <- c('\\\\n', '\\.\\\\.\\"\\', '\\')

# 自然语言 -> 正则表达式 -> 表达式在 R 中书写转换（逐个字符自己脑子打草换）
# '\'在r和正则表达式中都是转义符号
# 记住这个准则

# 在str1中，如果我们想要查找'an'这个字段
str_view(str1, 'an')

# 在str1中，如果我们想要查找被“奥利奥”的字母a，
# 就需要使用正则表达式的'.'句点，表示匹配任意字符（除了换行符）
str_view(str1, '.a.')

# 然而，当我们想要只是查找实际中作为文本的'.'，
# 这时候请回忆起那个语言转化准则
str_view(str2, 'a\\.')

# 那么我们来用str3练习一下。
writeLines(str3)
str_view(str3, '\\\\\\\\n')
str_view(str3, '\\\\\\.\\\\\\\\\\.\\\\"\\\\')

str_view('aaaa$^$aaa', '\\$\\^\\$')




# 关于“锚点”
# 在之前的字符匹配中，我们发现查找的时候它会把整个字段的目标都搜出来。
# 那么这可能并不是我们需要的。所以我们可能需要定位辅助用的锚点
# 始于权力（^），终于金钱（$）。--Evan Misshula

# 假如我们只是想匹配开头为y的字词
str_view(stringr::words, '^y')

# 或者我们想要匹配结尾为g的字词
str_view(stringr::words, 'y$')


# 出了“.”，还有其他可以进行特殊匹配的字符
# 匹配任意一个数字的\d
str_view('a1f, 3g, ee5', '\\d')

# 匹配任意空白（如空格、制表符和换行符）的\s
str_view('a  d, dw w, ssd ie', '\\s')

# 或者或者非
str_view('slasaalskdfebvnlsjchcbbc', '[abc]') #匹配abc的其中之一
str_view('slasaalskdfebvnlsjchcbbc', '[^abc]') #匹配非a非b非c的项

# 用逻辑运算符也可以，不过建议带括号
str_view('gray, grey', 'gr(a|e)y')


# 匹配重复
str3 <- 'scckdfccccksjccccccisj'
str_view(str3, 'c{3}') #匹配重复3次的c
str_view(str3, 'c{2,4}') #匹配重复1-4次的c，{}里不可以有空格
str_view(str3, 'c{3,}') #匹配重复至少3次的c
str_view(str3, 'c{,4}') #匹配重复最多4次的c

# 还有一种不太精确的重复复配
str_view(str3, 'z?') #匹配重复0次或1次的z
str_view(str3, 's+') #匹配重复1次或多次的z
str_view(str3, 's*') #匹配重复0次或多次的s

#这两种重复匹配还能混合用
str_view(str3, 'c{3}?') # 懒惰模式就是加问号?

# 关于懒惰和贪婪这个例子
str4 <- 'abc123c123c123c'
str_view(str4, 'c.*c') #贪婪
str_view(str4, 'c.*?c') #懒惰


# 分组与回溯引用
# 括号用于分组，正则里的\1和\2说是回溯引用
str_view(stringr::fruit, '(.)\\1') #aa
str_view(stringr::fruit, '(..)\\1') #abab
str_view(fruit, '(.)(.)\\1') #aba
str_view(fruit, '(.)(.)\\2') #abb




# 匹配检测 str_detect()
# 将较于view返回搜索结果，detect会返回和输入长度一致的逻辑向量
# T=1，F=0，如果结合sum和mean会有别的用处
string4 <- tibble(`x1` = c(string1, string2, string3), 
                  `x2` = c(1, 2, 3), 
                  `x3` = c('我', '程', '柏'), 
                  `x4` = c('是', '诚', '然'), 
                  `y2` = c('10_02', '20_01', '08_16'))

str_detect(string4, '程') #输入是五列，返回向量长度也是5
str_detect(string4$x3, '程') #输入是3个元素的向量，返回长度也是3

# 以t为开头的单词有几个
sum(str_detect(words, 'e'), na.rm = T)

# 以元音为结尾的单词比例几何
mean(str_detect(words, '[aeiou]$'), na.rm = T)


# 找出不包含元音字母的单词
str_view(words, '^[^aeiou]+$') #太复杂了，书上也建议不要让正则表达式过于复杂

str_detect(words, '^[^aeiou]+$')
!str_detect(words, '[aeiou]') #因为detect输出是逻辑值，可以直接取反


# 其实detect要比view方便，如果我们不仅想知道匹配到的数量和逻辑值，同时也想实现“查找”功能时
# 有两种方法，第一种是逻辑子集[]、第二种是str_subset()函数
stringr::words[!str_detect(words, '[aeiou]')]
str_subset(words, '^[^aeiou]+$') #可惜这个函数不能取反


# 之前说过，str_detect返回逻辑值，这就可以可filter联动
df <- tibble(word = words, 
             i = seq_along(word))

# 这样就可以处理数据框的文本列了
df1 <- df %>% 
  filter(str_detect(word, '^t'))


# 关于匹配的函数出了view、detect
# 还有str_count()，该函数能够数出目标种匹配项的个数
# str_count() 期望的是字符向量，而不是数据框
str_count(c('tax', 'dstt', 'sdeds', 'kket'), 't')

# 所以能够有下面的应用
# 比如这样可以添加一列记录对应项目出现了几次目标
df <- df %>% 
  mutate(e_counts = str_count(word, 'e'))

# 在一个应用就是可以和sum、mean结合，它统计的是搜索的具体目标，不是单词哈，这个跟detect不一样
sum(str_count(df$word, 'e'), na.rm = T)

mean(str_count(df$word, 'e'), na.rm = T)


# 还有一个我以为可以留意一下
# str_replace()替换，不过还是得用正则查找
str_replace(string = df$word, 
            pattern = 'e+', 
            replacement = 'SISSY')


#这个可以将通配符转成正则
glob2rx('*.Rmd')


# 煎熬的第10章，这里发邮件问过胡传鹏教授，他说这其实有领域侧重，换言之，在数据科学领域，很多时候要面对对自然语言的处理
# 所以《R数据科学》这本书会花很多篇幅在字符串处理上。而如果我未来对实验数据处理的话，可能这方面要求没有那么高或者强烈
# 不过既然闲着没事，多学点我以为也不是什么坏事，至少我学了一些正则和通配符的东西。

















