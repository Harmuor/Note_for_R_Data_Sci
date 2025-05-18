# 练习1 批量读取文件
library(tidyverse)
# 这个能生成所有的文件路径为一个字符串向量，批次读取文件的时候肯定好用
dir('./saved_data/for循环批次读取文件', pattern = '^dia', full.names = T, ignore.case = TRUE)

files <- dir('./saved_data/for循环批次读取文件', pattern = '\\.csv$', full.names = T, ignore.case = T)
df <- tibble()

for (i in seq_along(files)) {
  dd <- read.csv(files[[i]])
  df <- bind_rows(df, dd)
}

# 其实就相当于这一行代码，tidyverse太牛了
final_df <- do.call(rbind, lapply(files, read.csv))



# 如果没有合并需求
files <- dir('./saved_data/for循环批次读取文件', pattern = '\\.csv$', full.names = T, ignore.case = T)

for (i in seq_along(files)) {
  df_names <- paste('df_', i, sep = '')
  assign(df_names, read.csv(files[[i]]))
}


# dir()一般不搜索子文件夹；如果需要搜索子目录，可改用 list.files() 并设置 recursive = TRUE
list.files('./saved_data', pattern = '^dia', full.names = T, ignore.case = T, recursive = T)


# 再试试一个合并
files <- list.files('./saved_data/for循环批次读取文件', pattern = '\\.csv$', full.names = T, recursive = T, ignore.case = T)

tdf <- tibble()

tdf <- do.call(rbind, map(files, read.csv))
