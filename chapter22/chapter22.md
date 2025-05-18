第22章 Rmarkdown输出类型
================
程诚 柏然
2025-05-18

不知不觉到了实际上的最后一章了呀。这一章的仪表盘和网站我还是感兴趣的，不过后者应该不咋好整。其实目前对于我这个懒种而言，更不好整的的输出PDF，因为我可能换电脑，懒得装Latex。然后也会略过一些章节，比如调整YAML能输出Word什么的。

# 笔记本

这个由在YAML文件头的outp设置`html_notebook: default`，这个根据书上可以方便同行交流代码，然后修改工作则需要git和github。在YAML设置`github_document: default`，能生成类似README的md文件。然后这个文件同行通过git来修改。

# 生成幻灯片

用Rmd生成幻灯的优点是有效率、方便插入代码。每个一级或二级标题是新幻灯的意思。然后又三种格式的演示文稿

    output: 
      ioslides_presentation  #ioslides 格式的 HTML演示文稿。
      slidy_presentation  #W3C Slidy 格式的HTML演示文稿。
      beamer_presentation  #LaTeX Beamer 格式的 PDF演示文稿。

# 仪表盘
