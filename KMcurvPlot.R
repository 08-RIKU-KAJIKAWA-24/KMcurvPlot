

## ========================================================================== ##
# 投稿論文用のKM曲線を作成するPGM
# written by Riku KAJIKAWA
# 2024/07/09
# 
## ========================================================================== ##



require("tidyverse")


# eps形式でKaplan-Meier曲線の画像を吐き出す関数の定義 -----
KMcurvPlot <- function(filename = "hogehoge", # outputされる画像の名前
                       extension = "eps",     # outputされる画像の拡張子
                       background = ,
                       xaxis = "y",         # x軸のスケールの指定: "y"=年, "m"=月, "d"=日
                       yaxis = "prop",      # 0% ~ 100%, 0 ~ 1   : "prop"= 0%~100%, "none"= 0~1
                       dpi = 300){
  
  # output先のフォルダを作成 ---
  dir.create(path = PATH)
  
  # 作成した画像の保存 ---
  ggsave(
    file = paste(FILENAME, EXTENSION),
    plot = last_plot(),
    device = NULL,
    path = PATH,
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm", "px"),
    dpi = 600,
    limitsize = TRUE,
    bg = TRUE,
  )
}



# ディレクトリの指定 -----
PATH <- "C:\\final\\xxxx\\"
setwd(PATH)

dat <- survival::leukemia
KM <- survfit(Surv(time,status)~x, dat)
sum <- summary(KM)
dat00 <- data.frame(surv=sum$surv)

dat <- cbind(dat,dat00)
