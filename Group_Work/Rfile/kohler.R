library(wordcloud2)

letterCloud(demoFreq, word ="R", wordSize = 2,color = 'random-dark')
wordcloud2(demoFreq, size = 0.15, shape = 'circle',backgroundColor = "black",
           fontFamily = "微软雅黑", color = "random-light",  
           figPath = '~/Rimage/good.jpg',
           minRotation = -pi/6, maxRotation = -pi/6)

wordcloud2(goodluck , size = 0.1, #shape = 'circle',
            backgroundColor = "black",
           fontFamily = "微软雅黑", color = "random-light",  
           figPath = '~/Rimage/man.jpg',
           minRotation = -pi/6, maxRotation = -pi/6)

