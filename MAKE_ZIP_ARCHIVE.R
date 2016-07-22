setwd("C:/aaaWork/Web/GitHub/RCourseKC2016")
file.remove("RCourseKC2016.zip")
zip(zipfile="RCourseKC2016",files=c("Exercises/*.pdf","Handouts/*.pdf","Scripts/","Slides/"))
