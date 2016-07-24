setwd("C:/aaaWork/Web/GitHub/RCourseNunavut2016")
## Just R scripts
file.remove("RScriptsNunavut2016.zip")
zip(zipfile="RScriptsNunavut2016",files=c("Scripts/"))

## Entire course materials
file.remove("RCourseNunavut2016.zip")
zip(zipfile="RCourseNunavut2016",files=c("Slides/","Handouts/*.pdf","Scripts/",
                                         "Exercises/*.pdf",
                                         "Exercises/Nunavut/*.pdf","Exercises/Nunavut/*.xlsx","Exercises/Nunavut/*.csv",
                                         "Exercises/Walleye/*.pdf","Exercises/Walleye/*.csv"))
