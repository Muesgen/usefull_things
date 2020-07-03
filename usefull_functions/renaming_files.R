#renaming files

data =list.files(path = "...", full.names = TRUE)

library(stringr)

v= stringr::str_sub(data, start= 1, end=-11)

file.rename(data, v)


