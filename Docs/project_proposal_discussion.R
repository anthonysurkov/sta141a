
session=list()
for(i in 1:18){
  session[[i]]=readRDS(paste('./Data/session',i,'.rds',sep=''))
   print(session[[i]]$mouse_name)
   print(session[[i]]$date_exp)
}

ls(session[[1]])

summary(session[[1]]$brain_area)

table(session[[1]]$brain_area)


# Behavioural data

summary(session[[1]]$contrast_left)
table(session[[1]]$contrast_left)
length(session[[1]]$contrast_left)

table(session[[1]]$contrast_right)
length(session[[1]]$contrast_right)


summary(session[[1]]$feedback_type)
table(session[[1]]$feedback_type)

# Neural data

typeof(session[[1]]$spks)

length(session[[1]]$spks)

dim(session[[1]]$spks[[10]]) # the number of neurons (734) by the number of time bins (40)

length(session[[1]]$spks[[10]][5,])


typeof(session[[1]]$time)
session[[1]]$time[[10]]

session[[1]]$feedback_type[10]


session[[1]]$contrast_left[10]

session[[1]]$contrast_right[10]


# session[[1]]$spks[[10]] ----  session[[1]]$feedback_type[10]
