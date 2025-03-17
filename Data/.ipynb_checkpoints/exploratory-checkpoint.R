#session=list()
#for (i in 1:18){
#  session[[i]] = readRDS(paste('~/Documents/ucd/sta141a/course_project/Data/session',i,'.rds',sep=''))
#  length(session[[i]])
  #print(session[[i]]$mouse_name)
  #print(session[[i]]$date_exp)
#}

total_len = 0
for (i in 1:18) {
  session[[i]] = readRDS(paste('~/Documents/ucd/sta141a/course_project/Data/session', i, '.rds', sep=''))
  total_len = total_len + length(session[[i]]$feedback_type)
}
print(total_len)