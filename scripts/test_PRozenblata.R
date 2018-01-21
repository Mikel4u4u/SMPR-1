m_i_e<-matrix(0,2,1)
t_m<-matrix(0,2,1)
for(i in 1:100){
  source("PRozenblata.R")
  t_m[1,1]=iteration
  t_m[2,1]=e
  m_i_e=cbind(m_i_e,t_m)
}
m_i_e=m_i_e[,-1]