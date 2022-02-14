install.packages("wesanderson")

library("jrvFinance")
library("readxl")
library("tibble")
library("stringr")
library(readr)
library("wesanderson")

data <- read_csv("~/Desktop/2022winter/apm466/A1/to_submit/final_output.csv")
df = data.frame(data)

cur_date = c('2022-01-10', '2022-01-11', '2022-01-12', '2022-01-13',
           '2022-01-14', '2022-01-17', '2022-01-18', '2022-01-19',
           '2022-01-20', '2022-01-21')

maturity = c('2022-02-28', '2022-05-31', '2023-02-28', '2023-05-31', 
           '2024-02-29', '2024-08-31', '2025-02-28', '2025-08-31', 
           '2026-02-28', '2026-08-31', '2027-02-28')

last_cp = df$last_cp_date
last_cp
today <- as.Date("2022-02-01")

maturity01 = as.numeric(as.Date(maturity[1]) - today) /365
maturity02 = as.numeric(as.Date(maturity[2]) - today) /365
maturity03 = as.numeric(as.Date(maturity[3]) - today) /365
maturity04 = as.numeric(as.Date(maturity[4]) - today) /365
maturity05 = as.numeric(as.Date(maturity[5]) - today) /365
maturity06 = as.numeric(as.Date(maturity[6]) - today) /365
maturity07 = as.numeric(as.Date(maturity[7]) - today) /365
maturity08 = as.numeric(as.Date(maturity[8]) - today) /365
maturity09 = as.numeric(as.Date(maturity[9]) - today) /365
maturity10 = as.numeric(as.Date(maturity[10]) - today) /365
maturity11 = as.numeric(as.Date(maturity[11]) - today) /365

maturity_yrs = c(maturity01, maturity02, maturity03, maturity04, 
                 maturity05, maturity06, maturity07, maturity08, 
                 maturity09, maturity10, maturity11)
maturity_yrs

last_cp01 = as.numeric(as.Date(last_cp[1]) - today) /365
last_cp02 = as.numeric(as.Date(last_cp[2]) - today) /365
last_cp03 = as.numeric(as.Date(last_cp[3]) - today) /365
last_cp04 = as.numeric(as.Date(last_cp[4]) - today) /365
last_cp05 = as.numeric(as.Date(last_cp[5]) - today) /365
last_cp06 = as.numeric(as.Date(last_cp[6]) - today) /365
last_cp07 = as.numeric(as.Date(last_cp[7]) - today) /365
last_cp08 = as.numeric(as.Date(last_cp[8]) - today) /365
last_cp09 = as.numeric(as.Date(last_cp[9]) - today) /365
last_cp10 = as.numeric(as.Date(last_cp[10]) - today) /365
last_cp11 = as.numeric(as.Date(last_cp[11]) - today) /365

last_cp_yrs <- c(last_cp01, last_cp02, last_cp03, last_cp04, 
                 last_cp05, last_cp06, last_cp07, last_cp08, 
                 last_cp09, last_cp10, last_cp11)


# coupons with length 11
coupons = as.numeric(sub("%", "",df$coupon_.,fixed=TRUE))/100
semi_coupons = coupons / 2

# clean price is vector with length 11
clean_0110 = data$`2022-01-10`
clean_0111 = data$`2022-01-11`
clean_0112 = data$`2022-01-12`
clean_0113 = data$`2022-01-13`
clean_0114 = data$`2022-01-14`
clean_0117 = data$`2022-01-17`
clean_0118 = data$`2022-01-18`
clean_0119 = data$`2022-01-19`
clean_0120 = data$`2022-01-20`
clean_0121 = data$`2022-01-21`

#clean_matrix is 11 * 10 matrix
clean_matrix <- cbind(clean_0110, clean_0111, clean_0112, clean_0113,
                      clean_0114,clean_0117,clean_0118,clean_0119,
                      clean_0120,clean_0121)

dirty_0110 = data$`2022-01-10_dirty`
dirty_0111 = data$`2022-01-11_dirty`
dirty_0112 = data$`2022-01-12_dirty`
dirty_0113 = data$`2022-01-13_dirty`
dirty_0114 = data$`2022-01-14_dirty`
dirty_0117 = data$`2022-01-17_dirty`
dirty_0118 = data$`2022-01-18_dirty`
dirty_0119 = data$`2022-01-19_dirty`
dirty_0120 = data$`2022-01-20_dirty`
dirty_0121 = data$`2022-01-21_dirty`

dirty_matrix <- cbind(dirty_0110, dirty_0111, dirty_0112, dirty_0113, 
                      dirty_0114, dirty_0117, dirty_0118, dirty_0119, 
                      dirty_0120, dirty_0121)


#Q4a
ytm_matrix <- data.frame(matrix(nrow = 11, ncol=10))

for (i in 1:11){
  # bond number i
  cur_bond_price = clean_matrix[i,] # this is a row with all the data of one bond
  for (j in 1:10){ 
    ytm_matrix[i,j] = bond.yield(cur_date[j],maturity[i],coupons[i],
                                 freq=2,cur_bond_price[j],
                                 convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                 comp.freq = 2,redemption_value = 100)
  }
}
ytm_matrix

# color_list with length of 10, representing each date
color_list <- c("blue", "darkgreen", "orange", "red", "purple", 
                "deeppink1", "coral", "grey", "brown", "green")
png(file="~/Desktop/2022winter/apm466/A1/to_submit/4(a).png",width=600, height=500)
year = seq(0,5,0.5)
plot(year, ytm_matrix[,1],type='l',col=color_list[1],
     xlab = '# of Year from 2022', ylab = "YTM",
     ylim=c(0,0.02),
     main = "0-5 Year Yield Curves")
lines(year,ytm_matrix[,2],col=color_list[2])
lines(year,ytm_matrix[,3],col=color_list[3])
lines(year,ytm_matrix[,4],col=color_list[4])
lines(year,ytm_matrix[,5],col=color_list[5])
lines(year,ytm_matrix[,6],col=color_list[6])
lines(year,ytm_matrix[,7],col=color_list[7])
lines(year,ytm_matrix[,8],col=color_list[8])
lines(year,ytm_matrix[,9],col=color_list[9])
lines(year,ytm_matrix[,10],col=color_list[10])

legend("topleft", legend = cur_date, pch = 16, cex = 0.8, col=color_list)

dev.off()

# Q4b
spot_matrix <- data.frame(matrix(nrow = 11, ncol=10))
for (i in 1:11){
  for (j in 1:10){
    if (j == 1){
      spot_matrix[i,j] = -2*log(dirty_matrix[i,j])/(coupons[i] * 100 + 100)
    }
    else{
      pre_sum <- 0
      for (k in 1:(j-1)){
        pre_sum <- pre_sum + coupons[i] * 100 * exp(-spot_matrix[i,k]*k/2)
      }
      spot_matrix[i,j] <- -log(clean_matrix[i,j]/(coupons[i]*100 + 100))/(j/2)
    }
  }
}
spot_matrix

spt_matrix <- data.frame(matrix(nrow = 11, ncol=10))

num_coupon_left <- c(1,1,2,2,3,5,6,7,7,9,10)

# Start from the second bond
for (i in 1:11){
  for (j in 1:10){
    if (i == 1){
      Spots_set[1,j] = log(dirty_matrix[i,j] / (semi_coupons[i] * 100 +100))
    }else{
      coupon = semi_coupons[i]*100
      prv = 0
      for (k in 1:10){
        prv <- prv + coupon/(1+spt_matrix[i-1,]/2)^(num_coupon_left[k])
      }
      pc <- dirty_matrix[i,j]-prv

      spt_matrix[i,j] <- ((coupon+100)/pc)^(1/(2*maturity_yrs[i]))-1
    }
  }
}

spt_matrix

sr1 = c(0.01597401, 0.01782766, 0.01612215, 0.01718236, 0.01642124, 0.01565212, 0.01640305,
        0.01715398, 0.01709005, 0.01866994, 0.02403152)

sr2 = c(0.01558887, 0.01730849, 0.01580159, 0.01677074, 0.01605509, 0.01534744, 0.01613210,
        0.01692477,0.01676379, 0.01841219, 0.02357038)

sr3 = c(0.01528975, 0.01653404, 0.01554269, 0.01637078, 0.01585487, 0.01543896, 0.01603172,
        0.01672448, 0.01665431, 0.01855140, 0.02403898)

sr4 = c(0.01549351, 0.01694449, 0.01596633, 0.01664176, 0.01595134, 0.01526093, 0.01609331,
        0.01692569, 0.01676435, 0.01875380, 0.02420658)

sr5 = c(0.01549765, 0.01662325, 0.01570452, 0.01659322, 0.01598153, 0.01536983, 0.01615555,
        0.01694126, 0.01696415, 0.01864631, 0.02438052)

sr6 = c(0.01586648, 0.01721681, 0.01607466, 0.01696937, 0.01627491, 0.01558045, 0.01636221,
        0.01714397, 0.01698698, 0.01869423, 0.02456187)

sr7 = c(0.01597071, 0.01712146, 0.01621629, 0.01731547, 0.01640624, 0.01549702, 0.01632814,
        0.01715926, 0.01736598, 0.01874271, 0.02468964)

sr8 = c(0.01599490, 0.01705180, 0.01622315, 0.01745693, 0.01653277, 0.01560862, 0.01657239,
        0.01752217, 0.01725862, 0.01904978, 0.02536083)

sr9 = c(0.01578910, 0.01721998, 0.01601963, 0.01708408, 0.01639900, 0.01571393, 0.01653235,
        0.01735078, 0.01728288, 0.01894098, 0.02558168)

sr10 = c(0.01552434, 0.01697264, 0.01576773, 0.01683799, 0.01616855, 0.01549911, 0.01640238,
         0.01730564, 0.01703731, 0.01867110, 0.02581296)

spot_matrix = cbind(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9,sr10)


png(file="~/Desktop/2022winter/apm466/A1/to_submit/4(b).png",width=600, height=500)
year = seq(0,5,0.5)
plot(year, spot_matrix[,1],type='l',col=color_list[1],
     xlab = '# of Year from 2022', ylab = "Spot Rate",
     main = "0-5 Year Spot Rate Curves")
lines(year,spot_matrix[,2],col=color_list[2])
lines(year,spot_matrix[,3],col=color_list[3])
lines(year,spot_matrix[,4],col=color_list[4])
lines(year,spot_matrix[,5],col=color_list[5])
lines(year,spot_matrix[,6],col=color_list[6])
lines(year,spot_matrix[,7],col=color_list[7])
lines(year,spot_matrix[,8],col=color_list[8])
lines(year,spot_matrix[,9],col=color_list[9])
lines(year,spot_matrix[,10],col=color_list[10])
legend("topleft", legend = cur_date, pch = 16, cex = 0.8, col=color_list)
dev.off()


#Q4c
forward_matrix <- data.frame(matrix(nrow=4,ncol = 10))
for(i in 1:10){
  for(j in 2:5){
    ra = (1+spot_matrix[1+j*2,i])^(j)
    rb = (1+spot_matrix[1,i])
    forward_matrix[j-1,i] = ra / rb - 1
    
  }
}
forward_matrix

for(j in 1:10){
  for (i in 1:4){
    f[i] = (as.numeric(spot_matrix[i+1,j])*(i+1) - as.numeric(spot_matrix[1,j])*1)/i
  }
}

f1 = c(0.01570047, 0.01544068 ,0.01608579, 0.01632426)
f2 = c(0.01548009, 0.01508927 ,0.01572018, 0.01585498)
f3 = c(0.01525282, 0.01492886, 0.01531546, 0.01527894)
f4 = c(0.01532486, 0.01497573, 0.01577015, 0.01557071)
f5 = c(0.01531434, 0.0149724 ,0.01546844 ,0.01542148)
f6 = c(0.01569592, 0.01537134, 0.0159486, 0.01595087)
f7 = c(0.01537129, 0.01534291, 0.01611828, 0.01593193)
f8 = c(0.0159048, 0.01556136, 0.01615746, 0.01591186)
f9 = c(0.01577633, 0.01547269, 0.01582474 ,0.01577445)
f10 = c(0.01591185, 0.01526892, 0.01557313 ,0.01550443)

forward_matrix = cbind(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
forward_matrix

png(file="~/Desktop/2022winter/apm466/A1/to_submit/4(c).png",width=600, height=500)
year = seq(1,4,1)
plot(year, forward_matrix[,1],type='l',col=color_list[1],ylim=c(0.0148,0.0165),
     xlab = '# of Year from 2022', ylab = "Forward Rate",
     main = "1-1, 1-2, 1-3, 1-4 Forward Rate Curves")
lines(year,forward_matrix[,2],col=color_list[2])
lines(year,forward_matrix[,3],col=color_list[3])
lines(year,forward_matrix[,4],col=color_list[4])
lines(year,forward_matrix[,5],col=color_list[5])
lines(year,forward_matrix[,6],col=color_list[6])
lines(year,forward_matrix[,7],col=color_list[7])
lines(year,forward_matrix[,8],col=color_list[8])
lines(year,forward_matrix[,9],col=color_list[9])
lines(year,forward_matrix[,10],col=color_list[10])
legend("topleft", legend = cur_date, pch = 16, cex = 0.8, col=color_list)
dev.off()


#Q5
ytm_series_matrix <- matrix(nrow = 5, ncol = 9)

for (i in c(1:5)){
  for (j in c(1:9)){
    ytm_series_matrix[i,j]<- log(ytm_matrix[2*i-1,j+1]/ytm_matrix[2*i-1,j])
  }
}
ytm_series_matrix
ytm_cov_matrix = cov(t(ytm_series_matrix))
ytm_cov_matrix


fwd_series_matrix <- matrix(nrow = 4, ncol = 9)

for (i in c(1:4)){
  for (j in c(1:9)){
    fwd_series_matrix[i, j] <- log(forward_matrix[i, j+1]/forward_matrix[i, j])
  }
}
fwd_series_matrix
fwd_cov_matrix = cov(t(fwd_series_matrix))
fwd_cov_matrix

#Q6
ytm_cov_eigen = eigen(ytm_cov_matrix)
ytm_cov_eigen

fwd_cov_eigen = eigen(fwd_cov_matrix)
fwd_cov_eigen
