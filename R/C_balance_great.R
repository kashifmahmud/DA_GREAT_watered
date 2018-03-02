# Matching C balance of the entire experiment considering C inputs and outputs
C.balance = data.frame(matrix(ncol = 7, nrow = length(treat.group)))
names(C.balance) = c("Room","GPP","Rm","biomass","growth.resp","C.output","storage")
C.balance$Room = treat.group
  
for (v in 1:length(treat.group)) {
  data.set = subset(data.all,(Room %in% treat.group[v]))
  # data.set[nrow(data.set),c(10:17)] = data.set[nrow(data.set)-1,c(10:17)]
  data.set[,c(10:17)] = na.spline(data.set[,c(10:17)])
  # plot(data.set$Date, data.set$LM)
  
  C.balance$GPP[v] = sum(data.set$GPP) 
  
  C.balance$Rm[v] = sum(data.set$R_leaf * data.set$LM + data.set$R_wood * data.set$WM + data.set$R_root * data.set$RM)
  C.balance$biomass[v] = (data.set$LM[nrow(data.set)] - data.set$LM[1]) + (data.set$WM[nrow(data.set)] - data.set$WM[1]) + (data.set$RM[nrow(data.set)] - data.set$RM[1])
  C.balance$growth.resp[v] = 0.3 * C.balance$biomass[v]
  C.balance$C.output[v] = C.balance$Rm[v] + C.balance$biomass[v] + C.balance$growth.resp[v]
  C.balance$storage[v] = C.balance$GPP[v] - C.balance$C.output[v]
}

C.balance.fraction = C.balance[, c(3:5,7)]
C.balance.fraction[,] = C.balance.fraction[,] / C.balance[,2] * 100
row.names(C.balance.fraction) <- c("1","2","3","4","5","6")
C.balance.fraction = abs(C.balance.fraction)
  
C.balance = C.balance[,-c(6)]
colnames(C.balance) <- c("Room", "GPP (g C)", "Rm (g C)", "Cs (g C)", "Rg (g C)", "Cn (g C)")
# C.balance = C.balance[,c(10,1,2,3,4,7,5,6,8,9)]
write.csv(C.balance, file = "output/C_partitioning_great.csv", row.names = FALSE)


cbPalette = c("gray", "orange", "skyblue", "green3", "#009E73", "yellow3", "#0072B2", "#D55E00")
png("output/Figure_1a_C_balance_great.png", units="px", width=1200, height=1000, res=200)
par(mfrow = c(1, 1), mar=c(5, 4, 2, 6))
# bb = barplot(as.matrix(t(Ct.fraction.group)), ylim=c(0, 107), ylab = "C Partitioning (%)", xlab = "Treatments (Container size)",  
#         col = rainbow(20),legend = colnames(Ct.fraction.group), 
#         args.legend = list(x = "topright", bty = "n", inset=c(-0.15, 0)))
bb = barplot(as.matrix(t(C.balance.fraction)), ylim=c(0, max(rowSums(C.balance.fraction))+10), ylab = "C Partitioning (%)", xlab = "Container size (L))",  
             col = cbPalette[1:4],legend = c(expression(R["m,tot"]),expression(C[s]),expression(R[g]),expression(C[n])), 
             args.legend = list(x = "topright", bty = "n", inset=c(-0.17, 0)))
# text( bb, Ct.fraction.group[,1]+Ct.fraction.group[,2]+Ct.fraction.group[,3]+Ct.fraction.group[,4]+Ct.fraction.group[,5]+Ct.fraction.group[,6]+Ct.fraction.group[,7]-1, labels = round(Ct.group[,9],1), cex=.9)
text( bb, rowSums(C.balance.fraction)+0.5, labels = round(C.balance[,2],1), pos = 3, cex=1, col="red")

dev.off()
