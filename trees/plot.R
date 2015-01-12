library(lattice)

csv <- read.csv(file="data.csv",sep=",",head=TRUE)
d <- data.frame(csv)

d$size <- d$size^2

pdf("times.pdf", width=8, height=4)

xyplot(normal+builder+total_normal+total_builder~size,
       data=d,
       type='l',
       auto.key = list(space='right'),
       ylab="Time",
       xlab="AST nodes",
       labels=names)

dev.off()
