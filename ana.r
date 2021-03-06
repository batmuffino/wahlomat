#from package lsa
cosine<-function (x, y = NULL) 
{

    if (is.matrix(x) && is.null(y)) {
        co = array(0, c(ncol(x), ncol(x)))
        f = colnames(x)
        dimnames(co) = list(f, f)
        for (i in 2:ncol(x)) {
            for (j in 1:(i - 1)) {
                co[i, j] = cosine(x[, i], x[, j])
            }
        }
        co = co + t(co)
        diag(co) = 1
        return(as.matrix(co))
    }
    else if (is.vector(x) && is.vector(y)) {
        return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
    }
    else if (is.vector(x) && is.matrix(y)) {
        co = vector(mode = "numeric", length = ncol(y))
        names(co) = colnames(y)
        for (i in 1:ncol(y)) {
            co[i] = cosine(x, y[, i])
        }
        return(co)
    }
    else {
        stop("argument mismatch. Either one matrix or two vectors needed as input.")
    }
}



jpeg=function(x) grDevices::jpeg(x,height = 6, width = 6, units = 'in', res=300,quality=95)


#code begins here
d<-read.csv('./data.csv')


dis=1-cosine(as.matrix(d))

jpeg('parteien_cos_dist.jpg')
heatmap(cosine(as.matrix(d)),scale='none')
dev.off()

jpeg('parteien_mds.jpg')
plot(cmdscale(as.dist(dis)),pch='',xlab='',ylab='')
text(cmdscale(as.dist(dis)),label=row.names(dis))
dev.off()

jpeg('parteien_pca.jpg')
p<-princomp(d)
#biplot(p)
biplot(ca)
dev.off()

jpeg('parteien_alleinstellung.jpg')
library(ade4)
ca<-dudi.coa(t(1+d),nf=6,scannf=F)
heatmap(as.matrix(d[order(ca$cw),]),scale='none',Rowv=NA)
dev.off()

jpeg('parteien_for_fun.jpg')
heatmap(as.matrix(d),scale='none')
dev.off()

