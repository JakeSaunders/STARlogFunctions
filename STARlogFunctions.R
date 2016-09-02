# write function to read in logs.final.out from STAR Aligner into a data frame
readSTARlogsFinalOut <- function(logs) {
    
    # name directory with STAR log files
    logs.list <- list.files(path = logs, recursive=TRUE, pattern=".Log.final.out", full=TRUE)
        #consider adding recursive opition to this fuction, with default to TRUE
    
    # get row names from 1st file
    c.names <- read.csv(
        logs.list[1], 
        header = FALSE, 
        sep = "|", 
        colClasses = c("character","NULL"),
        col.names = c("sample", NA),
        strip.white=TRUE
    )        
    
    # get data column from each log file
    logs.col <- t(
        as.data.frame(
            sapply(
                logs.list, 
                read.csv, 
                header = FALSE, 
                sep = "|", 
                colClasses = c("NULL","character"),
                strip.white=TRUE,
                stringsAsFactors = FALSE
            ),
            col.names = logs # name cols after file locations,
        )
    )
    
    # de-string dataframe 
    logs.col <-  as.data.frame(logs.col, stringsAsFactors = FALSE)
    
    # assign row names
    colnames(logs.col) <- c.names[[1]]
    
    # adds % to colnames that are missing it
    colnames(logs.col)[18] <- paste(colnames(logs.col)[18],"%")
    colnames(logs.col)[20] <- paste(colnames(logs.col)[20],"%")
    
    # sub % out
    for(i in 4:33) {
        logs.col[,i] <- as.numeric(gsub("%","",logs.col[,i]))
    }
    logs.col$`Started job on` <- strptime(logs.col$`Started job on`, "%B %d %H:%M:%S")
    logs.col$`Started mapping on` <- strptime(logs.col$`Started mapping on`, "%B %d %H:%M:%S")
    logs.col$`Finished on` <- strptime(logs.col$`Finished on`, "%B %d %H:%M:%S")
    
    # add row names as file path
    rownames(logs.col) <- logs.list
    
    # out put data fraem
    logs.col
}

# frame table made in readSTARlogsFinalOut function generate graphs of important factors
makeSTARreport <- function(final.logs, samples.lab = rownames(final.logs)){
    
    samples.lab <- rownames(final.logs)
    
    par(mfrow=c(2,2), mar = c(8,2,2,2))
    
    # graph `Number of input reads`
    barplot(
        height = final.logs$`Number of input reads`/10^6,
        ylab = "Millions of reads",
        main = "Number of Input Reads"
        # names.arg = samples.lab,las=2,cex.names=1.5
    )

    # graph `Uniquely mapped reads number`
    barplot(
        height = final.logs$`Uniquely mapped reads number`/10^6,
        ylab = "Millions of reads",
        main = "Number of Uniquely Mapped Reads"
        # names.arg = samples.lab,las=2
    )
    
    # graph `Uniquely mapped reads %`
    barplot(
        height = final.logs$`Uniquely mapped reads %`,
        ylab = "Percent of reads",
        main = "Percent of Input Reads Uniquely Mapped",
        names.arg = samples.lab,las=2
    )

    # # graph `Number of reads mapped to multiple loci`
    #     barplot(
    #         height = as.numeric(final.logs$`Number of reads mapped to multiple loci`)/10^6,
    #         ylab = "Millions of reads",
    #         main = "Number of Reads Mapped to Multiple Loci`",
    #         col = samples.col,
    #         names.arg = samples.lab
    #     #   ylim = c(0,40)
    #     )
    #     
    #     dev.copy(png,'plots/log.#multimapped.png')
    #     dev.off()
    
    # graph `% of reads mapped to multiple loci`
    barplot(
        height = final.logs$`% of reads mapped to multiple loci`,
        ylab = "Percent of reads",
        main = "Percent of Input Reads Mapped to Multiple Loci",
        names.arg = samples.lab,las=2
    )
}

# save log graphs
# dev.copy(png,'plots/log.PercentUniquelymapped.png')
# dev.off()



