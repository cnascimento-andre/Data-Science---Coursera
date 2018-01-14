# exploratory data analysis - course project

plot1<-function(directory="D:/0_backup/pessoal/cursos/Data_science_JHU_MOOC/04_exploratory_data_analysis"){
    # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
    # make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
    setwd(directory) 
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI<-transform(NEI, year = factor(year))
    total<-tapply(NEI$Emissions, NEI$year, sum)
    barplot(total, ylab = "sum(PM2.5) - [tons]", main = "Total Emissions")
}

plot2<-function(directory="D:/0_backup/pessoal/cursos/Data_science_JHU_MOOC/04_exploratory_data_analysis"){
    # Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
    # Use the base plotting system to make a plot answering this question.
    setwd(directory) 
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI<-transform(NEI, year = factor(year))
    total<-tapply(NEI$Emissions[NEI$fips == 24510], NEI$year[NEI$fips == 24510], sum)
    plot(names(total), total,  ylab = "sum(PM2.5) - [tons]", xlab = "Years", main = "Total Emissions - Baltimore City, Maryland", pch = 19)
    lines(names(total),total)
}

plot3<-function(directory="D:/0_backup/pessoal/cursos/Data_science_JHU_MOOC/04_exploratory_data_analysis"){
    # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
    # which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
    # Which have seen increases in emissions from 1999–2008?
    setwd(directory) 
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI<-transform(NEI, year = factor(year), type = factor(type))
    totalNR <- tapply(NEI$Emissions[NEI$fips == 24510 & NEI$type == "NON-ROAD"], NEI$year[NEI$fips == 24510 & NEI$type == "NON-ROAD"], sum)
    totalOR <- tapply(NEI$Emissions[NEI$fips == 24510 & NEI$type == "ON-ROAD"], NEI$year[NEI$fips == 24510 & NEI$type == "ON-ROAD"], sum)
    totalNP <- tapply(NEI$Emissions[NEI$fips == 24510 & NEI$type == "NONPOINT"], NEI$year[NEI$fips == 24510 & NEI$type == "NONPOINT"], sum)
    totalPP <- tapply(NEI$Emissions[NEI$fips == 24510 & NEI$type == "POINT"], NEI$year[NEI$fips == 24510 & NEI$type == "POINT"], sum)
    total <- as.table(rbind(totalNR, totalOR, totalNP, totalPP))
    rownames(total)<-c("NON-ROAD", "ON_ROAD", "NON-POINT", "POINT")
    matplot(colnames(total),t(total), type = "l",  ylab = "sum(PM2.5) - [tons]", xlab = "Years", main = "Total Emissions - Baltimore City, Maryland")
    legend("topright", rownames(total), col=seq_len(total), cex=0.8, fill=seq_len(total))
}
plot4<-function(directory="D:/0_backup/pessoal/cursos/Data_science_JHU_MOOC/04_exploratory_data_analysis"){
    # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
    setwd(directory) 
    SCC <- readRDS("Source_Classification_Code.rds")
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI<-transform(NEI, year = factor(year), type = factor(type))
    coal_rows <- grepl("Coal", SCC$Short.Name)
    coal_SCC <- SCC$SCC[coal_rows]
    total <- tapply(NEI$Emissions[NEI$SCC %in% coal_SCC], NEI$year[NEI$SCC %in% coal_SCC], mean)
    plot(names(total), total,  ylab = "mean(PM2.5) - [tons]", xlab = "Years", main = "Total Emissions - Coal combustion-related", pch = 19)
    lines(names(total),total)
}
