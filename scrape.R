#
#
# Author: Darshan M.S.
#
#

library(XML)
library('openNLP')
library('stringr')
links=NULL
fileConn<-file("links.txt")
for( i in 1 :23 )
{
	prefix<-'http://www.moneycontrol.com/stocks/company_info/stock_news.php?sc_id=IT&pageno='
	suffix<-'&prev=1&durationType=Y&Year=2012&duration=1&news_type='
	url1<-paste(prefix,i,suffix,sep="")
#url1<-'http://www.moneycontrol.com/stocks/company_info/stock_news.php?sc_id=IT&pageno=1&prev=1&durationType=Y&Year=2012&duration=1&news_type='
	doc.html = htmlTreeParse(url1,useInternal = TRUE)
	doc.text = unlist(xpathApply(doc.html, "//a[@href]", xmlGetAttr, "href"))
	a<-grep("^/news/business/",doc.text)
	b<-grep("^/news/stocksviews",doc.text)
	#doc.text[a]
	#doc.text[b]
	li<-c(doc.text[a],doc.text[b])
	li<-li[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)] 
	links<-c(links,li)
	writeLines(links, fileConn,sep="\n")
}
#doc.text = unlist(xpathApply(doc.html, "//p[@class='PT3 a_10dgry']", xmlValue))   #dates from the list of articles. Dont need this.
fil<-file("articles.txt")
fileConn<-file("Sentiments.txt")
links<-scan('links.txt', what='character', comment.char=';')
pos<-scan('positive.txt', what='character', comment.char=';')
neg<-scan('negative.txt',what='character', comment.char=';')
prefix="http://www.moneycontrol.com"
result=NULL
tex=NULL
for( i in 1 : length(links))
{
	url2<-paste(prefix,links[i],sep="")
	#print(url2)
	print(i)
	#url2<-'http://www.moneycontrol.com/news/business/mid-tier-it-firms-outperform-big-boys2012_801086.html'
	doc.html <- htmlTreeParse(url2,useInternal = TRUE)
	doc.date <-unlist(xpathApply(doc.html, "//p[@class='gD_12']", xmlValue))   #date from the article	
	doc.date<-format(as.Date(tolower(doc.date), format='%b %d, %Y'), format='%d/%m/%Y')  #date variable
	#doc.date<-unclass(as.POSIXct(doc.date, "%d/%m/%Y",tz="UTC")) # get POSIX value.
	#print(doc.date[1])
	doc.text <- unlist(xpathApply(doc.html, "//div[@class='gD_15n FL']", xmlValue)) # get articles from the Website
	d<- unlist(xpathApply(doc.html, "//div[@class='gD_15n PT25 FL']", xmlValue))
	
	#writeLines(doc.text, fil,sep="\n")
	doc.text<-c(doc.text,d)
	tex<-paste(tex,doc.text,sep="\n\t\t#####END OF ARTICLE###\n")
	doc.sent<-sentDetect(doc.text)
	
	doc.sent = gsub('[[:punct:]]', '', doc.sent)
	doc.sent = tolower(doc.sent)
	word.list = str_split(doc.sent, '\\s+')
	words = unlist(word.list)
	#print(words)
	pos.matches = match(words, pos)
	neg.matches = match(words, neg)
	pos.matches = !is.na(pos.matches)
	neg.matches = !is.na(neg.matches)
	score = sum(pos.matches) - sum(neg.matches)
	#print(sum(pos.matches))
	score<-score/(sum(pos.matches) + sum(neg.matches))
	print(score)
	if(!is.nan(score))
	{
		pa<-paste(doc.date,score)
		result<-c(result,pa)
	}
}	
write(tex,fil,sep="\n")
write(result,fileConn,sep="\n")





