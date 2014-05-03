require(RSQLite)
mysqldb='testsqlite.db'

library(sqldf)
f<-file('bigdf.csv')
bigdf<-sqldf("select * from f",file.format=list(header=T,row.names=F))

# Make a table with it.
m<-dbDriver("SQLite")
con=dbConnect(m,dbname=mysqldb)
dbWriteTable(con,"bigdf",bigdf,overwrite=TRUE,row.names=F)
dbDisconnect(con)

# Lendo a tabela 'bigdf' armazenada em testsqlite.db
read_con=dbConnect(m,dbname=mysqldb)
dbListTables(read_con)
# read_query=dbSendQuery(read_con,"SELECT * FROM bigdf")
read_query=dbGetQuery(read_con,"SELECT * FROM bigdf")
dbGetQuery(read_con,"SELECT dim, avg(fact2) as m2 FROM bigdf group by dim")
dbGetQuery(read_con,"SELECT distinct dim FROM bigdf")
dbGetQuery(read_con,"SELECT count(*) FROM bigdf")

ind<-toString(sample(1:4e5,20))
que<-paste("select * from bigdf where X in (",ind,")")
dbGetQuery(read_con,que)
dbDisconnect(read_con)


read_con=dbConnect(m,dbname=mysqldb)
final<-dbGetQuery(read_con,
	 "SELECT dim,fact1,fact2 FROM bigdf where dim in ('c','f','j')")
dbDisconnect(read_con)



read_con=dbConnect(m,dbname=mysqldb)
read_query=dbGetQuery(read_con,"SELECT * FROM dfab")
dbDisconnect(read_con)


# Conexão com MySQL via RODBC
library(RODBC)
con1<-odbcConnect(dsn='mysql')
sqlTables(con1)
sqlQuery(con1,"select * from final where salario<2000")
sqlQuery(con1,"select distinct departamento from final")
dados<-sqlFetch(con1,"final")
odbcClose(con1)

odbcDataSources()
odbcDriverConnect()

con2<-odbcConnect(dsn='Excel Files')
sqlTables(con2)
dados<-sqlFetch(con2,"charts")
sqlQuery(con2,"select * from \"charts$\" where Close>30 and Volume>8e7")
sqlQuery(con2,"select Date, close from \"charts$\" where day(Date)=12 and
year(Date) in (2002,2003,2006)")
dt<-sqlQuery(con2,"select Year(Date) as ano, volume from \"charts$\" where
		 month(Date) in (10)")
boxplot(log(volume)~ano,data=dt)
dt1<-sqlQuery(con2,"select Year(Date) as ano, month(Date) as mes, 
	avg(close) as avgclose from \"charts$\" group by Year(Date), month(Date)")
plot(dt1$avgclose,type='l')
head(dados)
nrow(dados)
odbcClose(con2)

con2<-odbcConnect(dsn='CSV1')
sqlTables(con2)
dados<-sqlFetch(con2,"comtrade_trade_data")
head(dados)
nrow(dados)



