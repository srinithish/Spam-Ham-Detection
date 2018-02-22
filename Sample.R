
a = file.choose()
print(paste("path is ",a))

require(RDCOMClient)
require(RDCOMEvents)

xlHandle = COMCreate("Excel.Application")


wb = xlHandle$Workbooks()$Open("M//Data analytics//Test//TestWB.xls")
# library(`RDCOMEvents_0.3-1`)
