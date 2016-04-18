D:\DEVELOP\ALP\ALP DRVSTUB.ASM +Sfs | more
pause
link386 /E:2 /M:full /SE:10 /W drvstub D:\DEVELOP\VPASCAL\OUT.OS2\UNITS\DRVSTUB1, drvstub.sys,,os2386 D:\DEVELOP\VPASCAL\OUT.OS2\UNITS\DRVBASE D:\DEVELOP\VPASCAL\OUT.OS2\UNITS\RMBASE D:\DEVELOP\VPASCAL\OUT.OS2\UNITS\MACBASE D:\DEVELOP\VPASCAL\OUT.OS2\UNITS\USBBASE,drvstub | more
mapsym drvstub.map
copy drvstub.sys c:\ibmcom\macs\mcs7830.os2
copy drvstub.sym c:\ibmcom\macs\mcs7830.sym
