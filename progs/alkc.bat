Move .\Object\%1.Obj .
link  -subsystem:console -entry:mainCRTStartup -out:%1.exe %1.obj libc.lib kernel32.lib -map:%1.map
Del %1.Obj
Rem Del %1.Map
Rem The first is for Quadpower, the second is for Homepower
Copy %1.Exe \RRS\Docs
Copy %1.Exe \RRS-Docs
Move %1.Exe ..\Source
Move .\Object\%1.Dbg ..\Source
