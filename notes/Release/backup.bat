cd D:\MyDocs\spsurvey
xcopy DESCRIPTION L:\Public\tkincaid\spsurvey\DESCRIPTION /d /y
cd data
xcopy *.rda L:\Public\tkincaid\spsurvey\data\*.rda /d /y
cd ..
cd inst
xcopy *.* L:\Public\tkincaid\spsurvey\inst\*.* /d /y
cd ..
cd R
xcopy *.R L:\Public\tkincaid\spsurvey\R\*.R /d /y
cd ..
cd vignettes
xcopy *.* L:\Public\tkincaid\spsurvey\vignettes\*.* /d /y
cd ..
cd Release
xcopy *.* L:\Public\tkincaid\spsurvey\Release\*.* /d /y
cd ..
cd Tests
xcopy *.* L:\Public\tkincaid\spsurvey\Tests\*.* /d /y
pause
