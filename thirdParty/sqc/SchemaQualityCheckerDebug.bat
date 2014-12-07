@echo off
set OLDCLASSPATH=%CLASSPATH%
set STYLESHEET=.
set XML4J=xml4j.jar
set MOF=mofrt.jar
set REGEX=regex4j.jar
set XSCHEMA=xschemaREC.jar
set BASE64DECODER=mail.jar
set XERCES=xerces.jar
set parameters=%1
:begin
shift
if "%1"=="" goto end
set parameters=%parameters% %1
goto begin
:end
set CLASSPATH=.;%XSCHEMA%;%XERCES%;%XML4J%;%MOF%;%REGEX%;%BASE64DECODER%;%STYLESHEET%
jdb -classpath %CLASSPATH% com.ibm.sketch.util.SchemaQualityChecker %parameters%
set CLASSPATH=%OLDCLASSPATH%