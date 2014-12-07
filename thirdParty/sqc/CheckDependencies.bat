@echo off
set OLDCLASSPATH=%CLASSPATH%
set COMMON=..\common\
set STYLESHEET=%COMMON%
set XML4J=%COMMON%xml4j.jar
set MOF=%COMMON%mofrt.jar
set REGEX=%COMMON%regex4j.jar
set XSCHEMA=%COMMON%xschemaREC.jar
set BASE64DECODER=%COMMON%mail.jar
set XERCES=%COMMON%xerces.jar
set parameters=%1
:begin
shift
if "%1"=="" goto end
set parameters=%parameters% %1
goto begin
:end
set CLASSPATH=.;%COMMON%;%XSCHEMA%;%XERCES%;%XML4J%;%MOF%;%REGEX%;%BASE64DECODER%;%STYLESHEET%;
java -classpath %CLASSPATH% com.ibm.sketch.utilities.ExecutionDependencyChecker %parameters%
set CLASSPATH=%OLDCLASSPATH%