set USER_HOME=c:\Users\jlas\workspace
cd /d %USER_HOME%
set JAVA_HOME=C:\Program Files (x86)\Java\jdk1.7.0_45
set CATALINA_HOME=%USER_HOME%\tomcat
set PATH=%PATH%;%USER_HOME%\node;%USER_HOME%\cygwin\bin;%USER_HOME%\svn\bin;%JAVA_HOME%\bin;%USER_HOME%\eclipse_3_6;%USER_HOME%\maven\apache-maven-3.2.3\bin;%USER_HOME%\tomcat\bin;%USER_HOME%\perl\perl\bin;%USER_HOME%\bin;"C:\Program Files (x86)\Python26";"C:\Program Files (x86)\Python26\Scripts";%USER_HOME%\Git\bin;%USER_HOME%\Git\cmd
DOSKEY svnfind=gfind $1 ^| grep -v "\.svn" ^| xargs grep -s -i $2
DOSKEY gruntcli=.\node_modules\.bin\grunt $*
DOSKEY gulpcli=.\node_modules\.bin\gulp $*
DOSKEY svn-clean=perl %USER_HOME%\bin\svn-clean.pl $*
