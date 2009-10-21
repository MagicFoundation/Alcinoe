You can download the latest version available at
http://sourceforge.net/projects/alcinoe/

you can access the last svn version at:
svn co https://alcinoe.svn.sourceforge.net/svnroot/alcinoe alcinoe 
or with a web browser at:
http://alcinoe.svn.sourceforge.net/viewvc/alcinoe/

Global Ignore pattern of tortoise svn: 
__history *.exe *.dll *.dcu *.identcache *.local *.res

also in svn config.ini add this :

enable-auto-props = yes

[auto-props]
*.dproj = svn:mime-type=application/octet-stream
*.groupproj = svn:mime-type=application/octet-stream