<!-- Build instruction for SpiderMonkey45 for Windows to use with SyNode -->

##Preparation
* Download and install **MozillaBuild**. See instruction here [MozillaBuild](https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions/Windows_Prerequisites#mozillabuild)

* Get Mozilla Source Code from here https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Releases/45

* Apply patches.(todo: make patch)
 
## Build SpiderMonkey 45
Follow instruction from [Mozilla Build Documentation](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Build_Documentation)
The valid options for configure is:

	../configure --enable-ctypes --disable-jemalloc

## Minimize a library size
You can minimize a icu56 library size by customizing a languages included in the data file icudt56.dll

It can be done using [ICU Data Library Customizer](http://apps.icu-project.org/datacustom/ICUData56.html)

Use a Advanced Options in the bottom of page to filter and deselect intems you not required
Our distribution include icudt56.dll WITHOUT
 - Urdu
 - Chinese
 - Japanese
 - Korean 
 - Zulu
 - Vietnamese