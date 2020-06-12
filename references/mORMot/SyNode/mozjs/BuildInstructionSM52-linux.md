** CentOS / Ubuntu notes** If you need to use libmozjs-52 on bot CentOS / Ubuntu distribution we recommend to compile it on CentOS,
because Ubintu 16.04 LTS have newer version of glibc (3.4.21)

Before proceeding Spider Monkey build pease consider reading general
[Mozilla sources build instructuions](https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions)
and author's recommended approach to [build Spider Monkey](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Build_Documentation).

You may also find usefull following hiperlinks in the documents and read more about Mozilla sources in general.
Basic knowledge of C/C++ language and linux build tools whould be helpfull when reading this documentation.

The "zero" step when building Mozilla sources is to prepare build box. Do this according to the 
[Mozilla Linux_Prerequisites documentation](https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions/Linux_Prerequisites).
It worth to note that this instruction has been checked against Ubuntu GNOME v16.04. You may find some differences building Mozilla sources on another version of Linux.

At the moment of writing this instruction there was no official release of Spider Monkey v52.
At a later time you whould be able to find official sources of this version at [SpiderMonkey Releases page](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Releases).

Currently it is possible to download [nightly build from official repository](https://hg.mozilla.org/releases)
 or use source control tools  to retrive sources from any of the Mozilla source control repositories.
The author recommends to use git as a tool to take sources from GitHub.
Issue the following command in a terminal window to clone GitHub repository:

```bash
git clone https://github.com/mozilla/gecko-dev.git
```

Be prepared for long wait - cloning the repository may take up to half an hour or so depending on build box hardware and the Internet connection speed.
This retrieves all Mozilla sources. A few different programs can be built using this sources like FireFox or Thunder Bird.
But there is no way to retrive only part of the sources needed to build Spider Monkey only.
Upon finish dive into the root sources directory and checkout branch esr52. Please note - you should _not_ use master branch to build Spider Monkey v52!

```bash
cd gecko-dev
git checkout esr52
```

Now it's time to apply patch to the sources provided by SyNode:

```
git apply <mORMot_sources_root>/SyNode/mozjs/esr52-git.patch
```

Spider Monkey sources are located at "js\src":

```
cd js/src
```

Next run autoconfigure comand:

```
autoconf2.13
```

The next step is to crate the directory for build artifacts. It is recommended to use _OPT.OBJ directory for that (exact spelling),
it's due to current state of .gitignore file.

```
mkdir _OPT.OBJ
cd _OPT.OBJ
```

After that configure sources. This should be done by issuing the following command from the build artifacts directory:

```
../configure --enable-ctypes --disable-jemalloc --enable-nspr-build --disable-debug-symbols
```

The options stated should be used to successfully build and run SyNode.

And now run build itself:

```
make
```

Upon successfull build go to dist/bin subdirectory and take libmozjs-52.so library.
To avoid conflicts with a packaged version of this library the name have to be changed, but issuing mv command to rename the file is not enough.
Patchelf utility not less than version 0.9 should be used to change internal SONAME field:

```
patchelf --set-soname libsynmozjs52.so libmozjs-52.so
mv libmozjs-52.so libsynmozjs52.so
```

It is recommended to copy resulting libsynmozjs52.so file to `/usr/lib` directory
