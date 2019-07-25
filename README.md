# YAGE
Yet Another Global Encoder

- **YAGE** is a command line tool for text/xml file converson for classic MS Dynamics NAV client application. Classic client only supports non-unicode code pages. But external interfaces may required common unicode files. Thus the data must be converted accordingly when unloading prior import.

### Command line parameters:
Usage: yage.exe SourceFile [keys]

### Keys:
* [/enc:Encoding] - file encoding
valid values:
  - **"utf8" (default value)**
  - **"unicode"**
  
* [/lang:Language] - destination file language
valid values:
  - **"ge" (Georgian, default value)**
  - **"az" (Azerbaijanian)**
  
* [/file:ResultFileName] - result file name
* [/log] - show execution details

### Downloads:
- [Latest release](https://github.com/incanav/yage/releases/latest)
