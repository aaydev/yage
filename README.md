# YAGE
Yet Another Global Encoder

- **YAGE** is a command line tool for text/xml file converson for classic MS Dynamics NAV client application. Classic client supports non-unicode code pages only. External interfaces may required common unicode files. Thus the data must be converted accordingly when unloading prior import.

### Command line parameters:
Usage: yage.exe SourceFile ConversionTableFile [keys]

### Keys:
* [/from:Encoding] - source file encoding
valid values:
  - **"oem" (default value), "utf8", "unicode"**
  
* [/to:Encoding] - destination file encoding
valid values:
  - **"oem", "utf8" (default value), "unicode"**
  
* [/file:ResultFileName] - destination file name
* [/excel] - use COM to convert Excel (MS Excel must be installed on the target PC!)
* [/log] - show execution details

### Example:
yage.exe EXAMPLE.xlsx GE.txt /excel /log
- MS Excel file "EXAMPLE.xlsx" will be converted from OEM to UTF8 encoding using mapping of Georgian characters with add. logging

### Downloads:
- [Latest build](https://github.com/zxrepo/aaydev.yage/releases/latest)
