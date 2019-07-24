# YAGE
Yet Another Georgian Encoder

- **YAGE** is a command line tool for text/xml file converson for classic MS Dynamics NAV client application. Classic client only supports non-unicode code pages. But external interfaces may required common unicode files. Thus the data must be converted accordingly when unloading prior import.

### Command line parameters:
yage.exe TextFile Language Encoding
* TextFile is a source file for conversion. This file will be converted and overwritten.
* Language possible values:
  - **'ge' for Georgian language**
  - **'az' for Azerbaijani language**
* Encoding possible values:
  - **'utf8'**
  - **'unicode' (e.g. UTF16)**
