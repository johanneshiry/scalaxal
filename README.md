# scalaxal an extensible Address Language (xAL) library written in scala.

## References
 
1) The Organization for the Advancement of Structured Information Standards [OASIS], https://www.oasis-open.org/committees/ciq/
2) Extensible Address Language (xAL) Standard Description Document for W3C DTD/Schema Version 2.0

## Packages

The scalaxal library is in 2 major parts:
- 1) package com.scalaxal.xAL, the set of xAL classes
- 2) package com.scalakml.io, the reading and writing of xAL from/to xml

## Documentation

See the OASIS xAL Standard v2.0

- 1) https://www.oasis-open.org/committees/ciq/ciq.html
- 2) https://www.oasis-open.org/committees/ciq/download.html

Also the OASIS xAL Standard v2.0 in pdf is included here in the doc directory.

In addition to the base classes are the helper methods: With(..) and addTo(..). 
These methods return a new object with the specified field name changed. For example:

- AddressDetails() With("addressDetailsType", Some(newAddressLines))

returns a new copy of the AddressDetails object with a new newAddressLines as the addressDetailsType, 
all other fields are the same as before. See the examples.

# Usage

no examples yet

## Status

Tested only on the XAL.XML and XAL_AU.XML example files from OASIS, included here in the xal-files directory.

please contribute

Ringo Wathelet
