

z88dk C Simple Example using overlays
Copyright (c) Mike Dailly 2025


This is a very simple framework for z88dk to get you going. 
It has a main loop and a "kernal" that stays paged in at $E000 with an IRQ handler


V1.1.0 - 03/11/2025
-------------------
* Added Load(char* pName, uint16 bank, uint16 offset) function - can load >64k
* Added UploadCopper(uint8* pCopper, uint16 length) function - uploads with DMA
* Border(uint8 colour) function - sets ULA border colour
* Added sprite struct
* Updated Copyright message above, "all rights" are no longer reserved due to MIT license.


