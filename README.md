# a3Driverutil
Python program to convert o65 relocatable binary files for use as Apple /// drivers, plus support for adding/updating and removing drivers from the sos.driver file.

The normal procedure for assembling Apple /// drivers is to load the assembler source file into either an emulator or real Apple II or /// computer. And then use the Pascal assembler to assemble and create the required relocatable PCD object file that the Apple /// System Utilities SCP accepts. I bumped into the o65 relocatable binary format while looking around the internet and then noticed that the ca65 assembler includes support for this. I wondered if this could be used and converted for use in driver development. 

The spec for the o65 file is available here, plus detail on the usages, etc. :
http://6502.org/users/andre/o65/

I've been trying to learn python more, so this seemed like a good excuse to delve into it a bit more. The resultant program has expanded quite a bit on the original idea, and its added a bit of scp functionality to it, eg list, add, delete, update and extract drivers from a SOS.DRIVER file.


# Usage
## Convert o65 binary and add/update to a SOS.DRIVER file

To support the conversion process and the o65 format, the ca65 source file needs the comment part in the 'TEXT' segment, and the code part in the 'DATA' segment.
An example skeleton source file is shown below:
(see examples in the Drivers folder)
   
   ```
           .setcpu "6502"
           .reloc
   ;
   ;driver comment field
   ;this is put in the TEXT segment so we can get the length and output it ok
   ;
           .segment "TEXT"
           .word   $FFFF               ;Comment follows..
           .word   12                  ;12 bytes long
           .byte   "Test Driver."
   
   ;
   ;Driver code part
   ;this is put in the DATA segement
   ;
              
           .segment "DATA"
   DIB:    .word   0000                            ; link
           .word   START                           ; entry point
           .byte   05                              ; name len
           .byte   ".TEST          "               ; device name
   
   ........rest of the code
   
           .endproc
   ```

Then we assemble and link this with ca65 and ld65 using the Apple3_o65.cfg file (see files)
   ```
   ca65.exe test.s -l test.lst
   ld65.exe test.o -o test.o65 -C Apple3_o65.cfg
   ```

Once we have the binary, then we can convert it and add/update to an existing SOS.DRIVER file. Note 'add' will check to see if the drivername already exists, and then only add if it does not. Once a driver exists in a SOS.DRIVER file, then the 'update' command can be used. Update will only update if the driver exists. The program uses the driver name from the converted o65 file, so there is no need to specify it on the command line.

   ```
   python a3Driverutil.py add test.o65 SOS.DRIVER
   ```

Then we can use the disk util of choice to add to a dsk image and run in an emulator or a real machine.


I have used a windows batch file to automate this process to enable quick driver testing, an example of mine is shown here:
(i'm a windows user, so you can adapt as required)

   ```
   @REM Driver make/update
   @REM Variables
   SET FILENAME=grafix
   SET DISKIMAGE=C:\Storage\Projects\o65\batchtest\grafixtest.dsk
   
   @REM assemble and link source file
   ca65.exe %FILENAME%.s -l %FILENAME%.lst
   ld65.exe %FILENAME%.o -o %FILENAME%.o65 -C Apple3_o65.cfg
   
   @REM extract existing SOS.DRIVER file from disk image
   java -jar ac.jar -g %DISKIMAGE% SOS.DRIVER > SOS.DRIVER#0c0000
   
   @REM convert the o65 and update existing driver in the SOS.DRIVER file
   C:\python27\python.exe A3Driverutil.py update %FILENAME%.o65 SOS.DRIVER#0c0000
   
   @REM delete and then add updated SOS.DRIVER file to the disk image
   java -jar ac.jar -d %DISKIMAGE% SOS.DRIVER
   java -jar ac.jar -p %DISKIMAGE% SOS.DRIVER SOS $0000 < SOS.DRIVER#0c0000
   
   @REM run the disk image in Mess
   C:\Storage\_emu\Mess\mess.exe apple3 -rompath C:\Storage\_emu\Mess\roms -skip_gameinfo -resolution 640x480 -window -flop1 %DISKIMAGE%
   ```
Command line syntax for these Options:

   ```
   usage: a3Driverutil.py add o65file sosfile

   positional arguments:
     o65file     Input o65 code file to be converted
     sosfile     SOS.DRIVER file to add driver to (driver must not already exist)
     
   usage: a3Driverutil.py update o65file sosfile

   positional arguments:
     o65file     Input o65 code file to be converted
     sosfile     SOS.DRIVER file to be updated (an existing driver will be updated)
```

## Convert o65 binary and output as driver binary file
This converts the o65 binary and outputs as a binary file with comment length, comment, code length, code, reloc length and reloc table. This is the same output format as the extract command.

   ```
   usage: a3Driverutil.py bin o65file binfile

   positional arguments:
     o65file     Input o65 code file to be converted
     binfile     Binary output file
   ```

## Convert o65 binary and output as SOS.DRIVER file
This converts the o65 binary and outputs as a SOS.DRIVER that contains just one driver. The program adds a full SOS.DRIVER file header structure, ie 'SOS DRVR' and dummy char set and keyboard map. This allows the file to be loaded as a secondary driver file with SCP. ie read in a full SOS.DRIVER file with SCP, and then read this one in as a secondary one to add to the other drivers. 

   ```
   usage: a3Driverutil.py sos o65file sosfile

   positional arguments:
     o65file     Input o65 code file to be converted
     sosfile     SOS.DRIVER Binary output file
```

## List Drivers in a SOS.DRIVER file
This will list the drivers contained in a SOS.DRIVER file. 
Now prints the driver comment field.

   ```
   usage: a3Driverutil.py list sosfile

   positional arguments:
     sosfile     SOS.DRIVER file to list drivers in
  
  Example:
   A3Driverutil.py list SOS.DRIVERtdm#0c0000
   DriverName        Status    Slot  Unit Manid Release Comment
   .AUDIO            active    N/A   00   0001  1000    Audio Driver --  Copyright Apple Computer, Inc. 1980
   .FMTD1            active    N/A   00   0001  1300    Disk /// Formatter Driver -- Copyright Apple Computer, Inc. 1980-83
     .FMTD2          active    N/A   01   0001  1300      ^Sub device
     .FMTD3          inactive  N/A   02   0001  1300      ^Sub device
     .FMTD4          inactive  N/A   03   0001  1300      ^Sub device
   .RS232            active    N/A   00   0001  1300    Built-in Serial Port RS-232 Driver -- Copyright Apple Computer, Inc. 1981-83
   .PARPRINTER       active    1     00   0001  1300    Parallel Printer Driver -- Copyright (C) 1983 by Apple Computer Inc.
   .SILENTYPE        active    N/A   00   0001  1040    Silentype driver - Copyright Apple Computer, Inc. 1980-83
   .PROFILE          active    1     00   4453  1000    Apple /// CFFA3000 (Compact Flash For Apple 3000) Driver by David Schmidt 2011
     .CFFA3000D2     active    1     01   4453  1000      ^Sub device
     .CFFA3000D3     active    1     02   4453  1000      ^Sub device
     .CFFA3000D4     active    1     03   4453  1000      ^Sub device
     .CFFA3000D5     active    1     04   4453  1000      ^Sub device
     .CFFA3000D6     active    1     05   4453  1000      ^Sub device
     .CFFA3000D7     active    1     06   4453  1000      ^Sub device
     .CFFA3000D8     active    1     07   4453  1000      ^Sub device
   .CONSOLE          active    4     00   0001  1310    Console Driver -- Copyright Apple Computer, Inc. 1980-83
     .MOUSE          active    4     01   5555  2310      ^Sub device
   .DESKTOPMANAGER   active    N/A   00   3333  1400    The Desktop Manager -- (c) 1985-1986 ON THREE Inc. Written By ROB TURNER
   .GRAFIX           active    N/A   00   0001  1301    Copyright (C) 1983 Apple Computer, Inc.  Graphics Driver.
   .GRAFIX           active    N/A   00   0001  1301    Copyright (C) 1983 Apple Computer, Inc.  Graphics Driver.
   .GRAFIX           active    N/A   00   0001  1301    Copyright (C) 1983 Apple Computer, Inc.  Graphics Driver  .

   Total size:  53134
```
The list also displays the total size of the SOS.DRIVER file. This program does not do any specific size checking currently, so you will need to keep an eye on this. I think I have read that SOS will work with up to around 60k, but i think the System Utils will not allow you to create one this big.  

## Add driver to SOS.DRIVER file
This adds the complete driver binary block of data to the SOS.DRIVER file. This file includes the comment length, comment, code length, code, relocation length and relocation data. The driver can be extracted from another SOS.DRIVER file with the 'extract' command.

   ```
   usage: a3Driverutil.py addbin driverbinfile sosfile

   positional arguments:
     driverbinfile  Name of driver binary file to be added
     sosfile        SOS.DRIVER file to add the driver to
```


## Extract driver from SOS.DRIVER file
This extracts the complete driver block of data from the SOS.DRIVER file and outputs as one binary file. This includes the comment length, comment, code length, code, relocation length and relocation data. This can be added to another SOS.DRIVER file with the 'addbin' command.

   ```
   usage: a3Driverutil.py extract drivername sosfile

   positional arguments:
     drivername  Name of driver to be extracted (include . eg: ".console"
     sosfile     SOS.DRIVER file to extract the driver from
```

## Extract driver code from SOS.DRIVER file
This extracts just the code for a specified driver from the SOS.DRIVER file, and then relocates it to $2000 base address. This is for use when disassembling a driver as there is no ambiguity with the zero page as there would be if the base address was $0000. You can then use your disassembler of choice to disassemble the code block.

   ```
   usage: a3Driverutil.py extractcode drivername sosfile

   positional arguments:
     drivername  Name of driver to be extracted (include . eg: ".console"
     sosfile     SOS.DRIVER file to extract the driver from
```

## Delete driver code from the SOS.DRIVER file
This allows a driver to be deleted from a SOS.DRIVER file.

   ```
   usage: a3Driverutil.py delete drivername sosfile

   positional arguments:
     drivername  Name of driver to be deleted (include . eg: ".console"
     sosfile     SOS.DRIVER file to delete the driver from
  ```

## Set driver slot in a SOS.DRIVER file
This allows the slot to be set for a driver in a SOS.DRIVER file.

   ```
   usage: a3Driverutil.py slot slotnumber drivername sosfile

   positional arguments:
     slotnumber  New slot number
     drivername  Name of driver to be updated (include . eg: ".console"
     sosfile     SOS.DRIVER file to update the driver slot
  ```
