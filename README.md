# A3Driverutil
Python program to convert o65 relocatable binary files for use as Apple /// drivers, plus support for adding/updating and removing drivers from the sos.driver file.

The normal procedure for assembling Apple /// drivers is to load the assembler source file into either an emulator or real Apple II or /// computer. And then use the Pascal assembler to assemble and create the required relocatable PCD object file that the Apple /// System Utilities SCP accepts. I bumped into the o65 relocatable binary format while looking around the internet and then noticed that the ca65 assembler includes support for this. I wondered if this could be used and converted for use in driver development. 

The spec for the o65 file is available here, plus detail on the usages, etc. :
http://6502.org/users/andre/o65/

I've been trying to learn python more, so this seemed like a good excuse to delve into it a bit more. The resultant program has expanded quite a bit on the original idea, and its added a bit of scp functionality to it, eg list, add, delete, update and extract drivers from a SOS.DRIVER file.


# Usage
## Convert o65 binary and add/update to a SOS.DRIVER file

To support the conversion procoess and, the ca65 source file needs the comment in the 'TEXT' segment, and the code in the 'DATA' segment.
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

Then we assemble and link this with ca65 and ld65 using the Apple3_o65.cfg file (see src folder)
   ```
   ca65.exe test.s -l test.lst
   ld65.exe test.o -o test.o65 -C Apple3_o65.cfg
   ```

once we have the binary, then we can convert it and add to an existing SOS.DRIVER file. Note 'add' will check to see if the drivername already exists, and then only add if it does not. Once a driver exists in a SOS.DRIVER file, then the 'update' command can be used. 

   ```
   python A3Driverutil.py add test.o65 SOS.DRIVER
   ```

Then we can use the disk util of choice to add to a dsk image and run in an emulator or a real machine.


I have used a windows batch file to automate this process to enable quick driver testing, example of mine is shown here:

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

## List Drivers in a SOS.DRIVER file
This will list the drivers contained in a SOS.DRIVER file.

   ```
   A3Driverutil.py list SOS.DRIVERtdm#0c0000
   DriverName        Status     Slot   Unit   Manid  Release
   .GRAFIX           active     N/A     00     0001   1300
   .AUDIO            active     N/A     00     0001   1000
   .FMTD1            active     N/A     00     0001   1300
     .FMTD2          active     N/A     01     0001   1300
     .FMTD3          inactive   N/A     02     0001   1300
     .FMTD4          inactive   N/A     03     0001   1300
   .RS232            active     N/A     00     0001   1300
   .PARPRINTER       active       1     00     0001   1300
   .SILENTYPE        active     N/A     00     0001   1040
   .PROFILE          active       1     00     4453   1000
     .CFFA3000D2     active       1     01     4453   1000
     .CFFA3000D3     active       1     02     4453   1000
     .CFFA3000D4     active       1     03     4453   1000
     .CFFA3000D5     active       1     04     4453   1000
     .CFFA3000D6     active       1     05     4453   1000
     .CFFA3000D7     active       1     06     4453   1000
     .CFFA3000D8     active       1     07     4453   1000
   .CONSOLE          active       4     00     0001   1310
     .MOUSE          active       4     01     5555   2310
   .DESKTOPMANAGER   active     N/A     00     3333   1400
   
    Total size:  42496
```

## Extract driver code from SOS.DRIVER file
I added this to allow a driver to be extracted from a SOS.DRIVER file. There are two options available here.
### 1. extract
This extracts the complete driver block of data from the SOS.DRIVER file and outputs as one file. This includes the comment length, comment, code length, code, relocation length and relocation data. This is more for future use, maybe i need to add a way to add this back into another sos.driver.

### 2. extractcode
This one extracts just the code for a specified driver, and then relocates it to $2000 base address. This is for use when disassembling a driver as there is no ambiguity with the zero page as there would be if the base address was $0000. You can then use your disassembler of choice to disassemble the code block.

