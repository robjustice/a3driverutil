# A3Driverutil
Python program to convert o65 relocatable binary files for use as Apple /// drivers, plus support for adding/updating and removing drivers from the sos.driver file.

The normal procedure for assembling Apple /// drivers is to load the assembler source file into either an emulator or real Apple II or /// computer. And then use the Pascal assembler to assemble and create the required relocatable PCD object file that the Apple /// System Utilities SCP accepts. I bumped into the o65 relocatable binary format while looking around the internet and then noticed that the ca65 assembler includes support for this. I wondered if this could be used and converted for use in driver development. 

The spec for the o65 file is available here, plus detail on the usages, etc. :
http://6502.org/users/andre/o65/

I've been trying to learn python more, so this seemed like a good excuse to delve into it a bit more. The resultant program has expanded quite a bit on the original idea, and its added a bit of scp functionality to it, eg list, add, delete, update and extract drivers from a SOS.DRIVER file.


# Usage:

The ca65 source file needs the comment in the 'TEXT' segment, and the code in the 'DATA' segment.
An example skeleton source file is shown below:
   
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

once we have the binary, then we can convert it and add to an existing SOS.DRIVER file

   ```
   python -f A3Driverutil.py add test.o65 SOS.DRIVER
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
   C:\python27\python.exe o65convert0.5.py update %FILENAME%.o65 SOS.DRIVER#0c0000
   
   @REM delete and then add updated SOS.DRIVER file to the disk image
   java -jar ac.jar -d %DISKIMAGE% SOS.DRIVER
   java -jar ac.jar -p %DISKIMAGE% SOS.DRIVER SOS $0000 < SOS.DRIVER#0c0000
   
   @REM run the disk image in 
   C:\Storage\_emu\Mess\mess.exe apple3 -rompath C:\Storage\_emu\Mess\roms -skip_gameinfo -resolution 640x480 -window -nothrottle -flop1 %DISKIMAGE%
   ```

