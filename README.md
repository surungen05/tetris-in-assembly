This is an attempt at rewriting the original tetris (written by Alexey Pajitnov in 1984) in assembly by a 2nd year computer engineering student for a school project. Thank you to @mkh2097 for the awesome code. 
link to the mentioned code, also in the description: https://github.com/mkh2097/Tetris-Assembly-8086/blob/main/tetris.asm

How to Run: (also described in the link)
1. Download DOSBox and emu8086 if you do not have them.
   links to download DOSBox and emu8086:
   - emu8086: https://en.softonic.com/download/emu8086-microprocessor-emulator/windows/post-download
   - DOSBox: https://sourceforge.net/projects/dosbox/
2. Copy and paste the code to a new emu8086 file to compile or skip this step by downloading the exe file.
3. To run the .exe file, enter the following to DOSBox:
   - mount [drive] [pathtoexefile]
   - [drive]:
   - (optionally) cd [repositoryfortheexefile]
   - [file].exe
   Example:
   - mount c c:\emu8086
   - c:
   - cd mybuild
   - tetris.exe
     
No score implementation yet. Hopefully in the future.
