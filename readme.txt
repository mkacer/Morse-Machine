About Morse Machine
===================

About the Software
------------------

Morse Machine is a Windows implementation of "A Fully Automatic Morse Code Teaching Machine" first described in a May 1977 QST article of the same name by Ward Cunningham, that was written by Julian Moss, G4ILO.

Versions of this program written by Jim Wilson are available on Ward Cunningham's site: http://c2.com/morse/. The Windows version on this site is really a DOS program written in C that uses portable libraries to create a graphical interface. Due to some quirk of the way it is written, the program runs the computer CPU at 100% which causes the fan to come on all the time, creating a noisy distraction.

Therefore this version has been written as a native Windows program using Lazarus, an open source clone of Borland Delphi that is based on Free Pascal. This implementation is based on the first portable version of "A Fully Automatic Morse Code Teaching Machine". It is released under the GNU General Public License Version 2 (see the file license.txt). Consequently a copy of the program source code has been included in the archive source.zip. If you have no interest in making your own modifications to this program then you may ignore or even delete this file.

Using Morse Machine
-------------------

This program teaches you to receive Morse code by sending a character and waiting for you to type what you heard on the keyboard. If you don't know the character, it prints it and sends it again after a short pause. If you get it wrong, it prints it and sends it again. The program grades your score and adds new characters when it sees that you are ready.

If you are completely new to the code, look at the screen and wait for the program to send a character and type it. This will teach you the sounds of the characters. Once you know the code, don't look at the screen during practise unless you're stuck and the program keeps sending the character over and over and you need to see what the problem is.

The characters are taught in a special order so that you learn characters that sound similar and are easily confused together. The minimum speed the program sends each character at is equivalent to 20wpm. This is so you get used to hearing the sound of each character instead of counting the dots and dashes. Even if you're starting to learn code, consider increasing the speed to 25 or 30wpm. It makes little difference to the initial learning curve, but will make it easier to achieve a higher speed once you're proficient.

Try to do some practise every day. Practise for as long as you can, but stop when you are feeling tired and start to make more mistakes, then rest for a few minutes.