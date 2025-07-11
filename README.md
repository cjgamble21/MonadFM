# MonadFM

Welcome to my pet project!
MonadFM is a purely functional decoder for MP3 audio files written in Haskell.
As of right now, I am only supporting MPEG-1 Layer 3.
I will update this README as I go along as the MP3 decoding process is quite involved.

## Step 1 (MP3 File Parsing)

All MP3 files are composed of many "frames", made up of a header, encoder information, and main data. 
The first part of this step is to find the first valid MP3 frame header, then parse that header into a custom abstraction which we understand as humans.

![MP3 Frame Data](https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/MP3filestructure.svg/2010px-MP3filestructure.svg.png)

This documentation has been incredibly helpful for me thus far:
https://www.datavoyage.com/mpgscript/mpeghdr.htm
