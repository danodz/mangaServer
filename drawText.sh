#convert favicon.png -font /System/Library/Fonts/Helvetica.dfont text:- "1" testImg.png
convert /Users/Erwan/projects/haskell/mangaServer/iconBase.png -font /System/Library/Fonts/Helvetica.dfont -gravity Center -pointsize 16 -annotate +0+2 $1 /Users/Erwan/projects/haskell/mangaServer/favicon.ico
