This is an application I wrote in Haskell using Happstack.  I didn't write the CouchUtils library (a friend of mine did), but I did the rest.

For now, it requires a bit of manual setup for the database. I'll automatise it someday. You need two databases named "manga" and "chapters". You will have to create the design documents for those two using the json found in the design_*.json files

This project brought me to learn about Haskell, Happstack and couchdb. At the beginning, it was writting html files on disk. I recovered a bit of code and used a server and database. The objective with this project was to reduce the loading time when I read mangas and to get experience with Haskell(It's my first real project with Haskell).

The mangaUpdate directory contains a Google Chrome extension showing how many unread mangas remains. The popup contains the list of manga.

It fetches the images on mangafox.me and I take no credit for them.
