This is an application I wrote in Haskell using Happstack. It requires a running couchdb with a database named "manga". I didn't write the CouchUtils library (a friend of mine did), but I did the rest.
This project brought me to learn about Haskell, Happstack and couchdb. At the beginning, it was writting html files on disk. I recovered a bit of code and used a server and database. The objective with this project was to reduce the loading time when I read mangas and to get experience with Haskell(It's my first real project with Haskell).

The mangaUpdate directory contains a Google Chrome extension showing how many unread mangas remains. The popup contains the list of manga.

It fetches the images on mangafox.me and I take no credit for them.
