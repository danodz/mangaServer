function countNew(page)
{
    total = 0;

<<<<<<< HEAD
    workPage = document.createElement("DIV");

    $(workPage).html(page);

    mangas = $(workPage).children("div").children(".mangaDiv");

    for(var i = 0; i < mangas.length; i++)
    {
        if($(mangas[i]).children(".toggleIgnore").attr("onclick").split("'")[3] == "True")
        {
            total += parseInt( $(mangas[i]).children(".mangaPres").attr("class").split(" ")[1] );
        }
    }

    return total;
=======
    workPage = page.split('<body><div><input class="newManga"><button onclick="submitNew()">Submit</button></div><div>')[1].split("(");
    workPage.splice(0,1);
    workPage.map(function(x,y)
    {
        total += parseInt(x.split(")")[0])
    });

    return total
>>>>>>> 6c80267a0aa1d81bc3bce763ff5f9b2cb0002211
}

function update()
{
    ajax = new XMLHttpRequest();
    ajax.open("GET","http://localhost:8000/manga",false);
<<<<<<< HEAD
    try
    {
        ajax.send();
        mangaPage = ajax.responseText
        nbNew = countNew(ajax.responseText);
        chrome.browserAction.setBadgeText({text: String(nbNew)});
    } catch(e)
    {
        mangaPage = "Server is down";
        chrome.browserAction.setBadgeText({text: "----"});
    }
}

mangaPage = "Server is down";

=======
    ajax.send();
    
    nbNew = countNew(ajax.responseText);
    mangaPage = ajax.responseText
    chrome.browserAction.setBadgeText({text: String(nbNew)});
}
page = ""
>>>>>>> 6c80267a0aa1d81bc3bce763ff5f9b2cb0002211
update()
setInterval(update, 180000)
