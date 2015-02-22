function countNew(page)
{
    total = 0;

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
}

function update()
{
    ajax = new XMLHttpRequest();
    ajax.open("GET","http://localhost:8000/manga",false);
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

update()
setInterval(update, 180000)
