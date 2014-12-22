function countNew(page)
{
    total = 0;

    workPage = page.split('<body><div><input class="newManga"><button onclick="submitNew()">Submit</button></div><div>')[1].split("(");
    workPage.splice(0,1);
    workPage.map(function(x,y)
    {
        total += parseInt(x.split(")")[0])
    });

    return total
}

function update()
{
    ajax = new XMLHttpRequest();
    ajax.open("GET","http://localhost:8000/manga",false);
    ajax.send();
    
    nbNew = countNew(ajax.responseText);
    mangaPage = ajax.responseText
    chrome.browserAction.setBadgeText({text: String(nbNew)});
}
page = ""
update()
setInterval(update, 180000)
