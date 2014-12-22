page = chrome.extension.getBackgroundPage().mangaPage

page = page.split("<head>").join('<link href="popup.css" rel="stylesheet" type="text/css">')

page = page.split('<div><input class="newManga"><button onclick="submitNew()">Submit</button></div><div>').join("").split("<a href=\"").join("<a target='_blank' href=\"http://localhost:8000");

document.write(page);

document.onreadystatechange = function(state)
{
    console.log()
    if(document.readyState == 'complete')
    {
        divHeight = document.getElementsByTagName("div")[0].clientHeight;
        
        document.getElementsByTagName("html")[0].setAttribute("style",  "height:" + divHeight + "px");
        document.getElementsByTagName("body")[0].setAttribute("style", "height:" + divHeight + "px");
    }
};
