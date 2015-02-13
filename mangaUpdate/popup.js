page = chrome.extension.getBackgroundPage().mangaPage

<<<<<<< HEAD
if(page == "Server is down")
{
    document.write("<div>Server is down</div>");
}
else
{
    document.write(page);

    link = document.createElement("LINK");
    $(link).attr("href", "popup.css");
    $("head").append(link);
    
}

document.onreadystatechange = function(state)
{
    if(document.readyState == 'complete')
    {
        $($("div")[0]).html("");
        $(".toggleIgnore").html("");

        links = $("a");
        links.attr("target", "_blank")

        for(var i = 0; i < links.length; i++)
        {
            $(links[i]).attr("href", "http://localhost:8000" + $(links[i]).attr("href"))
        }

        divHeight = $(".mangaDiv").height();
        $(".mangaDiv").width("200px");
=======
page = page.split("<head>").join('<link href="popup.css" rel="stylesheet" type="text/css">')

page = page.split('<div><input class="newManga"><button onclick="submitNew()">Submit</button></div><div>').join("").split("<a href=\"").join("<a target='_blank' href=\"http://localhost:8000");

document.write(page);

document.onreadystatechange = function(state)
{
    console.log()
    if(document.readyState == 'complete')
    {
        divHeight = document.getElementsByTagName("div")[0].clientHeight;
>>>>>>> 6c80267a0aa1d81bc3bce763ff5f9b2cb0002211
        
        document.getElementsByTagName("html")[0].setAttribute("style",  "height:" + divHeight + "px");
        document.getElementsByTagName("body")[0].setAttribute("style", "height:" + divHeight + "px");
    }
};
