page = chrome.extension.getBackgroundPage().mangaPage

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
        
        document.getElementsByTagName("html")[0].setAttribute("style",  "height:" + divHeight + "px");
        document.getElementsByTagName("body")[0].setAttribute("style", "height:" + divHeight + "px");
    }
};
