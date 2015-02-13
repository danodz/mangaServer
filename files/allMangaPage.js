function submitNew()
{
    $.ajax("http://localhost:8000/addManga/" + $(".newManga")[0].value);
}

function toggleIgnore(name, value)
{
    $.ajax("http://localhost:8000/toggleIgnore/" + name + "?value=" + value);


    if(value == "False")
    {
        $("." + name + " a.toggleIgnore").html("N'est pas ignoré");
        $("." + name + " a.toggleIgnore").attr("onclick", "toggleIgnore('"+ name +"', 'True')");
    }
    else
    {
        $("." + name + " a.toggleIgnore").html("Ignoré");
        $("." + name + " a.toggleIgnore").attr("onclick", "toggleIgnore('"+ name +"', 'False')");
    }
}
