function submitNew()
{
    $.ajax("http://localhost:8000/addManga/" + $(".newManga")[0].value);
}
