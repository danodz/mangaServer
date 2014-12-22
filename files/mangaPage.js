$(function()
{
    boxes = $(".readState");
    previousBox = 0;

    checked = boxes.filter( function(x,y){ return y.checked; } );
    if(checked.length != 0)
    {
        checked[0].scrollIntoView();
        scrollBy(0, -25);
    }
    else
    {
        scrollTo(0,document.body.scrollHeight);
    }

    readState = [];
    for(var i = 0; i < boxes.length; i++)
    {
        readState.push([boxes[i], boxes[i].checked]);
    }
    
    boxes.on("click", function(e)
    {
        currentBox = boxes.index($(e.currentTarget));

        if(currentBox < previousBox)
        {
            topBox = currentBox;
            bottomBox = previousBox;
        }
        else
        {
            topBox = previousBox;
            bottomBox = currentBox;
        }

        if(e.shiftKey)
        {
            for(var i = topBox; i <= bottomBox; i++)
            {
                boxes[i].checked = boxes[previousBox].checked;
            }
        }
                
        
        previousBox = currentBox;
    });
});

function submitReadChange()
{
    newState = "["

    for(var i = 0; i < boxes.length; i++)
    {
        if(boxes[i].checked != readState[i][1])
            newState += '("' + $( boxes[i] ).siblings('a').html() + '",' + boxes[i].checked + '),';

        readState[i] = [boxes[i], boxes[i].checked];
    }

    newState = newState.slice(0,-1).replace(/,t/g,",T").replace(/,f/g,",F") + "]";
    $.ajax("http://localhost:8000/changeRead?changeList=" + newState);
}
