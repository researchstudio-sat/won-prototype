function selectText(element) {
    var doc = document;
    var text = doc.getElementById(element);

    if (doc.body.createTextRange) { // ms
        var range = doc.body.createTextRange();
        range.moveToElementText(text);
        range.select();
    } else if (window.getSelection) { // moz, opera, webkit
        var selection = window.getSelection();
        var range = doc.createRange();
        range.selectNodeContents(text);
        selection.removeAllRanges();
        selection.addRange(range);
    }
}

function shareFacebook() {
    window.location = "http://www.facebook.com/sharer/sharer.php?u=" + encodeURIComponent($("#shareLink").text()) + "&t=" + encodeURIComponent($("#postTitle").text());
}

function shareTwitter() {
    window.location = "http://twitter.com/share?url=" + encodeURIComponent($("#shareLink").text()) + "&text=" + encodeURIComponent($("#postTitle").text()) + "&via=WON&related=WON"
}

