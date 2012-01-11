/*
 * Javascript for classifying spam.
 */

(function () {

    function classify (as, comment_id) {
        $.post("/comments/spam/classify", { as: as, comment_id: comment_id }, removeClassified, "json");
    }

    function removeClassified (data, textStatus) {
        var toRemove = findComment(data.comment_id);
        var header = toRemove.prevAll('.category-header').first();
        toRemove.remove();
        setHeaderCount(header);
        selectFirst();
    }

    function explain (comment_id) {
        var comment = findComment(comment_id);
        if (comment.children('.spam-explanation').length == 0) {
            $.post("/comments/spam/explain", { comment_id: comment_id }, explainer(comment), "html");
        } else {
            comment.children('.spam-explanation').remove();
        }
    }

    function explainer (comment) {
        return function (data, textStatus) {
            comment.append($(data).click(function () { $(this).remove(); }));
        };
    }

    function selectedID () {
        return $('.selected').find('.comment-id').first().text();
    }

    function findComment (comment_id) {
        return $('.comment').filter(function (index) {
            return $(this).find('.comment-id').first().text() === comment_id;
        });
    }

    function setHeaderCount (h) {
        var count = $(h).nextUntil('.category-header').length;
        if (count == 0) {
            $(h).remove();
        } else {
            $(h).children('.category-count').remove();
            var c = count == 1 ? " comment" : " comments";
            $(h).append($('<span>').text(" (" + count + c + ")").addClass('category-count'));
        }
    }

    function setCounts () {
        $.map($('.category-header'), setHeaderCount);
    }

    function keyDispatcher (e) {
        if (e.which == ' '.charCodeAt(0)) {
        } else if (e.which == 's'.charCodeAt(0)) {
            classify('spam', selectedID());
        } else if (e.which == 'h'.charCodeAt(0)) {
            classify('ham', selectedID());
        } else if (e.which == 'e'.charCodeAt(0)) {
            explain(selectedID());
        } else {
            alert('e.which: ' + e.which);
        }
    }

    function selectFirst () {
        $('.comment').first().addClass('selected');
    }

    $(document).ready(function () {
        $(document).keypress(keyDispatcher);
        setCounts();
        selectFirst();
    });

})();
