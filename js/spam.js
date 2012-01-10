/*
 * Javascript for classifying spam.
 */

function classify (as, comment_id, div_id) {
    $.post("/comments/spam/classify", { as: as, comment_id: comment_id, div_id: div_id }, showMarked, "json");
}

function explain (comment_id, div_id) {
    $.post("/comments/spam/explain", { comment_id: comment_id, div_id: div_id }, explainer(div_id), "html");
}

function showMarked (data, textStatus) {
    $('#' + data.div_id).remove();
}

function explainer (div_id) {
    return function (data, textStatus) {
        $('#' + div_id).append($(data).click(function () { $(this).remove(); }));
    };
}
