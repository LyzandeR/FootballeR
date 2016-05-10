$(function() {

    var $sidebar   = $(".well"), 
        $window    = $(window),
        offset     = $sidebar.offset(),
        topPadding = -30;

    $window.scroll(function() {
        if ($window.scrollTop() > offset.top + 70) {
            $sidebar.stop().animate({
                marginTop: $window.scrollTop() - offset.top + topPadding 
            });
        } else {
            $sidebar.stop().animate({
                marginTop: 0
            });
        }
    });
    
});
