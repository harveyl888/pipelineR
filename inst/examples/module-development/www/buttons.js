 /* button on / off */

 $(document).ready(function() {

 Shiny.addCustomMessageHandler("disableButton", function(x) {
    $("#" + x.button).prop("disabled", x.disabled) });

});
