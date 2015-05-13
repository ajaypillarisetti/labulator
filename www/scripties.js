(function ($) {
  $(document).ready(function() {
    // inelegant, shameful, inelegant!
    $("#hide_ce").click(function () {
      if ($('#allDataTable').is(":visible")) {
        $(this).html($(this).html().replace(/hide/, 'show'));
      } else {
        $(this).html($(this).html().replace(/show/, 'hide'));
      }
       $("#allDataTable").slideToggle("fast");
    });
    $("#hide_summary").click(function () {
      if ($('#summaryTable').is(":visible")) {
       $(this).html($(this).html().replace(/hide/, 'show'));
      } else {
       $(this).html($(this).html().replace(/show/, 'hide'));
      }
      $("#summaryTable").slideToggle("fast");
    }); 
    $("#hide_nights").click(function () {
      if ($('#nightTable').is(":visible")) {
       $(this).html($(this).html().replace(/hide/, 'show'));
      } else {
       $(this).html($(this).html().replace(/show/, 'hide'));
      }
      $("#nightTable").slideToggle("fast");
    });       
  });
})(jQuery);