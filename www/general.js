$(document).ready(function() {
  $("#search_tweets").addClass("btn-primary").removeClass("btn-default");
  $("#update_cloud").addClass("btn-primary").removeClass("btn-default");
  
  $("#search_tweets").click(function() {
    $("a:contains(Word Cloud)").click();
  });
});
