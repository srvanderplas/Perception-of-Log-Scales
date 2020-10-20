function frame(i, strokecol="#336699", strokewidth = "5") {
  var panel = document.getElementById(i);
  panel.setAttribute("stroke-width", strokewidth);
  panel.setAttribute("stroke-opacity", "1");
  panel.setAttribute("stroke", strokecol);
  panel.setAttribute("frame", true);
}

function deframe(i, strokecol="#e5e5e5", strokewidth = 0) {
  var panel = document.getElementById(i);
  panel.setAttribute("stroke-width", strokewidth);
  panel.setAttribute("stroke-opacity", "0");
  panel.setAttribute("stroke", strokecol);
  panel.removeAttribute("frame");
}

function togglehigh(evt, i) {
  var panel = document.getElementById(i);
  if (panel.getAttribute("selected")) { 
    panel.removeAttribute("selected");
    panel.setAttribute("fill", tinycolor(panel.getAttribute("fill")).lighten(10));
  } else {
    panel.setAttribute("selected", true);
    panel.setAttribute("fill", tinycolor(panel.getAttribute("fill")).darken(10));
  }
  writeSelectedPanels(evt, i);
}

function writeSelectedPanels(evt, i, shinyid = "response_no"){
  var x=evt.currentTarget;
  var plot = x.id; 

   var responses = document.getElementById(shinyid).value;
   if (responses.length < 1) {
      responses = plot ;
      } 
   else {
      var response_vector = responses.split(",");
      var notSelectedBefore = true;
      for (var i =0; i< response_vector.length; i++){
         if(response_vector[i]==plot){
            response_vector.splice(i,1);
            responses = response_vector.join(",");
            notSelectedBefore = false;
         }
      }
      if(notSelectedBefore) responses = responses + ","+ plot ;
   }
   Shiny.onInputChange(shinyid, responses);
   document.getElementById(shinyid).value = responses;
}